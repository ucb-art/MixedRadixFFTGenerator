package dspblocks.fft

import chisel3._
import chisel3.experimental._
import dsptools.numbers._
import dsptools.numbers.implicits._
import barstools.tapeout.transforms._
import chisel3.util._
import dsptools.{hasContext, DspContext}

object FFTNormalization {
  def getNormalizationBits(ffastParams: FFASTParams): Int = {
    // TODO: Don't take max of sub FFTns in case they cross power of 2 boundaries
    val minSubFFT = ffastParams.subFFTns.min
    val maxSubFFT = ffastParams.subFFTns.max
    val subFFTBitLengthTemp = BigInt(maxSubFFT).bitLength
    val subFFTBitLengthPow2 = 1 << subFFTBitLengthTemp
    val subFFTBitLengthPow2M1 = subFFTBitLengthPow2 / 2

    if (maxSubFFT < subFFTBitLengthPow2 && minSubFFT > subFFTBitLengthPow2M1)
      subFFTBitLengthTemp - 1
    else
      throw new Exception("Unsupported!")  
  }

  def getNormalizedDataType[T <: Data:RealBits](dspDataType: => T, ffastParams: FFASTParams): T = {
    val subFFTBitLength = getNormalizationBits(ffastParams)
    val temp = dspDataType match {
      case r: DspReal => 
        r
        // WARNING: Need to case out in code
      case f: FixedPoint =>
        require(f.binaryPoint.known)
        require(f.widthKnown)
        val fpWidth = f.getWidth
        val fpBP = f.binaryPoint.get
        // Need to renormalize FFT output by FFT value. Accomplish part of this by 
        // "right shifting", but keeping same # of bits. 
        // i.e. instead of divide by 800, "shift" by 9 (/512) and then multiply by (512/800)
        val newFpBP = fpBP + subFFTBitLength
        require(fpWidth > newFpBP)
        FixedPoint(fpWidth.W, newFpBP.BP)
      case _ =>
        throw new Exception("Invalid data type!")
    }
    temp.asInstanceOf[T]
  }

  def fromMainMemory[T <: Data:RealBits](from: DspComplex[T], to: DspComplex[T], ffastParams: FFASTParams): DspComplex[T] = {
    val newR = fromMainMemory(from.real, to.real, ffastParams)
    val newI = fromMainMemory(from.imag, to.imag, ffastParams)
    val newComplex = Wire(DspComplex(newR.cloneType))
    newComplex.real := newR
    newComplex.imag := newI
    newComplex
  }
  def fromMainMemory[T <: Data:RealBits](from: T, to: T, ffastParams: FFASTParams): T = {
    val subFFTBitLength = getNormalizationBits(ffastParams) 
    val outTemp = (from, to) match {
      case (fromR: DspReal, toR: DspReal) =>
        val pow2Normalization = 1 << subFFTBitLength
        fromR / DspReal(pow2Normalization.toDouble)
      case (fromF: FixedPoint, toF: FixedPoint) =>
        require(fromF.binaryPoint.known)
        require(toF.binaryPoint.known)
        require((toF.binaryPoint.get - fromF.binaryPoint.get) == subFFTBitLength)
        to.fromBits(from.asUInt)
    }
    outTemp.asInstanceOf[T]
  }

  def toMainMemory[T <: Data:RealBits](from: DspComplex[T], to: DspComplex[T], ffastParams: FFASTParams): DspComplex[T] = {
    val newR = toMainMemory(from.real, to.real, ffastParams)
    val newI = toMainMemory(from.imag, to.imag, ffastParams)
    val newComplex = Wire(DspComplex(newR.cloneType))
    newComplex.real := newR
    newComplex.imag := newI
    newComplex
  }
  def toMainMemory[T <: Data:RealBits](from: T, to: T, ffastParams: FFASTParams): T = {
    val subFFTBitLength = getNormalizationBits(ffastParams) 
    val outTemp = (from, to) match {
      case (fromR: DspReal, toR: DspReal) =>
        val pow2Normalization = 1 << subFFTBitLength
        fromR * DspReal(pow2Normalization.toDouble)
      case (fromF: FixedPoint, toF: FixedPoint) =>
        require(fromF.binaryPoint.known)
        require(toF.binaryPoint.known)
        require((fromF.binaryPoint.get - toF.binaryPoint.get) == subFFTBitLength)
        to.fromBits(from.asUInt)
    }
    outTemp.asInstanceOf[T]
  }
}

object DelayOptimization {
  def apply[T <: Data:RealBits](dspDataType: => T, ffastParams: FFASTParams): T = {
    val timingResolutionBits = 11
    // TODO: Something more reasonable
    val maxDelayBits = BigInt(ffastParams.adcDelays.max * 8).bitLength + 1
    val delayTypeTemp = dspDataType match {
      case r: DspReal => 
        r
      case f: FixedPoint =>
        val delayTypeWidth = maxDelayBits + timingResolutionBits
        FixedPoint(delayTypeWidth.W, timingResolutionBits.BP)
    }
    delayTypeTemp.asInstanceOf[T]
  }
}

class PeelingSCR[T <: Data:RealBits](dspDataType: => T, ffastParams: FFASTParams) extends SCRBundle {
  val maxSubFFT = ffastParams.subFFTns.max
  val k = ffastParams.k
  val n = ffastParams.fftn

  // TODO: Generalize?
  // TODO: Don't hard code ; overprovision

  val zeroThresholdPwr = CustomIndexedBundle(
    Input(FFTNormalization.getNormalizedDataType(dspDataType, ffastParams)), ffastParams.subFFTns)
  val sigThresholdPwr = CustomIndexedBundle(
    Input(FFTNormalization.getNormalizedDataType(dspDataType, ffastParams)), ffastParams.subFFTns)
  val delayCalibration = CustomIndexedBundle(
    CustomIndexedBundle(Input(DelayOptimization(dspDataType, ffastParams)), ffastParams.adcDelays), ffastParams.subFFTns)

  val ffastOutRIdx = Input(UInt(range"[0, $k)"))
  val ffastOutVal = Output(FFTNormalization.getNormalizedDataType(dspDataType, ffastParams))
  val ffastOutBin = Output(UInt(range"[0, $n)"))
  val ffastREFromCPU = Input(Bool())
  val ffastREToCPU = Output(Bool())

  val ffastOutNumFound = Output(ffastOutRIdx.cloneType)

  // DEBUG
  val cbTopPointer = new CustomIndexedBundle(ffastParams.subFFTns.map(subn => subn -> Output(UInt(range"[0, $subn)"))): _*)
  val cbBottomPointer = Output(cbTopPointer.cloneType)
  val currentStageInitialBufferLength = Output(cbTopPointer.cloneType)

  val cbRIdxFromCPU = Input(UInt(range"[0, $maxSubFFT)"))
  val cbSubBinToCPU = Output(cbTopPointer.cloneType)
  val cbREFromCPU = Input(Bool())
  val cbREToCPU = Output(Bool())

  override def cloneType = (new PeelingSCR(dspDataType, ffastParams)).asInstanceOf[this.type]
}

class PeelingIO[T <: Data:RealBits](dspDataType: => T, ffastParams: FFASTParams, subFFTnsColMaxs: Map[Int, Seq[Int]]) extends SCRBundle {
  val peelScr = new PeelingSCR(dspDataType, ffastParams)
  val clk = Input(Clock())
  val stateInfo = new StateTransitionIO
  // Special IO to indicate populating non-zerotons (as opposed to actual peeling) (should go high with corresponding start indicator)
  val resetPeel = Input(Bool())

  // Not normalized! Only access 1 at a time
  val dataToMemory = Flipped(FFASTMemInputLanes(dspDataType, ffastParams))
  val dataFromMemory = Flipped(FFASTMemOutputLanes(dspDataType, ffastParams))
  val idxToBankAddr = Flipped(new SubFFTIdxToBankAddrLUTsIO(subFFTnsColMaxs))

  override def cloneType = (new PeelingIO(dspDataType, ffastParams, subFFTnsColMaxs)).asInstanceOf[this.type]
}

// TODO: Hard coded idx to bank addr delay 
@chiselName
class Peeling[T <: Data:RealBits](dspDataType: => T, ffastParams: FFASTParams, subFFTnsColMaxs: Map[Int, Seq[Int]], memOutDelay: Int) extends Module with hasContext {

  // TODO: Don't hard code
  val idxToBankAddrDelay = 1
  val peelingDelay = idxToBankAddrDelay + memOutDelay + 5
  val initialSearchDelay = idxToBankAddrDelay + memOutDelay + context.numMulPipes + 2

  val io = IO(new PeelingIO(dspDataType, ffastParams, subFFTnsColMaxs))

  FFASTMemOutputLanes.connectToDefault(io.dataFromMemory, ffastParams)
  FFASTMemInputLanes.connectToDefault(io.dataToMemory, ffastParams)

  // Everything done in this stage is normalized!
  val dataFromMemory = Wire(CustomIndexedBundle(CustomIndexedBundle(
    DspComplex(FFTNormalization.getNormalizedDataType(dspDataType, ffastParams)), ffastParams.adcDelays), ffastParams.subFFTns))
  val dataToMemory = Wire(dataFromMemory.cloneType)
  ffastParams.getSubFFTDelayKeys.map { case (n, ph) => 
    // Only first lane is used
    FFTNormalization.fromMainMemory(from = io.dataFromMemory(n)(ph)(0).dout, to = dataFromMemory(n)(ph), ffastParams) 
    FFTNormalization.toMainMemory(from = dataToMemory(n)(ph), to = io.dataToMemory(n)(ph)(0).din, ffastParams) 
  }

  val done = Wire(Bool())
  io.stateInfo.done := done

  // TODO: Probably should separate out
  withClock(io.clk) {
    
    // TODO: Redundant
    // TODO: Can combine? Peeling + initial search
    // Reset has precedence
    val isInitialSearch = withReset(io.resetPeel) { RegEnable(next = false.B, init = true.B, enable = done) }
    val normalPeel = io.stateInfo.inState & ~isInitialSearch

    val maxSubFFT = ffastParams.subFFTns.max
    val initialCount = Wire(UInt(range"[0, $maxSubFFT)"))
    val isMaxInitialCount = initialCount === (maxSubFFT - 1).U
    val initialCountEn = ~isMaxInitialCount
    initialCount := withReset(io.resetPeel) { RegEnable(next = initialCount + 1.U, init = 0.U, enable = initialCountEn) }

    // Technically, done should go high on last valid clk cycle to indicate transition
    // However, "doneNoDelay" goes high on the cycle after that -- one less pipeline should be used
    val initialSearchDoneNoDelay = withReset(io.resetPeel) { RegNext(isMaxInitialCount, init = false.B) } 
    val initialSearchDone = withReset(io.resetPeel) {
      ShiftRegister(
        in = initialSearchDoneNoDelay, 
        n = initialSearchDelay - 1, 
        resetData = false.B, 
        en = isInitialSearch
      )  
    }

    val initialRE = ffastParams.subFFTns.map { case n =>
      // TODO: Parameterize
      // Match Idx -> Bank Addr delay
      val re = withReset(io.resetPeel) {
        if (n == ffastParams.subFFTns.max) 
          RegNext(~initialSearchDoneNoDelay, init = false.B)
        else 
          RegNext(initialCount < n, init = false.B)
      }
      re.suggestName(s"initialRE$n")
      n -> re
    }.toMap

    val initialWE = ffastParams.subFFTns.map { case n => 
      val we = withReset(io.resetPeel) {
        ShiftRegister(
          in = initialRE(n),
          n = initialSearchDelay - idxToBankAddrDelay,
          resetData = false.B,
          en = isInitialSearch
        )
      }
      n -> we
    }.toMap

    val currentSubFFTUpdate = Wire(Bool())

    // In initial: increment bottom every time non-zeroton detected
    // In normal peeling: 
    // B > T: length = B - T + 1
    // B < T: length = length @ end of previous peeling iteration + (B - T) + 1
    // Check length @ end
    // TODO: Update
    val cbLengthNext = ffastParams.subFFTns.map { case n =>
      val o = Wire(UInt(range"[0, $n]")) 
      o.suggestName(s"cbLengthNext$n") 
      n -> o
    }.toMap

    val cbLength = ffastParams.subFFTns.map { case n =>
      val o = withReset(io.resetPeel) { 
        RegEnable(next = cbLengthNext(n), init = 0.U, enable = currentSubFFTUpdate | initialSearchDone)
      }
      o.suggestName(s"cbLength$n") 
      n -> o
    }.toMap

    val idxToSubFFTMap = ffastParams.subFFTns.zipWithIndex.map { case (n, idx) => idx -> n }.toMap

    val stallCount = Wire(UInt(range"[0, $peelingDelay]"))
    val stallMaxed = stallCount === peelingDelay.U

    val currentSubFFTBools = ffastParams.subFFTns.zipWithIndex.map { case (n, idx) => 
      val o = withReset(io.stateInfo.start) { 
        if (idx == 0) RegInit(init = true.B)
        else RegInit(init = false.B)
      }
      o.suggestName(s"currSubFFTBool$n")
      n -> o
    }.toMap

    val currentSubFFTCBlen = Mux1H(currentSubFFTBools.toSeq.map(_._2).zip(cbLength.toSeq.map(_._2)))
    require(currentSubFFTBools.toSeq.map(_._1) == cbLength.toSeq.map(_._1), "Must be ordered!")

    ffastParams.subFFTns.zipWithIndex.map { case (n, idx) =>
      // TODO: ScanLeft?
      val next = if (idx == 0) false.B else currentSubFFTBools.toSeq(idx - 1)._2
      currentSubFFTBools(n) := Mux(currentSubFFTUpdate, next, currentSubFFTBools(n))
    }

    val peelingCount = Wire(UInt(range"[0, $maxSubFFT)"))
    val isMaxPeelingCount = peelingCount === (currentSubFFTCBlen - 1.U)

    // Note: Also where you update new length for next iteration
    currentSubFFTUpdate := isMaxPeelingCount & (stallCount === (peelingDelay - 1).U)

    peelingCount := withReset(io.stateInfo.start | currentSubFFTUpdate) { 
      RegEnable(next = peelingCount + 1.U, init = 0.U, enable = normalPeel & ~isMaxPeelingCount) 
    }

    val stallCountNext = Mux(stallMaxed, 0.U, stallCount + 1.U)
    stallCount := withReset(io.stateInfo.start) { RegEnable(stallCountNext, init = peelingDelay.U, enable = normalPeel & isMaxPeelingCount) }

    // Say circular buffer lengths are 3 (first FFT) and 4 (second FFT), and you have a pipeline delay of 3
    // Peel Count     StallCount
    // 0              3
    // 1              3
    // 2              3
    // --
    // 2              0 -- stall   data out 0 valid
    // 2              1 -- stall   data out 1 valid
    // 2              2 -- stall        --> Save new circular buffer length, etc. (end of first FFT) ; data out 2 valid
    // --
    // 0              3
    // 1              3
    // 2              3
    // 3              3            data out 0 valid
    // --
    // 3              0 -- stall   data out 1 valid
    // 3              1 -- stall   data out 2 valid
    // 3              2 -- stall        --> Save new circular buffer length, etc. (end of second FFT) ; data out 3 valid

    // Already accounts for pipeline delay
    val peelingIterDone = currentSubFFTUpdate & currentSubFFTBools.toSeq.last._2
   
    done := Mux(isInitialSearch, initialSearchDone, peelingIterDone)


















    val fftOutNormalized = ffastParams.getSubFFTDelayKeys.map { case (n, ph) => 
      val in = dataFromMemory(n)(ph)
      val pwr2 = 1 << FFTNormalization.getNormalizationBits(ffastParams)
      val normalizationFactor = in.fromDouble(pwr2.toDouble / n)
      val norm = dataFromMemory(n)(ph) context_* normalizationFactor
      norm.suggestName(s"fftOutNormalized_$n_$ph")
      (n, ph) -> norm
    }


    // in general in















val initialRE = ffastParams.subFFTns.map { case n =>
      // TODO: Parameterize
      // Match Idx -> Bank Addr delay
      val re = withReset(io.stateInfo.start) {
        if (n == ffastParams.subFFTns.max) 
          RegNext(~initialSearchDoneNoDelay, init = false.B)
        else 
          RegNext(initialCount < n, init = false.B)
      }
      re.suggestName(s"initialRE$n")
      n -> re
    }.toMap





    ffastParams.subFFTns.map { case n => 
      // Delay 0
      io.idxToBankAddr.idxs(n) := initialCount
      // Delay 1 (LUT outputs registered)
      val bankAddr = io.idxToBankAddr.bankAddrs(n)

      ffastParams.adcDelays foreach { case ph =>
        io.dataFromMemory(n)(ph)(0).loc.addr := bankAddr.addr
        io.dataFromMemory(n)(ph)(0).loc.bank := bankAddr.bank
        io.dataFromMemory(n)(ph)(0).re := re
      }

      n -> 
    }









    val zerotonDetectors = ffastParams.subFFTns.map { case n =>
      val mod = Module(new Zeroton(dspDataType, ffastParams))
      mod.suggestName(s"zerotonDetector$n")
      mod.io.clk := io.clk
      mod.io.bin := 
      mod.io.zeroThresholdPwr := io.peelingScr.zeroTHresholdPwr(n)
    }












    val bottomPointers = ffastParams.subFFTns.map { case n => 
      val bPointer = Wire(UInt(range"[0, $n)")) 
      // TODO: Probably don't need subtract -- renormalize in other direction (store cbLength - 1)
      val currentCBMax = cbLength(n) - 1.U
      val bPointerIsMax = Mux(isInitialSearch, bPointer === (n - 1).U, currentCBMax == bPointer)
      val bPointerNext = Mux(bPointerIsMax, 0.U, bPointer + 1.U)
      // For initial search, enable when non zeroton detected
      val initialSearchEnable = ~zerotonDetectors(n).io.isZeroton & isInitialSearch
      val o = withReset(io.resetPeel) { 
        RegEnable(next = bPointerNext, init = 0.U, enable = )
      }
      o.suggestName(s"cbLength$n") 
      n -> o
    }















// CURRENT FFT -> PICK RIGHT BINS, PICK RIGHT THRESHOLDS
// for initial -> 3 in parallel


/*

class ZerotonIO[T <: Data:RealBits](dspDataType: => T, ffastParams: FFASTParams) extends Bundle {
  
  val isZeroton = Output(Bool())
  override def cloneType = (new ZerotonIO(dspDataType ffastParams)).asInstanceOf[this.type]
}  
*/


////////// WARNING PLACEHOLDER
    cbLengthNext foreach { case (n, o) =>
      if (n == 675) o := 10.U
      else if (n == 800) o := 11.U
      else o := 12.U
    }

  }

}








  







/*



// DIVIDE APPROPRIATELY


    





    io.dataToMemory(n)(ph)(0).we := RegNext(delayedValid & (~isMaxCount))

     
      
*/




/*

skiptoend

////////////////////////////////////// PEELING MEMORIES
  // TOOD: DON'T HARD CODE MEMORY EXCEPTIONS, name should be np
  val circularBuffers = ffastParams.subFFTns.map { case np =>
    val n = if (np == 675) 688 else np
    val mod = Module(new WriteBeforeReadMem(UInt(range"[0, $np)"), n, s"circBuffer_sram_$n"))
    mod.suggestName(s"cb_$np")
    mod.io.clk := globalClk
    np -> mod
  }.toMap
  // TODO: Normalized: Different fraction!
  val k = Seq(ffastParams.k, 1952).max
  val n = ffastParams.fftn
  val outIdxsMem = Module(new SMem1P(UInt(range"[0, $n)"), k, "ffastOutBinIdxs"))
  outIdxsMem.io.clk := globalClk
  val outValsMem = Module(new SMem1P(DspComplex(dspDataType), k, "ffastOutBinVals"))
  outValsMem.io.clk := globalClk
////////////////////////////////////// PEELING MEMORIES

*/


