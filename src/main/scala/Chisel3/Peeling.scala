package dspblocks.fft

import chisel3._
import chisel3.experimental._
import dsptools.numbers._
import dsptools.numbers.implicits._
import barstools.tapeout.transforms._
import chisel3.util._
import dsptools.{hasContext, DspContext}

// TODO: Note: Initial search step is kind of costly/extraneous

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
}

object DelayOptimization {
  def apply[T <: Data:RealBits](dspDataType: => T, ffastParams: FFASTParams, fullFraction: Boolean = false): T = {
    val timingResolutionBits = 8
    // TODO: Something more reasonable
    val maxDelayBits = BigInt(ffastParams.adcDelays.max * 4).bitLength + 1
    val delayTypeTemp = dspDataType match {
      case r: DspReal => 
        r
      case f: FixedPoint =>
        val delayTypeWidth = maxDelayBits + timingResolutionBits
        // TODO: Make less arbitrary?
        if (fullFraction)
          FixedPoint(delayTypeWidth.W, (delayTypeWidth - 2).BP)
        else
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
  // Threshold power * # delays
  val sigThresholdPwrMulDlys = CustomIndexedBundle(
    Input(FFTNormalization.getNormalizedDataType(dspDataType, ffastParams)), ffastParams.subFFTns)
  val delayCalibration = CustomIndexedBundle(
    CustomIndexedBundle(Input(DelayOptimization(dspDataType, ffastParams)), ffastParams.adcDelays), ffastParams.subFFTns)

  // TODO: Generalize, overprovisioning
  // Note: Calculate externally rather than wasting power
  val delayCalcConstants = CustomIndexedBundle(
    CustomIndexedBundle(Seq(
      Input(DelayOptimization(dspDataType, ffastParams)),
      Input(DelayOptimization(dspDataType, ffastParams)),
      Input(DelayOptimization(dspDataType, ffastParams, fullFraction = true))
    )),
    ffastParams.subFFTns
  )

  val ffastOutRIdx = Input(UInt(range"[0, $k)"))
  val ffastOutVal = Output(FFTNormalization.getNormalizedDataType(dspDataType, ffastParams))
  val ffastOutBin = Output(UInt(range"[0, $n)"))
  val ffastREFromCPU = Input(Bool())
  val ffastREToCPU = Output(Bool())

  val ffastOutNumFound = Output(ffastOutRIdx.cloneType)

  // DEBUG
  val cbLength = new CustomIndexedBundle(ffastParams.subFFTns.map(subn => subn -> Output(UInt(range"[0, $subn]"))): _*)

  val cbRIdxFromCPU = Input(UInt(range"[0, $maxSubFFT)"))
  // TODO: Overprovisions by + 1
  val cbSubBinToCPU = Output(cbLength.cloneType)
  val cbREFromCPU = Input(Bool())
  val cbREToCPU = Output(Bool())

  // Access to singleton estimator
  // Should be one hot
  val singletonEstCurrentFFTBools = Input(UInt(ffastParams.subFFTns.length.W))
  val singletonEstSubFFTIdx = Input(UInt(range"[0, $maxSubFFT)"))
  val singletonIns = CustomIndexedBundle(
    Input(DspComplex(FFTNormalization.getNormalizedDataType(dspDataType, ffastParams))), 
    ffastParams.adcDelays
  )

  val seBinType = new CustomBundle(ffastParams.binTypes.map(_ -> Output(Bool())): _*) 
  val seBinLoc = Output(UInt(range"[0, $n)"))
  val seBinSignal = Output(DspComplex(FFTNormalization.getNormalizedDataType(dspDataType, ffastParams)))
  val seRotatedOuts = CustomIndexedBundle(
    Output(DspComplex(FFTNormalization.getNormalizedDataType(dspDataType, ffastParams))), 
    ffastParams.adcDelays
  )

  val numSubFFTs = ffastParams.subFFTns.length
  val peelDebugFFTIdx = Input(UInt(range"[0, $numSubFFTs)"))

  val binMapBinLoc = Input(UInt(range"[0, $n)"))
  val binMapSubFFTIdx = Output(new CustomIndexedBundle(
     ffastParams.subFFTns.map(subFFT => subFFT -> UInt(range"[0, $subFFT)")): _*
  ))

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

  val skipToEnd = Output(Bool())

  override def cloneType = (new PeelingIO(dspDataType, ffastParams, subFFTnsColMaxs)).asInstanceOf[this.type]
}

// TODO: Hard coded idx to bank addr delay 
@chiselName
class Peeling[T <: Data:RealBits](dspDataType: => T, ffastParams: FFASTParams, subFFTnsColMaxs: Map[Int, Seq[Int]], memOutDelay: Int) extends Module with hasContext {

  // TODO: Move back to normalization, clean up extra wires

  def fromMainMemory[T <: Data:RealBits](from: DspComplex[T], to: DspComplex[T], ffastParams: FFASTParams): Unit = {
    val newR = fromMainMemoryCast(from.real, to.real, ffastParams)
    val newI = fromMainMemoryCast(from.imag, to.imag, ffastParams)
    val newComplex = Wire(DspComplex(newR.cloneType))
    newComplex.real := newR
    newComplex.imag := newI
    newComplex.suggestName("newComplex_fromMainMemory")
    to := newComplex
  }
  def fromMainMemoryCast[T <: Data:RealBits](from: T, to: T, ffastParams: FFASTParams): T = {
    val subFFTBitLength = FFTNormalization.getNormalizationBits(ffastParams) 
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

  def toMainMemory[T <: Data:RealBits](from: DspComplex[T], to: DspComplex[T], ffastParams: FFASTParams): Unit = {
    val newR = toMainMemory(from.real, to.real, ffastParams)
    val newI = toMainMemory(from.imag, to.imag, ffastParams)
    val newComplex = Wire(DspComplex(newR.cloneType))
    newComplex.real := newR
    newComplex.imag := newI
    newComplex.suggestName("newComplex_toMainMemory")
    to := newComplex
  }
  def toMainMemory[T <: Data:RealBits](from: T, to: T, ffastParams: FFASTParams): T = {
    val subFFTBitLength = FFTNormalization.getNormalizationBits(ffastParams) 
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

  // TODO: Don't hard code
  val idxToBankAddrDelay = 1
  val zerotonDelay = 2
  val binToSubFFTIdxDelay = 2     // TODO: Note doesn't include idx to bank addr block! (just 2 muls for Barrett)
  val peelSubDelay = 9
  val initialSearchDelay = idxToBankAddrDelay + memOutDelay + context.numMulPipes + zerotonDelay

  // TODO: Don't hard code
  val earlyLocToPeelEndDelay = 14
  val locToAvgSignalDelay = 11
  require(earlyLocToPeelEndDelay - (binToSubFFTIdxDelay + idxToBankAddrDelay + memOutDelay) == peelSubDelay)

  // CB Memory -> Idx to Bank Address -> Main Memory -> Is Zeroton?
  //                                                                -> Singleton Estimator (loc, val, isSingle) -> Bin To Sub FFT Idx -> Idx to Bank Address (others) -> Main Memory (others) -> Peel Subtraction
  //                                                                                                                                                                                          -> Zeroton (others)
  // DON'T HARD CODE
  val peelingDelayPartsT = Seq(
    "cb1" -> memOutDelay, 
    "idxToBA1" -> idxToBankAddrDelay, 
    "mainMem1" -> memOutDelay, 
    "seToLoc" -> SingletonEstimatorDelays.toLocDelay.sum, 
    "binToSubFFTIdx" -> binToSubFFTIdxDelay, 
    "idxToBA2" -> idxToBankAddrDelay, 
    "mainMem2" -> memOutDelay, 
    "peelEnd" -> peelSubDelay)                                      // Should be longer than zeroton estimator delay
  
  peelingDelayPartsT foreach { case (name, amount) =>
    println(s"Delay $name : $amount")
  }

  val peelingDelayParts = peelingDelayPartsT.map(_._2)
  val peelingDelayInputMap = peelingDelayPartsT.toMap

  val peelingDelay = peelingDelayParts.sum

  val io = IO(new PeelingIO(dspDataType, ffastParams, subFFTnsColMaxs))

  FFASTMemOutputLanes.connectToDefault(io.dataFromMemory, ffastParams)
  FFASTMemInputLanes.connectToDefault(io.dataToMemory, ffastParams)

  // Everything done in this stage is normalized!
  val dataFromMemory = Wire(CustomIndexedBundle(CustomIndexedBundle(
    DspComplex(FFTNormalization.getNormalizedDataType(dspDataType, ffastParams)), ffastParams.adcDelays), ffastParams.subFFTns))
  val dataToMemory = Wire(dataFromMemory.cloneType)
  ffastParams.getSubFFTDelayKeys.map { case (n, ph) => 
    // Only first lane is used
    fromMainMemory(from = io.dataFromMemory(n)(ph)(0).dout, to = dataFromMemory(n)(ph), ffastParams) 
    toMainMemory(from = dataToMemory(n)(ph), to = io.dataToMemory(n)(ph)(0).din, ffastParams) 
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
    // Not delayed!
    initialCount := withReset(io.resetPeel) { RegEnable(next = initialCount + 1.U, init = 0.U, enable = initialCountEn) }

    // Technically, done should go high on last valid clk cycle to indicate transition
    // However, "doneNoDelay" goes high on the cycle after that -- one less pipeline should be used
    // TODO: isInitialSearch necessary???
    val initialSearchDoneNoDelay = withReset(io.stateInfo.start) { RegEnable(next = isMaxInitialCount, init = false.B, enable = isInitialSearch) } 
    val initialSearchDone = Wire(Bool())
    // TODO: Clean up
    // Only pulse for 1 cycle!
    initialSearchDone := withReset(~io.stateInfo.inState | initialSearchDone) {
      ShiftRegister(
        in = initialSearchDoneNoDelay & isInitialSearch, 
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

    val subFFTPeelDoneIdxed = Wire(Vec(ffastParams.subFFTns.length, Bool()))

    // TODO: GET RID OF EXTRA STALL CYCLES WHEN THINGS ARE DONE! 

    val currentSubFFTBools = ffastParams.subFFTns.zipWithIndex.map { case (n, idx) => 
      val o = withReset(io.stateInfo.start) { 
        if (idx == 0) RegInit(init = true.B)
        else RegInit(init = false.B)
      }
      o.suggestName(s"currSubFFTBool$n")
      n -> o
    }.toMap

    val updateCBLength = ffastParams.subFFTns.map { case n =>
      // Initial search --> checks things in parallel
      // Normal peeling -> checks sub FFTs one by one
      val o = (currentSubFFTUpdate & currentSubFFTBools(n)) | initialSearchDone
      o.suggestName(s"updateCBLength$n")
      n -> o
    }.toMap

    val subFFTPeelDone = ffastParams.subFFTns.map { case n =>
      n -> Wire(Bool())
    }.toMap

    val cbLength = ffastParams.subFFTns.map { case n =>
      val o = withReset(io.resetPeel) { 
        val newNext = Mux(subFFTPeelDone(n), 0.U, cbLengthNext(n))
        RegEnable(next = newNext, init = 0.U, enable = updateCBLength(n) | subFFTPeelDone(n))
      }
      o.suggestName(s"cbLength$n") 
      io.peelScr.cbLength(n) := o
      n -> o
    }.toMap

    val idxToSubFFTMap = ffastParams.subFFTns.zipWithIndex.map { case (n, idx) => idx -> n }.toMap

    val stallCount = Wire(UInt(range"[0, $peelingDelay]"))
    val stallMaxed = stallCount === peelingDelay.U

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
    // TODO: Not sure if normalPeel condition is necessary
    currentSubFFTUpdate := isMaxPeelingCount & (stallCount === (peelingDelay - 1).U) & normalPeel

    peelingCount := withReset(io.stateInfo.start | currentSubFFTUpdate) { 
      RegEnable(next = peelingCount + 1.U, init = 0.U, enable = normalPeel & ~isMaxPeelingCount) 
    }

    // 0 - input to circular buffer
    // 1 - input to index to bank/address
    val peelingPartsEnable = peelingDelayParts.scanLeft(stallMaxed) { case (accumDelay, nextShift) => 
      withReset(io.stateInfo.start) {
        ShiftRegister(
          in = accumDelay, 
          n = nextShift,
          resetData = false.B,
          en = normalPeel
        ) 
      }
    }
    // Note: last is referring to output (not input)
    val peelingPartsEnableAtIn = peelingDelayPartsT.map(_._1).zip(peelingPartsEnable.init).map { case (name, cond) =>
      val temp = Wire(Bool())
      temp := cond
      temp.suggestName(s"peelingPartsEnableAtIn_${name}")
      name -> cond
    }.toMap

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
      val normalizationFactor = in.real.fromDouble(pwr2.toDouble / n)
      val ir = Mux1H(Seq(isInitialSearch -> in.real))
      val ii = Mux1H(Seq(isInitialSearch -> in.imag))
      val normR = ir context_* normalizationFactor
      val normI = ii context_* normalizationFactor
      val norm = Wire(dataFromMemory(n)(ph).cloneType)
      norm.real := normR
      norm.imag := normI
      norm.suggestName(s"fftOutNormalized_${n}_${ph}")
      (n, ph) -> norm
    }.toMap

    val dataFromMemoryPeeling = Wire(dataFromMemory.cloneType)

    // TODO: Can probably get rid of several sets of registers
    ffastParams.getSubFFTDelayKeys.foreach { case (n, ph) => 
      // TODO: Match delay?
      dataFromMemoryPeeling(n)(ph) := Mux(isInitialSearch, fftOutNormalized(n, ph), dataFromMemory(n)(ph))
    }

    val isZeroton = ffastParams.subFFTns.map { case n =>
      val mod = Module(new Zeroton(dspDataType, ffastParams))
      mod.suggestName(s"zerotonDetector$n")
      mod.io.clk := io.clk
      // TODO: Prevent bad things from happening
      ffastParams.adcDelays.map { case d =>
        mod.io.bin(d) := Mux1H(Seq(io.stateInfo.inState -> dataFromMemoryPeeling(n)(d)))
      }
      mod.io.sigThresholdPwr := io.peelScr.sigThresholdPwrMulDlys(n)
      // TODO: Don't do require; set
      require(mod.moduleDelay == zerotonDelay)
      val isZeroton = mod.io.isZeroton
      isZeroton.suggestName("isZeroton$n")
      n -> isZeroton
    }.toMap

    // Should align with the write enables
    val initialCountDelayed = ShiftRegister(initialCount, initialSearchDelay)

    val isMultiton = Wire(Bool())
    val multitonUpdate = peelingPartsEnable.last & isMultiton
    // For normal peeling! --> only update current n
    val updateBottomPointers = ffastParams.subFFTns.map { case n =>
      // For initial search, enable when non zeroton detected
      // Should be lined up!
      val updateBP = Mux(isInitialSearch, ~isZeroton(n) & initialWE(n), currentSubFFTBools(n) & multitonUpdate)
      updateBP.suggestName(s"updateBP$n")
      n -> updateBP 
    }.toMap

    val bottomPointers = ffastParams.subFFTns.map { case n =>
      val bPointer = Wire(UInt(range"[0, $n]")) 
      bPointer.suggestName(s"bPointer$n")
      n -> bPointer
    }.toMap

    val cbEmpty = ffastParams.subFFTns.map { case n =>
      val cbE = bottomPointers(n) === n.U 
      cbE.suggestName(s"cbEmpty$n")
      n -> cbE
    }.toMap

    val bottomPointersNext = ffastParams.subFFTns.map { case n =>
      // TODO: Probably don't need subtract -- renormalize in other direction (store cbLength - 1)
      val currentCBMax = cbLength(n) - 1.U
      // Should not have to worry about wrapping in initialization code
      val bPointerIsMax = Mux(normalPeel, bottomPointers(n) === currentCBMax, bottomPointers(n) === n.U)        
      val bPointerNext = Mux(bPointerIsMax, 0.U, bottomPointers(n) + 1.U)
      bPointerNext.suggestName(s"bpNext$n")
      n -> bPointerNext
    }.toMap

    ffastParams.subFFTns.foreach { case n => 
      bottomPointers(n) := withReset(io.resetPeel) { 
        RegEnable(next = bottomPointersNext(n), init = n.U, enable = updateBottomPointers(n))
      }
    }

    // Only allow SCR reading in debug
    io.peelScr.cbREToCPU := withReset(io.stateInfo.done) {
      ShiftRegister(io.peelScr.cbREFromCPU, memOutDelay, resetData = false.B, en = true.B)
    }

    val cbraddr = Mux(normalPeel, peelingCount, io.peelScr.cbRIdxFromCPU)

    ////////////////////////////////////// PEELING MEMORIES
    // TOOD: DON'T HARD CODE MEMORY EXCEPTIONS, name should be np
    val circularBuffersSubFFTIdx = ffastParams.subFFTns.map { case np =>
      val n = if (np == 675) 688 else np
      val mod = Module(new WriteBeforeReadMem(UInt(range"[0, $np)"), n, s"circBuffer_sram_$n"))
      mod.suggestName(s"cb_$np")
      mod.io.waddr := bottomPointersNext(np)
      mod.io.we := updateBottomPointers(np)
      mod.io.clk := io.clk
      // In normal peeling mode, if the sub FFT bin tested was a multiton, put its index back into the 
      // circular buffer. In initial search mode, if the (sequentially searched) sub FFT bin is 
      // not a zero-ton, add to the circular buffer
      // Register data out (not just address -- to be consistent with delay #'s)
      val registeredOut = ShiftRegister(mod.io.dout, 1)
      val delayedSubFFTIdx = ShiftRegister(registeredOut, peelingDelayParts.tail.sum)

      mod.io.din := Mux(normalPeel, delayedSubFFTIdx, initialCountDelayed)

      // Only read in normal peeling mode OR debug
      mod.io.raddr := cbraddr
      mod.io.re := Mux(normalPeel, peelingPartsEnable.head & currentSubFFTBools(np), io.peelScr.cbREFromCPU)
      // TODO: Separate out normal, initial, debug states
      io.peelScr.cbSubBinToCPU(np) := Mux1H(Seq(~io.stateInfo.inState -> registeredOut)) 
      // Note: By default, MemWrapper doesn't register output
      np -> registeredOut
    }.toMap

    val lengthUnchanged = ffastParams.subFFTns.map { case n =>
      val lengthUnchangedN = cbLengthNext(n) === cbLength(n)  
      lengthUnchangedN.suggestName(s"lengthUnchanged$n")
      n -> lengthUnchangedN
    }.toMap

    // Done --> circular buffer length doesn't change between iterations
    // Note that you must also consider whether the bottom pointer should be updated, since it's possible
    // that all entries are multitons -> bottom pointer keeps moving until it's back to its original place [i.e. peeler is stuck]
    ffastParams.subFFTns.zipWithIndex.foreach { case (n, idx) =>
      val o = withReset(io.resetPeel) {
        RegEnable(next = true.B, init = false.B, enable = lengthUnchanged(n) & ~updateBottomPointers(n) & updateCBLength(n))
      }
      subFFTPeelDoneIdxed(idx) := o
      o.suggestName(s"subFFTPeelDone$n")
      subFFTPeelDone(n) := o
    }

    ffastParams.subFFTns.foreach { case n =>
      val temp = Mux(updateBottomPointers(n), bottomPointersNext(n), bottomPointers(n))
      // TODO: Reundant condition (?)
      cbLengthNext(n) := Mux(temp === n.U, 0.U, temp + 1.U)
    }

    val subFFTStuckAtEnd = ffastParams.subFFTns.zipWithIndex.map { case (n, idx) =>
      val o = Wire(Bool())
      if (idx == ffastParams.subFFTns.length - 1) {
        // Always updating last sub FFT CB @ last cycle of state
        o := lengthUnchanged(n) & updateBottomPointers(n)
      }
      else {
        // Note: for initial search: If it should be non-empty, length will definitely have changed (not stuck)
        // If length hasn't changed, it's b/c it's empty (update pointer = false)
        o := RegEnable(next = lengthUnchanged(n) & updateBottomPointers(n), enable = updateCBLength(n))
      }
      o.suggestName(s"subFFTStuckAtEnd$n")
      n -> o
    }.toMap

    val skipToEnd = withReset(io.resetPeel) { 
      val anyDone = subFFTPeelDone.map(_._2).reduce(_ | _)
      val allStuck = subFFTStuckAtEnd.map(_._2).reduce(_ & _)
      val skip = anyDone | allStuck
      RegEnable(next = true.B, init = false.B, enable = skip)
    }

    io.skipToEnd := skipToEnd

    // TODO: ZEROTON -- doesn't write, doesn't change bottom counter BUT SHOULD ALSO NOT WASTE CYCLES STALLING
   
// ------------------------------------------------------------------

    val singletonEstCurrentFFTBools = ffastParams.subFFTns.zipWithIndex.map { case (n, idx) =>
      n -> io.peelScr.singletonEstCurrentFFTBools(idx)
    }.toMap   

    val currentSubFFTBoolsFinal = ffastParams.subFFTns.map { case n =>
      n -> Mux(normalPeel, currentSubFFTBools(n), singletonEstCurrentFFTBools(n))
    }

    val subFFTInverse = Wire(CustomIndexedBundle(FFTNormalization.getNormalizedDataType(dspDataType, ffastParams), ffastParams.subFFTns))
    ffastParams.subFFTns foreach { n =>
      subFFTInverse(n) := subFFTInverse(n).fromDouble(1.toDouble / n)
    }

    val currentSubFFT = Mux1H(currentSubFFTBoolsFinal.map { case (n, isN) => isN -> n.U } )
    val currentSubFFTIdx = Mux1H(currentSubFFTBoolsFinal.map { case (n, isN) => isN -> circularBuffersSubFFTIdx(n) } )
    val currentSubFFTInverse = Mux1H(currentSubFFTBoolsFinal.map { case (n, isN) => isN -> subFFTInverse(n) } )
    
    // TODO: This is to prevent weirdness; maybe not needed
    val currentDelays = ffastParams.adcDelays.map { d => 
      d -> Mux1H(currentSubFFTBoolsFinal.map { case (n, isN) => isN -> io.peelScr.delayCalibration(n)(d) } )
    }.toMap

    val currentFFTInsT = ffastParams.adcDelays.map { d => 
      d -> Mux1H(currentSubFFTBoolsFinal.map { case (n, isN) => isN -> dataFromMemory(n)(d) } )
    }.toMap
    val currentFFTIns = ffastParams.adcDelays.map { d =>
      d -> Mux(normalPeel, currentFFTInsT(d), io.peelScr.singletonIns(d))
    }.toMap

    // TODO: Don't do this
    val singletonEstimator = Module(new SingletonEstimator(dspDataType, ffastParams.copy(delays = FFASTTopParams.delays)))
    singletonEstimator.io.clk := io.clk
    // Need to match data delay
    singletonEstimator.io.subFFTIdx := ShiftRegister(Mux(normalPeel, currentSubFFTIdx, io.peelScr.singletonEstSubFFTIdx), idxToBankAddrDelay + memOutDelay)
    singletonEstimator.io.subFFT := currentSubFFT 
    singletonEstimator.io.subFFTInverse := currentSubFFTInverse
    
    ffastParams.adcDelays foreach { case d =>
      singletonEstimator.io.delays(d) := currentDelays(d)
      singletonEstimator.io.delayedIns(d) := currentFFTIns(d)
    }

    val currentZeroThresholdPwr = Mux1H(currentSubFFTBoolsFinal.map { case (n, isN) => isN -> io.peelScr.zeroThresholdPwr(n) } )
    val currentSigThresholdPwr = Mux1H(currentSubFFTBoolsFinal.map { case (n, isN) => isN -> io.peelScr.sigThresholdPwr(n) } )
    val currentSigThresholdPwrMulDlys = Mux1H(currentSubFFTBoolsFinal.map { case (n, isN) => isN -> io.peelScr.sigThresholdPwrMulDlys(n) } )
    val currentDelayCalcConstants = ffastParams.delays.zipWithIndex.map { case (dset, idx) =>
      idx -> Mux1H(currentSubFFTBoolsFinal.map { case (n, isN) => isN -> io.peelScr.delayCalcConstants(n)(idx) } )
    }.toMap

    singletonEstimator.io.zeroThresholdPwr := currentZeroThresholdPwr
    singletonEstimator.io.sigThresholdPwr := currentSigThresholdPwr
    singletonEstimator.io.sigThresholdPwrMulDlys := currentSigThresholdPwrMulDlys

    ffastParams.delays.zipWithIndex.map { case (dset, idx) =>
      singletonEstimator.io.delayCalcConstants(idx) := currentDelayCalcConstants(idx)
    }    

    val notPeeling = ~io.stateInfo.inState

    io.peelScr.seBinType := Mux1H(Seq(notPeeling -> singletonEstimator.io.binType)) 
    io.peelScr.seBinLoc := Mux1H(Seq(notPeeling -> singletonEstimator.io.binLoc))   
    io.peelScr.seBinSignal := Mux1H(Seq(notPeeling -> singletonEstimator.io.binSignal))
    isMultiton := singletonEstimator.io.binType("multi")   

    // TODO: Don't hack!!!
    val matchLocToSigOutDelay = 7
    val peelingREDelayMatch = ShiftRegister(peelingPartsEnableAtIn("mainMem2"), matchLocToSigOutDelay)
    val peelingLocDelayCompensation = ShiftRegister(singletonEstimator.io.binLocEarly, matchLocToSigOutDelay)
    // TODO: Don't hard code; double check
    // loc -> avg sig 11
    // loc -> sig out = 12
    // sig out -> pipeline end = 2 (mul + 1)
    // loc -> main mem out -> 5 (bin to sub FFT idx, idx to BA, mem out)
    // match loc -> mem out (5) with loc -> sig out (12)
    // -----> delay mem out path by 7 (raddr, re)

    // TODO: Wasteful: only need 2!
    val locToSubFFTIdxMod = Module(new BinToSubFFTIdx(ffastParams))
    locToSubFFTIdxMod.io.clk := io.clk 
    locToSubFFTIdxMod.io.fftBin := Mux(normalPeel, peelingLocDelayCompensation, io.peelScr.binMapBinLoc)
    io.peelScr.binMapSubFFTIdx := locToSubFFTIdxMod.io.subFFTIdx

    ffastParams.subFFTns.map { case n => 
      val peelingIndex = Mux(currentSubFFTBools(n), circularBuffersSubFFTIdx(n), locToSubFFTIdxMod.io.subFFTIdx(n))
      // Note: These are not delay matched, but that's OK since they're used in different states
      io.idxToBankAddr.idxs(n) := Mux(isInitialSearch, initialCount, peelingIndex)
      // Memory accessed at different time depending on if this is the current sub FFT you're looking into
      // vs. another sub FFT that you're peeling from due to results from this current FFT
      // TODO: Don't hack
      val peelingRE = Mux(currentSubFFTBools(n), peelingPartsEnableAtIn("mainMem1"), peelingREDelayMatch)
      val mainMemRE = Mux(isInitialSearch, initialRE(n), peelingRE)
      mainMemRE.suggestName(s"mainMemRE$n")

// ----------------------------------------------

      // Delay 1 (LUT outputs registered)
      val bankAddr = io.idxToBankAddr.bankAddrs(n)
      ffastParams.adcDelays foreach { case ph =>
        io.dataFromMemory(n)(ph)(0).loc.addr := bankAddr.addr
        io.dataFromMemory(n)(ph)(0).loc.bank := bankAddr.bank
        io.dataFromMemory(n)(ph)(0).re := mainMemRE
        val delayDeltaInit = initialSearchDelay - idxToBankAddrDelay
        val writeAddrInit = ShiftRegister(bankAddr.addr, delayDeltaInit)
        val writeBankInit = ShiftRegister(bankAddr.bank, delayDeltaInit)
        val delayDeltaNormal = peelingDelayInputMap("mainMem2") + peelingDelayInputMap("peelEnd")
        require(delayDeltaNormal > delayDeltaInit)
        // + 2 for sig out -> peeling end
        // TODO: Don't hard code!
        val writeAddrNorm = ShiftRegister(bankAddr.addr, memOutDelay + 2)
        val writeBankNorm = ShiftRegister(bankAddr.bank, memOutDelay + 2)
        require(zerotonDelay == 2)
        // Zeroton calculation should take the same time
        //val writeAddrNorm = ShiftRegister(writeAddrInit, delayDeltaNormal - delayDeltaInit)
        //val writeBankNorm = ShiftRegister(writeBankInit, delayDeltaNormal - delayDeltaInit)
        io.dataToMemory(n)(ph)(0).loc.addr := Mux(isInitialSearch, writeAddrInit, writeAddrNorm)
        io.dataToMemory(n)(ph)(0).loc.bank := Mux(isInitialSearch, writeBankInit, writeBankNorm) 
             
      }
    }

// ----------------------------------------------

    // Just to get outAngleType...
    val cordicParams = singletonEstimator.cordicParams
    // Need to be able to represent 1! (Int width >= 1; not including sign bit)
    val mag1 = Wire(DspComplex(cordicParams.outAngleType))
    mag1.real := cordicParams.outAngleType.fromDouble(1.0)
    mag1.imag := cordicParams.outAngleType.fromDouble(0.0)
    val locFixed = singletonEstimator.io.binLocEarly.asFixed.asInstanceOf[T] 

    // TODO: Unnecessary flip
    // TODO: How to generalize to > 3 Sub FFTs?
    val reverseSubFFTns = ffastParams.subFFTns.zipWithIndex.reverse
    val otherSubFFT1Idx = 
      reverseSubFFTns.tail.foldLeft(reverseSubFFTns.head._2.U) { case (accum, (nextn, idx)) => Mux(~currentSubFFTBools(nextn), idx.U, accum) }
    val otherSubFFT1NotN = reverseSubFFTns.map { case (n, idx) =>
      n -> (otherSubFFT1Idx =/= idx.U)
    }.toMap
    val otherSubFFT2Idx = Mux1H(
      reverseSubFFTns.map { case (n, idx) =>
        (~currentSubFFTBools(n) & otherSubFFT1NotN(n)) -> idx.U
      }
    )
    val otherSubFFTIdxs = Seq(otherSubFFT1Idx, otherSubFFT2Idx)

    // TODO: Make module (redundant w/ Se)
    // Sub FFT ADCs might have different delays
    val toBePeeled = otherSubFFTIdxs.zipWithIndex.map { case (otherIdx, idx) =>
      val otherIdxFinal = Mux(normalPeel, otherIdx, io.peelScr.peelDebugFFTIdx)
      val otherIdxBools = ffastParams.subFFTns.zipWithIndex.map { case (n, idx) =>
        n -> (otherIdxFinal === idx.U)
      }
      val otherDelays = ffastParams.adcDelays.map { d =>
        val o = Mux1H(otherIdxBools.map { case (n, isIdx) => isIdx -> io.peelScr.delayCalibration(n)(d) } )
        o.suggestName(s"otherDelays_${idx}_${d}")
        d -> o
      }.toMap

      // TODO: Don't hard code type, manual trim
      val aVecCordicIn = ffastParams.adcDelays.map { case d =>
        // WARNING: EXPECTS D IS POSITIVE
        val mulResult = locFixed context_* otherDelays(d)
        mulResult.suggestName(s"mulResult_${d}_${mulResult.asInstanceOf[FixedPoint].binaryPoint.get}")
        val temp = mulResult.asUInt
        // TODO: Make sure it gets the right UInt value (ignores MSB)
        val locMulDelay = temp(temp.getWidth - 2, 1)
        locMulDelay.suggestName(s"locMulDelays_$d")
        // TODO: require, asInstanceOf handling ; matches the fact that LSB is dropped
        val bpShift = mulResult.asInstanceOf[FixedPoint].binaryPoint.get - 1
        val nWithBPShift = ffastParams.fftn << bpShift
        // TODO: Don't hard code!
        val xmaxBR = (ffastParams.fftn * ffastParams.adcDelays.max * 4) << bpShift
        val moddedLocMulDelay = ConstantMod(locMulDelay, nWithBPShift, xmax = xmaxBR, io.clk)
        moddedLocMulDelay.suggestName(s"moddedLocMulDelay_$d")
        // TODO: Clean up, don't be arbitrary
        val normalizeDivBy = BigInt(ffastParams.fftn).bitLength - 1
        // Dividing without losing precision
        val modOut = (Cat(false.B, moddedLocMulDelay)).asFixedPoint((bpShift + normalizeDivBy).BP)
        val inverseFFTnBP = cordicParams.angleType.asInstanceOf[FixedPoint].binaryPoint.get
        val thetaOver2Pi = Wire(cordicParams.outAngleType)
        thetaOver2Pi :=  modOut context_* ((1 << normalizeDivBy).toDouble / ffastParams.fftn).F(inverseFFTnBP.BP)
        // println(s"aVecCordicIn_$d Width: ${thetaOver2Pi.getWidth} BinaryPoint: ${thetaOver2Pi.asInstanceOf[FixedPoint].binaryPoint.get}")
        thetaOver2Pi.suggestName(s"aVecCordicIn_${d}_${thetaOver2Pi.asInstanceOf[FixedPoint].binaryPoint.get}")
        d -> thetaOver2Pi
      }

      val aVecRectangular = aVecCordicIn.map { case (d, a) =>
        // To get conjugate
        val o = RotateComplex(mag1, a, io.clk, SingletonEstimatorDelays.cordicDelay)
        o.suggestName(s"aVecRectangular_${d}_${o.real.asInstanceOf[FixedPoint].binaryPoint.get}")
        d -> o
      }.toMap

      val toBePeeled = ffastParams.adcDelays.map { case d =>
        val pipe = SingletonEstimatorDelays.upToAvgSigDelay.sum - SingletonEstimatorDelays.upToSecondCordicDelay.sum
        val t = ShiftRegister(aVecRectangular(d), pipe).asInstanceOf[DspComplex[FixedPoint]]
        val o = singletonEstimator.io.binSignalEarly context_* t
        if (idx == 0)
          io.peelScr.seRotatedOuts(d) := o
        o.suggestName(s"toBePeeled_${idx}_${d}_${o.real.asInstanceOf[FixedPoint].binaryPoint.get}")
        d -> o
      }.toMap
      idx -> toBePeeled
    }.toMap

    val peelingOutValid = Wire(Bool())
    peelingOutValid := peelingPartsEnable.last
    peelingOutValid.suggestName("peelingOutValid")









    ffastParams.subFFTns.zipWithIndex.foreach { case (n, idxx) =>
      ffastParams.adcDelays.foreach { case d =>
        val peelA = dataFromMemoryPeeling(n)(d)
        val peelB = Wire(peelA.cloneType)
        peelB := Mux1H(
          otherSubFFTIdxs.zipWithIndex.map { case (otherIdx, idxy) =>
            (otherIdx === idxx.U) -> (toBePeeled(idxy)(d))
          }
        )
        peelB.suggestName(s"peelB_${n}_${d}_${peelB.real.asInstanceOf[FixedPoint].binaryPoint.get}")
        
        // To match final peel delay
        val peelOut = ShiftRegister(peelA context_- peelB, 2)
        val singletonWrite = currentSubFFTBools(n) & singletonEstimator.io.binType("single")
        // Write hard 0 if singleton
        val peelOutFinal = Mux1H(Seq(~singletonWrite -> peelOut))
        peelOutFinal.suggestName(s"peeledResult_${n}_${d}")

        val toMemInit = ShiftRegister(fftOutNormalized(n, d), zerotonDelay) 
        dataToMemory(n)(d) := Mux(normalPeel, peelOutFinal, toMemInit)
        val peelingWE = Mux(currentSubFFTBools(n), singletonEstimator.io.binType("single"), ~isZeroton(n)) 
 
        io.dataToMemory(n)(d)(0).we := Mux(isInitialSearch, initialWE(n), peelingWE) 

      }
    }





 
 






    // connect up output memory w/ peel result
    // scr -- ffastOut* = 6 off them

    // TODO: Normalized: Different fraction!
    val k = Seq(ffastParams.k, 1952).max
    val n = ffastParams.fftn
    val outIdxsMem = Module(new SMem1P(UInt(range"[0, $n)"), k, "ffastOutBinIdxs"))
    outIdxsMem.io.clk := io.clk
    val outValsMem = Module(new SMem1P(DspComplex(dspDataType), k, "ffastOutBinVals"))
    outValsMem.io.clk := io.clk

  }
}