package dspblocks.fft

import chisel3._
import chisel3.experimental._
import dsptools.numbers._
import dsptools.numbers.implicits._
import barstools.tapeout.transforms._

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

class Peeling[T <: Data:RealBits](dspDataType: => T, ffastParams: FFASTParams, subFFTnsColMaxs: Map[Int, Seq[Int]]) extends Module {
  val io = IO(new PeelingIO(dspDataType, ffastParams, subFFTnsColMaxs))
  io.stateInfo.done := true.B
}




/*

set / reset 0th (reset on done)
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


