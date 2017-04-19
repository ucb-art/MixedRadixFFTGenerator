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
      case r: DspReal => r
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








class PeelingSCR[T <: Data:RealBits](dspDataType: => T, ffastParams: FFASTParams) extends SCRBundle {

  








  val zeroThresholdPwr = CustomIndexedBundle(Input(dspDataType), ffastParams.subFFTns)
  val sigThresholdPwr = CustomIndexedBundle(Input(dspDataType), ffastParams.subFFTns)
  val timingDelay = CustomIndexedBundle(CustomIndexedBundle(Input(dspDataType), ffastParams.adcDelays), ffastParams.subFFTns)

}