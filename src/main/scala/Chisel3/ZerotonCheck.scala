package dspblocks.fft

import chisel3._
import chisel3.experimental._
import dsptools.numbers._
import dsptools.numbers.implicits._
import barstools.tapeout.transforms._
import chisel3.util._
import dsptools.{hasContext, DspContext}
import dsptools._

// WARNING: NEED TO MULTIPLY ZEROTON THRESHOLD BY # OF DELAYS (through rocket-chip)
// i.e. [average power = Sum(pwr) / numDelays] < zeroThreshold to be considered zeroton
// --> the same as Sum(pwr) < (zeroThreshold * numDelays)

class ZerotonIO[T <: Data:RealBits](dspDataType: => T, ffastParams: FFASTParams) extends Bundle {
  val clk = Input(Clock())
  val bin = Input(CustomIndexedBundle(DspComplex(FFTNormalization.getNormalizedDataType(dspDataType, ffastParams)), ffastParams.adcDelays))
  val sigThresholdPwr = Input(FFTNormalization.getNormalizedDataType(dspDataType, ffastParams))
  val isZeroton = Output(Bool())
  override def cloneType = (new ZerotonIO(dspDataType, ffastParams)).asInstanceOf[this.type]
}  

// TODO: Move to dsptools
object AbsSq {
  def apply[T <: Data:RealBits](in: DspComplex[T]): T = {
    (in.real context_* in.real) context_+ (in.imag context_* in.imag)
  }
}

object SumScalars {
  def apply[T <: Data:RealBits](in: Seq[T]): T = {
    val nestDepth = math.ceil(math.log(in.length)/math.log(2)).toInt
    // TODO: Get rid of copy pasta, add delay info
    def groupAndSum(inT: Seq[T]): Seq[T] = {
      inT.grouped(2).map { case e => 
        val out = Wire(inT.head.cloneType)
        out := DspContext.withOverflowType(Grow) { e.reduceLeft(_ context_+ _) }
        out
      }.toSeq
    }
    // TODO: Something better than foldLeft? Since index is kind of wasted
    // As you go deeper, the # of elements in the rows decreases
    val addrTree = 
      if (nestDepth == 1) groupAndSum(in)
      else {
        (0 until nestDepth - 1).foldLeft(groupAndSum(in)) { case (accum, _) =>
          // Deal with previous row
          groupAndSum(accum)
        }
      }
    require(addrTree.length == 1, "Nest depth calculation was incorrect! Deepest layer should only have 1 element!")
    addrTree.head
  }
}

@chiselName
class Zeroton[T <: Data:RealBits](dspDataType: => T, ffastParams: FFASTParams) extends Module with DelayTracking with hasContext {
  
  // Add depth = 4
  val customAddPipes = 1
  val moduleDelay = context.numMulPipes + customAddPipes
  require(context.numAddPipes == 0)
  // TODO: Don't hard code

  val io = IO(new ZerotonIO(dspDataType, ffastParams))

  withClock(io.clk) {
    val binPwrs = io.bin.elements.map { case (name, binV) => AbsSq(binV) }.toSeq
    val sumPwrs = ShiftRegister(SumScalars(binPwrs), customAddPipes)
    // Should already account for numDelays in threshold
    io.isZeroton := sumPwrs < io.sigThresholdPwr
  }

}