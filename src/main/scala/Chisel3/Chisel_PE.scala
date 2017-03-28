package dspblocks.fft
import chisel3._
import chisel3.experimental._
import dsptools.numbers._
import dsptools.numbers.implicits._
import dsptools.hasContext
import barstools.tapeout.transforms._
import chisel3.util._

class ProcessingElementIO[T <: Data:RealBits](
    twiddleType: => T,
    dataType: => T,
    fftParams: FactorizationParams
  ) extends Bundle {

  val maxNumBanks = fftParams.mem.maxNumBanks
  // TODO: Less copy paste -- redundant
  // Confuse max rad + max num banks
  val usedRads = fftParams.butterfly.rad 

  val clk = Input(Clock())
  val twiddles = Input(CustomIndexedBundle(DspComplex(twiddleType), (1 until maxNumBanks)))
  val currRad = new CustomIndexedBundle(usedRads.map(r => r -> Input(Bool())): _*)
  val y = Vec(maxNumBanks, Output(DspComplex(dataType)))
  val x = Vec(maxNumBanks, Input(DspComplex(dataType)))

  override def cloneType = (new ProcessingElementIO(twiddleType, dataType, fftParams)).asInstanceOf[this.type]
}

// TODO: Add programmable DIT/DIF support
class ProcessingElement[T <: Data:RealBits](
    twiddleType: => T, 
    dataType: => T, 
    fftParams: FactorizationParams, 
    fftType: FFTType) extends Module with DelayTracking with hasContext {

  val io = IO(new ProcessingElementIO(twiddleType, dataType, fftParams))
  val wfta = Module(new WFTA(dataType, fftParams))

  // TODO: Consistent naming
  val twDelay = context.complexMulPipe
  val moduleDelay = wfta.moduleDelay + twDelay

  withClock(io.clk) {
    // Delayed in CalcCtrl
    // TODO: What happens if only 1 radix is used?
    wfta.io.currRad := io.currRad

    // DIF twiddle multiplication after WFTA; DIT twiddle multiplication before WFTA
    val twMulIn = fftType match {
      case DIT => io.x
      case DIF => wfta.io.y
    }

    val twMulOut = twMulIn.zipWithIndex.map { case (in, idx) =>
      if (idx == 0) ShiftRegister(in, twDelay)
      else {
        in context_* io.twiddles(idx)
      }
    }

    // TODO: Can use non-delayed DIT flag for programmable
    fftType match {
      case DIT => 
        wfta.io.x := twMulOut
        io.y := wfta.io.y
      case DIF => 
        wfta.io.x := io.x
        io.y := twMulOut
    }
  }

}