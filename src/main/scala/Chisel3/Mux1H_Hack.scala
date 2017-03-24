package dspblocks.fft
import chisel3._
import chisel3.util._
import chisel3.core.SeqUtils
import dsptools.numbers._
import dsptools.numbers.implicits._
import chisel3.experimental._

// To get Verilator to actually do something correct, hopefully
// There was a horrible bug where when it cat the real and imaginary parts together,
// the VCD waveform would be wrong and PeekPoketTester would give garbage data for positive real
// WARNING: Mux1H with Seq of length 1 returns the input instead of zero!
object Mux1H {
  def apply[T <: Data](sel: Seq[Bool], in: Seq[T]): T =
    apply(sel zip in)
  def apply[T <: Data](in: Iterable[(Bool, T)]): T = {
    val (sels, possibleOuts) = in.unzip
    val out = possibleOuts.head match {
      case _: DspComplex[_] =>
        val cmplxs = scala.collection.mutable.ArrayBuffer[DspComplex[_]]()
        possibleOuts foreach { 
          case cx: DspComplex[_] => cmplxs += cx
          case _ => throw new Exception("Iterable elements should all be DspComplex")
        }
        val (possibleReals, possibleImags) = cmplxs.toSeq.map(c => (c.real, c.imag)).unzip
        val realOut = chisel3.util.Mux1H(sels.zip(possibleReals.map(x => x.asInstanceOf[Data])))
        val imagOut = chisel3.util.Mux1H(sels.zip(possibleImags.map(x => x.asInstanceOf[Data]))) 
        (realOut, imagOut) match {
          case (r: FixedPoint, i: FixedPoint) => DspComplex.wire(r, i)
          case (r: DspReal, i: DspReal) => DspComplex.wire(r, i)
          case (r: SInt, i: SInt) => DspComplex.wire(r, i)
          case _ => throw new Exception("Illegal complex real/imag types for Mux1H")
        }
      case _ => chisel3.util.Mux1H(in)
    }
    out.asInstanceOf[T]
  }
  def apply[T <: Data](sel: UInt, in: Seq[T]): T =
    apply((0 until in.size).map(sel(_)), in)
  def apply(sel: UInt, in: UInt): Bool = (sel & in).orR
}