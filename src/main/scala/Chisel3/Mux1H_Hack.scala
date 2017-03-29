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
// WARNING: Chisel Mux1H with Seq of length 1 returns the input instead of zero!
object Mux1H {
  def apply[T <: Data](sel: Seq[Bool], in: Seq[T]): T =
    apply(sel zip in)
  def apply[T <: Data](in: Iterable[(Bool, T)]): T = {
    // require(in.toSeq.length > 1, "Double check that Mux1H behavior is right! When in length = 1, it'll return in...")
    if (in.toSeq.length == 1) {
      val inX = in.toSeq.head
      val cond = inX._1
      val sig = inX._2
      // UInt zero is minimum width
      val out = Mux(cond, sig.asUInt, 0.U)
      sig.fromBits(out)
    }
    else {
      val (sels, possibleOuts) = in.toSeq.unzip
      val out = possibleOuts.head match {
        case _: DspComplex[_] =>
          val cmplxs = scala.collection.mutable.ArrayBuffer[DspComplex[_]]()
          possibleOuts foreach { 
            case cx: DspComplex[_] => cmplxs += cx
            case _ => throw new Exception("Iterable elements should all be DspComplex")
          }
          val (possibleReals, possibleImags) = cmplxs.toSeq.map(c => (c.real, c.imag)).unzip
          val realOut = Mux1H(sels.zip(possibleReals.map(x => x.asInstanceOf[Data])))
          val imagOut = Mux1H(sels.zip(possibleImags.map(x => x.asInstanceOf[Data]))) 
          (realOut, imagOut) match {
            case (r: FixedPoint, i: FixedPoint) => DspComplex.wire(r, i)
            case (r: DspReal, i: DspReal) => DspComplex.wire(r, i)
            case (r: SInt, i: SInt) => DspComplex.wire(r, i)
            case _ => throw new Exception("Illegal complex real/imag types for Mux1H")
          }
        // Mux1H signed stuff super derpy!
        // TODO: Add SInt
        case s: SInt => throw new Exception("SInt not supported yet!")
        case f: FixedPoint =>
          val (intWidths, binaryPoints) = possibleOuts.map { case o =>
            val fo = o.asInstanceOf[FixedPoint]
            require(fo.widthKnown && fo.binaryPoint.known, "Mux1H requires width/binary points to be defined")
            (fo.getWidth - fo.binaryPoint.get, fo.binaryPoint.get)
          }.unzip
          // All the same
          // TODO: Don't unzip?
          if (intWidths.distinct.length == 1 && binaryPoints.distinct.length == 1)
            chisel3.util.Mux1H(in)
          else {
            val maxIntWidth = intWidths.max
            val maxBP = binaryPoints.max
            val inWidthMatched = Seq.fill(possibleOuts.length)(Wire(FixedPoint((maxIntWidth + maxBP).W, maxBP.BP)))
            inWidthMatched.zipWithIndex foreach { case (e, idx) => e := possibleOuts(idx).asInstanceOf[FixedPoint] }
            chisel3.util.Mux1H(sels.zip(inWidthMatched))
          } 
        case _ => chisel3.util.Mux1H(in)
      }
      out.asInstanceOf[T]
    }
  }
  def apply[T <: Data](sel: UInt, in: Seq[T]): T =
    apply((0 until in.size).map(sel(_)), in)
  def apply(sel: UInt, in: UInt): Bool = (sel & in).orR
}