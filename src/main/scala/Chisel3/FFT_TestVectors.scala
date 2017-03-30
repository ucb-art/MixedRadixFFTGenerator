package dspblocks.fft 
import breeze.signal._
import breeze.linalg.DenseVector
import breeze.math.Complex
import scala.util._

// TODO: Make random tests, add noise
object FFTTestVectors {
  // Should be fs / 2
  val fixedRealFreq = Seq(0.2, 0.3, 0.4, 0.25)
  val fixedRealAmp = Seq(0.25, 0.15, 0.2, 0.03)

  // I'm not really quantizing here... just limiting input range
  def createInput(fftn: Int, numTones: Int = -1, fracBits: Int): Seq[Complex] = {
    for (i <- 0 until fftn) yield {
      val tones = fixedRealAmp.zip(fixedRealFreq).map { case (a, f) => a * math.cos(2 * math.Pi * f * i) }
      val outR = tones.reduce(_ + _)
      val complexR = 
        if (outR >= 1.0) 1.0 - 1.0 / math.pow(2, fracBits)
        else if (outR < -1.0) -1.0 
        else outR
      // Complex(complexR, 0.0)
      // Dither!
      Complex(complexR + Random.nextGaussian / (1 << (fracBits + 2)), 0.0)
      // Random.nextGaussian
    }
  }

  def createOutput(in: Seq[Complex]): Seq[Complex] = {
    val inFormatted = DenseVector(in.toArray)
    fourierTr(inFormatted).toArray.toSeq
  }

} 