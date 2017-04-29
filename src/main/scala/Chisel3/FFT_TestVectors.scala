package dspblocks.fft 
import breeze.signal._
import breeze.linalg.DenseVector
import breeze.math.Complex
import scala.util._
import org.scalatest.{FlatSpec, Matchers}

// TODO: Make random tests, add noise
object FFTTestVectors {
  // Should be fs / 2
  // val fixedRealFreq = Seq(0.2, 0.3, 0.4, 0.25)

  val r = new scala.util.Random
  val numFreq = 216 
  val fixedRealAmpT = Seq(0.25, 0.15, 0.2, 0.12, 0.13).map(_ / 3)
  val fixedRealFreq = (0 until numFreq).map(i => r.nextInt(21600 / 2).toDouble / 21600)
  val fixedRealAmp = (Seq.fill(numFreq / fixedRealAmpT.length)(fixedRealAmpT)).flatten

  // I'm not really quantizing here... just limiting input range
  def createInput(fftn: Int, numTones: Int = -1, fracBits: Int): Seq[Complex] = {
    println("Number of tones: " + fixedRealFreq.distinct.length)
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

  def createOutput(in: Seq[Complex], zeroThresholdPwr: Double = 1, disp: Boolean = false): Seq[Complex] = {
    createOutputInt(in, zeroThresholdPwr, disp)._1
  }

  def createOutputInt(in: Seq[Complex], zeroThresholdPwr: Double = 1, disp: Boolean = false): (Seq[Complex], Seq[Int]) = {
    val inFormatted = DenseVector(in.toArray)
    val out = fourierTr(inFormatted).toArray.toSeq
    val fft = out.length

    val nonZeroIdxs = out.zipWithIndex.map { case (o, i) => 
      val magSq = (o.real * o.real + o.imag * o.imag)/(fft * fft).toDouble
      if (magSq > zeroThresholdPwr) {
        if (disp) println(s"FFT: $fft Index: $i MagSq Norm: $magSq Normalized Output: ${o.real / fft} + ${o.imag / fft} i") 
        i 
      }
      else -1
    }.filter(_ != -1)

    (out, nonZeroIdxs)
  }

  def expectedSubSampleLocations(in: Seq[Complex], zeroThresholdPwr: Double = 1, disp: Boolean = false): Seq[Int] = {
    createOutputInt(in, zeroThresholdPwr, disp)._2
  }

} 

object PlotFFT { 
  def apply(exp: Seq[Complex]): Unit = {
    import breeze.linalg._
    import breeze.plot._

    val f = Figure()
    val p = f.subplot(0)

    p.legend_=(true)

    val xaxis = (0 until exp.length).map(e => e.toDouble).toSeq.toArray

    // Log 0 doesn't exist
    val plotMin = 0.0000000001
    val expPlot = exp.map(c => 20 * math.log10(Seq(c.abs, plotMin).max)).toSeq
    p += plot(xaxis, expPlot.toArray, name = "Expected")
    
    p.ylim(Seq(-100.0, expPlot.min).max, expPlot.max)
    p.title_=(s"FFT: ${exp.length}")

    p.xlabel = "Frequency Bin"
    p.ylabel = "20log10(||Vpeak||)"
    f.saveas(s"test_run_dir/fft_${exp.length}.pdf") 
  }
}

class FFTTestVectorSpec extends FlatSpec with Matchers {
  behavior of "FFT Test Vectors"
  it should "be reasonable" in {
    val fractionalBits = 8
    val fftn = 21600
    val in = FFTTestVectors.createInput(fftn, fracBits = fractionalBits).map { case xtot => 
      val z = xtot.real
      val x = if (z >= 1) 1 - math.pow(2, fractionalBits) else if (z < -1) -1 else z
      val y = math.round(x * math.pow(2, fractionalBits)) / math.pow(2, fractionalBits)
      Complex(y, 0.0)
    }
    val out = FFTTestVectors.createOutput(in).map(x => x / fftn)
    PlotFFT(out)

    val fftLargeThreshold = 0.004 * 0.004
    val fftLargeOutIdxs = FFTTestVectors.expectedSubSampleLocations(in, zeroThresholdPwr = fftLargeThreshold, disp = false)
    println(s"Actual # of non-zero bins: ${fftLargeOutIdxs.length}")

  }
}

