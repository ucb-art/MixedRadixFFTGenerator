package FFT
import ChiselDSP._
import org.json4s._
import native.Serialization._

object SQNRAnalysis {

  // FFT Sizes to analyze
  val fftn = List(72,128,64)
  // Fractional widths to analyze
  val bits = List(12,16,20,24,28,32)
  val intBits = 1
  // FFT Frames to analyze for SQNR
  val frames = 100

  def main(args: Array[String]): Unit = {

    val fft = new ChiselFFT
    val fracBits = bits.map(_ - intBits - 1)
    // Setup input vector generation (support smallest # of bits)
    val genParams = GeneratorParams(complex=ComplexParams(intBits=intBits,fracBits=fracBits.min, trimType = Round))
    Init({genParams}, jsonName = "", args = args)
    val (inVecs,outVecs) = TestVectors(fftn,frames)

    // SQNR (in dB) for all trials (add trial with dbl on top of fixed trials)
    val sqnrdb = (fracBits :+ -1).map( fb => { fftn.zipWithIndex.map{case (n,i) => {
      val isFixed = if (fb == -1) false else true
      val fracBits = if (isFixed) fb else 0
      // TODO: have FFT run support a list of sizes, so you don't need to recompile each time
      val out = fft.run(args,Some(n),Some(inVecs(i)),isFixed=Some(isFixed),Some((intBits,fracBits)))
      // SQNR = Sum(DblR^2 + DblI^2)/Sum((DblR-FixR)^2 + (DblI-FixI)^2)
      val sig = outVecs(i).map(x => math.pow(x.abs,2)).sum
      val noise = outVecs(i).zip(out).map{case(flo,fix) => math.pow((flo-fix).abs,2)}.sum
      val sqnr = sig/noise
      10*math.log10(sqnr)
    }}}).transpose

    // TODO: Convert Java writer to Scala writer in Testbench, cleaner JSON
    // JSON file: List of # bits tested; JSON with FFTN followed by SQNR (dB) for # of bits tested
    implicit val formats = DefaultFormats
    val jsonBits = write(("Number of Bits",bits))
    scala.tools.nsc.io.File("sqnr.json").writeAll(jsonBits)
    val jsonSQNRs = sqnrdb.zip(fftn).map{case (sqnr,n) => write((n.toString,sqnr))}
    jsonSQNRs.foreach{ x =>
      scala.tools.nsc.io.File("sqnr.json").appendAll("\n" + x)
    }

  }

}