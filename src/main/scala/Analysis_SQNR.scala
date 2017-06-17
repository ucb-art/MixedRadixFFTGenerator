/*

"intBits":4
"fracBits":19
"use4Muls":false
"mulPipe":2
"addPipe":0.333
"trimType":"Truncate"
"overflowType":"Grow"
"mulFracGrowth":1
"sizes":[12,24,36,48,60,72,96,108,120,144,180,192,216,240,288,300,324,360,384,432,480,540,576,600,648,720,768,864,900,960,972,1080,1152,1200,1296,64,128,256,512,1024,2048,1536]
"normalized":true
"generateOffset":true

*/

package FFT
import ChiselDSP._
import org.json4s._
import native.Serialization._

object SQNRAnalysis {

  // FFT Sizes to analyze
  val fftn = List(64)

  /*12, 24, 48, 96, 192, 384, 768, 36, 72, 144, 288, 576, 1152, 108, 216, 432, 864, 324, 648, 1296, 972,
          60, 120, 240, 480, 960, 180, 360, 720, 300, 600, 1200, 540, 1080, 900).sorted ++
          List(64, 128, 256, 512, 1024, 2048, 1536)*/ //List(72,128,64)

  // Fractional widths to analyze
  val bits = List(24) //List(12,16,20,24,28,32)
  val intBits = 1
  // FFT Frames to analyze for SQNR
  val frames = 10

  def main(args: Array[String]): Unit = {

    val fft = new ChiselFFT
    val fracBits = bits.map(_ - intBits - 1)
    // Setup input vector generation (support smallest # of bits)
    val genParams = GeneratorParams(complex=ComplexParams(intBits=intBits,fracBits=fracBits.min, trimType = Round))
    Init({genParams}, jsonName = "", args = args)
    

    val (inVecs,outVecs) = TestVectors(fftn,frames)

    // SQNR (in dB) for all trials (add trial with dbl on top of fixed trials)
    // (fracBits :+ -1)
    val sqnrdb = fracBits.map( fb => { fftn.zipWithIndex.map{case (n,i) => {
      val isFixed = if (fb == -1) false else true
      val fracBits = if (isFixed) fb else 0
      // TODO: have FFT run support a list of sizes, so you don't need to recompile each time
      val out = fft.run(args,Some(n),Some(inVecs(i)),isFixed=Some(isFixed),Some((intBits,fracBits)))
      // SQNR = Sum(DblR^2 + DblI^2)/Sum((DblR-FixR)^2 + (DblI-FixI)^2)
      val sig = outVecs(i).map(x => math.pow(x.abs,2)).sum
      val noise = outVecs(i).zip(out).map{case(flo,fix) => math.pow((flo-fix).abs,2)}.sum
      val sqnr = sig/noise
      val o = 10*math.log10(sqnr)
      println(o)
      o
    }}}).transpose

    // TODO: Convert Java writer to Scala writer in Testbench, cleaner JSON
    // JSON file: List of # bits tested; JSON with FFTN followed by SQNR (dB) for # of bits tested
    implicit val formats = DefaultFormats
    val jsonBits = write(("Number of Bits",bits))
    try {
      scala.tools.nsc.io.File("build/analysis/sqnr.json").writeAll(jsonBits)
      val jsonSQNRs = sqnrdb.zip(fftn).map{case (sqnr,n) => write((n.toString,sqnr))}
      jsonSQNRs.foreach{ x =>
        scala.tools.nsc.io.File("build/analysis/sqnr.json").appendAll("\n" + x)
      }
    }  
    catch {
      case ex: Exception => Warn("File exception")
    }

  }

}