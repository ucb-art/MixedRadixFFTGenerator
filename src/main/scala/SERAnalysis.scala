package FFT
import ChiselDSP._

object SERAnalysis {

  val bits = List(12,16,24,32) // List(12,16,20,24,28,32)

  def main(args: Array[String]): Unit = {

    // Time domain input data
    val inRealTemp = File2DblArray("src/main/resources/Matlab/generated/in_i.txt")
    val inImagTemp = File2DblArray("src/main/resources/Matlab/generated/in_q.txt")

    // TODO: Pass params in file, intBits base off of worst-case qam + processing gain renormalization
    val fftn = 128
    val mqam = 16
    val numFrames = 100
    val intBits = BigInt(mqam-1).bitLength + BigInt(math.sqrt(fftn).toInt).bitLength

    val fft = new ChiselFFT
    // Calculate # of fractional bits needed
    val fracBits = bits.map(_ - intBits - 1)
    // Run for floating pt design + fixed point designs
    (fracBits :+ -1) foreach { fb => {
      val isFixed = if (fb == -1) false else true
      val out = {
        if (isFixed) {
          // Clamp within fixed range
          val inReal = ClampRange(inRealTemp.toList,(-1*mqam,mqam)).toArray
          val inImag = ClampRange(inImagTemp.toList,(-1*mqam,mqam)).toArray
          fft.runMatlabFixed(fftn,inReal,inImag,intBits,fb,normalized=true)
        }  
        else fft.runMatlabDouble(fftn,inRealTemp,inImagTemp,normalized=true)
      }

      // Renormalization (!!)
      //val normalizedOut = out.toList.map(_ * (1/math.sqrt(fftn)))
      val runType = if(!isFixed) "Dbl" else (intBits+fb+1).toString
      //Data1D2File(normalizedOut,"build/analysis/ser_"+runType+".txt")
      Data1D2File(out.toList,"build/analysis/ser_"+runType+".txt")
    }}

  }

}