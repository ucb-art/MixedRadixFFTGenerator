package FFT
import ChiselDSP._
import Chisel.{Complex => _, _}

object DebugFFT {

  def main(args: Array[String]): Unit = {

    val fft = new ChiselFFT
    val n = 12
    val inVec = (0 until n).map(x => Complex(x.toDouble,0.0)).toList
    //fft.run(args,fftn=Some(n),inVec=Some(inVec),normal=Some(true),genOffset=Some(true),debugMode=true)
    fft.run(args,fftn=Some(n),inVec=Some(inVec),normal=Some(false),genOffset=Some(true))
  }

}