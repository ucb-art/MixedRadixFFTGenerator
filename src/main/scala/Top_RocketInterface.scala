package FFT
import ChiselDSP._
import Chisel._

// Module
/*
object RocketInterface {

  def main(args: Array[String]): Unit = {

    Status("Starting Rocket Interface testbench")

    // TODO: Fix
    val runArgs = args.slice(1, args.length)

    Chisel.chiselMainTest( runArgs, () => Module(new RocketToFFT()) ) {
      c => new RocketToFFTTests(c)
    }

  }

}
*/

// DSPModule
object RocketInterfaceWrapper {

  def main(args: Array[String]): Unit = {

    Status("Starting Rocket Interface Wrapper testbench")

    // TODO: Fix
    val runArgs = args.slice(1, args.length)

    Chisel.chiselMainTest( runArgs, () => DSPModule(new RocketToFFTWrapper) ) {
      c => 
        //List(1296, 64, 128, 256, 512, 1024, 2048, 1536).foreach { n =>
        RocketInterfaceParams().fft.sizes.foreach { n => 
          new RocketToFFTWrapperTests(c, n, 1)
        }
      // RocketInterfaceParams().fft.sizes.min
      new RocketToFFTWrapperTests(c, 24, 1)
    }

  }

}






