package FFT
import ChiselDSP._
import Chisel._

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





