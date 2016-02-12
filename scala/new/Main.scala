// TODO: Delay variations sweep

package FFT
import ChiselDSP._
import Chisel._

object Main {

  def main(args: Array[String]): Unit = {

    // TODO: Pull out as SBT run parameters
    val useJSON = false
    val nameExt = ""

    // Local generator params. Can use this instead of JSON values for design sweeps or Scala based parameter generation
    val defaultGenParams = GeneratorParams(
      complex = ComplexParams(
        intBits       = 16,
        fracBits      = 32,
        use4Muls      = true,
        mulPipe       = 1,
        addPipe       = 0.0,
        trimType      = Truncate,
        overflowType  = Grow,
        mulFracGrowth = 3
      ),
      fft = FFTParams(
        sizes   = List(3,4,5,16,2048)
      ),
      test = TestParams(
        frames  = 8
      )
    )

    // Extract Generator parameters (complex, FFT, etc.) from JSON or else from defaults
    // + fixed/double mode setup info
    val (isFixed,p) = Init({defaultGenParams}, jsonName = if (useJSON) "FFT" else "", args = args)
    // Setup FFT with user-defined parameters
    Params(p)

    // TODO: Get rid of placeholder
    FFTGenerator()

    // Setup module + tester
    val runArgs = args.slice(1, args.length)
    if (isFixed)
      Chisel.chiselMainTest(runArgs, () => DSPModule(new FFT({DSPFixed()}), nameExt)) { c => new FFTTests(c) }
    else
      Chisel.chiselMainTest(runArgs, () => DSPModule(new FFT({DSPDbl()}), nameExt)) { c => new FFTTests(c) }

  }

}
