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
        intBits       = 1,
        fracBits      = 30,
        use4Muls      = true,
        mulPipe       = 1,
        addPipe       = 0.0,
        trimType      = Truncate,
        overflowType  = Grow,
        mulFracGrowth = 4
      ),
      clock = ClockParams(
        periodx100ps  = 39
      ),
      fft = FFTParams(
        sizes   = List(12,24,48,96,192,384,768,36,72,144,288,576,1152,108,216,432,864,324,648,1296,972,
                       60,120,240,480,960,180,360,720,300,600,1200,540,1080,900)
      ),
      test = TestParams(
        frames  = 5
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
