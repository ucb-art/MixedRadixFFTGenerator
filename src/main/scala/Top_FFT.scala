// TODO: Delay variations sweep

package FFT
import ChiselDSP._
import Chisel.{Complex => _, _}
//import arbor.{Math => _, _}
import java.io._

object MainWithMatlab { // extends arbor.matlab.MATLABRepl {

  // How to refer to objects in Matlab
  val imports = Seq(
    Map(
      "classname"->"FFT.ChiselFFT",
      "varname"  -> "FFT"
    )
  )

  def main(args: Array[String]): Unit = {

    // TODO: Move to Arbor? Pull out helper for write to file
    // Handles spaces in folder + file names for Macs, create javapaths file for Matlab interface
    /*
    val matlabTemplate = getStartup.replaceAll("%20"," ")
    val javapaths = new File("MatlabScratch/javapaths.m")
    val javapathsBW = new BufferedWriter(new FileWriter(javapaths))
    javapathsBW.write(matlabTemplate)
    javapathsBW.close()
    */

    val fft = new ChiselFFT

    // fft.run(args)
    fft.run(args,normal=Some(true),genOffset=Some(true))

  }

}

class ChiselFFT() { // extends ArborSpec {

  // TODO: Support IFFT

  /** For Matlab Fixed experiments */
  def runMatlabFixed(fftn:Int,inReal:Array[Double],inImag:Array[Double], intBits: Int, fracBits:Int,
                     normalized:Boolean, genOffset:Boolean = false):
                    Array[Double] = {
    runMatlab(fftn,inReal,inImag,true,Some((intBits,fracBits)),normalized,genOffset)
  }
  /** For Matlab Double experiments */
  def runMatlabDouble(fftn:Int,inReal:Array[Double],inImag:Array[Double], normalized:Boolean,
                      genOffset:Boolean = false): Array[Double] = {
    runMatlab(fftn,inReal,inImag,false,None,normalized,genOffset)
  }

  /** Matlab interface */
  def runMatlab(fftn:Int,inReal:Array[Double],inImag:Array[Double],
                isFixed:Boolean,fixedParams:Option[(Int,Int)], normalized:Boolean, generateOffset:Boolean = false)
               : Array[Double] = {
    val inVec = (inReal.toList).zip(inImag.toList).map{case (real,imag) => Complex(real,imag)}
    // SBT run parameters (overridden)

    // TODO: To include Arbor or not?

    def dir = java.nio.file.Files.createTempDirectory("arbor-tests")
    val baseArgs = chiselEnvironmentArguments() ++
      Array[String](
        "--debug",
        "--compile",
        "--test"
      )
    val testArgs = baseArgs ++ Array(
      "--genHarness",
      "--targetDir",
      dir.toString()
    )

    val args = Array("-params_false_false") ++ testArgs
    val out = run(args,Some(fftn),Some(inVec),Some(isFixed),fixedParams, Some(normalized), Some(generateOffset))

    out.map(x => List(x.real,x.imag)).flatten.toArray

  }

  /** Run Chisel generator compilation, with possible Matlab interface */
  def run(args: Array[String], fftn: Option[Int] = None,
          inVec: Option[List[ScalaComplex]] = None,
          isFixed: Option[Boolean] = None,
          fixedParams: Option[(Int,Int)] = None,
          normal: Option[Boolean] = None,
          genOffset: Option[Boolean] = None,
          debugMode: Boolean = false
         ): List[ScalaComplex] = {

    // TODO: Pull out as SBT run parameters
    val useJSON = {
      if (fixedParams != None) false
      else false                        // Up to you
    }
    val nameExt = ""

    // Need to set default to run even though double doesn't use these values
    val (intBitsD,fracBitsD) = fixedParams.getOrElse((4,19))

    // Basic FFT doesn't renormalize outputs and doesn't have offset counter
    val normalized = normal.getOrElse(false)
    val generateOffset = genOffset.getOrElse(false)

    // Local generator params. Can use this instead of JSON values for design sweeps or Scala based parameter generation
    val defaultGenParams = GeneratorParams(
      complex = ComplexParams(
        intBits       = intBitsD,
        fracBits      = fracBitsD,
        use4Muls      = false,
        mulPipe       = 2,
        addPipe       = 0.33,
        trimType      = Truncate,
        overflowType  = Grow,
        mulFracGrowth = 1
      ),
      clock = ClockParams(
        periodx100ps  = Math.floor(162.76).toInt
      ),
      fft = FFTParams(
        sizes   = List(12,24,36,48),
                  /*List(12,24,48,96,192,384,768,36,72,144,288,576,1152,108,216,432,864,324,648,1296,972,
                       60,120,240,480,960,180,360,720,300,600,1200,540,1080,900).sorted ++
                  List(64,128,256,512,1024,2048,1536),*/
                /* List(5*7,5*5*7,5*7*7) */
                /*
                  List(64,72,128,2048,25)
                  List(12,24,48,96,192,384,768,36,72,144,288,576,1152,108,216,432,864,324,648,1296,972,
                       60,120,240,480,960,180,360,720,300,600,1200,540,1080,900) ++
                  List(30,128,256,243,125,15,200,3600,20,40,400,80,100,75,500,2000,225,50,4,2048)
                */
        normalized = normalized,
        generateOffset = generateOffset
      ),
      test = TestParams(
        frames  = 5
      )
    )

    // Extract Generator parameters (complex, FFT, etc.) from JSON or else from defaults
    // + fixed/double mode setup info
    val (isFixedParam,p) = Init({defaultGenParams}, jsonName = if (useJSON) "FFT" else "", args = args)

    // Setup module + tester
    val runArgs = args.slice(1, args.length)

    val checkFixed = isFixed.getOrElse(isFixedParam)

    if (checkFixed) {
      Status("Starting DSPFixed testbench")
      Chisel.chiselMainTest(
        runArgs, () => DSPModule(new FFT({DSPFixed(p.complex.getFixedParams)},p,debugMode), nameExt)
      ) {
        c => new FFTTests(c,fftn,inVec,normalized,generateOffset)
      }
    }
    else {
      Status("Starting DSPDbl testbench")
      Chisel.chiselMainTest(runArgs, () => DSPModule(new FFT({DSPDbl()},p,debugMode), nameExt)) {
        c => new FFTTests(c,fftn,inVec,normalized,generateOffset)
      }
    }

    // Clean up variable
    val out = Tracker.FFTOut
    Tracker.FFTOut = List[ScalaComplex]()
    out

  }

}