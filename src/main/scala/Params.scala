// TODO: Check to see that user entered FFT sizes are unique (no repeated)
// TODO: Make butterfly private again

package FFT
import ChiselDSP._

/** Parameters for different components of the generator */
object Params {

  private var butterfly = ButterflyParams()
  private var twiddle = TwiddleParams()
  private var calc = CalcParams()
  private var io = IOParams()
  private var mem = MemParams()
  private var fft = FFTParams()
  private var test = TestParams()

  // Different components should access parameters via the below functions
  def getBF() = butterfly.copy()
  def getTw() = twiddle.copy()
  def getCalc() = calc.copy()
  def getIO() = io.copy()
  def getMem() = mem.copy()
  def getFFT() = fft.copy()
  def getTest() = test.copy()

  // Pass in params extracted from JSON to setup used params
  def apply(p: GeneratorParams) : Unit = {
    fft = p.fft
    test = p.test
    TestVectors(fft.sizes,test.frames)

    val (rad,radPow,radOrder,maxRad,maxStages,coprimes,global) = Factorize(getFFT.sizes)
    butterfly.rad = rad
    calc.radPow = radPow
    calc.radOrder = radOrder
    calc.maxRad = maxRad
    calc.maxStages = maxStages
    io.coprimes = coprimes
    io.global = global










    val (qDIF,qDIT) = IOQ(fft.nCount,io.coprimes,io.global)
    io.qDIF = qDIF
    io.qDIT = qDIT
  }

}

/** Composition of generator parameters (with default values!) -> should be in JSON file */
case class GeneratorParams(
  complex: ComplexParams = ComplexParams(),
  clock: ClockParams = ClockParams(),
  fft: FFTParams = FFTParams(),
  test: TestParams = TestParams()
) extends JSONParams(complex,clock)

/** Tuning knobs for the FFT generator */
case class FFTParams (
  // FFT sizes supported
  sizes: List[Int] = List(2,3,4,5,7)
){
  val nCount = sizes.length
}

/** Parameters for test */
case class TestParams (
  // # of frames of data to test
  frames: Int = 1
)

///////////////////////////////////////////////////////

case class MemParams (
  // # of memory banks to ensure conflict free access
  var banks: Int = 7,
  // Memory sizes for each bank
  var lengths: List[Int] = List.fill(7)(32)
)

case class IOParams (
  // Globally, all primes used, their associated max radices (bases), and their associated max coprimes (max
  // 2^a,3^b,5^c used to support all generator sizes)
  // [prime,maxRadix,maxCoprime]
  var global: List[Tuple3[Int,Int,Int]] = List((1,1,1)),
  // Factorization into coprimes for each FFT length, along with their associated base primes, and # of
  // digits needed to represent a range of #'s up to the coprime value
  // [coprime,prime,numDigits]
  var coprimes:List[List[Tuple3[Int,Int,Int]]] = List.fill(10)(List((1,1,1))),
  // Ratio of fast clock (Calculation) to slow clock (IO) frequencies
  var clkRatio: Int = 2,
  // DIF Q
  var qDIF: List[List[Int]] = List(List(1)),
  // DIT Q
  var qDIT: List[List[Int]] = List(List(1))
)

case class CalcParams (
  // Order of the calculation radix stages
  var radOrder:List[List[Int]] = List.fill(10)(List(4,2,3,5)),
  // i.e. for N = 4^a1*2^a2*3^b*5^c (base order determined by radOrder), list contains [a1,a2,b,c]
  var radPow:List[List[Int]] = List.fill(10)(List(1,1,1,1)),
  // Maximum # of radix stages to support all Ns
  var maxStages: Int = 6,
  // Maximum radix required for each FFTN, along with its index relative to radOrder positions
  var maxRad: List[Tuple2[Int,Int]] = List((1,1))

)

case class TwiddleParams (
)

case class ButterflyParams (
  // Radices actually needed
  var rad: List[Int] = List(2,3,4,5,7),
  // Number of butterflies needed
  var num: Int = 1
)

// TODO: # butterflies, clk ratio, banks, length