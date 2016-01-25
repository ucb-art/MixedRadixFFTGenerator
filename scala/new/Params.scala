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
    val (rad,radPow,maxStages,primes,coprimes,maxCoprimes) = Factorize(getFFT.sizes)
    io.primes = primes
    io.coprimes = coprimes
    io.maxCoprimes = maxCoprimes
    calc.radPow = radPow
    calc.maxStages = maxStages
    butterfly.rad = rad
  }

}

/** Composition of generator parameters (with default values!) -> should be in JSON file */
case class GeneratorParams(
  complex: ComplexParams = ComplexParams(),
  fft: FFTParams = FFTParams(),
  test: TestParams = TestParams()
) extends JSONParams(complex)

/** Tuning knobs for the FFT generator */
case class FFTParams (
  // FFT sizes supported
  sizes: List[Int] = List(2,3,4,5,7)
)

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
  // Coprime bases used
  var primes: List[Int] = List(2,3,5),
  // Factorization into coprimes for each FFT length
  var coprimes:List[List[Int]] = List.fill(10)(List(2,3,5)),
  // Maximum 2^a,3^b,5^c used to support all generator sizes
  var maxCoprimes: List[Int] = List(2,3,5)
)

case class CalcParams (
  // Order of the calculation radix stages
  var radOrder:List[List[Int]] = List.fill(10)(List(4,2,3,5)),
  // i.e. for N = 4^a1*2^a2*3^b*5^c (base order determined by radOrder), list contains [a1,a2,b,c]
  var radPow:List[List[Int]] = List.fill(10)(List(1,1,1,1)),
  // -- Maximum # of radix stages to support all Ns
  var maxStages: Int = 6
)

case class TwiddleParams (
)

case class ButterflyParams (
  // -- Radices actually needed
  var rad: List[Int] = List(2,3,4,5,7),
  // Number of butterflies needed
  var num: Int = 1
)