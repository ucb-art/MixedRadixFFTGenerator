// TODO: Check to see that user entered FFT sizes are unique (no repeated)
// TODO: Make butterfly private again

package FFT
import ChiselDSP._

// TODO: Be less lazy with initial values

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

    val addrC = MemoryAccess(calc.radPow,calc.radOrder,calc.maxStages,calc.maxRad)
    mem.addrC = addrC

    val (twiddleCountMax,twiddleLUTScale,twiddles,twiddleSubcountMax) = Twiddles(io.coprimes,
                                                                                 io.global,
                                                                                 calc.radPow,
                                                                                 calc.radOrder,
                                                                                 calc.maxStages)
    twiddle.countMax = twiddleCountMax
    twiddle.LUTScale = twiddleLUTScale
    twiddle.vals = twiddles
    twiddle.subcountMax = twiddleSubcountMax

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
  var lengths: List[Int] = List.fill(7)(32),
  // Address constants for converting nx to memory bank addresses
  var addrC: List[List[Int]] = List(List(1))
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
  // DIF Q with corresponding base
  var qDIF: List[List[Tuple2[Int,Int]]] = List(List((1,1))),
  // DIT Q with corresponding base
  var qDIT: List[List[Tuple2[Int,Int]]] = List(List((1,1)))
)

case class CalcParams (
  // Order of the calculation radix stages
  var radOrder:List[List[Int]] = List.fill(10)(List(4,2,3,5)),
  // i.e. for N = 4^a1*2^a2*3^b*5^c (base order determined by radOrder), list contains [a1,a2,b,c]
  var radPow:List[List[Int]] = List.fill(10)(List(1,1,1,1)),
  // Maximum # of radix stages to support all Ns
  var maxStages: Int = 4,
  // Maximum radix required for each FFTN, along with its index relative to radOrder positions
  var maxRad: List[Tuple2[Int,Int]] = List((1,1))

)

case class TwiddleParams (
  // Main twiddle count max for each calculation stage (CTA decomposition -- within a coprime)
  var countMax: List[List[Int]] = List(List(1)),
  // Base multiply amount to scale range of twiddle counts to full twiddle LUT size (associated w/ coprime)
  var LUTScale: List[List[Int]] = List(List(1)),
  // Twiddles partitioned by coprimes, and then by corresponding radix (radix-1 sub-lists)
  var vals: List[List[List[ScalaComplex]]] = List(List(List(Complex(0.0,0.0)))),
  // Counts to hold twiddle (for PFA, based off of subsequent coprimes)
  var subcountMax: List[List[Int]] = List(List(1))

)

case class ButterflyParams (
  // Radices actually needed
  var rad: List[Int] = List(2,3,4,5,7),
  // Number of butterflies needed
  var num: Int = 1
)

// TODO: # butterflies, clk ratio, banks, length