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
  private var delays = PipelineParams()

  private var complex = ComplexParams()

  // Different components should access parameters via the below functions
  def getBF() = butterfly.copy()
  def getTw() = twiddle.copy()
  def getCalc() = calc.copy()
  def getIO() = io.copy()
  def getMem() = mem.copy()
  def getFFT() = fft.copy()
  def getTest() = test.copy()
  def getDelays() = delays.copy()

  def getComplex() = complex.copy()

  // Pass in params extracted from JSON to setup used params
  def apply(p: GeneratorParams) : Unit = {

    // Copy complex parameters
    complex = p.complex

    // Make delay parameters local
    delays.mulPipe = p.complex.mulPipe
    delays.addPipe = p.complex.addPipe

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

    Status("Used butterfly radices: " + rad.mkString(","))

    // Optimize clkRatio to be less than default if input FFT sizes don't require default
    // Note that ideal = 1
    io.clkRatio = ScheduleCalc(calc.radPow,calc.radOrder,calc.maxStages)
    Status("Used calculation to IO clock ratio: " + io.clkRatio)

    val (qDIF,qDIT) = IOQ(fft.nCount,io.coprimes,io.global)
    io.qDIF = qDIF
    io.qDIT = qDIT

    val (addrC,numBanks,memLengths) = MemoryAccess(calc.radPow,calc.radOrder,calc.maxStages,calc.maxRad,getFFT.sizes)
    mem.addrC = addrC
    mem.banks = numBanks
    mem.lengths = memLengths

    Status("# memory banks: 2x" + numBanks)
    Status("Memory bank lengths: " + memLengths.mkString(","))

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
  sizes: List[Int] = List(2,3,4,5,7),
  // To normalize FFT/IFFT output (/,* by sqrt(n)) for Parseval's
  normalized: Boolean = false,
  // Report k as data is streaming out (symbol # in current frame)
  generateOffset: Boolean = false
){
  def nCount = sizes.length
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
  // ? Ratio of fast clock (Calculation) to slow clock (IO) frequencies
  // TODO: Check why clks become misaligned for clkRatio > 2 (setup done error)
  var clkRatio: Int = 2,
  // DIF Q with corresponding base
  var qDIF: List[List[Tuple2[Int,Int]]] = List(List((1,1))),
  // DIT Q with corresponding base
  var qDIT: List[List[Tuple2[Int,Int]]] = List(List((1,1)))
){
  def numDigits = coprimes.map(_.map(_._3))
  def globalPrimes = List(1) ++ global.map(_._1)
  def globalRads = List(0) ++ global.map(_._2)
  def globalMaxCoprimes = global.map(_._3)
  def primes = coprimes.map(_.map(_._2))
}

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
){
  // TODO: more of this
  def addrMax = vals.map(x => x.length).max-1
}

case class ButterflyParams (
  // Radices actually needed
  var rad: List[Int] = List(2,3,4,5,7),
  // ? Number of butterflies needed
  var num: Int = 1
){
  def possibleRad = List(1) ++ rad
}

// TODO: Figure out how to include twiddle, IO, general setup delays

case class PipelineParams (

  // TODO: Generalize based off size of multiply + add?

  // Amount to pipeline fixed-point multiply
  var mulPipe: Int = 0,
  // Amount to pipeline fixed-point add
  var addPipe: Double = 0.0,

  // Total pipeline delay in nToAddrBank block
  var nToAddrBank: Int = 1,

  // Delay just in top level module code of CalcCtrl
  var calcTop: Int = 1,

  // Amount to pipeline input into WFTA before doing calculations
  var wftaInPipe: Int = 1,

  // Delay just in top level module code of IOCtrl
  var ioTop: Int = 1,

  // Memory output is registered
  var memOutReg: Boolean = true,
  // Memory sequential read (address delayed)
  var memSeqRead: Boolean = true,

  // Delay from memBankInterface input controls -> Memory control + r/w address inputs
  var memArbiterTop: Int = 1,

  // Twiddle address generation address = count * mul delay
  var twiddleAddrGen: Int = 4

){

  if (calcCtrl != ioCtrl) Error("Calculation + IO control delays must match")
  if ((calcCtrl + memArbiterTop + memSeqReadDly) != twiddleAddrGen)
    Error("Twiddle address must be valid when address into (internal) data memory is valid")

  // Delay between read address valid and dout
  def memReadAtoD = memOutRegDly + memSeqReadDly
  def memOutRegDly = {if (memOutReg) 1 else 0}
  def memSeqReadDly = {if (memSeqRead) 1 else 0}

  // Total pipeline delay in CalcCtrl
  def calcCtrl = nToAddrBank + calcTop

  // Total pipeline delay in IOCtrl
  def ioCtrl = nToAddrBank + ioTop

  // Delays through WFTA stages (as dictated by amount to pipeline fixed-point multiply and add operations)
  def wftaInternalDelays = {
    var count = 0
    // Spreads out add delays (handles addPipe < 1 too) + mul delays
    WFTA.stages.map { x => {
      count = {
        if (x != Add) 0
        else if (math.floor(count * addPipe) == 1) 1
        else count + 1
      }
      if (x == Mul) mulPipe
      else if (addPipe < 1) math.floor(count * addPipe).toInt
      else math.floor(addPipe).toInt
    }}
  }

  // Total wfta delay
  def wfta = wftaInPipe + wftaInternalDelays.sum

  // Delay in normalize module
  def normalize = mulPipe

  // Delay through processing element
  def pe = twiddle + wfta

  // Twiddle multiply delay
  def twiddle = {
    val dly = {
      if (Params.getComplex.use4Muls) math.floor(addPipe).toInt + mulPipe
      else 2*math.floor(addPipe).toInt + mulPipe
    }
    if (dly < 1) Error("Twiddle multiplication must be pipelined at least once!")
    dly
  }

  // Delay from undelayed IO ctrl signal to data out of FFT top
  def outFlagDelay = ioCtrl + memArbiterTop + memReadAtoD + (if (Params.getFFT.normalized) normalize else 0) + topOut

  // Delay between normalization out and output @ top level
  // Total output delay needs to be a multiple of the IO clock ratio so that the output
  // is valid on the correct fast clock cycle
  def topOut = {
    val dlyUpToOut = {
      if (Params.getFFT.normalized) ioCtrl + memArbiterTop + memReadAtoD + normalize
      else ioCtrl + memArbiterTop + memReadAtoD
    }
    Params.getIO.clkRatio - (dlyUpToOut % Params.getIO.clkRatio)
  }

  // TODO: Support pipeDin @ PE, 3 muls

}

// TODO: # butterflies, clk ratio