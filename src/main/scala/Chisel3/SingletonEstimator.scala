package dspblocks.fft

import chisel3._
import chisel3.experimental._
import dsptools.numbers._
import dsptools.numbers.implicits._
import chisel3.util._
import org.scalatest.{FlatSpec, Matchers}
import dsptools.{DspTester, DspTesterOptionsManager}
import barstools.tapeout.TestParams
import barstools.tapeout.transforms._
import dsptools.{hasContext, DspContext}
import dsptools._
import breeze.math.Complex

class SingletonEstimatorIO[T <: Data:RealBits](dspDataType: T, ffastParams: FFASTParams) extends Bundle {

  // NOTE: Expects subFFT, subFFTInverse, delays, zeroThresholdPwr, sigThresholdPwr,
  // delayCalcConstants to be held for duration of FFT
  
  val maxSubFFT = ffastParams.subFFTns.max
  val n = ffastParams.fftn

  // Note: Externally needs to select based off of which sub FFT you're using

  val delayedIns = CustomIndexedBundle(
    Input(DspComplex(FFTNormalization.getNormalizedDataType(dspDataType, ffastParams))), 
    ffastParams.adcDelays
  )
  val subFFTIdx = Input(UInt(range"[0, $maxSubFFT)"))
  
  val subFFT = Input(UInt(range"[0, $maxSubFFT]"))
  val subFFTInverse = Input(FFTNormalization.getNormalizedDataType(dspDataType, ffastParams))

  val delays = CustomIndexedBundle(
    Input(DelayOptimization(dspDataType, ffastParams)), 
    ffastParams.adcDelays
  )

  // Zero Threshold * # delays
  val zeroThresholdPwr = Input(FFTNormalization.getNormalizedDataType(dspDataType, ffastParams))
  // Sig Threshold (no multiplication by delays)
  val sigThresholdPwr = Input(FFTNormalization.getNormalizedDataType(dspDataType, ffastParams))

  // TODO: Generalize
  // Last constant is purely fractional
  require(ffastParams.delays.length == 3, "Hard coded for 3 sets of delays right now :(")

  val delayCalcConstants = CustomIndexedBundle(Seq(
    Input(DelayOptimization(dspDataType, ffastParams)),
    Input(DelayOptimization(dspDataType, ffastParams)),
    Input(DelayOptimization(dspDataType, ffastParams, fullFraction = true)))
  )

  val binType = new CustomBundle(ffastParams.binTypes.map(_ -> Output(Bool())): _*) 
  val binLoc = Output(UInt(range"[0, $n)"))
  val binSignal = Output(DspComplex(FFTNormalization.getNormalizedDataType(dspDataType, ffastParams)))
  
  val clk = Input(Clock())
  
  override def cloneType = (new SingletonEstimatorIO(dspDataType, ffastParams)).asInstanceOf[this.type]

}

// TODO: Check get known
object FitOps {
  def apply[T <: Data:RealBits](a: T, b: T, growth: Int): T = {
    require(growth >= 0)
    val temp = (a, b) match {
      case (a: FixedPoint, b: FixedPoint) =>
        // Includes sign bit
        val aIntWidth = a.getWidth - a.binaryPoint.get
        val bIntWidth = b.getWidth - b.binaryPoint.get 
        val maxIntWidth = Seq(aIntWidth, bIntWidth).max 
        val maxBP = Seq(a.binaryPoint.get, b.binaryPoint.get).max
        Wire(FixedPoint((maxIntWidth + maxBP + growth).W, (maxBP + growth).BP))
      case (a: DspReal, b: DspReal) => 
        Wire(a)
      case _ =>
        throw new Exception("Invalid")
    }
    temp.asInstanceOf[T]
  }
}

// ------------------

// TODO: Get rid of Get Normalied Data Type
@chiselName
class SingletonEstimator[T <: Data:RealBits](dspDataType: T, ffastParams: FFASTParams) extends Module with DelayTracking with hasContext {

  val cordicDelay = 5
  // TODO: Don't hard code, use map
  val toIDelay = Seq(
    context.numMulPipes,    // get phase diff ***
    cordicDelay,            // cordic ***
    1,                      // a ***
    context.numMulPipes,    // b ***
    1,                      // d ***
    context.numMulPipes,    // e ***
    1,                      // g ***
    context.numMulPipes,    // h / delta3 *
    context.numMulPipes     // h * n *
  )
  val toLDelay = toIDelay ++ Seq( 
    1,                      // i
    context.numMulPipes     // j
  )    
  val toLocDelay = toLDelay ++ Seq(
    context.numMulPipes     // l (includes its delay)
  )
  val moduleDelay = toLocDelay.sum

  val io = IO(new SingletonEstimatorIO(dspDataType, ffastParams))

  withClock(io.clk) {

    val thetaOver2Pis = ffastParams.delays.map { case d =>              // tx -- ordered by increasing delay deltas
      val dmax = io.delayedIns(d.max)
      val dmin = io.delayedIns(d.min)
      // Result has phase difference of min, max 
      val complexWithPhaseDelta = dmax context_* (dmin.conj())
      val t = GetAngle(complexWithPhaseDelta, io.clk, cordicDelay).zeroTo2Pi
      t.suggestName("thetaOver2Pis")
      t
    }

    // TODO: No Kay's estimator (MMSE average)

    // TODO: Play with bit growth
    // Note: I grossly overprovisioned b/c of ADC uncertainty

    // Successive approximation
    // Could try to round to 1, which cannot be expressed in Q0.X notation
    // Subsequent: # of integer bits proportional to amount required to represent delta delay
    val thetaOver2Pis0Rounded = DspContext.withOverflowType(Grow) { thetaOver2Pis(0).round }
    val a = ShiftRegister(thetaOver2Pis(0) context_- thetaOver2Pis0Rounded, 1)
    val bT = io.delayCalcConstants(0) context_* a
    val b = FitOps(io.delayCalcConstants(0), a, growth = 0)
    b := bT
    val thetaOver2Pis1Delay = ShiftRegister(thetaOver2Pis(1), context.numMulPipes + 1)
    val c = (thetaOver2Pis1Delay context_- b).round
    val d = ShiftRegister(thetaOver2Pis1Delay - c, 1) 
    val eT = io.delayCalcConstants(1) context_* d
    val e = FitOps(io.delayCalcConstants(1), d, growth = 0)
    e := eT
    val thetaOver2Pis2Delay = ShiftRegister(thetaOver2Pis(2), 2 * context.numMulPipes + 2)
    val f = (thetaOver2Pis2Delay context_- e).round
    val g = ShiftRegister(thetaOver2Pis2Delay context_- f, 1) 

    // Just to get outAngleType...
    val cordicParams = CordicParams(io.delayedIns(ffastParams.adcDelays.max).real, 0, false)
    
// --------------------------

    // Should ideally be < 1
    val hT1 = io.delayCalcConstants(2) context_* g
    val hT2 = Wire(cordicParams.outAngleType.cloneType)
    hT2 := hT1
    // TODO: Generalize; won't overflow b/c N isn't near power of 2
    val nAsFixed = (ffastParams.fftn).U.asFixed.asInstanceOf[T]
    val hT3 = nAsFixed context_* hT2
    val h = Wire(nAsFixed.cloneType)
    h := hT3.round

// --------------------------

    val doNothing = io.subFFTInverse.fromDouble(ffastParams.fftn.toDouble / ffastParams.subFFTns.min)
    // TODO: Check general overflow case -- 21600 is safe
    // TODO: Don't hard code type
    // Sub FFT info should be held for the duration that we care about
    val subFFTIdxMatchIDelay = ShiftRegister(io.subFFTIdx.asFixed.asInstanceOf[T], toIDelay.sum)
    val i = ShiftRegister(h context_- subFFTIdxMatchIDelay, 1)
    val jT = io.subFFTInverse context_* i
    val j = Wire(doNothing.cloneType)
    j := jT
    val k = j.round
    val l = Wire(nAsFixed.cloneType)
    // TODO: Don't hard code type
    l := k context_* io.subFFT.asFixed.asInstanceOf[T]
    val m = l context_+ ShiftRegister(subFFTIdxMatchIDelay, toLocDelay.sum - toIDelay.sum)
    val n = Mux(m.signBit, nAsFixed context_+ m, m)

    val loc = Wire(io.binLoc.cloneType)
    // Definitely cast to UInt (for Verilator)
    val temp = n.asUInt
    loc := temp(temp.getWidth - 1, 0)

    println("Singleton Estimator Node Sizes")
    val debug = Seq("a" -> a, "b" -> b, "c" -> c, "d" -> d, "e" -> e, "f" -> f, "g" -> g, "h" -> h, "i" -> i, "j" -> j, "k" -> k, "l" -> l, "m" -> m, "n" -> n)
    debug foreach { case (name, x) =>
      println(s"$name: ${x.getWidth}, ${x.asInstanceOf[FixedPoint].binaryPoint.get}") 
    }

    io.binLoc := loc

    // Successive approximation is recursive
    // theta_x -> CORDIC dumps theta_x / 2pi
    // loc_3 = n / delta_3 [ theta_3 / 2pi - round [theta_3 / 2pi - delta_3 / delta_2 [theta_2 / 2pi - round [ theta_2 / 2pi - delta_2 / delta_1 [theta_1 / 2pi - round[theta_1 / 2pi]]]]]]
    // loc_match = round[[loc_3 - subFFTBinIdx] / subFFTLen] * subFFTLen + subFFTBinIdx
    // loc = loc_match + n if loc_match < 0; otherwise loc_match

  }

}

class SingletonEstimatorWrapper[T <: Data:RealBits](val dspDataType: T, val ffastParams: FFASTParams) extends chisel3.Module {
  val mod = Module(new SingletonEstimator(dspDataType, ffastParams))
  val io = IO(mod.io.cloneType)
  mod.io.clk := clock

  mod.io.delayedIns := io.delayedIns
  mod.io.subFFTIdx := io.subFFTIdx
  mod.io.subFFT := io.subFFT
  mod.io.subFFTInverse := io.subFFTInverse
  mod.io.delays := io.delays
  mod.io.zeroThresholdPwr := io.zeroThresholdPwr
  mod.io.sigThresholdPwr := io.sigThresholdPwr
  mod.io.delayCalcConstants := io.delayCalcConstants

  io.binType := mod.io.binType 
  io.binLoc := mod.io.binLoc
  io.binSignal := mod.io.binSignal
}

class SingletonEstimatorSpec extends FlatSpec with Matchers {
  behavior of "Singleton Estimator"
  it should "work" in {

    import dspblocks.fft.FFASTTopParams._

    val opt = TestParams.optionsBTolWaveformTB(lsbs = fpBP - 1, outDir = "test_run_dir/SingletonEstimatorTB")

    dsptools.DspContext.alter(dspContext) {
      dsptools.Driver.execute(() => 
        new SingletonEstimatorWrapper(
          dspDataType = dspDataType,
          ffastParams = ffastParams
        ), opt
      ) { c =>
        new SingletonEstimatorTester(c)
      } should be (true)
    }
  }
}

case class SingletonEstimatorTest(
    ffastParams: FFASTParams, 
    subFFT: Int, 
    subFFTIdx: Int, 
    binLoc: Int, 
    isSingleton: Boolean, 
    delayedIns: Map[Int, Complex]) {

  val numDelays = ffastParams.adcDelays.length

  // TODO: Make less arbitrary? Factor in quantization noise, etc.
  val nf = 20
  val noiseThresholdPwr = nf * numDelays.toDouble / math.pow(subFFT, 2).toDouble
  val sigThresholdPwr = nf * 1.toDouble / math.pow(subFFT, 2).toDouble

  val n = ffastParams.fftn
  val subFFTInverse = 1.toDouble / subFFT
  val delays = delayedIns.toSeq.map(_._1)
  val inBins = delayedIns.toSeq.map(_._2)

  val aVector = delays.map { case d =>
    val theta = ((binLoc * d) % n) * math.Pi * 2 / n
    // conjugation removes phase difference
    Complex(math.cos(theta), math.sin(theta))
  }

  // Cordic
  val binSignalSamples = inBins.zip(aVector).map { case (in, a) =>
    in * a.conjugate
  }

  val avgBinSignalNotNormalized = binSignalSamples.reduce(_ + _) 
  val avgBinSignal = avgBinSignalNotNormalized / numDelays

  // Cordic undo
  val sigOut = aVector.map { case a =>
    avgBinSignal * a
  }

  val noise = inBins.zip(sigOut).map { case (a, b) => a - b }
  val noisePwr = noise.map { case n => n.real * n.real + n.imag + n.imag }.sum

  val sigPwr = avgBinSignal.real * avgBinSignal.real + avgBinSignal.imag * avgBinSignal.imag

  val isZeroton = sigPwr < sigThresholdPwr            // no * # delays
  val notSingleton = noisePwr > noiseThresholdPwr     // * # delays

  // Priority
  if (isZeroton)
    require(!isSingleton)
  else if (notSingleton)
    require(!isSingleton, s"Noise power: $noisePwr Noise Threshold $noiseThresholdPwr")

}

// TODO: See how much waste in delay choices
class SingletonEstimatorTester[T <: Data:RealBits](c: SingletonEstimatorWrapper[T]) extends DspTester(c) {

  val params = c.ffastParams

  val tests = Seq(
    SingletonEstimatorTest(params, subFFT = 675, subFFTIdx = 135, binLoc = 12960, isSingleton = true, 
      delayedIns = Seq(
        0 -> Complex(0.09999557432549745, 2.185843291722172E-5),
        1 -> Complex(-0.08087364851509277, -0.05876563484628506),
        6 -> Complex(-0.0809610863716996, -0.05877587352157287),
        9 -> Complex(-0.08085895723749413, 0.05880696639231166),
        12 -> Complex(0.030908872124478386, 0.09509897704946693),
        19 -> Complex(-0.08087959862445164, 0.05879325658840878)
      ).toMap
    ),
    SingletonEstimatorTest(params, subFFT = 675, subFFTIdx = 540, binLoc = 8640, isSingleton = true, 
      delayedIns = Seq(
        0 -> Complex(0.09999557432549745, -2.185843291722172E-5),
        1 -> Complex(-0.08087364851509277, 0.05876563484628506),
        6 -> Complex(-0.0809610863716996, 0.05877587352157287),
        9 -> Complex(-0.08085895723749413, -0.05880696639231166),
        12 -> Complex(0.030908872124478386, -0.09509897704946693),
        19 -> Complex(-0.08087959862445164, -0.05879325658840878)
      ).toMap
    ),
    SingletonEstimatorTest(params, subFFT = 675, subFFTIdx = 405, binLoc = -1, isSingleton = false, 
      delayedIns = Seq(
        0 -> Complex(0.20002372523225695, 2.5771843949787107E-5),
        1 -> Complex(0.015430804872307831, -0.04761434901662145),
        6 -> Complex(0.06180180142312776, -0.1902114377612409),
        9 -> Complex(0.015453889148703921, 0.04754759212145481),
        12 -> Complex(-0.16182547977844977, -0.11761838940287692),
        19 -> Complex(-0.16182547977844977, -0.11761838940287692)
      ).toMap
    ),
    SingletonEstimatorTest(params, subFFT = 675, subFFTIdx = 270, binLoc = -1, isSingleton = false, 
      delayedIns = Seq(
        0 -> Complex(0.20002372523225695, -2.5771843949787107E-5),
        1 -> Complex(0.015430804872307831, 0.04761434901662145),
        6 -> Complex(0.06180180142312776, 0.1902114377612409),
        9 -> Complex(0.015453889148703921, -0.04754759212145481),
        12 -> Complex(-0.16182547977844977, 0.11761838940287692),
        19 -> Complex(0.015452438259075718, -0.04754792491323378)
      ).toMap
    ),
    SingletonEstimatorTest(params, subFFT = 800, subFFTIdx = 80, binLoc = 6480, isSingleton = true, 
      delayedIns = Seq(
        0 -> Complex(0.07505754907026448, -9.88711578327783E-6),
        1 -> Complex(-0.023162198047061544, 0.07137936586443518),
        6 -> Complex(0.023160493448060534, -0.07131864130569361),
        9 -> Complex(-0.02317661804626503, -0.07132915805237064),
        12 -> Complex(-0.060667534932126094, -0.044049223769062014),
        19 -> Complex(-0.02316872549407996, -0.07135438671369307)
      ).toMap
    ),
    SingletonEstimatorTest(params, subFFT = 800, subFFTIdx = 480, binLoc = 17280, isSingleton = true, 
      delayedIns = Seq(
        0 -> Complex(0.12500247748261095, 2.8607824414897095E-6),
        1 -> Complex(0.03860225387314656, -0.11891074531743032),
        6 -> Complex(0.038609426789110196, -0.11894520083688347),
        9 -> Complex(0.038578786103015836, 0.11889660497337433),
        12 -> Complex(-0.10111928434170041, -0.07343443182412278),
        19 -> Complex(0.03863143600566978, 0.11888014029688444)
      ).toMap
    ),
    SingletonEstimatorTest(params, subFFT = 864, subFFTIdx = 216, binLoc = 5400, isSingleton = true, 
      delayedIns = Seq(
        0 -> Complex(0.014997587916270405, -1.4611794302388653E-5),
        1 -> Complex(-6.952493157763298E-6, 0.015067300447455209),
        6 -> Complex(-0.014982188601721572, 3.037222301300927E-5),
        9 -> Complex(-5.3238053424849465E-6, 0.015026578117480456),
        12 -> Complex(0.015010575985506246, 7.99244820256777E-6),
        19 -> Complex(-6.17468272969233E-6, -0.015025849049659945)
      ).toMap
    ),
    SingletonEstimatorTest(params, subFFT = 864, subFFTIdx = 648, binLoc = 16200, isSingleton = true, 
      delayedIns = Seq(
        0 -> Complex(0.014997587916270405, 1.4611794302393147E-5),
        1 -> Complex(-6.9524931577585786E-6, -0.015067300447455209),
        6 -> Complex(-0.01498218860172157, -3.0372223013014434E-5),
        9 -> Complex(-5.323805342480474E-6, -0.015026578117480456),
        12 -> Complex(0.015010575985506246, -7.992448202562405E-6),
        19 -> Complex(-6.174682729697717E-6, 0.015025849049659945)
      ).toMap
    ),
    SingletonEstimatorTest(params, subFFT = 864, subFFTIdx = 0, binLoc = -1, isSingleton = false, 
      delayedIns = Seq(
        0 -> Complex(0.45000915072857706, 0.0),
        1 -> Complex(-0.08454147427239703, 0.0),
        6 -> Complex(-0.08451047324270698, 0.0),
        9 -> Complex(-0.08447385513496702, 0.0),
        12 -> Complex(-0.1404173939445729, 0.0),
        19 -> Complex(-0.0846333604191663, 0.0)
      ).toMap
    ),
    SingletonEstimatorTest(params, subFFT = 864, subFFTIdx = 432, binLoc = -1, isSingleton = false, 
      delayedIns = Seq(
        0 -> Complex(0.15001938066981324, 2.3250113254411374E-17),
        1 -> Complex(-0.046318679343542335, -9.193632866316897E-18),
        6 -> Complex(0.046315616172956556, 7.228343884938407E-18),
        9 -> Complex(-0.046298964461923026, -8.72782748850831E-18),
        12 -> Complex(-0.12137921154023056, -2.557713236165341E-17),
        19 -> Complex(-0.04637789800562247, -9.937516023550007E-18)
      ).toMap
    )
  )

  val moduleDelay = c.mod.moduleDelay

  for (subFFT <- c.ffastParams.subFFTns) {
    val currentFFTTests = tests.filter(x => x.subFFT == subFFT)
    // Stuff up top doesn't change
    val t = currentFFTTests(0)
    updatableDspVerbose.withValue(false) {
      poke(c.io.subFFT, t.subFFT)
      poke(c.io.subFFTInverse, t.subFFTInverse)
      poke(c.io.zeroThresholdPwr, t.noiseThresholdPwr)
      poke(c.io.sigThresholdPwr, t.sigThresholdPwr)
      // "Calibration"
      c.ffastParams.delayConstants.zipWithIndex foreach { case (const, id) =>
        poke(c.io.delayCalcConstants(id), const)
      }
      t.delayedIns.toSeq foreach { case (dly, in) =>
        poke(c.io.delays(dly), dly)
      }
    }
    for (idx <- 0 until currentFFTTests.length + moduleDelay) {
      val t = currentFFTTests(idx % currentFFTTests.length)
      updatableDspVerbose.withValue(false) {
        t.delayedIns.toSeq foreach { case (dly, in) =>
          poke(c.io.delayedIns(dly), in)
        }
        poke(c.io.subFFTIdx, t.subFFTIdx)
        if (idx >= moduleDelay) {
          val outExpected = currentFFTTests(idx - moduleDelay)
          if (outExpected.isSingleton) expect(c.io.binLoc, outExpected.binLoc)
        }
        step(1)   
      }
    }
  }








  





  
 

 
    
    

    
   

    
    

    
  




 

 





// if singleton, write 0
// output should be bin - actually no need for peeled
//io.binType := mod.io.binType 
//io.binSignal := mod.io.binSignal
// need to delay loc to match sig + type




 

  
  
  
}