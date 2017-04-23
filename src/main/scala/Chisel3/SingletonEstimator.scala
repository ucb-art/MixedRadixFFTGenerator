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

  val zeroThresholdPwr = Input(FFTNormalization.getNormalizedDataType(dspDataType, ffastParams))
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
  // TODO: Don't hard code
  val moduleDelay = Seq(
    context.numMulPipes,    // get phase diff ***
    cordicDelay,            // cordic ***
    1,                      // a ***
    context.numMulPipes,    // b ***
    1,                      // d ***
    context.numMulPipes,    // e ***
    1,                      // g ***
    context.numMulPipes,    // h / delta3 *
    context.numMulPipes,    // h * n *
    1,                      // i
    context.numMulPipes,    // j
    context.numMulPipes     // l
  ).sum

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
    val i = ShiftRegister(h context_- io.subFFTIdx.asFixed.asInstanceOf[T], 1)
    val jT = io.subFFTInverse context_* i
    val j = Wire(doNothing.cloneType)
    j := jT
    val k = j.round
    val l = Wire(nAsFixed.cloneType)
    // TODO: Don't hard code type
    l := k context_* io.subFFT.asFixed.asInstanceOf[T]
    val m = l context_+ io.subFFTIdx.asFixed.asInstanceOf[T]
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

case class SingletonEstimatorTest(subFFT: Int, subFFTIdx: Int, binLoc: Int, isSingleton: Boolean, delayedIns: Map[Int, Complex]) {
  val subFFTInverse = 1.toDouble / subFFT
  val delays = delayedIns.toSeq.map(_._1)
}












class SingletonEstimatorTester[T <: Data:RealBits](c: SingletonEstimatorWrapper[T]) extends DspTester(c) {

  val tests = Seq(
    SingletonEstimatorTest(subFFT = 675, subFFTIdx = 135, binLoc = 12960, isSingleton = true,
      delayedIns = Seq(
        0 -> Complex(0.09999557432549745, 2.185843291722172E-5),
        1 -> Complex(-0.08087364851509277, -0.05876563484628506),
        6 -> Complex(-0.0809610863716996, -0.05877587352157287),
        9 -> Complex(-0.08085895723749413, 0.05880696639231166),
        12 -> Complex(0.030908872124478386, 0.09509897704946693),
        19 -> Complex(-0.08087959862445164, 0.05879325658840878)
      ).toMap
    )
  )

  for ((t, idx) <- tests.zipWithIndex) {
    t.delayedIns.toSeq foreach { case (dly, in) =>
      poke(c.io.delayedIns(dly), in)
      // "Calibration"
      poke(c.io.delays(dly), dly)
    }
    poke(c.io.subFFTIdx, t.subFFTIdx)
    poke(c.io.subFFT, t.subFFT)
    poke(c.io.subFFTInverse, t.subFFTInverse)

    val zeroThreshold = c.ffastParams.adcDelays.length.toDouble / math.pow(t.subFFT, 2).toDouble
    val sigThreshold = zeroThreshold
    poke(c.io.zeroThresholdPwr, zeroThreshold)
    poke(c.io.sigThresholdPwr, sigThreshold)

    // "Calibration"
    c.ffastParams.delayConstants.zipWithIndex foreach { case (const, id) =>
      poke(c.io.delayCalcConstants(id), const)
    }
    
    step(c.mod.moduleDelay)

    peek(c.io.binLoc)
  }




 

 




  // calculatee average
  //is singleton
//updatableDspVerbose.withValue(false) {
  
  //io.binType := mod.io.binType 
  //io.binLoc := mod.io.binLoc
  //io.binSignal := mod.io.binSignal
  
}












// need to delay loc to match sig + type
// binType precedence --> zero then single then multi