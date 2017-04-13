package dspblocks.fft
import chisel3._
import chisel3.experimental._
import barstools.tapeout.transforms._
import chisel3.util._
import dsptools.{hasContext, DspContext}
import math._
import dsptools.numbers._
import dsptools.numbers.implicits._
import breeze.math.Complex
import dsptools.{DspTester, DspTesterOptionsManager}
import org.scalatest.{FlatSpec, Matchers}
import barstools.tapeout.TestParams
import dsptools._
import breeze.signal._
import breeze.linalg.DenseVector
import breeze.math.Complex
import dsptools.DspTesterUtilities

// TODO: Consider separating out control signals for multi-butterfly support
// TODO: i.e. radix 2, when you try to bypass logic 0-(0-a) = a, which synthesis tools
// are able to see, BUT ONLY IF there isn't any intermediate registering. Either need to look for
// all double negative cases, or figure out a way to have Chisel do arithmetic optimization i.e.
// 0-(0-a) = a so no extra hardware is needed.

object WFTA {
  // Supported butterfly radices, grouped 
  // NOTE: This is fixed and should not be changed unless WFTA is redesigned (ORDER MATTERS)
  val groupedValidRad = Seq(Seq(4, 2), Seq(3), Seq(5), Seq(7))
  // WFTA stages (WFTA operation broken up into add/multiply stages)
  // Note: This is fixed and should not be changed unless WFTA is redesigned
  val stages = Seq(WFTAAdd, WFTAAdd, WFTAAdd, WFTAMul, WFTAAdd, WFTAAdd, WFTAAdd, WFTAAdd)

  def getValidRad = groupedValidRad.flatten
  def getRadIdx(rad: Int) = getValidRad.indexOf(rad)
}

abstract class WFTAStageType
case object WFTAMul extends WFTAStageType
case object WFTAAdd extends WFTAStageType

// TODO: Make an only real version (only real inputs)
// TODO: Already stated -- rename FactorizationParams
class WFTAIO[T <: Data:RealBits](dspDataType: => T, fftParams: FactorizationParams) extends Bundle {
  val usedRads = fftParams.butterfly.rad 
  // TODO: Less hacky -- Only go to 4 if it contains both 2 and 4 (which is always true?)
  // Radix-2: Perform 2 in parallel to decrease cycle count
  val maxRad = Seq(fftParams.butterfly.maxRad, (if (usedRads.contains(2) && usedRads.contains(4)) 4 else 0)).max
  
  val currRad = new CustomIndexedBundle(usedRads.map(r => r -> Input(Bool())): _*)
  
  // Not used
  // val currRadOut = Flipped(currRad.cloneType)

  // Output
  val y = Vec(maxRad, Output(DspComplex(dspDataType)))
  // Input
  val x = Vec(maxRad, Input(DspComplex(dspDataType)))
  
  val clk = Input(Clock())
  override def cloneType = (new WFTAIO(dspDataType, fftParams)).asInstanceOf[this.type]

/*
  val debugVecLen = 10
  val n0 = Vec(debugVecLen, Output(DspComplex(dspDataType)))
  val n1 = Vec(debugVecLen, Output(DspComplex(dspDataType)))
  val n2 = Vec(debugVecLen, Output(DspComplex(dspDataType)))
  val n3 = Vec(debugVecLen, Output(DspComplex(dspDataType)))
  val n4 = Vec(debugVecLen, Output(DspComplex(dspDataType)))
  val n5 = Vec(debugVecLen, Output(DspComplex(dspDataType)))
  val n6 = Vec(debugVecLen, Output(DspComplex(dspDataType)))
  val a = Vec(debugVecLen, Output(dspDataType))
  val r = Vec(debugVecLen, Output(Bool()))
*/

}

class WFTATester[T <: Data:RealBits](c: WFTAWrapper[T]) extends DspTester(c) {

  /** Fake inputs for full butterfly test */
  val ins = Seq(
    Complex(-0.0016156958292784854,-0.0038205920103660867),
    Complex(0.08396018021512272,-0.0013820177961438253),
    Complex(-0.013933768021206223,0.013053573473671093),
    Complex(-0.033684289651395055,0.028591395636659137),
    Complex(-0.015584356598410773,0.00337343167302713),
    Complex(0.015103657363909739,-0.012791286461996752),
    Complex(0.01926837435299409,-0.02547371574646024)
  )

  case class WFTATest(
    rad: Int,
    in: Seq[Complex],
    out: Seq[Complex]
  )

  val usedRads = c.usedRads 
  val tests = for (rad <- usedRads) yield {
    if (rad != 2) {
      val partialInputs = DenseVector(ins.take(rad).toArray)
      val partialOutputs = fourierTr(partialInputs)
      val padAmount = ins.length - partialInputs.length
      // Should be ignored
      val padIn = Seq.fill(padAmount)(Complex(0.12345, -0.12345))
      val padOut = Seq.fill(padAmount)(Complex(0.0, 0.0))
      WFTATest(
        rad = rad,
        in = partialInputs.toArray.toSeq ++ padIn,
        out = partialOutputs.toArray.toSeq ++ padOut
      )
    }
    else {
      val partialInputs1 = DenseVector(ins.take(rad).toArray)
      val partialInputs2 = DenseVector(ins.drop(rad).take(rad).toArray)
      val partialOutputs1 = fourierTr(partialInputs1)
      val partialOutputs2 = fourierTr(partialInputs2)
      val padAmount = ins.length - 2 * partialInputs1.length
      // Should be ignored
      val padIn = Seq.fill(padAmount)(Complex(0.12345, -0.12345))
      val padOut = Seq.fill(padAmount)(Complex(0.0, 0.0))
      WFTATest(
        rad = rad,
        in = (partialInputs1.toArray.toSeq ++ partialInputs2.toArray.toSeq) ++ padIn,
        out = (partialOutputs1.toArray.toSeq ++ partialOutputs2.toArray.toSeq) ++ padOut
      )
    }
    // TODO: Generalize
    
  }

  val delay = c.moduleDelay
  val zeroLanes = Seq.fill(usedRads.max)(Complex(0.0, 0.0))
  // Pad vectors for delay reasons
  val testsRightPadded = tests ++ Seq.fill(delay)(WFTATest(usedRads.min, zeroLanes, zeroLanes))

  for ((test, t) <- testsRightPadded.zipWithIndex) {
    // Default
    usedRads foreach { rad => poke(c.io.currRad(rad), false.B) }
    poke(c.io.currRad(test.rad), true.B)
    c.io.x.zipWithIndex foreach { case (x, idx) => poke(x, test.in(idx)) }
    if (t >= delay) {
      val outT = t - delay
      c.io.y.zipWithIndex foreach { case (y, idx) => expect(y, tests(outT).out(idx)) }
      usedRads foreach { rad =>
        val currRad = tests(outT).rad
        /*
        if (currRad == rad)
          expect(c.io.currRadOut(rad), true.B)
        else 
          expect(c.io.currRadOut(rad), false.B)
        */
      }
    }

    /*
    c.io.n0 foreach { x => peek(x) }
    c.io.n1 foreach { x => peek(x) }
    c.io.n2 foreach { x => peek(x) }
    c.io.n3 foreach { x => peek(x) }
    c.io.n4 foreach { x => peek(x) }
    c.io.n5 foreach { x => peek(x) }
    c.io.n6 foreach { x => peek(x) }
    c.io.a foreach { x => peek(x) }
    c.io.r foreach { x => peek(x) }
    */
    step(1)

  }

}

class WFTASpec extends FlatSpec with Matchers {
  behavior of "WFTA"
  it should "compute small FFTs" in {
    // Need to alter context externally
    DspContext.alter(DspContext.current.copy(numMulPipes = 1, numAddPipes = 0)) { 
      val opt = new DspTesterOptionsManager {
        dspTesterOptions = TestParams.options1TolWaveform.dspTesterOptions.copy(
          fixTolLSBs = 3
        )
        testerOptions = TestParams.options1TolWaveform.testerOptions
        commonOptions = TestParams.options1TolWaveform.commonOptions.copy(targetDirName = s"test_run_dir/WFTATB")
      }

      dsptools.Driver.execute(() => 
        new WFTAWrapper(
          //dspDataType = DspReal(),
          dspDataType = FixedPoint(28.W, 24.BP),
          fftParams = FactorizationParams(FFTNs(2, 3, 4, 5, 7))
        ), opt
      ) { c =>
        new WFTATester(c)
      } should be (true)
    }
  }
}

class WFTAWrapper[T <: Data:RealBits](dspDataType: => T, val fftParams: FactorizationParams) extends chisel3.Module {
  val mod = Module(new WFTA(dspDataType, fftParams))
  val usedRads = mod.usedRads
  val moduleDelay = mod.moduleDelay
  val io = IO(mod.io.cloneType)
  mod.io.currRad := io.currRad
  // TODO: Only need to calc currRad once, but not much overhead
  // io.currRadOut := mod.io.currRadOut
  mod.io.x := io.x
  io.y := mod.io.y
  mod.io.clk := clock
}

// TODO: @chiselName incompatible w/ context_complex_scalar_* defined twice
@chiselName
class WFTA[T <: Data:RealBits](dspDataType: => T, fftParams: FactorizationParams) extends Module 
    with hasContext with DelayTracking {

  println(context.toString)

  // TODO: Move to some delay params? + come up with a better name
  val inPipe = 1

  // TODO: Move to some delay params?
  // TODO: Support fractional numAddPipes (even though DspContext doesn't?) / check functional correctness
  // Delays through WFTA stages (as dictated by amount to pipeline fixed-point multiply and add operations)
  def wftaInternalDelays = {
    var count = 0
    // Spreads out add delays (handles addPipe < 1 too) + mul delays
    WFTA.stages.zipWithIndex.map { case (x, idx) => 
      count = {
        // Reset count on multiply (only handle sequence of adds)
        if (x != WFTAAdd) 0
        // Previously already added pipe, so "reset" (to 1 since this is also an Add)
        else if (math.floor(count * context.numAddPipes.toDouble) == 1) 1
        else count + 1
      }
      if (x == WFTAMul) context.numMulPipes
      // Add pipeline to last stage manually
      // TODO: UNDO
      else if (idx == WFTA.stages.length - 1) 1
      // Add a Pipe if enough adds in series
      else if (context.numAddPipes < 1) math.floor(count * context.numAddPipes.toDouble).toInt
      else math.floor(context.numAddPipes.toDouble).toInt
    }
  }

  val internalDelays = wftaInternalDelays

  val moduleDelay = inPipe + internalDelays.sum

  val io = IO(new WFTAIO(dspDataType, fftParams))

  val maxRad = io.maxRad
  val usedRads = io.usedRads

  val c1 = -2 * Pi / 3
  val C30 = dspDataType.fromDouble(cos(c1) - 1)
  val C31 = dspDataType.fromDouble(sin(c1))
  val C41 = dspDataType.fromDouble(-1.0)

  val c2 = -2 * Pi / 5
  val C50 = dspDataType.fromDouble((cos(c2) + cos(2 * c2)) / 2 - 1)
  val C51 = dspDataType.fromDouble((cos(c2) - cos(2 * c2)) / 2)
  val C52 = dspDataType.fromDouble((sin(c2) + sin(2 * c2)))
  val C53 = dspDataType.fromDouble(sin(c2))
  val C54 = dspDataType.fromDouble(-1 * (sin(c2) - sin(2 * c2)))

  val c3 = -2 * Pi / 7
  val C70 = dspDataType.fromDouble((cos(c3) + cos(2 * c3) + cos(3 * c3)) / 3 - 1)
  val C71 = dspDataType.fromDouble((2 * cos(c3) - cos(2 * c3) - cos(3 * c3)) / 3)
  // TODO: Does forcing with prevent optimization? What on earth is up with Chisel and undeeclared reference?
  // WithFixedWidth guarantees you can fully contain the constant
  val C72 = dspDataType.fromDouble((cos(c3) + cos(2 * c3) - 2 * cos(3 * c3)) / 3)
  val C73 = dspDataType.fromDouble((cos(c3) - 2 * cos(2 * c3) + cos(3 * c3)) / 3)
  val C74 = dspDataType.fromDouble((sin(c3) + sin(2 * c3) - sin(3 * c3)) / 3)
  val C75 = dspDataType.fromDouble((2 * sin(c3) - sin(2 * c3) + sin(3 * c3)) / 3)
  val C76 = dspDataType.fromDouble((sin(c3) + sin(2 * c3) + 2 * sin(3 * c3)) / 3)
  val C77 = dspDataType.fromDouble((sin(c3) - 2 * sin(2 * c3) - sin(3 * c3)) / 3)

  // TODO: Does using Ring[T].one vs. dspDataType.fromDouble(1.0) affect anything?
  val one = dspDataType.fromDouble(1.0)
  // TODO: What the heck is going on? keeps saying "Reference C72 is not declared", etc.
  val scalarZero = Ring[T].zero

  // TODO: Fix name with @chiselName elsewhere
  withClock(io.clk) {

    // Maps used radices to whatever is supported
    val radIn = Wire(CustomIndexedBundle(Bool(), WFTA.getValidRad))

    WFTA.getValidRad foreach { case rad => 
      val isUsed = usedRads.contains(rad)
      // Always true if only 1 radix is used (never switches)
      if (isUsed && usedRads.length == 1) 
        radIn(rad) := true.B
      // Unused
      else if (!isUsed) 
        radIn(rad) := false.B
      // Match valid radices to used radices
      else 
        radIn(rad) := ShiftRegister(io.currRad(rad), inPipe)
    }

    // Control signal for muxing inputs
    // TODO: Can you avoid hard-coding? Maybe impossible given that WFTA structure is fixed...
    val r2i = radIn(2)
    val r3i = radIn(3)
    val r4i = radIn(4)
    val r5i = radIn(5)
    val r7i = radIn(7)
    val r2r4i = r2i | r4i
    val r4r7i = r4i | r7i
    val r3r4i = r3i | r4i

    // 2x rad 2 butterfly
    // TODO: Pick better adder (reuse rather than add more)
    val r2r5i = r2i | r5i

    val zero = Wire(DspComplex(dspDataType))
    zero.real := Ring[T].zero
    zero.imag := Ring[T].zero

    // Assign internal "inputs" to 0 if >= max used radix
    val xp = Wire(Vec(WFTA.getValidRad.max, DspComplex(dspDataType)))
    xp.zipWithIndex foreach { case (x, idx) =>
      if (idx >= io.maxRad) {
        x := zero
      }
      else x := ShiftRegister(io.x(idx), inPipe)
    }
    // TODO: Move shift register here?
    // Input mux (see ICASSP paper for mapping)
    val x = Wire(xp.cloneType)
    x(0) := Mux(~r2r4i, xp(0), zero)
    x(1) := Mux(r2r4i, xp(0), xp(1))
    x(2) := Mux(r7i, xp(2), zero)
    x(3) := Mux1H(Seq(
      r2r5i -> xp(3),                         // To support 2x radix 2
      r4r7i -> xp(3)
    ))
    x(4) := Mux1H(Seq(
      r4i -> xp(1),
      r2r5i -> xp(2),                         // To support 2x radix 2
      r7i -> xp(4)
    ))
    x(5) := Mux(r7i, xp(5), zero)
    x(6) := Mux1H(Seq(
      r2i -> xp(1),
      r5i -> xp(4),
      r7i -> xp(6),
      r3r4i -> xp(2)
    ))

    // Values of rad(t) = outputs of delay stages @ t
    // i.e. rad(0) is the output of [radIn, which is a CustomIndexedBundle fed through 1 delay stage]
    // (Drop drops first element)
    val rad = Vec(internalDelays.scanLeft(radIn)((accum, dly) => ShiftRegister(accum, dly)).drop(1))

    // s0 = r7; s1 = !r4; s2 = !r3; s3 = (r5 + r7)

    // rad(stage # -- associated with delay)(radix)
    // TODO: Come up with better naming
    // Select into s0, s1, @ 2nd stage (index 1)
    val s0_1 = rad(0)(7)
    // Original optimization doesn't work for 2x rad 2 butterflies
    // val s1_1 = ~rad(0)(4)
    val s1_1 = ~(rad(0)(4) | rad(0)(2))

    // Other selects  sx_t
    val s3_4 = rad(3)(5) | rad(3)(7) | rad(3)(3)
    val s1_2 = ~rad(1)(4)
    val s2_5 = ~(rad(4)(3) | rad(4)(5))
    val s1_5 = ~rad(4)(4)
    val s0_6 = rad(5)(7)

    // Multiplier is 4th in chain (index 3 where input is from index 2)
    val r2m = rad(2)(2)
    val r3m = rad(2)(3)
    val r4m = rad(2)(4)
    val r5m = rad(2)(5)
    val r7m = rad(2)(7)
    val r5r7r3m = r5m | r7m | r3m 
    val r2r4m = r2m | r4m 

    // TODO: Should this be a Vec?

    val maxConstantWidth = 
      Seq(C30, C31, C41, C50, C51, C52, C53, C54, C70, C71, C72, C73, C74, C75, C76, C77).map(x => x.getWidth).max
    // one should be the largest represented constant
    require(maxConstantWidth <= one.getWidth, s"Cannot chop off MSBs. $maxConstantWidth > ${one.getWidth}")
    // Select between constants @ multiplier
    // val A = Wire(Vec(WFTA.getValidRad.max + 1, one.cloneType))
    
    val A0 = Mux1H(Seq(
      r3m -> C30,
      r5m -> C50,
      r7m -> C70
    ))
    val A1 = Mux1H(Seq(
      r5m -> C51,
      r7m -> C71
    ))
    val A2 = Mux1H(Seq(r7m -> C72))
    val A3 = Mux1H(Seq(r7m -> C73))
    val A4 = Mux1H(Seq(
      r3m -> C31,
      r4m -> C41,
      r5m -> C53,
      r7m -> C74
    ))
    val A5 = Mux1H(Seq(
      r4m -> one,
      r5m -> C52,
      r7m -> C75,
      r2m -> one
    ))
    val A6 = Mux1H(Seq(r7m -> C76))
    val A7 = Mux1H(Seq(
      r4m -> one,
      r5m -> C54, 
      r7m -> C77
    ))

    // TODO: Change caps convention
    // Flag to select multiply by imaginary (T) or real (F) component
    val MulI7 = ~r4m
    val MulI5 = ~r2r4m

    // TODO: Consolidate w/ hack
    def Mux1H0[T <: Data:RealBits](sel: Bool, in: DspComplex[T]): DspComplex[T] = {
      val zeroR = Wire(in.real.cloneType)
      zeroR := zeroR.fromDouble(0.0)
      val zeroI = Wire(in.imag.cloneType)
      zeroI := zeroI.fromDouble(0.0)
      val out = Wire(in.cloneType)
      out.real := Mux(sel, in.real, zeroR)
      out.imag := Mux(sel, in.imag, zeroI)
      out
    }

    // TODO: Make less copy-paste-y
    // Butterfly diagram (unified WFTA) stage 0 - 7
    val (n0a, n0b, n0c, n0d, n0e, n0f, n0g, n0cTemp, n0dTemp) = DspContext.withNumAddPipes(internalDelays(0)) {
      val n0a = x(1) context_+ x(6)
      val n0b = x(1) context_- x(6)
      val n0cTemp = x(4) context_+ x(3)                     // **
      val n0dTemp = x(4) context_- x(3)
      val n0e = x(2) context_+ x(5)
      val nr2_1 = ~rad(0)(2)                                // Don't propagate result (so it won't mess up other results)
      // TODO: Combat can't create Mux with non-equivalent types dsptools.numbers.DspComplex@440 and dsptools.numbers.DspComplex@9e    
      val n0c = Mux1H0(nr2_1, n0cTemp)                      // **
      val n0d = Mux1H0(nr2_1, n0dTemp)  
      val n0f = x(2) context_- x(5)
      val n0g = ShiftRegister(x(0), internalDelays(0)) 
      (n0a, n0b, n0c, n0d, n0e, n0f, n0g, n0cTemp, n0dTemp)   
    }

    // DEBUG
    // TODO: Add to utilities
    def connectVecToSeq(v: Vec[DspComplex[T]], s:Seq[DspComplex[T]]) = {
      val shortV = v.take(s.length)
      shortV.zip(s) foreach { case (vel, sel) => vel := sel }
    }
    //connectVecToSeq(io.n0, Seq(n0a, n0b, n0c, n0d, n0e, n0f, n0g, n0cTemp, n0dTemp))

    val (n1a, n1b, n1c, n1d, n1e, n1f, n1g, n1h, n1i, n1j, n1k) = DspContext.withNumAddPipes(internalDelays(1)) {
      val n1a = n0a context_+ n0c
      val n1b = n0a context_- n0c
      val n1c = n0e context_- n0a
      val n1d = n0cTemp context_- n0e                                   // ** Can pass through
      // TODO: What's going on?!
      val n1e = n0dTemp context_+ Mux1H0(s1_1, n0b)                // **
      val n1f = n0b context_- Mux1H0(s0_1, n0d)   
      val n1g = n0f context_- n0b
      val n1h = n0d context_- n0f
      val n1i = ShiftRegister(n0e, internalDelays(1))
      val n1j = ShiftRegister(n0f, internalDelays(1))
      val n1k = ShiftRegister(n0g, internalDelays(1))
      (n1a, n1b, n1c, n1d, n1e, n1f, n1g, n1h, n1i, n1j, n1k)
    }
    //connectVecToSeq(io.n1, Seq(n1a, n1b, n1c, n1d, n1e, n1f, n1g, n1h, n1i, n1j, n1k))

    val (n2a, n2b, n2c, n2d, n2e, n2f, n2g, n2h, n2i) = DspContext.withNumAddPipes(internalDelays(2)) {
      val n2a = n1a context_+ n1i
      val n2b = n1e context_+ n1j
      val n2c = ShiftRegister(Mux(s1_2, n1h, n1b), internalDelays(2))
      val n2d = ShiftRegister(n1b, internalDelays(2))
      val n2e = ShiftRegister(n1c, internalDelays(2))
      val n2f = ShiftRegister(n1d, internalDelays(2))
      val n2g = ShiftRegister(n1f, internalDelays(2))
      val n2h = ShiftRegister(n1g, internalDelays(2))
      val n2i = ShiftRegister(n1k, internalDelays(2))
      (n2a, n2b, n2c, n2d, n2e, n2f, n2g, n2h, n2i)
    }
    //connectVecToSeq(io.n2, Seq(n2a, n2b, n2c, n2d, n2e, n2f, n2g, n2h, n2i))

    // TODO: Check drop is right; can minimize registers some more
    val X0b = ShiftRegister(n1d, internalDelays.drop(2).sum)          // ** First output of second radix 2 butterfly
    val X1b = ShiftRegister(n2b, internalDelays.drop(3).sum)          // ** Second output of second radix 2 butterfly

    // TODO: Add to dsptools?
    abstract class Scalar
    object ScalarReal extends Scalar
    object ScalarImag extends Scalar
    // TODO: More interesting modes? -- using -& now (no pipe)
    def context_complex_scalar_*(a: DspComplex[T], b: T, mode: Scalar): DspComplex[T] = {
      val negB = 
        DspContext.alter(context.copy(overflowType = dsptools.Grow, numAddPipes = 0)) { 
          Ring[T].zero context_- b 
        }
      mode match {
        case ScalarReal => DspComplex.wire(a.real context_* b, a.imag context_* b)
        case ScalarImag => DspComplex.wire(a.imag context_* negB, a.real context_* b)
      }
    }
    def context_complex_scalar_reconfig_*(a: DspComplex[T], b: T, bIsImag: Bool): DspComplex[T] = {
      val negB = 
        DspContext.alter(context.copy(overflowType = dsptools.Grow, numAddPipes = 0)) { 
          Ring[T].zero context_- b 
        }
      val newB = Mux(bIsImag, negB, b)
      val x = a.real context_* b
      val y = a.imag context_* newB
      val bIsImagDelay = ShiftRegister(bIsImag, context.numMulPipes)
      DspComplex.wire(Mux(bIsImagDelay, y, x), Mux(bIsImagDelay, x, y))
    }

    // Multiplier logic optimized
    // Delay same as context
    // TODO: Theoretically don't need _reconfig
    val n3a = context_complex_scalar_*(n2a, A0, ScalarReal)
    val n3b = context_complex_scalar_*(n2d, A1, ScalarReal)
    val n3c = context_complex_scalar_*(n2e, A2, ScalarReal)
    val n3d = context_complex_scalar_*(n2f, A3, ScalarReal)
    val n3e = context_complex_scalar_*(n2b, A4, ScalarImag)
    val n3f = context_complex_scalar_reconfig_*(n2g, A5, bIsImag = MulI5)
    val n3g = context_complex_scalar_*(n2h, A6, ScalarImag)
    val n3h = context_complex_scalar_reconfig_*(n2c, A7, bIsImag = MulI7)
    val n3i = DspContext.withNumAddPipes(context.numMulPipes) { n2i context_+ n2a }
    
    //connectVecToSeq(io.n3, Seq(n3a, n3b, n3c, n3d, n3e, n3f, n3g, n3h, n3i))
    //connectVecToSeqScalar(io.a, Seq(A, C31, C41, C53, C74))
    //connectVecToSeqScalar(io.r, Seq(r4m, r4m, r7m, r2m))

    def connectVecToSeqScalar[R <: Data](v: Vec[R], s:Seq[R]) = {
      val shortV = v.take(s.length)
      shortV.zip(s) foreach { case (vel, sel) => vel := sel }
    }

    // TODO: can't create Mux with FixedPoint with differing binaryPoints -- not sure why that needs
    // to be illegal...

    val (n4a, n4b, n4c, n4d, n4e, n4f, n4g, n4h, n4i) = DspContext.withNumAddPipes(internalDelays(4)) {
      // TODO: Same :(
      val n4a = n3a context_+ Mux1H0(s3_4, n3i)     
      val n4b = ShiftRegister(n3b, internalDelays(4))
      val n4c = ShiftRegister(n3c, internalDelays(4))
      val n4d = ShiftRegister(n3d, internalDelays(4))
      val n4e = ShiftRegister(n3e, internalDelays(4))
      val n4f = ShiftRegister(n3f, internalDelays(4))
      val n4g = ShiftRegister(n3g, internalDelays(4))
      val n4h = ShiftRegister(n3h, internalDelays(4))
      val n4i = ShiftRegister(n3i, internalDelays(4))
      (n4a, n4b, n4c, n4d, n4e, n4f, n4g, n4h, n4i)
    }
    //connectVecToSeq(io.n4, Seq(n4a, n4b, n4c, n4d, n4e, n4f, n4g, n4h, n4i))

    val (n5a, n5b, n5c, n5d, n5e, n5f, n5g, n5h, n5i, n5j, n5k) = DspContext.withNumAddPipes(internalDelays(5)) {
      val n5a = n4a context_+ n4b
      val n5b = n4a context_- n4b
      val n5c = n4a context_- n4d
      val n5d = n4e context_+ Mux1H0(s2_5, n4f)    
      val n5e = n4e context_- n4f
      val n5f = Mux1H0(s1_5, n4e) context_- n4h
      //val n5g = ShiftRegister(Mux(s1_5, n4h, n4g), internalDelays(5))
      // TODO: ... sketchy
      //val temp3 = Wire(if (n4h.getWidth > n4g.getWidth) n4h.cloneType else n4g.cloneType)
      val temp3Real = Mux(s1_5, n4h.real, n4g.real)
      val temp3Imag = Mux(s1_5, n4h.imag, n4g.imag)
      val temp3 = Wire(DspComplex(temp3Real.cloneType, temp3Imag.cloneType))
      temp3.real := temp3Real
      temp3.imag := temp3Imag
      val n5g = ShiftRegister(temp3, internalDelays(5))
      val n5h = ShiftRegister(n4d, internalDelays(5)) 
      val n5i = ShiftRegister(n4c, internalDelays(5))
      val n5j = ShiftRegister(n4g, internalDelays(5)) 
      val n5k = ShiftRegister(n4i, internalDelays(5)) 
      (n5a, n5b, n5c, n5d, n5e, n5f, n5g, n5h, n5i, n5j, n5k)
    }
    //connectVecToSeq(io.n5, Seq(n5a, n5b, n5c, n5d, n5e, n5f, n5g, n5h, n5i, n5j, n5k))

    val (n6a, n6b, n6c, n6d, n6e, n6f, n6g) = DspContext.withNumAddPipes(internalDelays(6)) {
      val n6a = n5a context_+ n5h
      val n6b = n5b context_- n5i
      val n6c = n5i context_+ n5c
      val n6d = n5d context_+ n5g
      val n6e = n5e context_- Mux1H0(s0_6, n5j)
      val n6f = n5j context_+ n5f
      val n6g = ShiftRegister(n5k, internalDelays(6))
      (n6a, n6b, n6c, n6d, n6e, n6f, n6g)
    }
    //connectVecToSeq(io.n6, Seq(n6a, n6b, n6c, n6d, n6e, n6f, n6g))

    val y = DspContext.withNumAddPipes(internalDelays(7)) {
      val yScala = Seq(
        ShiftRegister(n6g, internalDelays(7)),
        n6a context_+ n6d,
        n6b context_+ n6e,
        n6c context_- n6f,
        n6c context_+ n6f,
        n6b context_- n6e,
        n6a context_- n6d
      )
      // TODO: Specialize trim type? -- here's where I bring it down
      // Mux1H also needs fixed BP, therefore can't have different BPs in CIB
      // val yNodes = Wire(CustomIndexedBundle(yScala.map(y => y.cloneType)))
      val yNodes = Wire(Vec(yScala.length, DspComplex(dspDataType)))   
      yScala.zipWithIndex foreach { case (y, idx) => yNodes(idx) := y }
      yNodes
    }

    // Output after 8th delay stage
    val r2o = rad(7)(2)
    val r3o = rad(7)(3)
    val r4o = rad(7)(4)
    val r5o = rad(7)(5)
    val r7o = rad(7)(7)

    // TODO: parameterize if you want 2x rad 2?
    io.y(0) := y(0)
    io.y(1) := Mux(r2o, y(5), y(1))

    if (maxRad > 2)
      io.y(2) := Mux1H(Seq(
        r3o -> y(6),
        r4o -> y(3),
        r5o -> y(5),
        r7o -> y(2),
        r2o -> X0b
      ))

    if (maxRad > 3 || usedRads.contains(2))
      io.y(3) := Mux1H(Seq(
        r5o -> y(2),
        r7o -> y(3),
        r4o -> y(5),
        // TODO: Re-label
        r2o -> X1b
      ))

    if (maxRad > 4)
      io.y(4) := Mux1H(Seq(
        r5o -> y(6),
        r7o -> y(4)
      ))

    // TODO: ... Silly me. I should've made a helper function to hack this
    if (maxRad > 5){
      io.y(5) := Mux1H0(r7o, y(5))    
    }

    if (maxRad > 6) {
      io.y(6) := Mux1H0(r7o, y(6))
    }

/*
    io.currRadOut.elements foreach { case (key, port) =>
      // TODO: Be smarter...
      if (key == "2") io.currRadOut(2) := r2o
      if (key == "3") io.currRadOut(3) := r3o
      if (key == "4") io.currRadOut(4) := r4o
      if (key == "5") io.currRadOut(5) := r5o
      if (key == "7") io.currRadOut(7) := r7o
    }
*/
    
  }
}

// TODO: externally set inputs to zero when not doing FFT