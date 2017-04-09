package dspblocks.fft

import chisel3._
import chisel3.experimental._
import dsptools.numbers._
import dsptools.numbers.implicits._
import chisel3.util._
import org.scalatest.{FlatSpec, Matchers}
import dsptools.{DspTester, DspTesterOptionsManager}
import barstools.tapeout.TestParams

// See http://www.andraka.com/files/crdcsrvy.pdf

// x + i * y
case class CordicParams[T <: Data:RealBits](xyType: T, unrollingFactor: Double) {
  
  val angleTypeTemp = xyType match {
    case _: DspReal => throw new Exception("CORDIC doesn't work with real types!")
    case _: UInt => throw new Exception("Must be signed!")
    case f: FixedPoint => 
      require(f.widthKnown, "xy type width unknown")
      FixedPoint(f.getWidth.W, (f.getWidth - 1).BP)
    case s: SInt => 
      require(s.widthKnown, "xy type width unknown")
      s
  }
  val angleType = angleTypeTemp.asInstanceOf[T]

  val xyWidth = xyType.getWidth
  val angleWidth = angleType.getWidth
  val totalIterations = xyWidth
  // unrollingFactor = 0 -> no unfolding (1 stage)
  // unrollingFactor = 1 -> complete unfolding (iterations # of stages)
  val numStages = if (unrollingFactor > 0) math.ceil(totalIterations * unrollingFactor).toInt else 1
  val numIterationsPerStage = totalIterations / numStages
  // Want to distribute iterations as evenly as possibly (difference of 1 max)
  // i.e. if leftoveriterations is non-zero, stages will have different #'s of iterations
  val leftoverIterations = totalIterations % numStages

}

class CordicIOCore[T <: Data:RealBits](cordicParams: CordicParams[T]) extends Bundle {
  val x = Input(cordicParams.xyType)
  val y = Input(cordicParams.xyType)
  // Angle is signed (modulo 2Pi)
  val angle = Input(cordicParams.angleType)
  // true = rotation; false = vectoring
  val isRotation = Input(Bool())
  val valid = Input(Bool())
  override def cloneType = (new CordicIOCore(cordicParams)).asInstanceOf[this.type]
}

class CordicIO[T <: Data:RealBits](cordicParams: CordicParams[T]) extends Bundle {
  val in = new CordicIOCore(cordicParams)
  val out = Flipped(new CordicIOCore(cordicParams))
  val clk = Input(Clock())
  val reset = Input(Bool())
  override def cloneType = (new CordicIO(cordicParams)).asInstanceOf[this.type]
}

// Ex: 16-bit SInt Z --> Pi / 6 rad = (Pi / 6) / (2 * Pi) * 2 ^ 16
// Valid angle [0, 2 * pi)
// Cordic can do a max rotation of +/- Pi / 2
// In rotation mode, if Pi / 2 < Z < 3 * Pi / 2,
// need additional rotation by Pi for Cordic to work
// In vectoring mode, if X < 0 (quadrant 2, 3), need additional rotation by Pi 
// Since you're rotating back to positive X when you're correcting:
// x' = -x
// y' = -y
// z' = z - Pi
@chiselName
class Cordic[T <: Data:RealBits](cordicParams: CordicParams[T]) extends Module {
  
  val io = IO(new CordicIO(cordicParams))

  // 4-bit signed:
  // 15 = -1 = 1111
  // 12 = -4 = 1100 = -Pi/2  = 3 * Pi / 2
  //  9 = -7 = 1001
  //  8 = -8 = 1000 = -Pi    = Pi
  //  7 =  7 = 0111
  //  4 =  4 = 0100 = Pi / 2 = Pi / 2
  //  0 =  0 = 0000
  // i.e. looking at [-pi, pi) vs. [0, 2pi)

  // let's be consistent and represent as unsigned [0, 2pi) as a reference
  val halfPi = 1 << (cordicParams.angleWidth - 2)
  val pi = 1 << (cordicParams.angleWidth - 1)
  val threeHalvesPi = halfPi * 3
  val angleZeroTo2Pi = io.in.angle.asUInt

  val correctInput = Mux(
    io.in.isRotation,
    (angleZeroTo2Pi > halfPi.U) & (angleZeroTo2Pi < threeHalvesPi.U),
    io.in.x.signBit
  )

  // Distribute "leftover iterations" properly total # of iterations 
  // doesn't divide # of stages evenly (extras given to front stages)
  val stageIterations = (0 until cordicParams.numStages).map { case stage => 
    if (stage < cordicParams.leftoverIterations) 
      cordicParams.numIterationsPerStage + 1
    else
      cordicParams.numIterationsPerStage
  }

  // Each stage starts at offset based off of how many iterations have been previously done
  // Initial stage has offset 0 from default value
  val stageOffset = stageIterations.init.scanLeft(0) { case (accum, next) => accum + next }

  // CORDIC is more bit-level, so do everything as SInt
  val sintCordicParams = cordicParams.copy(
    xyType = SInt(cordicParams.xyWidth.W)
  )

  val cordicStages = stageIterations.zip(stageOffset).map { case (iterations, offset) => 
    Module(new CordicStage(sintCordicParams, offset = offset, iterations = iterations))
  }

  // Assign first stage inputs to top level inputs
  // TODO: This doesn't bit grow
  cordicStages(0).io.in.isRotation := io.in.isRotation
  cordicStages(0).io.in.valid := io.in.valid
  cordicStages(0).io.in.x := Mux(correctInput, -io.in.x, io.in.x).asUInt.asSInt
  cordicStages(0).io.in.y := Mux(correctInput, -io.in.y, io.in.y).asUInt.asSInt
  // Note that for -Pi, overflow is OK, b/c -Pi and +Pi take you to the same place
  cordicStages(0).io.in.angle := 
    Mux(correctInput, io.in.angle.asUInt.asSInt - pi.S, io.in.angle.asUInt.asSInt)

  (0 until cordicParams.numStages) foreach { case stage =>
    cordicStages(stage).io.clk := io.clk
    cordicStages(stage).io.reset := io.reset
    if (stage == cordicParams.numStages - 1) {
      io.out.isRotation := cordicStages(stage).io.out.isRotation
      io.out.valid := cordicStages(stage).io.out.valid
      io.out.x := cordicParams.xyType.fromBits(cordicStages(stage).io.out.x.asUInt)
      io.out.y := cordicParams.xyType.fromBits(cordicStages(stage).io.out.y.asUInt)
      io.out.angle := cordicParams.angleType.fromBits(cordicStages(stage).io.out.angle.asUInt)
    }
    else cordicStages(stage + 1).io.in := cordicStages(stage).io.out
  }
}

// Offset is the starting iteration to indicate shift/angle constant value 
// First rotation (offset 0) handles x0, y0, angle0, arctan(2^0)
// For example, CordicStage(offset=2, iterations=3) should generate the HW to do the following
// - In the same cycle that the input is valid, perform rotation with arctan(2^-2) : count = 0
// - In the next cycle, perform rotation with arctan(2^-3) on the previous result : count = 1
// - In the next cycle, perform rotation with arctan(2^-4) on the previous result : count = 2
// - In the next cycle, output should be valid (so the next stage can perform rotation with arctan(2^-5)) : count = 0
//   and computation will start again *if* the input is valid

@chiselName
class CordicStage[T <: Data:RealBits](cordicParams: CordicParams[T], offset: Int, iterations: Int) extends Module {
  require(offset >= 0)
  require(iterations >= 1)

  val io = IO(new CordicIO(cordicParams))

  withClockAndReset(io.clk, io.reset) {

    // z == angle
    // x(i + 1) = x(i) - y(i) * d(i) * 2^(-i)
    // y(i + 1) = y(i) + x(i) * d(i) * 2^(-i)
    // z(i + 1) = z(i) - d(i) * arctan[2^(-i)] where arctan[...] = angle LUT
    // divide by 2^n corresponds to shifting right by n
    // For ROTATION: d(i)=-1 if z(i) < 0, +1 otherwise
    // For VECTORING: d(i)=+1 if y(i) < 0, -1 otherwise

    // TODO: Make this clearer? -- do my usual way; this = remnants from class project
    val countReset = Wire(Bool())
    val count = withClockAndReset(io.clk, countReset) {
      RegInit(0.U(BigInt(iterations - 1).bitLength.W))
    }
    val countIsMax = count === (iterations - 1).U
    val countIsZero = count === 0.U
    count := count + 1.U
    countReset := countIsMax || (countIsZero && ~(io.in.valid))

    // Valid should only be high for 1 clk cycle
    val valid = {
      val next = 
        if (iterations == 1) io.in.valid
        else countIsMax
      RegNext(next = next, init = false.B)
    }
    io.out.valid := valid

    // Mode needs to be registered for pipelining in case
    // stage isn't done processing previous data/mode when a new mode comes in
    val regIsRotation = RegEnable(next = io.in.isRotation, enable = countIsZero) 
    io.out.isRotation := regIsRotation

    // When performing calculations, curr__ is = input __ during the 0th count
    // but is = the registered value in the block during all subsequent counts
    // since the values are always updated on the next clk cycle after 
    // a condition is satisfied
    val currIsRotation = Mux(countIsZero, io.in.isRotation, regIsRotation)
    val regX = Reg(t = cordicParams.xyType)
    val regY = Reg(t = cordicParams.xyType)
    val regAngle = Reg(t = cordicParams.angleType)
    val currX = Mux(countIsZero, io.in.x, regX)
    val currY = Mux(countIsZero, io.in.y, regY)
    val currAngle = Mux(countIsZero, io.in.angle, regAngle)

    // TODO: This won't bit grow, so what's a sufficient # of bits?
    // Barrel shifter
    val maxShift = offset + iterations - 1
    val shift = Wire(UInt(range"[0, $maxShift]"))
    shift := offset.U + count

    // Need to offset which shift is being done depending on which iteration the stage
    // is currently processing
    val y2i = currY >> shift
    val x2i = currX >> shift

    // Check the MSB of Z or Y when in ROTATION/VECTORING mode to determine sign
    val dSel = Mux(currIsRotation, ~(currAngle.signBit), currY.signBit)

    // atan output is from -pi / 2 to pi / 2
    // TODO: Lots of clean up wrt data types
    val (angleLUTVals, widths) =
      (0 until iterations).map { case i => 
        val const = math.atan(math.pow(2, -(offset + i))) 
        val constNormalizedTo2Pi = const / (2 * math.Pi)
        val sintConst = math.round(constNormalizedTo2Pi * (1 << cordicParams.angleWidth))
        val width = BigInt(sintConst).bitLength + 1
        (sintConst, width)
      }.unzip
    // TODO: Convert to LUT? / use width
    val lutMaxWidth = widths.max
    val angleLUT = Vec(angleLUTVals.map(v => 
      cordicParams.angleType.fromDoubleWithFixedWidth(v)
    ))
    val angleLUTout = angleLUT(count)

    val dy2i = Mux(dSel, -y2i, y2i)
    val dx2i = Mux(dSel, x2i, -x2i)
    val dAngle = Mux(dSel, -angleLUTout, angleLUTout)

    regY := currY + dx2i
    regX := currX + dy2i
    // TODO: Remove hack
    regAngle := currAngle + dAngle

    io.out.x := regX
    io.out.y := regY
    io.out.angle := regAngle

    // Note: -pi, pi are the same thing, so overflow with multiplication by a negative is OK
  }
}

class CordicSpec extends FlatSpec with Matchers {
  behavior of "Cordic"
  it should "pass tests" in {

    import dspblocks.fft.FFASTTopParams._

    val opt = TestParams.optionsBTolWaveformTB(
      lsbs = 8, 
      outDir = "test_run_dir/CordicTB", 
      genVerilogTB = false
    )
    val params = CordicParams(
      xyType = FixedPoint(23.W, 21.BP),
      unrollingFactor = 0
    )
    
    dsptools.Driver.execute(() => 
      new CordicWrapper(params), opt
    ) { c =>
      new CordicTester(c)
    } should be (true)
    
  }
}

class CordicWrapper[T <: Data:RealBits](cordicParams: CordicParams[T]) extends chisel3.Module {
  val io = IO(new CordicIO(cordicParams))
  val mod = Module(new Cordic(cordicParams))
  mod.io.in := io.in
  io.out := mod.io.out
  mod.io.clk := clock
  mod.io.reset := reset
}

// TODO: Have this calculate x, y?
case class CordicTests(x: Double, y: Double, r: Double, theta: Double)

class CordicTester[T <: Data:RealBits](c:CordicWrapper[T]) extends DspTester(c) {
  // Have step size be half of 1 bin 
  val fftn = 21600
  val stepSize = 1.toDouble / fftn * 2 * math.Pi / 2
  val magnitudes = Seq(0.01, 0.1, 1.0)
  val angles = -math.Pi until math.Pi by stepSize
  val tests = for (r <- magnitudes; theta <- angles) yield {
    val x = r * math.cos(theta)
    val y = r * math.sin(theta)
    CordicTests(x, y, r, theta)
  }

  println("Number of angles tested: " + angles.length)

  updatableDspVerbose.withValue(false) {
    for (t <- tests) {
      poke(c.io.in.x, t.x)
      poke(c.io.in.y, t.y)
      poke(c.io.in.angle, 0.0)
      poke(c.io.in.isRotation, false)
      poke(c.io.in.valid, true)
      step(1)
      poke(c.io.in.valid, false)
      while(!peek(c.io.out.valid)) {
        step(1)
      }
      expect(c.io.out.angle, t.theta / math.Pi)
      // Note: theta = math.atan2(y, x)
    }
  }

}

// TODO: remove all pipeline regs
// is the last angle meaningful?
// renormalize memory -- also renormalize this (asUInt)
// is mod 2Pi correct for SInt? 