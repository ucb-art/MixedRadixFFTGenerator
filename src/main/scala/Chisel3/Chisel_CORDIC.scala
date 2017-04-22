package dspblocks.fft

import chisel3._
import chisel3.experimental._
import dsptools.numbers._
import dsptools.numbers.implicits._
import chisel3.util._
import org.scalatest.{FlatSpec, Matchers}
import dsptools.{DspTester, DspTesterOptionsManager}
import barstools.tapeout.TestParams

// TODO: Clean up stall stuff from 290C, make rotation/vectoring runtime programmable

// See http://www.andraka.com/files/crdcsrvy.pdf

// x + i * y
case class CordicParams[T <: Data:RealBits](xyType: T, numPipes: Int, isRotation: Boolean) {
  
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
  val numStages = xyWidth

  // Spread out pipeline delay
  private val tempPipes = {
    if (numPipes != 0) {
      val numZeroPipes = numStages / numPipes - 1
      Seq.fill(numPipes)(Seq.fill(numZeroPipes)(0) ++ Seq(1))
    }
    else {
      Seq(Seq.fill(numStages)(0))
    }
  }
  private val leftOverZeroPipes = numStages - tempPipes.flatten.length
  val cordicDelays = tempPipes.zipWithIndex.map { case (pipe, idx) =>
    if (idx < leftOverZeroPipes)
      Seq(0) ++ pipe
    else
      pipe
  }.reverse.flatten
  require(cordicDelays.length == numStages)

}

class CordicIOCore[T <: Data:RealBits](cordicParams: CordicParams[T]) extends Bundle {
  val x = Input(cordicParams.xyType)
  val y = Input(cordicParams.xyType)
  // Angle is signed (modulo 2Pi)
  val angle = Input(cordicParams.angleType)
  // true = rotation; false = vectoring
  // val isRotation = Input(Bool())
  override def cloneType = (new CordicIOCore(cordicParams)).asInstanceOf[this.type]
}

class CordicIO[T <: Data:RealBits](cordicParams: CordicParams[T]) extends Bundle {
  val in = new CordicIOCore(cordicParams)
  val out = Flipped(new CordicIOCore(cordicParams))
  val clk = Input(Clock())
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
class Cordic[T <: Data:RealBits](cordicParams: CordicParams[T]) extends Module with DelayTracking {

  val moduleDelay = cordicParams.numPipes
  
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

  val correctInput = {
    if (cordicParams.isRotation)
      (angleZeroTo2Pi > halfPi.U) & (angleZeroTo2Pi < threeHalvesPi.U)
    else
      io.in.x.signBit
  }

  // CORDIC is more bit-level, so do everything as SInt
  val sintCordicParams = cordicParams.copy(
    xyType = SInt(cordicParams.xyWidth.W)
  )

  val cordicStages = cordicParams.cordicDelays.zipWithIndex.map { case (numPipes, idx) =>
    Module(new CordicStage(sintCordicParams, offset = idx, numPipes = numPipes))
  } 

  // Assign first stage inputs to top level inputs
  // TODO: This doesn't bit grow
  cordicStages(0).io.in.x := Mux(correctInput, -io.in.x, io.in.x).asUInt.asSInt
  cordicStages(0).io.in.y := Mux(correctInput, -io.in.y, io.in.y).asUInt.asSInt
  // Note that for -Pi, overflow is OK, b/c -Pi and +Pi take you to the same place
  cordicStages(0).io.in.angle := 
    Mux(correctInput, io.in.angle.asUInt.asSInt - pi.S, io.in.angle.asUInt.asSInt)

  (0 until cordicParams.numStages) foreach { case stage =>
    cordicStages(stage).io.clk := io.clk
    if (stage == cordicParams.numStages - 1) {
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
class CordicStage[T <: Data:RealBits](cordicParams: CordicParams[T], offset: Int, numPipes: Int) extends Module {
  require(offset >= 0)
  require(numPipes >= 0)

  val io = IO(new CordicIO(cordicParams))

  withClock(io.clk) {

    // z == angle
    // x(i + 1) = x(i) - y(i) * d(i) * 2^(-i)
    // y(i + 1) = y(i) + x(i) * d(i) * 2^(-i)
    // z(i + 1) = z(i) - d(i) * arctan[2^(-i)] where arctan[...] = angle LUT
    // divide by 2^n corresponds to shifting right by n
    // For ROTATION: d(i)=-1 if z(i) < 0, +1 otherwise
    // For VECTORING: d(i)=+1 if y(i) < 0, -1 otherwise

    val y2i = io.in.y >> offset.U
    val x2i = io.in.x >> offset.U

    // Check the MSB of Z or Y when in ROTATION/VECTORING mode to determine sign
    val dSel = 
      if (cordicParams.isRotation) ~(io.in.angle.signBit)
      else io.in.y.signBit

    // atan output is from -pi / 2 to pi / 2
    // TODO: Lots of clean up wrt data types
    val constNormalizedTo2Pi = math.atan(math.pow(2, -offset))  / (2 * math.Pi)
    val sintConst = math.round(constNormalizedTo2Pi * (1 << cordicParams.angleWidth))
    
    val angleChoice = cordicParams.angleType.fromDouble(sintConst)
    val angleChoiceOpposite = cordicParams.angleType.fromDouble(-sintConst)

    val dy2i = Mux(dSel, -y2i, y2i)
    val dx2i = Mux(dSel, x2i, -x2i)
    val dAngle = Mux(dSel, angleChoiceOpposite, angleChoice)

    io.out.x := ShiftRegister(io.in.x + dy2i, numPipes)
    io.out.y := ShiftRegister(io.in.y + dx2i, numPipes)
    io.out.angle := ShiftRegister(io.in.angle + dAngle, numPipes)

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
      numPipes = 3,
      isRotation = false
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
      //step(1)
      //peek(c.io.out.angle)
      step(c.mod.moduleDelay)
      expect(c.io.out.angle, t.theta / math.Pi)
      // Note: theta = math.atan2(y, x)
    }
  }

}

// is the last angle meaningful?
// renormalize memory -- also renormalize this (asUInt)
// is mod 2Pi correct for SInt? 