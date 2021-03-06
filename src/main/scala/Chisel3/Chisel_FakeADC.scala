package dspblocks.fft
import dsptools.numbers._
import dsptools.numbers.implicits._
import chisel3._
import chisel3.experimental._
import dsptools.{DspTester, DspTesterOptionsManager}
import org.scalatest.{FlatSpec, Matchers}
import barstools.tapeout.TestParams
import rocketchiselutil._
import chisel3.util._

class FakeADCIO[T <: Data:RealBits](gen: => T) extends Bundle {
  val clk = Input(Clock())
  val analogIn = Input(DspReal())
  val digitalOut = Output(gen)
  override def cloneType = (new FakeADCIO(gen)).asInstanceOf[this.type]
}

// WARNING: NON-SYNTHESIZABLE!!!
// 2 cycle "delay": need to sample one 1 clk, then valid @ end of SAR logic
class FakeADC[T <: Data:RealBits](gen: => T) extends Module {
  val io = IO(new FakeADCIO(gen))
  withClock(io.clk) {
     gen match {
      case _: UInt => throw new Exception("ADC gen should be signed!")
      case _: SInt => io.digitalOut := ShiftRegister(io.analogIn.intPart, 2)
      case f: FixedPoint => io.digitalOut := ShiftRegister(io.analogIn.asFixed(f), 2)
      case _: DspReal => io.digitalOut := ShiftRegister(io.analogIn, 2)
      case _ => throw new Exception("Invalid gen for ADC!")
    }
  }
}

class FakeADCWrapper[T <: Data:RealBits](gen: => T) extends chisel3.Module {
  val mod = Module(new FakeADC(gen))
  val io = IO(new FakeADCIO(gen))
  mod.io.clk := clock 
  mod.io.analogIn := io.analogIn
  io.digitalOut := mod.io.digitalOut
}

class FakeADCSpec extends FlatSpec with Matchers {
  behavior of "FakeADC"
  it should "digitize Real inputs" in {
    dsptools.Driver.execute(() => new FakeADCWrapper(FixedPoint(10.W, 5.BP)), TestParams.options1Tol) { c =>
      new FakeADCTester(c)
    } should be (true)
  }
}

class FakeADCTester(c: FakeADCWrapper[FixedPoint]) extends DspTester(c) {
  val ins = (-5.0 until 5.0 by 0.1) ++ Seq.fill(2)(0.0)
  val outs = Seq.fill(2)(ins.last) ++ ins.init.init
  // Delay of 2
  ins.zip(outs).zipWithIndex foreach { case ((in, out), idx) =>
    poke(c.io.analogIn, in)
    // Last bit of digitizer is uncertain (hence need options1Tol)
    if (idx > 1) expect(c.io.digitalOut, out)
    step(1)
  }

}