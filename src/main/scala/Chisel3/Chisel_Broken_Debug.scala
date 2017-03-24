package dspblocks.fft
import chisel3._
import chisel3.experimental._
import dsptools.numbers._
import dsptools.numbers.implicits._
import org.scalatest.{FlatSpec, Matchers}
import dsptools.{DspTesterOptionsManager, DspTester}

class Borked[T <: Data:RealBits](dspDataType: => T) extends Module {
  val io = IO(new Bundle {
    val sels = Input(Vec(4, Bool()))
    val out = Output(DspComplex(dspDataType))
  })

  val one = Wire(dspDataType)
  one := dspDataType.fromDoubleWithFixedWidth(1.0)
  val C52 = Wire(dspDataType)
  C52 := dspDataType.fromDoubleWithFixedWidth(-1.5388417840003967)
  val C75 = Wire(dspDataType)
  C75 := dspDataType.fromDoubleWithFixedWidth(-0.34087294340133667)
  val A5 = Mux1H(Seq(
    io.sels(0) -> one,
    io.sels(1) -> C52,
    io.sels(2) -> C75,
    io.sels(3) -> one
  ))
  io.out.real := A5
}

class BorkedSpec extends FlatSpec with Matchers {
  behavior of "Borked"
  it should "not suck" in {
    val opt = new DspTesterOptionsManager 
    dsptools.Driver.execute(() => new Borked(dspDataType = FixedPoint(28.W, 24.BP)), opt) { c =>
      new BorkedTester(c)
    } should be (true)
  }
}

class BorkedTester[T <: Data:RealBits](c: Borked[T]) extends DspTester(c) {
  poke(c.io.sels(0), false.B)
  poke(c.io.sels(1), false.B)
  poke(c.io.sels(3), false.B)
  poke(c.io.sels(2), true.B)
  peek(c.io.out.real)
}