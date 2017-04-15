package dspblocks.fft
import chisel3._
import chisel3.experimental._
import org.scalatest.{FlatSpec, Matchers}
import barstools.tapeout.TestParams
import dsptools.{DspTester, DspTesterOptionsManager}
import dsptools.numbers._
import breeze.math.Complex
import chisel3.experimental.FixedPoint
import dsptools.numbers.implicits._

// TODO: Write mask -- gradually enable across FFT stages!

class WriteBeforeReadMemWrapper[T <: Data:Ring](dataType: => T, val depth: Int) extends chisel3.Module {
  val mod = Module(new WriteBeforeReadMem(dataType, depth))
  val io = IO(new WriteBeforeReadMemIO(dataType, depth))
  mod.io.waddr := io.waddr
  mod.io.raddr := io.raddr
  mod.io.we := io.we
  mod.io.din := io.din
  io.dout := mod.io.dout
  mod.io.clk := clock
  mod.io.re := io.re
}

class WriteBeforeReadMemIO[T <: Data:Ring](dataType: => T, depth: Int) extends Bundle {
  val clk = Input(Clock())
  val waddr = Input(UInt(range"[0, $depth)"))
  val raddr = Input(UInt(range"[0, $depth)"))
  val we = Input(Bool())
  val re = Input(Bool())
  val din = Input(dataType)
  val dout = Output(dataType)
  override def cloneType = (new WriteBeforeReadMemIO(dataType, depth)).asInstanceOf[this.type]
}

// Dual-ported memory
// If memory is written and read at the same time (same address),
// the written memory is passed through to read
@chiselName
class WriteBeforeReadMem[T <: Data:Ring](dataType: => T, val depth: Int, name: String = "") extends Module with DelayTracking {

  // Read address delayed
  val moduleDelay = 1

  // TODO: Save power by using write mask, read enable
  val io = IO(new WriteBeforeReadMemIO(dataType, depth))
  withClock(io.clk) {
    val mem = SyncReadMem(depth, dataType)
    mem.suggestName(name)
    // Read always enabled
    // When read, write happen at the same time, pass write through
    // Note: Need to match delays!

    // This is, in reality, kind of annoying, but whatever...
    // When re is low, prevent garbage from going through. (Write output has 1 delay)

    // TODO: Loosen so no ring?
    // Safer to read twice! (or technically check re)
    val memOut = mem.read(io.raddr, io.re)
    // Mux1H(Seq(RegNext(io.re) -> mem.read(io.raddr, io.re)))

    io.dout := Mux(
      RegNext(io.waddr === io.raddr && io.we), 
      RegNext(io.din), 
      memOut)
    when (io.we) { mem.write(io.waddr, io.din) }
  }
}

class WriteBeforeReadMemSpec extends FlatSpec with Matchers {
  behavior of "RightBeforeReadMem"
  it should "behave as --> 0: write addr + data, 1: read addr (same), 2: read data valid with Complex" in {
    val opt = new DspTesterOptionsManager {
      dspTesterOptions = TestParams.options0Tol.dspTesterOptions.copy(genVerilogTb = true)
      testerOptions = TestParams.options0Tol.testerOptions
      commonOptions = TestParams.options0Tol.commonOptions.copy(targetDirName = s"test_run_dir/ComplexWriteBeforeReadMem")
    }
    dsptools.Driver.execute(() => new WriteBeforeReadMemWrapper(DspComplex(FixedPoint(7.W, 1.BP)), 20), opt) { c =>
      new WriteBeforeReadMemComplexTester(c)
    } should be (true)
  }
  it should "behave as --> 0: write addr + same read addr + data, 1: read data valid (Write Before Read) with UInt" in {
    val opt = new DspTesterOptionsManager {
      dspTesterOptions = TestParams.options0Tol.dspTesterOptions
      testerOptions = TestParams.options0Tol.testerOptions
      commonOptions = TestParams.options0Tol.commonOptions.copy(targetDirName = s"test_run_dir/WriteBeforeReadMemConflict")
    }
    dsptools.Driver.execute(() => new WriteBeforeReadMemWrapper(UInt(5.W), 20), opt) { c =>
      new WriteBeforeReadMemConflictTester(c)
    } should be (true)
  }
}

class WriteBeforeReadMemComplexTester(c: WriteBeforeReadMemWrapper[DspComplex[FixedPoint]]) extends DspTester(c) {
  // Default seq mem behavior: If you write data at time t; you can get the data
  // back with a read 2 cycles later (no conflict handling)
  val writeAddr = (0 until c.depth + 2)
  val writeData = writeAddr.map(x => Complex(x + 0.5, -x))
  val readAddr = Seq(writeAddr.last) ++ writeAddr.init
  val readData = Seq.fill(2)(Complex(0, 0)) ++ writeData.dropRight(2)

  reset(10)
  poke(c.io.re, true.B)
  poke(c.io.we, true.B)
  for (t <- writeAddr) {
    poke(c.io.waddr, writeAddr(t))
    poke(c.io.din.asInstanceOf[DspComplex[FixedPoint]], writeData(t))
    poke(c.io.raddr, readAddr(t))
    if (t > 1) expect(c.io.dout.asInstanceOf[DspComplex[FixedPoint]], readData(t))
    step(1)
  }
  // Should keep old value
  poke(c.io.we, false.B)
  for (t <- writeAddr) {
    poke(c.io.waddr, writeAddr(t))
    poke(c.io.din.asInstanceOf[DspComplex[FixedPoint]], Complex(c.depth + 1, 0) - writeData(t))
    poke(c.io.raddr, readAddr(t))
    if (t > 1) expect(c.io.dout.asInstanceOf[DspComplex[FixedPoint]], readData(t))
    step(1)
  }
}

class WriteBeforeReadMemConflictTester[T <: Data](c: WriteBeforeReadMemWrapper[T]) extends DspTester(c) {
  // Default seq mem behavior: If you write data at time t; you can get the data
  // back with a read 2 cycles later (no conflict handling)
  val writeData = (0 until c.depth + 1)
  val writeAddr = writeData
  val readData = Seq.fill(1)(0) ++ (0 until c.depth)

  reset(10)
  poke(c.io.re, true.B)
  poke(c.io.we, true.B)
  for (t <- writeData) {
    poke(c.io.waddr, writeAddr(t))
    poke(c.io.din, writeData(t))
    poke(c.io.raddr, writeAddr(t))
    if (t > 0) expect(c.io.dout, readData(t))
    step(1)
  }
  // Note: when address exceeds depth (but is still valid wrt address bitwidth) it shouldn't write!
  // i.e. address 20 for depth 20
  poke(c.io.we, false.B)
  for (t <- writeData) {
    poke(c.io.waddr, writeAddr(t))
    // garbage b/c we is off
    poke(c.io.din, c.depth - writeData(t))
    poke(c.io.raddr, writeAddr(t))
    if (t > 0) expect(c.io.dout, readData(t))
    step(1)
  }
}