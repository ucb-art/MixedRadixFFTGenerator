package dspblocks.fft
import chisel3._
import chisel3.experimental._
import org.scalatest.{FlatSpec, Matchers}
import barstools.tapeout.TestParams
import dsptools.{DspTester, DspTesterOptionsManager}

class WriteBeforeReadMemWrapper[T <: Data](dataType: => T, val depth: Int) extends Module {
  val mod = Module(new WriteBeforeReadMem(dataType, depth))
  val io = IO(new WriteBeforeReadMemIO(dataType, depth))
  mod.io.waddr := io.waddr
  mod.io.raddr := io.waddr
  mod.io.we := io.we
  mod.io.din := io.din
  io.dout := mod.io.dout
  mod.io.clk := clock
}

class WriteBeforeReadMemIO[T <: Data](dataType: => T, depth: Int) extends Bundle {
  val clk = Input(Clock())
  val waddr = Input(UInt(range"[0, $depth)"))
  val raddr = Input(UInt(range"[0, $depth)"))
  val we = Input(Bool())
  val din = Input(dataType)
  val dout = Output(dataType)
  override def cloneType = (new WriteBeforeReadMemIO(dataType, depth)).asInstanceOf[this.type]
}

// Dual-ported memory
// If memory is written and read at the same time (same address),
// the written memory is passed through to read
class WriteBeforeReadMem[T <: Data](dataType: => T, val depth: Int) extends Module {
  // TODO: Save power by using write mask, read enable
  val io = IO(new WriteBeforeReadMemIO(dataType, depth))
  withClock(io.clk) {
    val mem = SyncReadMem(depth, dataType)
    // Read always enabled
    io.dout := mem.read(io.raddr, true.B)
    when (io.we) { mem.write(io.waddr, io.din) }
  }
}

class WriteBeforeReadMemSpec extends FlatSpec with Matchers {
  behavior of "RightBeforeReadMem"
  it should "behave as --> 0: write addr + data, 1: read addr (same), 2: read data valid" in {
    dsptools.Driver.execute(() => new WriteBeforeReadMemWrapper(UInt(5.W), 20), TestParams.options0Tol) { c =>
      new WriteBeforeReadMemNoConflictTester(c)
    } should be (true)
  }
}

class WriteBeforeReadMemNoConflictTester[T <: Data](c: WriteBeforeReadMemWrapper[T]) extends DspTester(c) {
  // Default seq mem behavior: If you write data at time t; you can get the data
  // back with a read 2 cycles later (no conflict handling)
  val writeData = (0 until c.depth + 2)
  val writeAddr = writeData.map(_ % c.depth)
  val readAddr = Seq(writeAddr.last) ++ writeAddr.init
  val readData = Seq.fill(2)(0) ++ (0 until c.depth)

  reset(10)
  poke(c.io.we, true.B)
  for (t <- writeData) {
    poke(c.io.waddr, writeAddr(t))
    poke(c.io.din, writeData(t))
    poke(c.io.raddr, readAddr(t))
    if (t > 1) expect(c.io.dout, readData(t), "Memory read data wrong!")
    step(1)
  }
}

// Write enable -- try
// Actually check WRiteBeforeRead functionality