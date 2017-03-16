package dspblocks.fft
import chisel3._
import chisel3.experimental._
import org.scalatest.{FlatSpec, Matchers}
import barstools.tapeout.TestParams
import dsptools.{DspTester, DspTesterOptionsManager}

/*

class WriteBeforeReadMemWrapper[T <: Data](dataType: => T, val depth: Int) extends Module {
  // TODO: Save power by using write mask, read enable
  val io = IO(new Bundle {
    val waddr = Input(UInt(range"[0, $depth)"))
    val raddr = Input(UInt(range"[0, $depth)"))
    val we = Input(Bool())
    val din = Input(dataType)
    val dout = Output(dataType)
  })
  val mod = Modules(new WriteBeforeReadMem(dataType, depth))
  mod.io.waddr := io.wddr
  mod.io.raddr := io.waddr
  mod.io.we := io.we
  mod.io.din := io.din
  io.dout := mod.io.dout
  mod.io.clk := clock
}

// Dual-ported memory
// If memory is written and read at the same time (same address),
// the written memory is passed through to read
class WriteBeforeReadMem[T <: Data](dataType: => T, val depth: Int) extends Module {
  // TODO: Save power by using write mask, read enable
  val io = IO(new Bundle {
    val clk = Input(Clock())
    val waddr = Input(UInt(range"[0, $depth)"))
    val raddr = Input(UInt(range"[0, $depth)"))
    val we = Input(Bool())
    val din = Input(dataType)
    val dout = Output(dataType)
  })
  
  val mem = SyncReadMem(depth, dataType)
  // Read always enabled
  io.dout := mem.read(io.raddr, Bool(true))
  when (io.we) { mem.write(io.wAddr, io.din) }
}

class WriteBeforeReadMemSpec extends FlatSpec with Matchers {
  behavior of "RightBeforeReadMem"
  it should "behave as --> 0: write addr + data, 1: read addr (same), 2: read data valid" in {
    dsptools.Driver.execute(() => new WriteBeforeReadMem(UInt(5.W), 20), TestParams.options0Tol) { c =>
      new WriteBeforeReadMemNoConflictTester(c)
    } should be (true)
  }
}





class WriteBeforeReadMemNoConflictTester(c: WriteBeforeReadMem) extends DspTester(c) {
  // Default seq mem behavior: If you write data at time t; you can get the data
  // back with a read 2 cycles later (no conflict handling)
  val writeData = (0 until c.depth + 2)
  val writeAddr = writeData.map(_ % c.depth)
  val rAddr = Seq(writeAddr.last) ++ writeAddr.init
  val readData = Seq.fill(2)(0) ++ (0 until c.depth)




// we???



  reset(10)
  for (t <- writeData) {
    step(1)
  }
}





class UIntLUT2DTester(c: UIntLUT2D) extends DspTester(c) {
  c.tableIn.zipWithIndex foreach { case (row, idx) =>
    poke(c.io.addr, idx)
    val peekedVals = c.io.dout.elements.map { case (key, value) => peek(value) }
    expect(peekedVals == row, s"Peeked LUT value must match for ${c.blackBoxName}")
  }
}

*/