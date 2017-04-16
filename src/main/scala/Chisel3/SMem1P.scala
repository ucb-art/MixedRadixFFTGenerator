package dspblocks.fft
import chisel3._
import dsptools.numbers._
import dsptools.numbers.implicits._
import chisel3.experimental._

class SMem1PIO[T <: Data](dataType: => T, val depth: Int) extends Bundle {
  val clk = Input(Clock())
  // From backend
  val waddr = Input(UInt(range"[0, $depth)"))
  // From SCR
  val raddr = Input(UInt(range"[0, $depth)"))
  val we = Input(Bool())
  val re = Input(Bool())
  val din = Input(dataType)
  val dout = Output(dataType)
  override def cloneType = (new SMem1PIO(dataType, depth)).asInstanceOf[this.type]
}

@chiselName
class SMem1P[T <: Data](dataType: => T, depth: Int, name: String = "") extends Module with DelayTracking {
  val moduleDelay = 2
  val io = IO(new SMem1PIO(dataType, depth))
  val mem = SyncReadMem(depth, dataType)
  mem.suggestName(name)
  withClock(io.clk) {
    when (io.we) { mem.write(io.waddr, io.din) }
    val re = io.re & (~io.we) 
    // 1 clk delay (read address registered), also register dout due to long scr path
    io.dout := RegNext(mem.read(io.raddr, re))
  }
}