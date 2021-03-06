package dspblocks.fft
import chisel3._
import barstools.tapeout.transforms._
import chisel3.experimental._
import dsptools.numbers._
import dsptools.numbers.implicits._

// Separate this from crossbar to make macro placement easier
@chiselName
class MemBanks[T <: Data:Ring](dataType: => T, bankLengths: Seq[Int], name: String = "") extends Module with DelayTracking {
  val io = IO(new Bundle {
    val bank = CustomIndexedBundle(bankLengths.map(len => new WriteBeforeReadMemIO(dataType, len)))
  })
  val mems = bankLengths.zipWithIndex.map { case (len, idx) => 
    val mod = Module(new WriteBeforeReadMem(dataType, len, name = s"${name}_$idx"))
    mod.io <> io.bank(idx)
    mod
  }

  val memDelays = mems.map(x => x.moduleDelay).distinct
  require(memDelays.length == 1)
  val moduleDelay = memDelays.head
}