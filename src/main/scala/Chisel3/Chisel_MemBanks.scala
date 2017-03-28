package dspblocks.fft
import chisel3._
import barstools.tapeout.transforms._
import chisel3.experimental._

// Separate this from crossbar to make macro placement easier
@chiselName
class MemBanks[T <: Data](dataType: => T, bankLengths: Seq[Int], name: String = "") extends Module {
  val io = IO(new Bundle {
    val bank = CustomIndexedBundle(bankLengths.map(len => new WriteBeforeReadMemIO(dataType, len)))
  })
  val mems = bankLengths.zipWithIndex.map { case (len, idx) => 
    val mod = Module(new WriteBeforeReadMem(dataType, len, name = s"${name}_$idx"))
    mod.io <> io.bank(idx)
    mod
  }
}