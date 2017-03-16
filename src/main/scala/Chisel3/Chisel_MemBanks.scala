package dspblocks.fft
import chisel3._

// Separate this from crossbar to make macro placement easier
class MemBanks[T <: Data](dataType: => T, bankLengths: Seq[Int]) extends Module {
  val io = IO(new Bundle {
    val bank = Vec(
      bankLengths.map(len => new WriteBeforeReadMemIO(dataType, len))
    )
  })
  val mems = bankLengths.zipWithIndex.map { case (len, idx) => 
    val mod = Module(new WriteBeforeReadMem(dataType, len))
    mod.io <> io.bank(idx)
    mod
  }
}