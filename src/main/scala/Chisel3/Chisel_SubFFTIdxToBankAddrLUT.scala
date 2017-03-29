package dspblocks.fft
import chisel3._
import barstools.tapeout.transforms._
import chisel3.experimental._

// Only ever used in debug mode + ADC input (also peeling)
// Seq = (bank, addr)
// TODO: Don't use Seq

class SubFFTIdxToBankAddrLUTsIOWithClock(subFFTnsColMaxs: Map[Int, Seq[Int]]) extends Bundle {
  val clk = Input(Clock())
  val pack = new SubFFTIdxToBankAddrLUTsIO(subFFTnsColMaxs)
  override def cloneType = (new SubFFTIdxToBankAddrLUTsIOWithClock(subFFTnsColMaxs)).asInstanceOf[this.type]
}

class SubFFTIdxToBankAddrLUTsIO(val subFFTnsColMaxs: Map[Int, Seq[Int]]) extends Bundle {
  val idxs = new CustomIndexedBundle(
    subFFTnsColMaxs.map { case (subfft, colmax) => subfft -> Input(UInt(range"[0, $subfft)"))}.toSeq: _*
  )
  val bankAddrs = new CustomIndexedBundle(
    subFFTnsColMaxs.map { case (subfft, colmax) => subfft -> new BankAddressBundle(colmax) }.toSeq: _*
  )
  override def cloneType = (new SubFFTIdxToBankAddrLUTsIO(subFFTnsColMaxs)).asInstanceOf[this.type]
}

class SubFFTIdxToBankAddrLUTs(ffastParams: FFASTParams, fftType: FFTType) extends Module with DelayTracking {
  val (lutsTemp, colMaxsTemp) = ffastParams.subFFTns.map { case n => 
    val lutName = s"${fftType.serialize}IdxToBankAddr$n"
    val lutConsts = PeelingScheduling.getIOMemBankAddr(n, fftType).map(x => x.getBankAddr)
    // TODO: Maybe should be programmable?
    val lutMod = Module(new UIntLUT2D(lutName, lutConsts, Seq("bank", "addr"), outputReg = true))
    ((n -> lutMod), (n -> lutMod.colMax))
  }.unzip
  val luts = lutsTemp.toMap
  val colMaxs = colMaxsTemp.toMap
  val io = IO(new SubFFTIdxToBankAddrLUTsIOWithClock(colMaxs))
  ffastParams.subFFTns.foreach { case n => 
    luts(n).io.clk := io.clk
    luts(n).io.addr := io.pack.idxs(n)
    io.pack.bankAddrs(n).bank := luts(n).io.dout("bank")
    io.pack.bankAddrs(n).addr := luts(n).io.dout("addr")  
  }
  val moduleDelay = luts.head._2.moduleDelay
}

    
  