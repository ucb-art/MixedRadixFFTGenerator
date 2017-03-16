package dspblocks.fft

import chisel3._
import chisel3.experimental._
import chisel3.util.ShiftRegister
import dsptools.{hasContext, DspContext}
import dsptools.numbers._
import dsptools.numbers.implicits._
import barstools.tapeout.transforms._
import org.scalatest.{FlatSpec, Matchers}
import dsptools.{DspTester, DspTesterOptionsManager}
import barstools.tapeout.TestParams
import barstools.modules.{UIntLUT2D}

class BinToSubFFTIdxIO(fftBinMax: Int, subFFTns: Seq[Int]) extends Bundle {
  val clk = Input(Clock())
  val fftBin = Input(UInt(range"[0, $fftBinMax]"))
  val subFFTIdx = Output(new CustomIndexedBundle(
     subFFTns.map(subFFT => subFFT -> UInt(range"[0, $subFFT)")): _*
  ))
  override def cloneType = (new BinToSubFFTIdxIO(fftBinMax, subFFTns)).asInstanceOf[this.type]
}

class BinToSubFFTIdx(val ffastParams: FFASTParams) extends Module with DelayTracking {

  val fftBinMax = ffastParams.fftn - 1

  val io = IO(new BinToSubFFTIdxIO(fftBinMax, ffastParams.subFFTns))

  val modOuts = ffastParams.subFFTns.map(n => ConstantMod(io.fftBin, n, xmax = fftBinMax))
  io.subFFTIdx.elements.zip(modOuts) foreach { case ((key, o), (res, mod)) =>
    o := res
    mod.io.clk := io.clk
  }

  val moduleDelay = modOuts.head._2.moduleDelay 

}

class BinToSubFFTIdxWrapper(val ffastParams: FFASTParams) extends Module {
  val mod = Module(new BinToSubFFTIdx(ffastParams))
  val io = IO(new BinToSubFFTIdxIO(mod.fftBinMax, ffastParams.subFFTns))
  mod.io.clk := clock
  mod.io.fftBin := io.fftBin 
  io.subFFTIdx := mod.io.subFFTIdx
}

class BinToSubFFTIdxSpec extends FlatSpec with Matchers {
  behavior of "BinToSubFFTIdx"
  it should "map an FFT bin to all sub FFT bins via mod" in {
    val ffastParams = FFASTParams(fftn = 21600, subFFTns = Seq(675, 800, 864))
    dsptools.Driver.execute(() => new BinToSubFFTIdxWrapper(ffastParams), TestParams.options0Tol) { c =>
      new BinToSubFFTIdxTester(c)
    } should be (true)
  }
}

class BinToSubFFTIdxTester(c: BinToSubFFTIdxWrapper) extends DspTester(c) {
  val map = PeelingScheduling.getBinToSubFFTIdxMap(c.ffastParams)
  map.zipWithIndex foreach { case (row, idx) =>
    poke(c.io.fftBin, idx)
    row foreach { case (subFFT, expectedIdx) =>
      require(expect(c.io.subFFTIdx(subFFT), expectedIdx), s"BinToSubFFTIdx failed for bin $idx")
    }
  }
}

class BankAddressBundle(colMax: Seq[Int]) extends Bundle {
  // NOTE: NEEDS TO BE IN THIS ORDER
  val bankMax = colMax.head
  val addrMax = colMax.last
  val bank = Output(UInt(range"[0, $bankMax]"))
  val addr = Output(UInt(range"[0, $addrMax]"))
  override def cloneType = (new BankAddressBundle(colMax)).asInstanceOf[this.type]
}

class BinToSubFFTBankAddrIO[T <: Data](fftBinType: T, subFFTns: Seq[Int], colMaxs: Map[Int, Seq[Int]]) extends Bundle {
  val clk = Input(Clock())
  val fftBin = Input(fftBinType.cloneType)
  val bankAddrs = Output(new CustomIndexedBundle(
    subFFTns.map(
      subFFT => subFFT -> new BankAddressBundle(colMaxs(subFFT))
    ): _*
  ))
  override def cloneType = (new BinToSubFFTBankAddrIO(fftBinType, subFFTns, colMaxs)).asInstanceOf[this.type]
}

class BinToSubFFTBankAddr(val ffastParams: FFASTParams, val fftType: FFTType) extends Module with DelayTracking {

  val binToSubFFTIdx = Module(new BinToSubFFTIdx(ffastParams))
  val subFFTIdxToBankAddrLUTMods = ffastParams.subFFTns.map { n => 
    val lut = dspblocks.fft.PeelingScheduling.getIOMemBankAddr(n, fftType).map(x => x.getBankAddr)
    val lutMod = Module(new UIntLUT2D(s"subFFTIdxToBankAddrLUT$n", lut, Seq("bank", "addr")))
    lutMod.io.addr := binToSubFFTIdx.io.subFFTIdx(n)
    n -> lutMod
  }.toMap

  val colMaxs = subFFTIdxToBankAddrLUTMods.map { case (subFFT, mod) => subFFT -> mod.colMax}.toMap

  val io = IO(new BinToSubFFTBankAddrIO(binToSubFFTIdx.io.fftBin, ffastParams.subFFTns, colMaxs))

  binToSubFFTIdx.io.fftBin := io.fftBin
  io.bankAddrs.elements foreach { case (subFFT, bankAddrOutput) =>
    bankAddrOutput.bank := subFFTIdxToBankAddrLUTMods(subFFT.toInt).io.dout("bank")
    bankAddrOutput.addr := subFFTIdxToBankAddrLUTMods(subFFT.toInt).io.dout("addr")
  }

  binToSubFFTIdx.io.clk := io.clk
  subFFTIdxToBankAddrLUTMods foreach { case (n, mod) => mod.io.clk := io.clk }

  def moduleDelay = binToSubFFTIdx.moduleDelay + subFFTIdxToBankAddrLUTMods(ffastParams.subFFTns.head).moduleDelay

}

class BinToSubFFTBankAddrWrapper(val ffastParams: FFASTParams, val fftType: FFTType) extends Module {
  val mod = Module(new BinToSubFFTBankAddr(ffastParams, fftType))
  val io = IO(new BinToSubFFTBankAddrIO(mod.io.fftBin, ffastParams.subFFTns, mod.colMaxs))
  mod.io.fftBin := io.fftBin
  io.bankAddrs := mod.io.bankAddrs
  mod.io.clk := clock
}

class BinToSubFFTBankAddrSpec extends FlatSpec with Matchers {
  behavior of "BinToSubFFTBankAddr"
  it should "map an FFT bin to all sub FFT bin bank, addresses" in {
    val ffastParams = FFASTParams(fftn = 21600, subFFTns = Seq(675, 800, 864))
    dsptools.Driver.execute(() => new BinToSubFFTBankAddrWrapper(ffastParams, DIT), TestParams.options0Tol) { c =>
      new BinToSubFFTBankAddrTester(c)
    } should be (true)
  }
}

class BinToSubFFTBankAddrTester(c: BinToSubFFTBankAddrWrapper) extends DspTester(c) {
  val binToSubFFTIdxMap = PeelingScheduling.getBinToSubFFTIdxMap(c.ffastParams)
  val subFFTIOToBankAddrMap = PeelingScheduling.getSubFFTToIOBankAddrMap(c.ffastParams, c.fftType)
  binToSubFFTIdxMap.zipWithIndex foreach { case (row, idx) =>
    poke(c.io.fftBin, idx)
    row foreach { case (subFFT, subFFTIdx) =>
      val binToBankAddrMapCurrentSubFFT = subFFTIOToBankAddrMap(subFFT)
      val expectedBankAddr = binToBankAddrMapCurrentSubFFT(subFFTIdx)
      expect(c.io.bankAddrs(subFFT).bank, expectedBankAddr.bank)
      expect(c.io.bankAddrs(subFFT).addr, expectedBankAddr.addr)
    }
  }
}