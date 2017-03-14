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

// TODO: Add mul delay if needed? + add non constant mod

// x % n where n is constant and x can be really big
class ConstantMod(xType: UInt, n: Int, fractionalBits: Option[Int] = None) extends Module with hasContext {
  val binaryPt = fractionalBits.getOrElse(BigInt(n).bitLength) + 5
  val nInverse = (1 / n).F(binaryPt.BP)
  val io = IO(new Bundle {
    val x = Input(xType.cloneType)
    val out = Output(UInt(range"[0, $n)"))
  })
  // mod = x - n * floor (x * (1/n))
  // floorContents --> uint
  val floorContents = (io.x.asFixed context_* nInverse).floor.intPart.asUInt
  // Subtraction will never overflow since x is always the biggest
  io.out := ShiftRegister(io.x, 2 * context.numMulPipes) - (n.U context_* floorContents(floorContents.getWidth - 2, 0))

  io.out := io.x -& (n.U * (io.x.asFixed * nInverse).floor.intPart.asUInt)
}
object ConstantMod {
  def apply(x: UInt, n: Int): UInt = {
    val mod = Module(new ConstantMod(x, n))
    mod.io.x := x
    mod.io.out
  }
}

class BinToSubFFTIdx(val ffastParams: FFASTParams) extends Module {
  val io = IO(new Bundle {
    val fftBinMax = ffastParams.fftn - 1
    val fftBin = Input(UInt(range"[0, $fftBinMax]"))
    val subFFTIdx = Output(new CustomIndexedBundle(
      ffastParams.subFFTns.map(subFFT => subFFT -> UInt(range"[0, $subFFT)")): _*
    ))
  })

  val modOuts = ffastParams.subFFTns.map(n => ConstantMod(io.fftBin, n))
  io.subFFTIdx.elements.zip(modOuts) foreach { case ((key, o), res) =>
    o := res
  }
}

class BinToSubFFTIdxSpec extends FlatSpec with Matchers {
  behavior of "BinToSubFFTIdx"
  it should "map an FFT bin to all sub FFT bins via mod" in {
    val ffastParams = FFASTParams(fftn = 21600, subFFTns = Seq(675, 800, 864))
    dsptools.Driver.execute(() => new BinToSubFFTIdx(ffastParams), TestParams.options0Tol) { c =>
      new BinToSubFFTIdxTester(c)
    } should be (true)
  }
}

class BinToSubFFTIdxTester(c: BinToSubFFTIdx) extends DspTester(c) {
  val map = PeelingScheduling.getBinToSubFFTIdxMap(c.ffastParams)
  map.zipWithIndex foreach { case (row, idx) =>
    poke(c.io.fftBin, idx)
    row foreach { case (subFFT, expectedIdx) =>
      peek(c.io.subFFTIdx(subFFT))
      println(s"theory: $expectedIdx")
    }
  }
}








// test
// then need bin to bank, addr