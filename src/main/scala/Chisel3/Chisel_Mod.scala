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

trait DelayTracking {
  def moduleDelay: Int
}

// Barrett reduction (See Wiki)
// // x % n where n is constant and x can be really big
class BarrettReduction(xType: UInt, n: Int, xmax: Option[Int] = None) extends Module with hasContext with DelayTracking {

  val moduleDelay = 2 * context.numMulPipes
  
  // TODO: Change to range?
  val bitWidthMax = (1 << xType.getWidth) - 1
  val xmaxFinal = xmax match {
    case Some(max) => 
      require(max <= bitWidthMax, "Supplied max should be within UInt range")
      max
    case None => bitWidthMax
  }
  require(xmaxFinal < n * n, "Barrett Reduction only guaranteed if x < n^2")

  // Approximate 1/n with m/2^k where m = floor(2^k/n)
  def getRequiredK(xmax: Int): Int = {
    def getRequiredK(xmax: Int, kStart: Int): Int = {
      // Error = 1/n - m/2^k
      val twok = 1 << kStart
      val error = 1.toDouble / n - math.floor(twok.toDouble / n) / twok
      // Max value of x must be smaller than 1/e
      if (xmax < 1.toDouble / error) kStart
      // Otherwise, need larger 2^k
      else getRequiredK(xmax, kStart + 1)
    }
    getRequiredK(xmax, 0)
  }

  val k = getRequiredK(xmaxFinal)
  // Probably didn't need math.floor since I didn't cast to Double
  val m = math.floor((1 << k) / n).toInt

  // Actual hardware starting here

  val io = IO(new Bundle {
    val x = Input(xType.cloneType)
    val out = Output(UInt(range"[0, $n)"))
  })

  // In case more info is known, can only use relevant bits
  val x = Wire(UInt(range"[0, $xmaxFinal]"))
  x := io.x

  // q = FLOOR((x * m) / 2^k) -- note FLOOR b/c of >> on UInt so q is an integer
  // r = x - q * n
  // if n <= r, mod is r - n
  // else mod is r
  val q = (x context_* m.U) >> k
  // r Should never overflow since r will never be negative and is always less than x
  // TODO: Check if this optimization buys you anything (i.e. tools smart enough to figure out)
  val twon = 2 * n
  val chiselN = n.U
  val r = Wire(UInt(range"[0, $twon)")) 
  r := ShiftRegister(x, moduleDelay) -% (q context_* chiselN)
  // TODO: Maybe only use one reg after second mul?
  // r only guaranteed to be < 2n, since m/2^k <= 1/n
  io.out := Mux(r < chiselN, r, r -% chiselN)

}

object ConstantMod {
  def apply(x: UInt, n: Int, xmax: Int): (UInt, Module with DelayTracking) = apply(x, n, Some(xmax))
  def apply(x: UInt, n: Int, xmax: Option[Int] = None): (UInt, Module with DelayTracking) = {
    val mod = Module(new BarrettReduction(x, n, xmax))
    mod.io.x := x
    (mod.io.out, mod)
  }
}