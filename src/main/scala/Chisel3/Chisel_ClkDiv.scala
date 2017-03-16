package dspblocks.fft

import barstools.tapeout.transforms.clkgen.SEClkDivider
import barstools.tapeout.transforms.CustomIndexedBundle
import chisel3._
import org.scalatest.{FlatSpec, Matchers}
import dsptools.{DspTester, DspTesterOptionsManager}
import barstools.tapeout.TestParams

class FFASTClkDivWrapper(val ffastParams: FFASTParams) extends Module {

  val mod = Module(new FFASTClkDiv(ffastParams))
  val io = IO(new Bundle {
    // Each subFFT chain requires a different clk division factor
    // All subFFT stages use the same set of delays
    val outClks = CustomIndexedBundle(CustomIndexedBundle(Bool(), mod.delays), ffastParams.subFFTns)
  })
  mod.io.reset := reset
  mod.io.inClk := clock
  for (subFFT <- ffastParams.subFFTns; ph <- mod.delays) {
    io.outClks(subFFT)(ph) := mod.io.outClks(subFFT)(ph).asUInt
  }

}

class FFASTClkDivIO(delays: Seq[Int], subFFTns: Seq[Int]) extends Bundle {
  val reset = Input(Bool())
  val inClk = Input(Clock())
  // Each subFFT chain requires a different clk division factor
  // All subFFT stages use the same set of delays
  val outClks = CustomIndexedBundle(CustomIndexedBundle(Clock(), delays), subFFTns)
  override def cloneType = (new FFASTClkDivIO(delays, subFFTns)).asInstanceOf[this.type]
}

class FFASTClkDiv(val ffastParams: FFASTParams) extends Module {

  val delays = ffastParams.delays.flatten

  val io = IO(new FFASTClkDivIO(delays, ffastParams.subFFTns))

  val clkDivMods = ffastParams.subSamplingFactors.map { case (subFFT, divBy) =>
    val mod = Module(new SEClkDivider(divBy = divBy, phases = delays))
    mod.io.inClk := io.inClk
    mod.io.reset := io.reset
    delays foreach { case d => 
      io.outClks(subFFT)(d) := mod.io.outClks(d)
    }
    mod
  }

}

class FFASTClkDivTester(c: FFASTClkDivWrapper) extends DspTester(c) {
  val clkDivAmounts = c.ffastParams.subSamplingFactors.map(_._2)
  val outPhases = c.ffastParams.delays.flatten
  val maxT = clkDivAmounts.max * clkDivAmounts.max * 4
  val clkOutputs = c.ffastParams.subFFTns.map { case subfft =>
    subfft -> outPhases.map { case d => 
      d -> scala.collection.mutable.ArrayBuffer[Int]()
    }.toMap
  }.toMap

  reset(10)

  for (t <- 0 until maxT) {
    for (subfft <- c.ffastParams.subFFTns; ph <- outPhases) {
      clkOutputs(subfft)(ph) += peek(c.io.outClks(subfft)(ph)).intValue
    }
    step(1)
  }

  val clkCounts = (0 until maxT)
  val clkCountsModDiv = clkDivAmounts.zip(c.ffastParams.subFFTns).map { 
    case (divBy, subFFT) => subFFT -> clkCounts.map(_ % divBy) 
  }.toMap

  for (subFFT <- c.ffastParams.subFFTns; ph <- outPhases) {
    val peekedVal = clkOutputs(subFFT)(ph)
    val expected = clkCountsModDiv(subFFT).map(x => if (x == ph) 1 else 0)
    // Always prints out string, even if passed!
    require(expect(peekedVal == expected, s"SubFFT: $subFFT Phase: $ph clk"), "FAILED!")
    println(s"SubFFT: $subFFT Phase: $ph \t:: ${expected.mkString("")}")
  }

}  

class FFASTClkDivSpec extends FlatSpec with Matchers {

  behavior of "FFASTClkDiv"
  it should "generate the right clks" in {
    val ffastParams = FFASTParams(fftn = 20, subFFTns = Seq(4, 5))
    //FFASTParams(fftn = 21600, subFFTns = Seq(675, 800, 864))
    dsptools.Driver.execute(() => new FFASTClkDivWrapper(ffastParams), TestParams.options0Tol) { c =>
      new FFASTClkDivTester(c)
    } should be (true)
  }

}