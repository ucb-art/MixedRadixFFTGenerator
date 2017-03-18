package dspblocks.fft

import barstools.tapeout.transforms.clkgen._
import barstools.tapeout.transforms.CustomIndexedBundle
import chisel3._
import org.scalatest.{FlatSpec, Matchers}
import dsptools.{DspTester, DspTesterOptionsManager}
import barstools.tapeout.TestParams
import barstools.tapeout.transforms.pads._

// WARNING: WILL FAILL IF TOP MODULE USES PADS!

class FFASTClkDivWrapper(val ffastParams: FFASTParams) extends TopModule(usePads = false) {
  // Need to annotate top-level clk when using clk div
  annotateClkPort(clock, 
    id = "clock", // not in io bundle
    sink = Sink(Some(ClkSrc(period = 5.0)))
  )

  val mod = Module(new FFASTClkDiv(ffastParams))
  val io = IO(new Bundle {
    // Each subFFT chain requires a different clk division factor
    // All subFFT stages use the same set of delays
    val outClks = CustomIndexedBundle(CustomIndexedBundle(Bool(), mod.delays), ffastParams.subFFTns)
    val frameAligned = Output(Bool())
  })
  mod.io.resetClk := reset
  mod.io.inClk := clock
  for (subFFT <- ffastParams.subFFTns; ph <- mod.delays) {
    io.outClks(subFFT)(ph) := mod.io.outClks(subFFT)(ph).asUInt
  }
  io.frameAligned := mod.io.frameAligned
}

class FFASTClkDivIO(delays: Seq[Int], subFFTns: Seq[Int]) extends Bundle {
  val resetClk = Input(Bool())
  val inClk = Input(Clock())
  // Each subFFT chain requires a different clk division factor
  // All subFFT stages use the same set of delays
  val outClks = CustomIndexedBundle(CustomIndexedBundle(Clock(), delays), subFFTns)
  // Useful for debug
  val frameAligned = Output(Bool())
  override def cloneType = (new FFASTClkDivIO(delays, subFFTns)).asInstanceOf[this.type]
}

class FFASTClkDiv(val ffastParams: FFASTParams) extends Module {

  val delays = ffastParams.delays.flatten

  val io = IO(new FFASTClkDivIO(delays, ffastParams.subFFTns))

  val clkDivMods = ffastParams.subSamplingFactors.map { case (subFFT, divBy) =>
    val mod = Module(new SEClkDivider(divBy = divBy, phases = delays))
    mod.io.inClk := io.inClk
    mod.io.reset := io.resetClk
    delays foreach { case d => 
      io.outClks(subFFT)(d) := mod.io.outClks(d)
    }
    mod
  }

  // When ph0 clk of each subsampling stage is high (assumes 1/divBy duty cycle)
  io.frameAligned := clkDivMods.foldLeft(1.U)((accum, clkDivMod) => accum & clkDivMod.io.outClks(0).asUInt)

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
    var aligned = true
    for (subfft <- c.ffastParams.subFFTns; ph <- outPhases) {
      val peekedVal = peek(c.io.outClks(subfft)(ph)).intValue
      clkOutputs(subfft)(ph) += peekedVal
      // Aligned when phase 0 of each sub FFT stage is high
      if (ph == 0 && peekedVal == 0) aligned = false
    }
    expect(c.io.frameAligned, aligned)
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