/*package barstools.tapeout.transforms.clkgen

import chisel3.experimental.{withClockAndReset, withClock, withReset}
import chisel3._
import chisel3.util.RegInit
import barstools.tapeout.transforms._
import chisel3.util.HasBlackBoxInline

class DifferentialClkDividerIO(phases: Seq[Int]) extends Bundle {
  val reset = Input(Bool())
  val inClk = Input(Clock())
  val inClkB = Input(Clock())
  // NOTE: Phases relative to 2x inClk
  val outClks = Output(CustomIndexedBundle(Clock(), phases))
  override def cloneType = (new DifferentialClkDividerIO(phases)).asInstanceOf[this.type]
}

class DifferentialClkDividerBB(phases: Seq[Int], f: String) extends BlackBox with HasBlackBoxInline {
  val verilog = scala.io.Source.fromFile(f).getLines.mkString("\n")
  // names without io
  val io = IO(new DifferentialClkDividerIO(phases))
  val modName = this.getClass.getSimpleName
  require(verilog contains modName, "Clk divider Verilog module must be named DifferentialClkDividerBB")
  io.elements foreach { case (field, elt) => 
    require(verilog contains field, s"Verilog file should contain io ${field}")
  }
  setInline(s"${modName}.v", verilog)
}

// If syncReset = false, it's implied that reset is strobed before any clk rising edge happens
// i.e. when this is a clkgen fed by another clkgen --> need to adjust the indexing b/c
// you're already shifting on the first clk rising edge
class DifferentialClkDivider(divBy: Int, phases: Seq[Int], analogFile: String = "", syncReset: Boolean = true) 
    extends Module with IsClkModule {

  require(phases.distinct.length == phases.length, "Phases should be distinct!")
  phases foreach { p => require(p < divBy, "Phase must be < divide by amount") }
  val io = IO(new DifferentialClkDividerIO(phases))

  annotateClkPort(io.inClk, Sink())
  annotateClkPort(io.inClkB, Sink())

  // Since it's differential, we care about falling edges too (dual data rate)
  val referenceEdges = phases.map(p => Seq(p, p + 1, p + divBy))
  val generatedClks = io.outClks.elements.zip(referenceEdges).map { case ((field, eltx), edges) =>
    val elt = eltx.asInstanceOf[Element]
    annotateClkPort(elt) 
    GeneratedClk(getIOName(elt), sources = Seq(getIOName(io.inClk)), edges)
  }.toSeq
  annotateDerivedClks(ClkDiv, generatedClks)

  require(divBy > 1, "Differential clk division factor must be > 1")

  divBy match {
    case i: Int if i == 2 =>
      io.outClks.elements foreach { case (phase, out)
        if (phase.toInt == 0) out := io.inClk
        if (phase.toInt == 1) out := io.inClkB
      }
    case i: Int if i > 2 && analogFile == "" =>

// STOPPED HERE

      // Shift register based clock divider (duty cycle is NOT 50%)
      val initVals = Seq(true.B) ++ Seq.fill(divBy - 1)(false.B)
      val regs = initVals.zipWithIndex map { case (init, idx) => 
        val clk = if (idx % 2 == 0) io.inClk else io.inClkB
        AsyncRegInit(clk, io.reset, i) 
      }




      regs.head.io.in := regs.last.io.out
      regs.tail.zip(regs.init) foreach { case (lhs, rhs) => lhs.io.in := rhs.io.out }
      phases foreach { idx => 
        val regIdx = if (syncReset) idx else (idx + 1) % divBy
        io.outClks(idx) := regs(regIdx).io.out.asClock 
      }

    case _ =>
      if (new java.io.File(analogFile).exists) {
        val bb = Module(new SEClkDividerBB(phases, analogFile))
        io <> bb.io
      }
      else throw new Exception("Clock divider Verilog file invalid!")
  }
}*/