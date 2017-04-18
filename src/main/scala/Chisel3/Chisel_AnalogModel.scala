package dspblocks.fft
import rocketchiselutil._
import chisel3._
import chisel3.util._
import dsptools.numbers._
import dsptools.numbers.implicits._
import rocketchiselutil._
import barstools.tapeout.transforms._
import chisel3.experimental._
import barstools.tapeout.transforms.clkgen._

// TODO: Get rid of copy paste, don't use fixed params

class ADCSCR(ffastParams: FFASTParams) extends SCRBundle {
  val subsamplingFactors = ffastParams.subSamplingFactors.map(_._2).toSeq
  val adcDelays = ffastParams.adcDelays

  val asclkd = CustomIndexedBundle(CustomIndexedBundle(
    Input(UInt(4.W)), 
    adcDelays), subsamplingFactors)

  val extsel_clk = CustomIndexedBundle(CustomIndexedBundle(
    Input(Bool()), 
    adcDelays), subsamplingFactors)

  val vref0 = Input(UInt(8.W))
  val vref1 = Input(UInt(8.W))
  val vref2 = Input(UInt(8.W))

  val clkgcal = CustomIndexedBundle(Input(UInt(8.W)), subsamplingFactors)

  val clkgbias = Input(UInt(8.W))

  override def cloneType = (new ADCSCR(ffastParams)).asInstanceOf[this.type]
}

trait PeripheryADCBundle {
  val ADCINP = Analog(1.W)
  val ADCINM = Analog(1.W)
  val ADCCLKP = Input(Bool())
  val ADCCLKM = Input(Bool())
  val ADCBIAS = Input(Bool())
  val clkrst = Input(Bool())
}

class AnalogModelIO[T <: Data:RealBits](adcDataType: => T, ffastParams: FFASTParams) extends ADCSCR(ffastParams) with PeripheryADCBundle {

  val adcout = CustomIndexedBundle(CustomIndexedBundle(
    Output(adcDataType), 
    adcDelays), subsamplingFactors)

  val clkout = CustomIndexedBundle(CustomIndexedBundle(
    Output(Clock()), 
    adcDelays), subsamplingFactors)

  override def cloneType = (new AnalogModelIO(adcDataType, ffastParams)).asInstanceOf[this.type]
}

object ModuleHierarchy {
  // TODO: Clean up
  // Doesn't count top level
  def getHierarchyLevel(level: Int, mod: Option[ModuleWithParentInfo]): (Int, Option[ModuleWithParentInfo]) = {
    mod match {
      case Some(inst) =>
        val parent = inst.getParent
        parent match {
          case Some(parentMod) => 
            parentMod match {
              case t: barstools.tapeout.transforms.pads.TopModule => (level, mod)
              case _ => getHierarchyLevel(level + 1, Some(parentMod.asInstanceOf[ModuleWithParentInfo]))
            }
          case None => (level - 1, mod)
        }
      case None => throw new Exception("Shouldn't ever get here")
    }
  }
}

class RealToBits extends BlackBox {
  val io = IO(new Bundle {
    val in = Analog(1.W)
    val out = Output(UInt(64.W))
  })
  setInline("RealToBits.v",
    s"""
    |module RealToBits(
    |  input real in,
    |  output [63:0] out);
    |  always @* begin
    |    out = $$realtobits(in);
    |  end
    |endmodule""".stripMargin
  )
}

class BitsToReal extends BlackBox {
  val io = IO(new Bundle {
    val in = Input(UInt(64.W))
    val out = Analog(1.W)
  })
  setInline("BitsToReal.v",
    s"""
    |module BitsToReal(
    |  input [63:0] in,
    |  output real out);
    |  always @* begin
    |    out = $$bitstoreal(in);
    |  end
    |endmodule""".stripMargin
  )
}

object RealToBits {
  def apply(in: Analog): DspReal = {
    val realToBitsMod = Module(new RealToBits)
    attach(realToBitsMod.io.in, in)
    val out = Wire(DspReal())
    out.node := realToBitsMod.io.out
    out
  }
}

object BitsToReal {
  def apply(in: DspReal): Analog = {
    val bitsToRealMod = Module(new BitsToReal)
    bitsToRealMod.io.in := in.node
    bitsToRealMod.io.out
  }
}

class AnalogModel[T <: Data:RealBits](adcDataType: => T, ffastParams: FFASTParams) extends Module with RealAnalogAnnotator {
  val io = IO(new AnalogModelIO(adcDataType, ffastParams))

  // Used to double check that connections are OK
  val excludedConnections = Seq("ADCCLKM", "ADCCLKP", "ADCINM", "ADCINP", "clkout", "adcout", "clkrst")
  val connectionCheckMap = SCRHelper(io, print = false).filterNot { case (el, str) => 
    excludedConnections.map(s => str contains s).reduce(_ | _)
  }.map { case (el, str) => 
    require(el.isWidthKnown)
    val maxVal = (1 << el.getWidth) - 1
    str -> (el, maxVal)
  }.toMap
  val eq = connectionCheckMap.map { case (str, (el, maxVal)) => el.asUInt === maxVal.U }.reduce(_ & _)

  annotateReal()
  val ffastClkDiv = Module(new FFASTClkDiv(ffastParams))
  ffastClkDiv.io.inClk := (io.ADCCLKP & ~io.ADCCLKM).asClock
  ffastClkDiv.io.resetClk := io.clkrst
  // Valid data only in CollectADCSamplesState (becomes active
  // 6 cycles after reset is lowered), should be aligned to when all PH0's are aligned
  // TODO: Switch to this syntax everywhere
  val adcs = ffastParams.getSubFFTDelayKeys.map { case (n, ph) => 
    val thisClk = ffastClkDiv.io.outClks(n)(ph)
    val subsamplingFactor = ffastParams.subSamplingFactors(n)
    // Model the fact that there is some oscillation when reset is high
    io.clkout(subsamplingFactor)(ph) := (thisClk.asUInt.toBool | (io.clkrst & io.ADCCLKP)).asClock
    val adc = Module(new FakeADC(adcDataType))
    adc.io.clk := thisClk
    // TODO: Figure out why I can't plug in contstant directly
    val zro = Wire(DspReal())
    zro := DspReal(0.0)
    adc.io.analogIn := Mux(eq, RealToBits(io.ADCINP) - RealToBits(io.ADCINM), zro)
    io.adcout(subsamplingFactor)(ph) := adc.io.digitalOut
    adc
  }
}

// TODO: Don't use BBInline to print stuff...
class TISARADC_SFFT[T <: Data:RealBits](adcDataType: => T, ffastParams: FFASTParams, name : String) extends BlackBox {
  // val io = IO(new AnalogModelIO(adcDataType, ffastParams))
  val io = IO(new AnalogModelIO(adcDataType, ffastParams))

  // This module = 1, then goes up
  // val (level, topMod) = ModuleHierarchy.getHierarchyLevel(1, Some(this))
  val sdcRegExpr = s"${name}/"

  val pinsSDC = Seq(
    s"set pin_inClk [get_pins -hier ${sdcRegExpr}ADCCLKP]"
  )

  val fastClkSDC = s"create_clock -name IOFASTCLK -period ${FFASTTopParams.fastClkPeriod} $$pin_inClk"

  // Fastest clk freq
  val smallestDivBy = ffastParams.subSamplingFactors(ffastParams.subFFTns.max)
  val halfPeriodPh = smallestDivBy / 2
    
  val outClkConstraints = ffastParams.subSamplingFactors.map { case (subFFT, divBy) =>

    val phases = ffastParams.clkDelays

    // SDC starts at edge 1, not 0
    val referenceEdges = phases.map(p => Seq(2 * p, 2 * (p + 1), 2 * (p + divBy)).map(_ + 1))

    phases.zip(referenceEdges).map { case (clkDelay, edges) =>

      if (ffastParams.adcDelays.contains(clkDelay)) {
        Seq(
          // Don't need set input delay on ADC out b/c inormation provided by Lib
          s"create_generated_clock -name clkout_${divBy}_${clkDelay} -source $$pin_inClk -edges {${edges.mkString(" ")}} [get_pins -hier ${sdcRegExpr}clkout_${divBy}_${clkDelay}]"
        )
      }
      else {  
        Seq.empty
        //ffastParams.adcDelays.filter(_ != 0).map { case dly =>
        //  s"set_multicycle_path -from [get_pins -hier ${sdcRegExpr}adcSubFFTValid_${subFFT}] -to [get_clocks adcClks_${subFFT}_${dly}] -setup 2"
        //}
        // Zeroth ph shouldn't have multi-cycle b/c there should be enough time before the next edge
        // For non-zeroth ph, you miss the first clk so need multi cycle (input delay is longer than when first edge appears)
        // See: http://application-notes.digchip.com/038/38-21077.pdf
        // https://www.xilinx.com/support/documentation/sw_manuals/xilinx2015_1/ug903-vivado-using-constraints.pdf
        // https://www.xilinx.com/support/answers/63222.html
        // However, for example, if PH3 is used and valid goes high on PH23 (23 * fast_clk_period after PH0 rising edge)
        // you will have totally missed a PH3 rising, which causes timing failure -- need multicycle
      }
    }.flatten

  }.flatten

  val adcClkNames = ffastParams.getSubFFTDelayKeys.map { case (n, ph) =>
    val divBy = ffastParams.subSamplingFactors(n)
    s"clkout_${divBy}_${ph}"
  }

  //val adcCollectRegExpr = Seq.fill(level - 2)("*/").mkString("")

  val rstMaxDly = FFASTTopParams.rstMaxDly 
  val rstMinDly = FFASTTopParams.rstMinDly

  val otherConstraints = Seq(
    // Below are top-level ports
    // == resetClk
    "if {[get_db core] == \"FFASTTop\"} {",
    // core_reset in chip top
    "  set_false_path -from [get_ports io_stateMachineReset]",
    // CLK_CPU created in chip top = core_clock
    s"  create_clock -name CLK_CPU -period ${FFASTTopParams.slowClkExtPeriod} [get_ports io_extSlowClk]",

    // Should be auto constrained at chip top level, but here, these are inputs
    s"  set_input_delay -clock CLK_CPU ${FFASTTopParams.inputDelay} [get_ports io_scr*]", 
    s"  set_input_delay -clock CLK_CPU ${FFASTTopParams.inputDelay} [get_ports io_adcScr*]", 
    "}",
    s"set_clock_groups -asynchronous -group CLK_CPU -group { IOFASTCLK ${adcClkNames.mkString(" ")} }",
    s"set_max_delay -from [get_ports *clkrst] $rstMaxDly",
    s"set_min_delay -from [get_ports *clkrst] $rstMinDly",
    s"set_clock_uncertainty 0.1 [all_clocks]",
    s"set_max_transition 0.06 -clock_path [ get_clocks clkout_*_* ]"
  )

  val constraints = pinsSDC ++ Seq(fastClkSDC) ++ outClkConstraints ++ otherConstraints
  // TODO: Don't hardcode?
  setInline(s"FFASTTop.sdc", constraints.toSeq.mkString("\n"))

}

// Wrapper that handles some amount of synchronization

class AnalogModelWrapperIO[T <: Data:RealBits](adcDataType: => T, ffastParams: FFASTParams) extends Bundle with PeripheryADCBundle {
  val adcScr = new ADCSCR(ffastParams)
  // HOLD -- indicates you're in the state where ADC inputs matter
  val collectADCSamplesState = Input(Bool())
  val adcClks = CustomIndexedBundle(CustomIndexedBundle(Output(Clock()), ffastParams.adcDelays), ffastParams.subFFTns)
  val adcDigitalOut = CustomIndexedBundle(CustomIndexedBundle(
    Output(new ValidIO(adcDataType)), ffastParams.adcDelays), ffastParams.subFFTns)
  override def cloneType = (new AnalogModelWrapperIO(adcDataType, ffastParams)).asInstanceOf[this.type]
}
















@chiselName
class AnalogModelWrapper[T <: Data:RealBits](adcDataType: => T, ffastParams: FFASTParams, useBlackBox: Boolean) extends Module with RealAnalogAnnotator {

  val io = IO(new AnalogModelWrapperIO(adcDataType, ffastParams))

  annotateReal()

  val analogModel = 
    if (useBlackBox) {
      // TODO: WARNING: Should only exist once!
      val analogBlockName = "analogBlockAngieModel"
      val m = Module(new TISARADC_SFFT(adcDataType, ffastParams, name = analogBlockName))
      m.suggestName(analogBlockName)
      m
    }
    else
      Module(new AnalogModel(adcDataType, ffastParams))

  analogModel.io.asclkd := io.adcScr.asclkd
  analogModel.io.extsel_clk := io.adcScr.extsel_clk
  analogModel.io.vref0 := io.adcScr.vref0
  analogModel.io.vref1 := io.adcScr.vref1
  analogModel.io.vref2 := io.adcScr.vref2
  analogModel.io.clkgcal := io.adcScr.clkgcal
  analogModel.io.clkgbias := io.adcScr.clkgbias
     
  attach(io.ADCINP, analogModel.io.ADCINP)
  attach(io.ADCINM, analogModel.io.ADCINM)
  analogModel.io.ADCCLKP := io.ADCCLKP
  analogModel.io.ADCCLKM := io.ADCCLKM
  analogModel.io.ADCBIAS := io.ADCBIAS
  analogModel.io.clkrst := io.clkrst

  val validLast = ffastParams.subSamplingFactors.map { case (n, divBy) => 
    // Intermediate valid synchronized to the last phase; reset guaranteed to go low x fast clk cycles before
    // the last phase is actually generated (i.e. there is a pulse)
    val lastPh = analogModel.io.clkout(divBy)(ffastParams.adcDelays.max)
    // See: http://www.sunburst-design.com/papers/CummingsSNUG2003Boston_Resets.pdf
    val resetClkAlignment = AsyncRegInit(clk = lastPh, reset = io.clkrst, init = true.B)
    resetClkAlignment.io.in := false.B
    val countReset = resetClkAlignment.io.out
    // Count should be 0 when first valid data comes out of the sampled ADC (1 cycle delay from sample/hold time)
    // Now synchronous reset
    val count = Wire(UInt(range"[0, $n)"))
    val isMaxCount = count === (n - 1).U 
    val countNext = Mux(isMaxCount, 0.U, count + 1.U)
    val alignmentCount = withClockAndReset(lastPh, countReset) {
      count := RegNext(next = countNext, init = 0.U)
      count
    }

    // Clk domain crossing
    val notCollectADCSamplesState = withClock(lastPh) {
      ShiftRegister(~io.collectADCSamplesState, 3)
    }

    val alignedValid = withClockAndReset(lastPh, notCollectADCSamplesState) {
      RegEnable(next = true.B, init = false.B, enable = isMaxCount)
    }
    n -> alignedValid
  }.toMap

  ffastParams.getSubFFTDelayKeys foreach { case (n, ph) => 
    val subsamplingFactor = ffastParams.subSamplingFactors(n)
    val thisClk = analogModel.io.clkout(subsamplingFactor)(ph)
    io.adcClks(n)(ph) := thisClk

    val dataFromADC = analogModel.io.adcout(subsamplingFactor)(ph)
    val internalValid = validLast(n)
    val validAlignedWithDataFromADC = withClock(thisClk) {
      RegNext(internalValid)
    }

    validAlignedWithDataFromADC.suggestName(s"validAlignedWithDataFromADC_${n}_${ph}")

    // TODO: Unnecessary synchronization?
    io.adcDigitalOut(n)(ph).valid := withClock(thisClk) {
      ShiftRegister(validAlignedWithDataFromADC, 2)
    }
    io.adcDigitalOut(n)(ph).bits := withClock(thisClk) {
      ShiftRegister(dataFromADC, 2)
    }
  }
}
