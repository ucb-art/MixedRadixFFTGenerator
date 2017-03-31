package dspblocks.fft
import rocketchiselutil._
import chisel3._
import chisel3.util._
import dsptools.numbers._
import dsptools.numbers.implicits._
import rocketchiselutil._
import barstools.tapeout.transforms._

class AnalogModelIO[T <: Data:RealBits](adcDataType: => T, ffastParams: FFASTParams) extends Bundle {
  // Chip startup
  val resetClk = Input(Bool())
  // Fast clk
  val inClk = Input(Clock())
  // HOLD -- indicates you're in the state where ADC inputs matter
  val collectADCSamplesState = Input(Bool())
  // PULSE -- for async reset of ADC out valid
  val stopCollectingADCSamples = Input(Bool())
  // Full rate ADC in
  val analogIn = Input(DspReal())
  val adcClks = CustomIndexedBundle(CustomIndexedBundle(Output(Clock()), ffastParams.adcDelays), ffastParams.subFFTns)
  val adcDigitalOut = CustomIndexedBundle(CustomIndexedBundle(
    Output(adcDataType), ffastParams.adcDelays), ffastParams.subFFTns)
  val adcSubFFTValid = CustomIndexedBundle(Output(Bool), ffastParams.subFFTns)
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

// TODO: Don't use BBInline to print stuff...
class AnalogModelBlackBox[T <: Data:RealBits](adcDataType: => T, ffastParams: FFASTParams, name : String) extends BlackBox {
  val io = IO(new AnalogModelIO(adcDataType, ffastParams))

  // TODO: Don't hard code
  val fastClk = 0.1
  val inputDelay = 0.02

  // This module = 1, then goes up
  val (level, topMod) = ModuleHierarchy.getHierarchyLevel(1, Some(this))
  val sdcRegExpr = (Seq.fill(level - 1)("*/") ++ Seq(s"${name}*/")).mkString("")

  // TODO: Less hack-ish SDC generation -- DON'T HARD CODE
  val topClkSDC = s"create_clock -name IOFASTCLK -period ${fastClk} [get_pins ${sdcRegExpr}inClk]"
  val resetSDC = s"set_input_delay -clock IOFASTCLK ${inputDelay} [get_pins ${sdcRegExpr}resetClk]"
  val adcSDC = s"set_input_delay -clock IOFASTCLK ${inputDelay} [get_pins ${sdcRegExpr}analogIn_node*]"











/*
  clkMux.io.sel := io.extSlowClkSel
  clkMux.io.clk0 := globalClkInternal
  clkMux.io.clk1 := io.extSlowClk 
  val globalClk = clkMux.io.clkOut
  set_size_only on instance ClkMuxBlackBox / clkMux

  set_false_path       -from  [get_port *reset]
  stateMachineReset
  resetClk
  extSlowClkSel

  extSlowClk async with everything

  scr(other sigs)


clk: async reset

virtual clk


**add another delay to the counter for lut

set_false_path

set_output_delay??

set_max_delay
**change enq valid
**currentstate needs to be internally reg in analog (collectadcsamplesstate)
// bits is synced but not valid -- for adcDigitalOut
**analogBlock.io.stopCollectingADCSamples := done



valid: virtual clk
set_input_delay of some clk to q
max delay to clk is X
via set_max_delay

*/















  val outConstraints = ffastParams.subSamplingFactors.map { case (subFFT, divBy) =>

    val phases = ffastParams.adcDelays
    // SDC starts at edge 1, not 0
    val referenceEdges = phases.map(p => Seq(2 * p, 2 * (p + 1), 2 * (p + divBy)).map(_ + 1))
    phases.zip(referenceEdges).map { case (adcDelay, edges) =>
      Seq(s"create_generated_clock -name adcClks_${subFFT}_${adcDelay} -source [get_pins ${sdcRegExpr}inClk] -edges {${edges.mkString(" ")}} [get_pins ${sdcRegExpr}adcClks_${subFFT}_${adcDelay}]") ++
      Seq(s"set_input_delay -clock adcClks_${subFFT}_${adcDelay} ${inputDelay} [get_pins ${sdcRegExpr}adcDigitalOut_${subFFT}_${adcDelay}*]")
    }.flatten

  }.flatten

  val constraints = Seq(topClkSDC, resetSDC, adcSDC) ++ outConstraints

  setInline(s"analog.sdc", constraints.toSeq.mkString("\n"))

}


class AnalogModel[T <: Data:RealBits](adcDataType: => T, ffastParams: FFASTParams) extends Module {
  val io = IO(new AnalogModelIO(adcDataType, ffastParams))
  val ffastClkDiv = Module(new FFASTClkDiv(ffastParams, syncReset = false))
  ffastClkDiv.io.inClk := io.inClk
  ffastClkDiv.io.resetClk := io.resetClk
  // Valid data only in CollectADCSamplesState, should be aligned to when all PH0's are aligned
  // Asynchronously reset when done
  val frameAligned1 = AsyncResetReg(true.B, 
    clk = (ffastClkDiv.io.frameAligned & io.collectADCSamplesState).asClock, 
    rst = io.stopCollectingADCSamples) 
  // Takes 1 cycle after frameAligned for sub-ADC output to be valid (@ loc 0), then always streaming
  // Make the data appear in order by waiting until all sub ADCs have collected their first data
  // (but not @ output of register)
  val frameAligned2 = ffastParams.subFFTns.map { n => 
    n -> AsyncResetReg(
      frameAligned1,
      clk = ffastClkDiv.io.outClks(n)(ffastParams.clkDelays.max),
      // Reset should happen with lots of margin (async)
      rst = io.stopCollectingADCSamples)
  }.toMap

  // TODO: Switch to this syntax everywhere
  val adcs = ffastParams.getSubFFTDelayKeys.map { case (n, ph) => 
    io.adcClks(n)(ph) := ffastClkDiv.io.outClks(n)(ph)
    val adc = Module(new FakeADC(adcDataType))
    adc.io.clk := ffastClkDiv.io.outClks(n)(ph)
    adc.io.analogIn := io.analogIn
    io.adcDigitalOut(n)(ph).bits := adc.io.digitalOut
    // Synchronize valid to local clk
    io.adcDigitalOut(n)(ph).valid := AsyncResetReg(
      frameAligned2(n),
      clk = ffastClkDiv.io.outClks(n)(ph),
      rst = io.stopCollectingADCSamples)
    adc
  }
}

// Wrapper that handles some amount of synchronization

class AnalogModelWrapperIO[T <: Data:RealBits](adcDataType: => T, ffastParams: FFASTParams) extends Bundle {
  // Chip startup
  val resetClk = Input(Bool())
  // Fast clk
  val inClk = Input(Clock())
  // HOLD -- indicates you're in the state where ADC inputs matter
  val collectADCSamplesState = Input(Bool())
  // PULSE -- for async reset of ADC out valid
  val stopCollectingADCSamples = Input(Bool())
  // Full rate ADC in
  val analogIn = Input(DspReal())
  val adcClks = CustomIndexedBundle(CustomIndexedBundle(Output(Clock()), ffastParams.adcDelays), ffastParams.subFFTns)
  val adcDigitalOut = CustomIndexedBundle(CustomIndexedBundle(
    Output(new ValidIO(adcDataType)), ffastParams.adcDelays), ffastParams.subFFTns)
  override def cloneType = (new AnalogModelWrapperIO(adcDataType, ffastParams)).asInstanceOf[this.type]
}

class AnalogModelWrapper[T <: Data:RealBits](adcDataType: => T, ffastParams: FFASTParams, useBlackBox: Boolean) extends Module {

  val io = IO(new AnalogModelWrapperIO(adcDataType, ffastParams))

  val analogModel = 
    if (useBlackBox) {
      // TODO: WARNING: Should only exist once!
      val analogBlockName = "analogBlock"
      val m = Module(new AnalogModelBlackBox(adcDataType, ffastParams, name = analogBlockName))
      m.suggestName(analogBlockName)
      m
    }
    else
      Module(new AnalogModel(adcDataType, ffastParams))

  analogModel.io.resetClk := io.resetClk
  analogModel.io.inClk := io.inClk 

  // NOTE: These two signals should be internally registered @ the 10GHz rate
  analogModel.io.collectADCSamplesState := io.collectADCSamplesState
  analogModel.io.stopCollectingADCSamples := io.stopCollectingADCSamples
  





