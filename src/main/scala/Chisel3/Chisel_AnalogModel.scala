package dspblocks.fft
import rocketchiselutil._
import chisel3._
import chisel3.util._
import dsptools.numbers._
import dsptools.numbers.implicits._
import rocketchiselutil._
import barstools.tapeout.transforms._
import chisel3.experimental._

class AnalogModelIO[T <: Data:RealBits](adcDataType: => T, ffastParams: FFASTParams) extends Bundle {
  // Chip startup
  val resetClk = Input(Bool())
  // Fast clk
  val inClk = Input(Clock())

  // WARNING: NEEDS INTERNAL REGISTERING
  // HOLD -- indicates you're in the state where ADC inputs matter
  val collectADCSamplesState = Input(Bool())
  // NOTE: This goes low 6 slowest clock cycles before collectADCSamplesState goes high
  // -- should internally register to synchronize to the right clk
  val resetValid = Input(Bool())
  // Full rate ADC in

  val analogIn = Input(DspReal())
  val adcClks = CustomIndexedBundle(CustomIndexedBundle(Output(Clock()), ffastParams.adcDelays), ffastParams.subFFTns)
  val adcDigitalOut = CustomIndexedBundle(CustomIndexedBundle(
    Output(adcDataType), ffastParams.adcDelays), ffastParams.subFFTns)
  val adcSubFFTValid = CustomIndexedBundle(Output(Bool()), ffastParams.subFFTns)
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

  // This module = 1, then goes up
  val (level, topMod) = ModuleHierarchy.getHierarchyLevel(1, Some(this))
  val sdcRegExpr = (Seq.fill(level - 1)("*/") ++ Seq(s"${name}/")).mkString("")

  val fastClkSDC = s"create_clock -name IOFASTCLK -period ${FFASTTopParams.fastClkPeriod} [get_pins ${sdcRegExpr}inClk]"

  val outClkConstraints = ffastParams.subSamplingFactors.map { case (subFFT, divBy) =>

    val phases = ffastParams.clkDelays

    // SDC starts at edge 1, not 0
    val referenceEdges = phases.map(p => Seq(2 * p, 2 * (p + 1), 2 * (p + divBy)).map(_ + 1))

    phases.zip(referenceEdges).map { case (clkDelay, edges) =>

      if (ffastParams.adcDelays.contains(clkDelay)) {
        Seq(
          s"create_generated_clock -name adcClks_${subFFT}_${clkDelay} -source [get_pins ${sdcRegExpr}inClk] -edges {${edges.mkString(" ")}} [get_pins ${sdcRegExpr}adcClks_${subFFT}_${clkDelay}]",
          s"set_input_delay -clock adcClks_${subFFT}_${clkDelay} ${FFASTTopParams.inputDelay} [get_pins ${sdcRegExpr}adcDigitalOut_${subFFT}_${clkDelay}]"
        )
      }
      else {
        val valid0InputDelay = clkDelay * FFASTTopParams.fastClkPeriod + FFASTTopParams.inputDelay
        Seq( 
          s"set_input_delay -clock adcClks_${subFFT}_0 ${valid0InputDelay} [get_pins ${sdcRegExpr}adcSubFFTValid_${subFFT}]"
        ) ++ ffastParams.adcDelays.filter(_ != 0).map { case dly =>
          s"set_multicycle_path -from [get_pins ${sdcRegExpr}adcSubFFTValid_${subFFT}] -to [get_clocks adcClks_${subFFT}_${dly}] -setup 2"
        }
        // Zeroth ph shouldn't have multi-cycle 
        // See: http://application-notes.digchip.com/038/38-21077.pdf
        // https://www.xilinx.com/support/documentation/sw_manuals/xilinx2015_1/ug903-vivado-using-constraints.pdf
        // https://www.xilinx.com/support/answers/63222.html
        // However, for example, if PH3 is used and valid goes high on PH23 (23 * fast_clk_period after PH0 rising edge)
        // you will have totally missed a PH3 rising, which causes timing failure -- need multicycle
      }
    }.flatten

  }.flatten

  val adcClkNames = ffastParams.getSubFFTDelayKeys.map { case (n, ph) =>
    s"adcClks_${n}_${ph}"
  }

  val adcCollectRegExpr = Seq.fill(level - 2)("*/").mkString("")

  val otherConstraints = Seq(
    // Below are top-level ports
    // == resetClk
    s"set_false_path -from [get_ports reset]",
    s"set_false_path -from [get_ports io_stateMachineReset]",
    s"set_false_path -from [get_ports io_extSlowClkSel]",
    s"set_size_only [get_cells ${adcCollectRegExpr}clkMux/clkMux]",
    s"create_clock -name EXTSLOWCLK -period ${FFASTTopParams.slowClkExtPeriod} [get_ports io_extSlowClk]",
    s"set_clock_groups -asynchronous -group EXTSLOWCLK -group {IOFASTCLK ${adcClkNames.mkString(" ")} }"
  )

  val constraints = Seq(fastClkSDC) ++ outClkConstraints ++ otherConstraints
  // TODO: Don't hardcode?
  setInline(s"FFASTTopWrapper.sdc", constraints.toSeq.mkString("\n"))

}

class AnalogModel[T <: Data:RealBits](adcDataType: => T, ffastParams: FFASTParams) extends Module {
  val io = IO(new AnalogModelIO(adcDataType, ffastParams))
  val ffastClkDiv = Module(new FFASTClkDiv(ffastParams))
  ffastClkDiv.io.inClk := io.inClk
  ffastClkDiv.io.resetClk := io.resetClk
  // Valid data only in CollectADCSamplesState (becomes active
  // 6 cycles after reset is lowered), should be aligned to when all PH0's are aligned
  val frameAlignedMasterTemp = AsyncResetReg(
    true.B, 
    clk = (ffastClkDiv.io.frameAligned & io.collectADCSamplesState).asClock, 
    rst = io.resetValid 
  )

  // For debugging purposes
  val frameAlignedMaster = Wire(Bool())
  frameAlignedMaster := frameAlignedMasterTemp

  io.adcSubFFTValid.elements foreach { case (nS, port) => 
    val n = nS.toInt
    val clkMaxPh = ffastClkDiv.io.outClks(n)(ffastParams.clkDelays.max)
    // TODO: 3 is probably overkill...
    val syncReset = withClock(clkMaxPh) {
      ShiftRegister(io.resetValid, 1)
    }
    val reg = AsyncResetReg(
      frameAlignedMaster,
      clk = clkMaxPh,
      rst = syncReset
    )
    port := reg
  }

  // TODO: Switch to this syntax everywhere
  val adcs = ffastParams.getSubFFTDelayKeys.map { case (n, ph) => 
    val thisClk = ffastClkDiv.io.outClks(n)(ph)
    io.adcClks(n)(ph) := thisClk
    val adc = Module(new FakeADC(adcDataType))
    adc.io.clk := thisClk
    adc.io.analogIn := io.analogIn
    io.adcDigitalOut(n)(ph) := adc.io.digitalOut
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
      val analogBlockName = "analogBlockAngieModel"
      val m = Module(new AnalogModelBlackBox(adcDataType, ffastParams, name = analogBlockName))
      m.suggestName(analogBlockName)
      m
    }
    else
      Module(new AnalogModel(adcDataType, ffastParams))

  analogModel.io.resetClk := io.resetClk
  analogModel.io.inClk := io.inClk 

  // TODO: Being too conservative here w/ lots of extra regs...
  // This makes sure input doesn't start until the below output valids have been reset
  // Slowest clk, ph0
  val validSyncClk = analogModel.io.adcClks(ffastParams.subFFTns.min)(0)
  val collectADCSamplesState = withClock(validSyncClk) {
    ShiftRegister(io.collectADCSamplesState, 10)
  }

  val notCollectADCSamplesStateNoDelay = ~io.collectADCSamplesState
  // TODO: Redundant overkill...
  val notCollectADCSamplesState = withClock(validSyncClk) {
    ShiftRegister(notCollectADCSamplesStateNoDelay, 3)
  }

  analogModel.io.resetValid := notCollectADCSamplesState
  analogModel.io.collectADCSamplesState := collectADCSamplesState
  analogModel.io.analogIn := io.analogIn
  io.adcClks := analogModel.io.adcClks

  ffastParams.getSubFFTDelayKeys foreach { case (n, ph) => 
    val thisClk = analogModel.io.adcClks(n)(ph)
    // Goes high after the last phase following synchronization
    val originalValid = analogModel.io.adcSubFFTValid(n)
    // Synchronized to correct clk
    io.adcDigitalOut(n)(ph).bits := withClock(thisClk) {
      ShiftRegister(analogModel.io.adcDigitalOut(n)(ph), 3)
    }
    val delayedReset = withClock(thisClk) {
      ShiftRegister(notCollectADCSamplesStateNoDelay, 3)
    }

    // TODO: HOW TO GUARANTEE TIMING ON THIS GUY?
    // First reg isn't adding pipeline delay -- it's to align with ADC data
    // since valid goes high before ADC data is output
    val synchronizedValid = withClock(thisClk) {
      ShiftRegister(analogModel.io.adcSubFFTValid(n), 3)
    }
    // Synchronize valid to local clk
    io.adcDigitalOut(n)(ph).valid := withClockAndReset(thisClk, delayedReset) {
      RegEnable(next = true.B, enable = synchronizedValid, init = false.B)
    }
  }

}