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

  // TODO: Don't hard code
  val fastClk = 0.1
  val slowClkExt = fastClk * 20
  val inputDelay = 0.02
  val maxDelay = 2 * inputDelay

  // This module = 1, then goes up
  val (level, topMod) = ModuleHierarchy.getHierarchyLevel(1, Some(this))
  val sdcRegExpr = (Seq.fill(level - 1)("*/") ++ Seq(s"${name}*/")).mkString("")

  // TODO: Less hack-ish SDC generation -- DON'T HARD CODE
  val fastClkSDC = s"create_clock -name IOFASTCLK -period ${fastClk} [get_pins ${sdcRegExpr}inClk]"

  val outClkConstraints = ffastParams.subSamplingFactors.map { case (subFFT, divBy) =>

    val phases = ffastParams.clkDelays
    // SDC starts at edge 1, not 0
    val referenceEdges = phases.map(p => Seq(2 * p, 2 * (p + 1), 2 * (p + divBy)).map(_ + 1))
    phases.zip(referenceEdges).map { case (clkDelay, edges) =>
      if (ffastParams.adcDelays.contains(clkDelay))
        Seq(
          s"create_generated_clock -name adcClks_${subFFT}_${clkDelay} -source [get_pins ${sdcRegExpr}inClk] -edges {${edges.mkString(" ")}} [get_pins ${sdcRegExpr}adcClks_${subFFT}_${clkDelay}]",
          s"set_input_delay -clock adcClks_${subFFT}_${clkDelay} ${inputDelay} [get_pins ${sdcRegExpr}adcDigitalOut_${subFFT}_${clkDelay}]"
        )
      else {





/// constrain to ph0 of fft clk -- input delay is some offsetoff

        val p1 = Seq(
          // Virtual clk
          //s"create_generated_clock -name adcClks_${subFFT}_${clkDelay} -source [get_pins ${sdcRegExpr}inClk] -edges {${edges.mkString(" ")}}",
          s"set_input_delay -clock adcClks_${subFFT}_0 ${clkDelay * fastClk + inputDelay} [get_pins ${sdcRegExpr}adcSubFFTValid_${subFFT}]"
        )
        /*
        val p2 = ffastParams.adcDelays.map { case dly =>
          s"set_max_delay -from [get_pins ${sdcRegExpr}adcSubFFTValid_${subFFT}] -to adcClks_${subFFT}_${dly} ${maxDelay}"
        }
        p1 ++ p2
        */
        p1
      }
    }.flatten

  }.flatten







val adcClkNames = ffastParams.getSubFFTDelayKeys.map { case (n, ph) =>
  s"adcClks_${n}_${ph}"
}



  val adcCollectRegExpr = Seq.fill(level - 2)("*/").mkString("")

  val otherConstraints = Seq(
    // s"set_input_delay -clock adcClks_${ffastParams.subFFTns.min}_${0} ${inputDelay} [get_pins ${sdcRegExpr}collectADCSamplesState]",
    // s"set_input_delay -clock adcClks_${ffastParams.subFFTns.min}_${0} ${inputDelay} [get_pins ${sdcRegExpr}resetValid]",
    // Below are top-level ports
    // == resetClk
    s"set_false_path -from [get_ports reset]",
    s"set_false_path -from [get_ports io_stateMachineReset]",
    s"set_false_path -from [get_ports io_extSlowClkSel]",
    // Fast clk src
    // s"set_dont_touch_network clock",
    // s"set_dont_touch analogIn_node",
    s"set_size_only [get_cells ${adcCollectRegExpr}clkMux/clkMux]",
    s"create_clock -name EXTSLOWCLK -period ${slowClkExt} [get_ports io_extSlowClk]",
    s"set_clock_groups -asynchronous -group EXTSLOWCLK -group {IOFASTCLK ${adcClkNames.mkString(" ")} }"
    // Choose fastest clk
    // s"create_clock -name SLOWCLK -period ${slowClkExt} [get_pins ${adcCollectRegExpr}clkMux/clkMux/Z]"
  )



// specify output delay
// make sure it stops writing per FFT




  


// for each of the ffts -- stop checking @ max
// phases different between FFTs -- delays different
 








  

  val constraints = Seq(fastClkSDC) ++ outClkConstraints ++ otherConstraints

  setInline(s"FFASTTopWrapper.sdc", constraints.toSeq.mkString("\n"))

}


class AnalogModel[T <: Data:RealBits](adcDataType: => T, ffastParams: FFASTParams) extends Module {
  val io = IO(new AnalogModelIO(adcDataType, ffastParams))
  val ffastClkDiv = Module(new FFASTClkDiv(ffastParams)) //, syncReset = false))
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
      val analogBlockName = "analogBlockYAYAY"
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
  val validSyncClk = analogModel.io.adcClks(ffastParams.subFFTns.min)(0)
  val collectADCSamplesState = withClock(validSyncClk) {
    ShiftRegister(io.collectADCSamplesState, 10)
  }
  val notCollectADCSamplesStateNoDelay = ~io.collectADCSamplesState
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
    val synchronizedValid = withClock(thisClk) {
      ShiftRegister(analogModel.io.adcSubFFTValid(n), 3)
    }
    // Synchronize valid to local clk
    io.adcDigitalOut(n)(ph).valid := withClockAndReset(thisClk, delayedReset) {
      RegEnable(next = true.B, enable = synchronizedValid, init = false.B)
    }
  }

}