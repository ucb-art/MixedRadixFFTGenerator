package dspblocks.fft
import rocketchiselutil._
import chisel3._
import chisel3.util._
import dsptools.numbers._
import dsptools.numbers.implicits._
import rocketchiselutil._
import barstools.tapeout.transforms._
import chisel3.experimental._

// TODO: Get rid of copy paste, don't use fixed params

class ADCSCR extends SCRBundle {
  val subsamplingFactors = FFASTTopParams.ffastParams.subSamplingFactors.map(_._2).toSeq
  val adcDelays = FFASTTopParams.ffastParams.adcDelays

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
}

trait PeripheryADCBundle {
  val ADCINP = Analog(1.W)
  val ADCINM = Analog(1.W)
  val ADCCLKP = Input(Bool())
  val ADCCLKM = Input(Bool())
  val ADCBIAS = Input(Bool())
  val clkrst = Input(Bool())
}

class AnalogModelIO extends ADCSCR with PeripheryADCBundle {

  val adcout = CustomIndexedBundle(CustomIndexedBundle(
    Output(FFASTTopParams.adcDataType), 
    adcDelays), subsamplingFactors)

  val clkout = CustomIndexedBundle(CustomIndexedBundle(
    Output(Clock()), 
    adcDelays), subsamplingFactors)
}

/*
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

  val widePulseSlowClk = Output(Clock())

  val analogIn = Input(DspReal())
  val adcClks = CustomIndexedBundle(CustomIndexedBundle(Output(Clock()), ffastParams.adcDelays), ffastParams.subFFTns)
  val adcDigitalOut = CustomIndexedBundle(CustomIndexedBundle(
    Output(adcDataType), ffastParams.adcDelays), ffastParams.subFFTns)
  // Per sub-FFT (aligned on close to last edge)
  val adcSubFFTValid = CustomIndexedBundle(Output(Bool()), ffastParams.subFFTns)
  override def cloneType = (new AnalogModelIO(adcDataType, ffastParams)).asInstanceOf[this.type]
} 
*/

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
    val in = Input(Analog(1.W))
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
    val out = Output(Analog(1.W))
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
  val io = IO(new AnalogModelIO)
  annotateReal()
  val ffastClkDiv = Module(new FFASTClkDiv(ffastParams))
  ffastClkDiv.io.inClk := io.ADCCLKP & ~io.ADCCLKM
  ffastClkDiv.io.resetClk := io.clkrst
  // Valid data only in CollectADCSamplesState (becomes active
  // 6 cycles after reset is lowered), should be aligned to when all PH0's are aligned
  // TODO: Switch to this syntax everywhere
  val adcs = ffastParams.getSubFFTDelayKeys.map { case (n, ph) => 
    val thisClk = ffastClkDiv.io.outClks(n)(ph)
    io.clkout(n)(ph) := thisClk
    val adc = Module(new FakeADC(adcDataType))
    adc.io.clk := thisClk
    adc.io.analogIn := RealToBits(io.ADCINP) - RealToBits(io.ADCINM)
    io.adcout(n)(ph) := adc.io.digitalOut
    adc
  }
}

// TODO: Don't use BBInline to print stuff...
class AnalogModelBlackBox[T <: Data:RealBits](adcDataType: => T, ffastParams: FFASTParams, name : String) extends BlackBox {
  // val io = IO(new AnalogModelIO(adcDataType, ffastParams))
  val io = IO(new AnalogModelIO)

  // This module = 1, then goes up
  val (level, topMod) = ModuleHierarchy.getHierarchyLevel(1, Some(this))
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

  val otherConstraints = Seq(
    // Below are top-level ports
    // == resetClk
    "if {[get_db core] == \"FFASTTopWrapper\"} {",
    // core_reset in chip top
    "  set_false_path -from [get_ports io_stateMachineReset]",
    // CLK_CPU created in chip top = core_clock
    s"  create_clock -name CLK_CPU -period ${FFASTTopParams.slowClkExtPeriod} [get_ports io_extSlowClk]",
    "}",
    s"  set_clock_groups -asynchronous -group CLK_CPU -group { IOFASTCLK ${adcClkNames.mkString(" ")} }"
  )

  val constraints = pinsSDC ++ Seq(fastClkSDC) ++ outClkConstraints ++ otherConstraints
  // TODO: Don't hardcode?
  setInline(s"FFASTTopWrapper.sdc", constraints.toSeq.mkString("\n"))

}

/*
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
    // Should definitely be done resetting before frameAligned goes high (due to the 4 to 10 difference in delay)
    val reg = AsyncResetReg(
      frameAlignedMaster,
      clk = clkMaxPh,
      rst = syncReset
    )
    port := reg
  }

  // Memories have minimum pulse width requirements
  io.widePulseSlowClk := ffastClkDiv.io.widePulseSlowClk

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
*/






















// Wrapper that handles some amount of synchronization

class AnalogModelWrapperIO[T <: Data:RealBits](adcDataType: => T, ffastParams: FFASTParams) extends Bundle with PeripheryADCBundle {
  val adcScr = new ADCSCR
  // HOLD -- indicates you're in the state where ADC inputs matter
  val collectADCSamplesState = Input(Bool())
  val adcClks = CustomIndexedBundle(CustomIndexedBundle(Output(Clock()), ffastParams.adcDelays), ffastParams.subFFTns)
  val adcDigitalOut = CustomIndexedBundle(CustomIndexedBundle(
    Output(new ValidIO(adcDataType)), ffastParams.adcDelays), ffastParams.subFFTns)
  override def cloneType = (new AnalogModelWrapperIO(adcDataType, ffastParams)).asInstanceOf[this.type]
}

class AnalogModelWrapper[T <: Data:RealBits](adcDataType: => T, ffastParams: FFASTParams, useBlackBox: Boolean) extends Module with RealAnalogAnnotator {

  val io = IO(new AnalogModelWrapperIO(adcDataType, ffastParams))

  annotateReal()



/*
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

  io.widePulseSlowClk := analogModel.io.widePulseSlowClk

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
      // Delay 3x the data that was valid at the right time from ADC
      ShiftRegister(analogModel.io.adcDigitalOut(n)(ph), 3)
    }
    val delayedReset = withClock(thisClk) {
      ShiftRegister(notCollectADCSamplesStateNoDelay, 3)
    }

    // TODO: HOW TO GUARANTEE TIMING ON THIS GUY?
    // First reg isn't adding pipeline delay -- it's to align with ADC data
    // since valid goes high before ADC data is output
    val synchronizedValid = withClock(thisClk) {
      // First guy is to align up with ADC output (since valid goes high on some weird phase)
      // The next 2 registers are synchronization
      ShiftRegister(analogModel.io.adcSubFFTValid(n), 3)
    }
    // Synchronize valid to local clk
    // Also matches delay of ADC
    io.adcDigitalOut(n)(ph).valid := withClockAndReset(thisClk, delayedReset) {
      RegEnable(next = true.B, enable = synchronizedValid, init = false.B)
    }
  }
*/

}
