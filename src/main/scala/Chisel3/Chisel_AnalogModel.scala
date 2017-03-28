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
  val globalClk = Output(Clock())
  val adcDigitalOut = CustomIndexedBundle(CustomIndexedBundle(
    Output(new ValidIO(adcDataType)), ffastParams.adcDelays), ffastParams.subFFTns)
  override def cloneType = (new AnalogModelIO(adcDataType, ffastParams)).asInstanceOf[this.type]












  // TODO: Less hack-ish SDC generation
  println(s"create_clock -name clk10GHz -period 0.1 [get_pins inClk]")

  ffastParams.subSamplingFactors.map { case (subFFT, divBy) =>

    val phases = ffastParams.clkDelays
    val referenceEdges = phases.map(p => Seq(2 * p, 2 * (p + 1), 2 * (p + divBy)))
    phases.zip(referenceEdges) foreach { case (adcDelay, edges) =>
      println(s"create_generated_clock -name adcClks_${subFFT}_${adcDelay} -source [get_pins inClk] -edges { ${edges.mkString(" ")} } [get_pins adcClks_${subFFT}_${adcDelay}]")
      println(s"set_input_delay -clock adcClks_${subFFT}_${adcDelay} 0.02 [get_pins adcDigitalOut_${subFFT}_${adcDelay}]")
    }
    

  }







}

class AnalogModelBlackBox[T <: Data:RealBits](adcDataType: => T, ffastParams: FFASTParams) extends BlackBox {
  val io = IO(new AnalogModelIO(adcDataType, ffastParams))
}

class AnalogModel[T <: Data:RealBits](adcDataType: => T, ffastParams: FFASTParams) extends Module {
  val io = IO(new AnalogModelIO(adcDataType, ffastParams))
  val ffastClkDiv = Module(new FFASTClkDiv(ffastParams))
  ffastClkDiv.io.inClk := io.inClk
  ffastClkDiv.io.resetClk := io.resetClk
  // Global clock = fastest clk, phase 0
  io.globalClk := ffastClkDiv.io.outClks(ffastParams.subFFTns.max)(0)
  // Valid data only in CollectADCSamplesState, should be aligned to when all PH0's are aligned
  // Asynchronously reset when done
  val frameAligned1 = AsyncResetReg(true.B, 
    clk = (ffastClkDiv.io.frameAligned & io.collectADCSamplesState).asClock, 
    rst = io.stopCollectingADCSamples) 
  // Takes 1 cycle after frameAligned for sub-ADC output to be valid (@ loc 0), then always streaming
  // Make the data appear in order by waiting until all sub ADCs have collected their first data
  // (but not @ output of register)
  val frameAligned2 = ffastParams.subFFTns.map { n => 
    n -> AsyncResetReg(frameAligned1,
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