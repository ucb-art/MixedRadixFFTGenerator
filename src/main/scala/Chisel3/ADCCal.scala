package dspblocks.fft
import chisel3._
import chisel3.experimental._
import barstools.tapeout.transforms._
import dsptools.numbers._
import dsptools.numbers.implicits._

class ADCCalSCR[T <: Data:RealBits](adcDataType: => T, ffastParams: FFASTParams) extends SCRBundle {
  require(adcDataType.asInstanceOf[Data].isWidthKnown)
  val numBits = adcDataType.getWidth
  val fftGroups = ffastParams.getSubFFTDelayKeys

  val calCoeff = CustomIndexedBundle(CustomIndexedBundle(
    Input(UInt(numBits.W)), ffastParams.adcDelays
  ), ffastParams.subFFTns)
  val loadAddr = Input(UInt(numBits.W))
  val calOut = CustomIndexedBundle(CustomIndexedBundle(
    Output(UInt(numBits.W)), ffastParams.adcDelays
  ), ffastParams.subFFTns)
  val calAllRE = Input(Bool())
  val calWE = Input(UInt(fftGroups.length.W))

  def getWE(n: Int, ph: Int): Bool = {
    val (groupTag, groupIdx) = fftGroups.zipWithIndex.filter { 
      case ((nG, phG), idx) => n == nG && ph == phG 
    }.head
    calWE(groupIdx)
  }

  override def cloneType = (new ADCCalSCR(adcDataType, ffastParams)).asInstanceOf[this.type]
}

// Expect to read raw data through main memory; not here
class SubADCCalIO(numBits: Int) extends Bundle {

  val calCoeff = Input(UInt(numBits.W))
  // Load calibration info address
  val loadAddr = Input(UInt(numBits.W))
  // WARNING: Internal logic doesn't let you read and write at the same time
  val we = Input(Bool())
  val re = Input(Bool())

  //////////// ABOVE IS SCR

  // When ADC is collecting data (doesn't let you modify calibration #'s)
  val isAdcCollect = Input(Bool())
  
  val adcIn = Input(UInt(numBits.W))
  val calOut = Output(UInt(numBits.W))

  // Should only read (i.e. LUT map) in ADC state

  val clk = Input(Clock())
  override def cloneType = (new SubADCCalIO(numBits)).asInstanceOf[this.type]
}

class SubADCCal(numBits: Int, calMemNameSuffix: String) extends Module with DelayTracking {
  // 1 for read address registering; another for registering dout
  val moduleDelay = 2

  val io = IO(new SubADCCalIO(numBits))

  withClock(io.clk) {
    // Uses 1 RW port memory
    // When in load mode, the address is specified from the SCR
    // When in normal operation, the ADC input serves as the address used for LUT mapping
    val addr = Mux(io.isAdcCollect, io.adcIn, io.loadAddr)
    
    val din = io.calCoeff

    val memDepth = 1 << numBits

    val mem = SyncReadMem(memDepth, UInt(numBits.W))
    mem.suggestName(s"adccal_$calMemNameSuffix")

    // Make conditions mutually exclusive to infer minimum size memory
    val re = io.isAdcCollect | io.re
    val we = (~io.isAdcCollect) & io.we & (~re)

    when(we) {
      mem.write(addr, din)
    }

    val dout = RegNext(mem.read(addr, re))
    io.calOut := dout
  }
}

class ADCCalIO[T <: Data:RealBits](adcDataType: => T, ffastParams: FFASTParams) extends Bundle {
  val adcCalScr = new ADCCalSCR(adcDataType, ffastParams)
  val isAdcCollect = Input(Bool())
  val adcIn = CustomIndexedBundle(CustomIndexedBundle(
    Input(adcDataType), ffastParams.adcDelays), ffastParams.subFFTns)
  val calOut = CustomIndexedBundle(CustomIndexedBundle(
    Output(adcDataType), ffastParams.adcDelays), ffastParams.subFFTns)
  val clk = Input(Clock())

  override def cloneType = (new ADCCalIO(adcDataType, ffastParams)).asInstanceOf[this.type]
}

class ADCCal[T <: Data:RealBits](adcDataType: => T, ffastParams: FFASTParams) extends Module with DelayTracking {
  val io = IO(new ADCCalIO(adcDataType, ffastParams))
  val (calMods, modDelays) = ffastParams.getSubFFTDelayKeys.map { case (n, ph) =>
    val mod = Module(new SubADCCal(io.adcCalScr.numBits, s"${n}_${ph}"))
    mod.suggestName(s"calMod_${n}_${ph}")
    mod.io.clk := io.clk 
    mod.io.adcIn := io.adcIn(n)(ph).asUInt
    io.calOut(n)(ph) := adcDataType.fromBits(mod.io.calOut)
    io.adcCalScr.calOut(n)(ph) := mod.io.calOut
    mod.io.isAdcCollect := io.isAdcCollect
    mod.io.calCoeff := io.adcCalScr.calCoeff(n)(ph)
    mod.io.loadAddr := io.adcCalScr.loadAddr
    mod.io.re := io.adcCalScr.calAllRE 
    mod.io.we := io.adcCalScr.getWE(n, ph)
    (mod, mod.moduleDelay)
  }.unzip
  val tempDly = modDelays.distinct
  require(tempDly.length == 1)
  val moduleDelay = tempDly.head
}