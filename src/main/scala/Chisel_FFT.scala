// Top level module
// TODO: Use := when not many io ports are used in bundle

package FFT
import ChiselDSP._
import Chisel.{Pipe =>_,Complex => _,Mux => _, RegInit => _, RegNext => _, Counter => _, _}

class FFT[T <: DSPQnm[T]](gen : => T, p: GeneratorParams, debugMode: Boolean = false) extends GenDSPModule (gen) {

  CheckDelay.off()

  // Setup FFT with user-defined parameters
  Params(p)

  // Data IO, setup IO, operating controls (top level)
  override val io = new FFTIO(gen)
  val setup = new SetupTopIO
  val ctrl = new IOCtrlIO

  // Setup basic controls used throughput -- i.e. resync control signals
  val GlobalInit = DSPModule(new GlobalInit)
  GlobalInit.setupI <> setup
  GlobalInit.ioCtrlI <> ctrl

  // Configure options used throughout
  val GeneralSetup =  DSPModule(new GeneralSetup)
  GeneralSetup.setupTop <> GlobalInit.setupO

  // IO specific setup
  val IOSetup = DSPModule (new IOSetup)
  IOSetup.setupTop <> GlobalInit.setupO
  IOSetup.generalSetup <> GeneralSetup.o

  // IO Control signal generation
  val IOCtrl = DSPModule(new IOCtrl)
  IOCtrl.ctrl.enable := GlobalInit.ioCtrlO.enable
  IOCtrl.ctrl.reset := GlobalInit.ioCtrlO.reset
  IOCtrl.generalSetup <> GeneralSetup.o
  IOCtrl.ioSetup <> IOSetup.o
  IOCtrl.calcCtrlI <> GlobalInit.calcCtrlO
  IOCtrl.ioCtrlI.clkEn := GlobalInit.ioCtrlI.clkEn

  // IOCtrl generates output flags
  ctrl.k := IOCtrl.ctrl.k
  ctrl.outValid := IOCtrl.ctrl.outValid

  // Twiddle specific setup
  val TwiddleSetup = DSPModule(new TwiddleSetup)
  TwiddleSetup.setupTop <> GlobalInit.setupO
  TwiddleSetup.generalSetup.radStageSum := GeneralSetup.o.radStageSum
  TwiddleSetup.generalSetup.stageRad := GeneralSetup.o.stageRad
  TwiddleSetup.ioSetup.stagePrimeIdx := IOSetup.o.stagePrimeIdx

  // Indicates setup done
  val SetupDone = DSPModule(new SetupDone(GeneralSetup.setupDelay + IOSetup.setupDelay + TwiddleSetup.setupDelay))
  SetupDone.io <> GlobalInit.setupDone
  setup.done := SetupDone.io.done

  // Calculation control logic
  val CalcCtrl = DSPModule(new CalcCtrl)
  CalcCtrl.ioCtrl.enable := GlobalInit.ioCtrlO.enable
  CalcCtrl.ioCtrl.reset := GlobalInit.ioCtrlO.reset
  CalcCtrl.generalSetup <> GeneralSetup.o
  CalcCtrl.ioFlags <> IOCtrl.ioFlagsNoDelay
  CalcCtrl.calcCtrlI <> GlobalInit.calcCtrlO

  // Generates twiddles
  val TwiddleGen = DSPModule(new TwiddleGen(gen))
  TwiddleGen.twiddleSetup <> TwiddleSetup.o
  TwiddleGen.calcCtrlFlag <> CalcCtrl.calcFlagsNoDelay
  TwiddleGen.ioSetup <> IOSetup.o

  // Memory banks + arbitration
  val MemBankInterface = DSPModule(new MemBankInterface(gen))
  MemBankInterface.calcCtrlFlags <> CalcCtrl.calcFlagsNoDelay
  MemBankInterface.calcCtrl <> CalcCtrl.o
  MemBankInterface.ioCtrlFlags <> IOCtrl.ioFlagsNoDelay
  MemBankInterface.ioCtrl <>IOCtrl.o

  // Total delay from calc counters to data valid @ butterfly
  val bfCtrlDly = Params.getDelays.calcCtrl + Params.getDelays.memArbiterTop + Params.getDelays.memReadAtoD

  // Butterfly
  // TODO: Support more PEs, get rid of checkDelay
  val Butterfly = DSPModule(new PE(gen))

  CheckDelay.off()

  Butterfly.io.currRad.get := Pipe(CalcCtrl.calcFlagsNoDelay.currRad,bfCtrlDly)
  Butterfly.io.calcDIT := CalcCtrl.calcFlagsNoDelay.isDIT.pipe(bfCtrlDly)

  // Pass in twiddles
  Butterfly.io.twiddles := TwiddleGen.o.twiddles

  // Memory <-> Butterfly
  Butterfly.io.x := MemBankInterface.butterfly.x

  if (!debugMode){
    MemBankInterface.butterfly.y := Butterfly.io.y
  } else {
    MemBankInterface.butterfly.y.zipWithIndex.foreach{x => {
      val currRadBF = Pipe(CalcCtrl.calcFlagsNoDelay.currRadNum,bfCtrlDly)
      val inTemp = MemBankInterface.butterfly.x(x._2)
      val in = Mux(DSPUInt(x._2) < currRadBF,inTemp,Complex(double2T(0.0),double2T(0.0)))
      x._1 := in ** (double2T(10.0),Real,mPipe = Butterfly.delay)
    }}
  }

  // Additional 1 IO clock cycle delay needed -- as an example,
  // if reset goes high on IO clk 0, the counter is actually reset to 0 on IO clk 1
  // and din to mem interface needs to line up with counter = 0
  MemBankInterface.topIO.din := io.din.pipe(Params.getIO.clkRatio+Params.getDelays.ioCtrl)

  val isFFT = GlobalInit.setupO.isFFT
  val topOutDly = Params.getDelays.topOut

  if (Params.getFFT.normalized) {
    val Normalize = DSPModule(new Normalize(gen), "normalize")
    Normalize.io.din := MemBankInterface.topIO.dout
    Normalize.setupTop <> GlobalInit.setupO
    val normalizedOut = Normalize.io.dout.cloneType()
    normalizedOut := Normalize.io.dout
    io.dout.real := Mux(isFFT, normalizedOut.real, normalizedOut.imag).pipe(topOutDly)
    io.dout.imag := Mux(isFFT, normalizedOut.imag, normalizedOut.real).pipe(topOutDly)
  }
  else {
    io.dout.real := Mux(isFFT,MemBankInterface.topIO.dout.real,MemBankInterface.topIO.dout.imag).pipe(topOutDly)
    io.dout.imag := Mux(isFFT, MemBankInterface.topIO.dout.imag, MemBankInterface.topIO.dout.real).pipe(topOutDly)
  }

  Status("Out count to output delay: " + Params.getDelays.outFlagDelay)

  // Note: extra + 1 due to the fact that input must be registered first to align with counters zeroing out after reset
  Status("Start of k = 0 (frame 0) " + (Params.getDelays.outFlagDelay/Params.getIO.clkRatio + 1) + " IO clocks" +
    " after start of n = 0 (frame 2)")

}