// Top level module
// TODO: Use := when not many io ports are used in bundle

package FFT
import ChiselDSP._
import Chisel.{Pipe =>_,Complex => _,Mux => _, RegInit => _, RegNext => _, Counter => _, _}

class FFT[T <: DSPQnm[T]](gen : => T, p: GeneratorParams) extends GenDSPModule (gen) {

  CheckDelay.off()

  // Setup FFT with user-defined parameters
  Params(p)

  // Data IO, setup IO, operating controls
  override val io = new FFTIO(gen)
  val setup = new SetupTopIO
  val ctrl = new IOCtrlIO

  // Setup basic controls used throughput
  val GlobalInit = DSPModule(new GlobalInit)
  GlobalInit.setupI <> setup
  GlobalInit.ioCtrlI <> ctrl

  // Configure options used throughout (setup
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

  val bfCtrlDly = Params.getDelays.calcCtrl + Params.getDelays.memArbiterTop + Params.getDelays.memReadAtoD

  // Butterfly
  // TODO: Support more PEs, get rid of checkDelay
  val butterfly = DSPModule(new PE(gen))

  CheckDelay.off()

  butterfly.io.currRad.get := Pipe(CalcCtrl.calcFlagsNoDelay.currRad,bfCtrlDly)
  butterfly.io.calcDIT := CalcCtrl.calcFlagsNoDelay.isDIT.pipe(bfCtrlDly)

  // Memory <-> Butterfly
  MemBankInterface.butterfly.y := butterfly.io.y
  butterfly.io.x := MemBankInterface.butterfly.x

  // Pass in twiddles
  butterfly.io.twiddles := TwiddleGen.o.twiddles







// CHECK BELOW, add IFFT back in capitalize bf



  // Input to memory bank interface, tf
  MemBankInterface.topIO.din := io.din.pipe(2+Params.getDelays.ioCtrl)


  val normalizedDelay = if (Params.getFFT.normalized) {
    val Normalize = DSPModule(new Normalize(gen), "normalize")
    Normalize.io.din := MemBankInterface.topIO.dout
    Normalize.setupTop <> GlobalInit.setupO
    val normalizedOut = Normalize.io.dout.cloneType()
    normalizedOut := Normalize.io.dout

    io.dout.real := normalizedOut.real.pipe(1)
    io.dout.imag := normalizedOut.imag.pipe(1)

    //io.dout.real := Mux(DSPBool(setup.isFFT), normalizedOut.real, normalizedOut.imag).pipe(1)
    //io.dout.imag := Mux(DSPBool(setup.isFFT), normalizedOut.imag, normalizedOut.real).pipe(1) // reg b/c delayed 1 cycle from memout reg, but delay another to get back to io cycle
    Normalize.delay
  }
  else {
    io.dout.real := Mux(DSPBool(setup.isFFT),MemBankInterface.topIO.dout.real,MemBankInterface.topIO.dout.imag).pipe(1)
    io.dout.imag := Mux(DSPBool(setup.isFFT), MemBankInterface.topIO.dout.imag, MemBankInterface.topIO.dout.real).pipe(1)   // reg b/c delayed 1 cycle from memout reg, but delay another to get back to io cycle
    0
  }

















}