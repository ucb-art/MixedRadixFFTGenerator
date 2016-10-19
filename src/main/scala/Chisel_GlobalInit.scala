package FFT
import ChiselDSP._
import Chisel.{Pipe =>_,Complex => _,Mux => _, RegInit => _, RegNext => _, _}

// TODO: Refactor IO
// TODO: Pull synchronization into FFT

class GlobalInit extends DSPModule {

  // Comes directly from FFT top IO
  val setupI = new SetupTopIO
  val ioCtrlI = new IOCtrlIO

  val setupO = (new SetupTopIO).flip
  val ioCtrlO = (new IOCtrlIO).flip
  val calcCtrlO = (new CalcCtrlI).flip

  val setupDone = (new SetupDoneIO).flip

  // IO "clocking"
  val clkRatio = Params.getIO.clkRatio
  val clkDiv = DSPModule(new ClkDiv(clkRatio))
  val slowEn = clkDiv.io.slowEn

  // Slow clock to the outside world
  if (Params.getIO.clkRatio > 1)
    ioCtrlI.clkEn.get := slowEn

  // Flags used
  val initSetup = slowEn & setupI.enable
  val resetCalc = slowEn & ioCtrlI.reset
  setupDone.init := initSetup

  // TODO: Double check clkRatio generalization
  // Enable signal to other setup blocks only goes high after the top-level setup enable
  // goes from high to low (@ IO clock rate) -- held high for IO clock rate
  val setupEnCapture = Vec(2,RegInit(DSPBool(false)))
  setupEnCapture(0) := initSetup | (!slowEn & setupEnCapture(0))
  setupEnCapture(1) := Mux(slowEn,setupEnCapture(0),setupEnCapture(1))
  val validSetup = setupEnCapture(1) & !setupEnCapture(0)
  setupO.enable := (validSetup).pipe(clkRatio)
  setupDone.lastSetupDelta := setupO.enable & slowEn

  setupDone.inc := slowEn

  // Only update setup parameters when setup is enabled (last setup IO clock)
  val captureIn = slowEn & validSetup
  if (setupO.fftIdx != None) {
    val fftIdxCapture = RegInit(DSPUInt(0,Params.getFFT.nCount-1))
    // 2 IO clks to align with captureIn
    fftIdxCapture := Mux(captureIn,setupI.fftIdx.get.pipe(2*clkRatio),fftIdxCapture)
    setupO.fftIdx.get := fftIdxCapture
  }
  val isFFTCapture = RegInit(DSPBool(true))
  isFFTCapture := Mux(captureIn,setupI.isFFT.pipe(2*clkRatio),isFFTCapture)
  setupO.isFFT := isFFTCapture

  // TODO: Add delay more appropriately? (don't rely on direct IO to trigger counter -- register first?)
  // Once setup enable is detected, disable all counters
  // Counters are re-enabled on the IO Cycle that startFrameIn goes high (no delay)
  // -- Note still a function of slowEn
  // Counters can also be held when ioCtrlI.enable is low (no delay from top level)
  val globalCalcEnable = RegInit(DSPBool(false))
  val globalCalcEnTemp = globalCalcEnable ? (!initSetup)
  globalCalcEnable := resetCalc | (globalCalcEnTemp ? !resetCalc)
  // Note calculation enables are not constrained by IO clock
  calcCtrlO.enable := (resetCalc | globalCalcEnable) & ioCtrlI.enable
  ioCtrlO.enable := calcCtrlO.enable & slowEn
  // Reset has precedence over enable (so that during setup, counters should be 0ed), no delay
  ioCtrlO.reset := initSetup | resetCalc

}

