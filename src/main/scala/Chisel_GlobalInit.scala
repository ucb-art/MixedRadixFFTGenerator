/*


package FFT
import ChiselDSP._
import Chisel.{Pipe =>_,Complex => _,Mux => _, RegInit => _, RegNext => _, _}

class GlobalInit extends DSPModule {

  val setupTop = new SetupTopIO
  val ioCtrl = new IOCtrlIO



  // should it be enable?! setupStart can only be valid for 1 clock cycle...

  val ioEnable = DSPBool(INPUT)
  // Reset IO counters & start inputting from n = 0 of frame
  val startFrameIn = DSPBool(INPUT)



  val enable = RegInit(DSPBool(true))

  val enable1 =  enable ? (!setupTop.enable)
  enable := Mux(frameFirstIn,DSPBool(true),enable1)


  val realEnable = enable ? ioEnable

  val reset = setupTop.enable | firstFrameIn



  // enable: only get last on (falling cycle)?

  // global enable, reset
  // local enable, reset
}

class SetupTopIO extends IOBundle {
  // Index of current FFT N
  val fftIdx = DSPUInt(INPUT,Params.getFFT.nCount - 1)
  // Enable setup
  val enable = DSPBool(INPUT)
  // Done with IO setup
  val done = DSPBool(OUTPUT)
  // Is FFT? (otherwise IFFT)
  val isFFT = DSPBool(INPUT)
}


*/