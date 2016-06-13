package FFT
import ChiselDSP._
import Chisel.{Pipe =>_,Complex => _,Mux => _, RegInit => _, RegNext => _, _}

class SetupDoneIO extends IOBundle {

  // "Resets" setup done timer whenever setup enable is detected
  val init = DSPBool(INPUT)
  // Wait until setup enable goes low to start setting up
  val lastSetupDelta = DSPBool(INPUT)
  // Count with IO clocks
  val inc = DSPBool(INPUT)
  // Setup done
  val done = DSPBool(INPUT)
}

class SetupDone(setupDly: Int) extends DSPModule {

  override val io = new SetupDoneIO

  // setupDly in fast clock cycles
  val baseIOSetupCycles = setupDly/Params.getIO.clkRatio
  val addIOSetupCycle = if (setupDly % Params.getIO.clkRatio != 0) 1 else 0
  val IOSetupCycles = baseIOSetupCycles + addIOSetupCycle

  val maxCount = IOSetupCycles + 2

  val setupCounter = RegInit(DSPUInt(0,maxCount))

  when (io.init) {
    setupCounter := DSPUInt(0)
  }.elsewhen (setupCounter === DSPUInt(0))


  // setup done = when maxed out

}