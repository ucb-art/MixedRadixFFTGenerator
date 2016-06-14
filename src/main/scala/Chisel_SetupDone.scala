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
  val done = DSPBool(OUTPUT)
}

class SetupDone(setupDly: Int) extends DSPModule {

  override val io = new SetupDoneIO

  // setupDly in fast clock cycles
  val baseIOSetupCycles = setupDly/Params.getIO.clkRatio
  val addIOSetupCycle = if (setupDly % Params.getIO.clkRatio != 0) 1 else 0
  val IOSetupCycles = baseIOSetupCycles + addIOSetupCycle

  // Additional 2 for going to reset + lastSetupDelta state
  val maxCount = IOSetupCycles + 2

  val setupCounter = IncReset(maxCount, nameExt = "setupCounter")

  // Always reset counter when setup enable detected
  setupCounter.iCtrl.reset := io.init
  // After setup enable detected, don't keep track of setup cycles until enable goes low
  // Then track setup cycles (will stop counter when counter maxed out = setup done)
  setupCounter.iCtrl.change.get := Mux(setupCounter.io.out === DSPUInt(0),
    io.lastSetupDelta,
    (setupCounter.io.out < DSPUInt(maxCount)) & io.inc
  )

  io.done := setupCounter.io.out === DSPUInt(maxCount)

}