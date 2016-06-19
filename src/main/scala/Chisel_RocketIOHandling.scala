package FFT
import ChiselDSP.{when => _, _}
import Chisel.{RegInit => _, Counter => _, _}

// Delay expected from rAddr @ memory to valid data out (can be generalized)
class RocketIOHandling(dataWidth:Int, addrWidth:Int, dly:Int) extends Module {

  require(dly > 0, "Delay should >0")

  val io = new Bundle {
    val smi = new FFTSmiIO(dataWidth,addrWidth).flip
    val ctrl = new Bundle {
      val wAddr = DSPUInt(OUTPUT,DSPUInt.toMax(addrWidth))
      val rAddr = DSPUInt(OUTPUT,DSPUInt.toMax(addrWidth))
      val we = DSPBool(OUTPUT)
      val din = Bits(width = dataWidth).asOutput
      val dout = Bits(width = dataWidth).asInput
      val rocketFire = DSPBool(OUTPUT)
    }
  }

  io.ctrl.din := io.smi.req.bits.data
  io.smi.resp.bits := io.ctrl.dout

  val rocketFire = io.smi.req.fire()
  io.ctrl.rocketFire := DSPBool(rocketFire)

  io.ctrl.wAddr := DSPUInt(io.smi.req.bits.addr,DSPUInt.toMax(addrWidth))
  // Write when Rocket out is valid & in write mode
  io.ctrl.we := DSPBool(rocketFire & io.smi.req.bits.rw)

  // Read data (and therefore address) needs to be held until Rocket acknowledges receipt
  val rAddr = RegInit(UInt(0,width = addrWidth))
  when (rocketFire){
    rAddr := io.smi.req.bits.addr
  }
  io.ctrl.rAddr := DSPUInt(rAddr,DSPUInt.toMax(addrWidth))

  // 3 main FFT response states
  // Idle: req.ready & !resp.valid --> (on req.fire = FFT received a request from Rocket)
  // Processing: !req.ready & !resp.valid --> (on counter = delay-1 a.k.a. FFT response going to be valid)
  // Complete: !req.ready & resp.valid --> (on resp.fire = Rocket received response)
  // Idle
  // Actual state machine for read, but use it for write too

  val ioIdle :: ioProcessing :: ioComplete :: Nil = Enum(UInt(),3)
  val ioState = RegInit(ioIdle)

  val fftReady = ioState === ioIdle
  val fftValid = ioState === ioComplete

  val dlyCount = RegInit(UInt(dly))
  val startProcessing = rocketFire & fftReady
  when(startProcessing){
    dlyCount := UInt(0)
  }.otherwise{
    dlyCount := dlyCount + UInt(1)
  }

  when(startProcessing){
    ioState := ioProcessing
  }.elsewhen((ioState === ioProcessing) & (dlyCount === UInt(dly-1))){
    ioState := ioComplete
  }.elsewhen(fftValid & io.smi.resp.fire()){
    ioState := ioIdle
  }

  io.smi.resp.valid := fftValid
  io.smi.req.ready := fftReady

}