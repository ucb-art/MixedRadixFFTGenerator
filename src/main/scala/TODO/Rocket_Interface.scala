package FFT
import ChiselDSP._
import Chisel.{Pipe =>_,Complex => _,Mux => _, RegInit => _, RegNext => _, Counter => _, _}

// Copy of IO needed for SMI interface
// TODO: Just use junctions one

// From Rocket
// io.smi.req.valid       --- Rocket wants to read or write
// io.smi.req.bits.rw     --- Rocket wants to read (false) or write (true)
// io.smi.req.bits.addr   --- Memory location Rocket wants to access
// io.smi.req.bits.data   --- Data Rocket wants to write (FFT should ignore on read)
// io.smi.resp.ready      --- Rocket ready to take back response

// From FFT
// * io.smi.req.ready       --- FFT capable of taking requests (tied high; nothing to stall)
// io.smi.resp.valid      --- FFT did something (write to memory successful; data to Rocket valid)
// io.smi.resp.bits       --- FFT data to be sent to Rocket

class FFTSmiReq(val dataWidth: Int, val addrWidth: Int) extends Bundle {
  val rw = Bool()
  val addr = UInt(width = addrWidth)
  val data = Bits(width = dataWidth)

  override def cloneType =
    new FFTSmiReq(dataWidth, addrWidth).asInstanceOf[this.type]
}

class FFTSmiIO(val dataWidth: Int, val addrWidth: Int) extends Bundle {
  // Rocket -> FFT
  val req = Decoupled(new FFTSmiReq(dataWidth, addrWidth))
  // FFT -> Rocket
  val resp = Decoupled(Bits(width = dataWidth)).flip

  override def cloneType =
    new FFTSmiIO(dataWidth, addrWidth).asInstanceOf[this.type]
}


class RocketToFFT extends Module {

///////////////////////////////////////////// SETUP FFT

  // TODO: Print used params
  val fftParams = GeneratorParams(
    complex = ComplexParams(
      intBits       = 4,
      fracBits      = 19,
      use4Muls      = true,
      mulPipe       = 2,
      addPipe       = 0.333,
      trimType      = Truncate,
      overflowType  = Grow,
      mulFracGrowth = 1
    ),
    clock = ClockParams(
      periodx100ps  = Math.floor(162.76).toInt
    ),
    fft = FFTParams(
      sizes   = List( 12,24,48,96,192,384,768,36,72,144,288,576,1152,108,216,432,864,324,648,1296,972,
                      60,120,240,480,960,180,360,720,300,600,1200,540,1080,900).sorted ++
                List( 64,128,256,512,1024,2048,1536),
      normalized = true,
      generateOffset = true
    ),
    test = TestParams(
      frames  = 5
    )
  )

  // TODO: Get rid of this (esp. in GenInit)
  val args = Array("-params_true_true")
  val (isFixedParam,p) = Init({fftParams}, jsonName = "", args = args)

  val fft = DSPModule(new FFT({DSPFixed(p.complex.getFixedParams)},p), "rocket_fft")

///////////////////////////////////////////// ROCKET <--> FFT INTERFACE

  // TODO: Write getter function
  val fixedWidth = p.complex.intBits + p.complex.fracBits + 1
  val complexWidth = 2*fixedWidth
  // Address width is also width of k
  val addrWidth = DSPUInt.toBitWidth(p.fft.sizes.max-1)
  val dataWidth = {
    val dblWidth = 64
    if (complexWidth + addrWidth > dblWidth) Error("Rocket data interface can't support > 64 bits")
    dblWidth
  }

  Status("Rocket memory length: " + p.fft.sizes.max)
  Status("Rocket memory address width: " + addrWidth)
  Status("Rocket data width needed (LSBs of 64): " + (fixedWidth + complexWidth))

  val io = new Bundle {
    val smi = new FFTSmiIO(dataWidth,addrWidth).flip
  }

  // FFT always can take inputs
  io.smi.req.ready := Bool(true)











  io.smi.resp.bits := fft.io.dout.toBits


// bring out slowen
// slow en, outvalid, stop






}