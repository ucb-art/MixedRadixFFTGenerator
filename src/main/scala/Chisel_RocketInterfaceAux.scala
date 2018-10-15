package FFT
import ChiselDSP._
import Chisel.{Pipe =>_,Complex => _,Mux => _, RegInit => _, RegNext => _, Counter => _, when => _, _}

object RocketInterfaceParams {
  def apply(): GeneratorParams = {
    GeneratorParams(
      complex = ComplexParams(
        intBits = 4,
        fracBits = 17, //11, //19,
        use4Muls = false,
        mulPipe = 2,
        addPipe = 0.333,
        trimType = Truncate,
        overflowType = Grow,
        mulFracGrowth = 1
      ),
      clock = ClockParams(
        periodx100ps = Math.floor(162.76).toInt
      ),
      fft = FFTParams(
        sizes =
          List(12, 24, 48, 96, 192, 384, 768, 36, 72, 144, 288, 576, 1152, 108, 216, 432, 864, 324, 648, 1296, 972,
          60, 120, 240, 480, 960, 180, 360, 720, 300, 600, 1200, 540, 1080, 900).sorted ++
          List(64, 128, 256, 512, 1024, 2048, 1536),
        normalized = true,
        generateOffset = true
      ),
      test = TestParams(
        frames = 1            
      )
    )
  }
}

// User-defined memory specs
case class MemorySpecs[T <: Data](
  key: String,
  depth: Int = 1,
  // is writable + readable from/to Rocket
  isWritable: Boolean = true,
  isReadable: Boolean = true,
  dataType: T,
  // Control register --> on write, initiate action (but don't actually write value)
  isCtrl: Boolean = false,
  // Base address of memory (Needs edit)
  base: Int = -1
){
  def addrMax = base + depth - 1
  def printMap = {
    Status("Address of [" + key + "]: \t\t" + base + " \t to \t" + addrMax)
  }
}

// Keeps track of read addresses into FFT
class CalcInCounter(max:Int) extends Counter(CountParams(countMax = max))

// See SCRFile in Uncore for usage example
// Copy of IO needed for SMI interface
// TODO: Just use junctions one

// From Rocket
// io.smi.req.valid       --- Rocket wants to read or write
// io.smi.req.bits.rw     --- Rocket wants to read (false) or write (true)
// io.smi.req.bits.addr   --- Memory location Rocket wants to access
// io.smi.req.bits.data   --- Data Rocket wants to write (FFT should ignore on read)
// io.smi.resp.ready      --- Rocket ready to take back response

// From FFT
// io.smi.req.ready       --- FFT capable of taking requests
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