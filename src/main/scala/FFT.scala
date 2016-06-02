package FFT

// ------- Imports START -- DO NOT MODIFY BELOW
import Chisel.{Complex => _, Mux => _, Reg => _, RegNext => _, RegInit => _, Pipe => _, Mem => _,
               Module => _, ModuleOverride => _, when => _, switch => _, is => _, unless => _, Round => _,  _}
import ChiselDSP._
// ------- Imports END -- OK TO MODIFY BELOW

// IO used for setup
class TopSetupIO extends IOBundle {
  // SETUP_INIT should be true when new FFT_INDEX, FFT is presented.
  // FFT = true -> FFT calculation; FFT = false -> IFFT calculation
  val SETUP_INIT = DSPBool(INPUT)
  val FFT_INDEX = DSPUInt(INPUT,Params.getFFT.nCount - 1)
  // TODO: Should be optional
  val FFT = DSPBool(INPUT)

  // SETUP_DONE = true -> Ready to take FFT data
  val SETUP_DONE = DSPBool(OUTPUT)
}

// IO used for main FFT processing control
class FFTCtrlIO extends IOBundle {
  // Enable calculation (and therefore all registers)
  val ENABLE = DSPBool(INPUT)
  // START_FIRST_FRAME should be high when 1st Frame n = 0 data is input (only once after setup)
  val START_FIRST_FRAME = DSPBool(INPUT)

  // High when first data of a valid frame is output i.e. k = 0
  val FRAME_FIRST_OUT = DSPBool(OUTPUT)

  // Offset (relative to # of symbols in a frame)
  val OFFSET = DSPUInt(OUTPUT,Params.getFFT.sizes.max-1)
}

// FFT data IO
class FFTIO[T <: DSPQnm[T]](gen : => T) extends IOBundle {
  val DATA_IN = Complex(gen).asInput
  val DATA_OUT = Complex(gen).asOutput
}
