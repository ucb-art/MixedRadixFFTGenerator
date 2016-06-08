package FFT
import Chisel.{Complex => _, Mux => _, Reg => _, RegNext => _, RegInit => _, Pipe => _, Mem => _,
Module => _, ModuleOverride => _, when => _, switch => _, is => _, unless => _, Round => _,  _}
import ChiselDSP._

// TODO: Pass enable, FFT/IFFT optional(!)
// TODO: fftIdx should  be optional?

class FFTIO[T <: DSPQnm[T]](gen : => T) extends IOBundle {
  val din = Complex(gen).asInput
  val dout = Complex(gen).asOutput
}

class Normalize[T <: DSPQnm[T]](gen : => T) extends GenDSPModule (gen) {

  override val io = new FFTIO(gen)
  val setupTop = new SetupTopIO

  // TODO: Add in support for IFFT (!)

  // Create LUT for FFT: To normalize, scale by * (1/sqrt(n))
  // For IFFT: To normalize, scale by * sqrt(n)
  val fftNormalizeVals = Params.getFFT.sizes.map(1/math.sqrt(_))
  // val ifftNormalizeVals = Params.getFFT.sizes.map(math.sqrt(_))
  val fftNormalizeLUT = DSPModule(new DblLUT(fftNormalizeVals,gen),"fftNormalizeFactor")
  // val ifftNormalizeLUT = DSPModule(new DblLUT(ifftNormalizeVals,gen),"ifftNormalizeFactor")

  fftNormalizeLUT.io.addr := setupTop.fftIdx
  //ifftNormalizeLUT.io.addr := setupTop.fftIdx

  // TODO: Do I actually need to cloneType?
  val fftNormalizeFactor = fftNormalizeLUT.io.dout.cloneType()
  fftNormalizeFactor := fftNormalizeLUT.io.dout
  //val ifftNormalizeFactor = ifftNormalizeLUT.io.dout.cloneType()
  //ifftNormalizeFactor := ifftNormalizeLUT.io.dout
  //val normalizeFactor = Mux(setupTop.isFFT,fftNormalizeFactor,ifftNormalizeFactor)
  val normalizeFactor = fftNormalizeFactor

  // TODO: Don't depend on trim defaults in complex!
  io.dout := (io.din ** (normalizeFactor,Real)).trim(gen.getFracWidth)

  // Delay through this block
  val delay = io.dout.getDelay-io.din.getDelay

}

