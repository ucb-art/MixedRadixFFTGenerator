package FFT
import Chisel.{Complex => _, Mux => _, Reg => _, RegNext => _, RegInit => _, Pipe => _, Mem => _,
Module => _, ModuleOverride => _, when => _, switch => _, is => _, unless => _, Round => _,  _}
import ChiselDSP._

// TODO: Pass enable, FFT/IFFT optional(!)

class NormalizeIO[T <: DSPQnm[T]](gen : => T, outDlyMatch:Boolean = true) extends IOBundle (outDlyMatch = outDlyMatch) {
  val in = Complex(gen).asInput
  val normalizedOut = Complex(gen).asOutput
  val fftIdx = DSPUInt(INPUT,Params.getFFT.nCount-1)
  val FFT = DSPBool(INPUT)

  // TODO: fftIdx should  be optional? 
}

class Normalize[T <: DSPQnm[T]](gen : => T) extends GenDSPModule (gen) {

  override val io = new NormalizeIO(gen)
  // TODO: Add in support for IFFT (!)

  // Create LUT for FFT: To normalize, scale by * (1/sqrt(n))
  // For IFFT: To normalize, scale by * sqrt(n)
  val fftNormalizeVals = Params.getFFT.sizes.map(1/math.sqrt(_))
  // val ifftNormalizeVals = Params.getFFT.sizes.map(math.sqrt(_))
  val fftNormalizeLUT = DSPModule(new DblLUT(fftNormalizeVals,gen),"fftNormalizeFactor")
  // val ifftNormalizeLUT = DSPModule(new DblLUT(ifftNormalizeVals,gen),"ifftNormalizeFactor")
  fftNormalizeLUT.io.addr := io.fftIdx
  //ifftNormalizeLUT.io.addr := io.fftIdx

  // TODO: Do I actually need to cloneType?
  val fftNormalizeFactor = fftNormalizeLUT.io.dout.cloneType()
  fftNormalizeFactor := fftNormalizeLUT.io.dout
  //val ifftNormalizeFactor = ifftNormalizeLUT.io.dout.cloneType()
  //ifftNormalizeFactor := ifftNormalizeLUT.io.dout
  //val normalizeFactor = Mux(io.FFT,fftNormalizeFactor,ifftNormalizeFactor)
  val normalizeFactor = fftNormalizeFactor

  // TODO: Don't depend on trim defaults in complex!
  io.normalizedOut := (io.in ** (normalizeFactor,Real)).trim(gen.getFracWidth)

  // Delay through this block
  val delay = io.normalizedOut.getDelay-io.in.getDelay

}

