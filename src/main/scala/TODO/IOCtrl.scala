package FFT
import ChiselDSP._
import Chisel.{Pipe =>_,Complex => _,Mux => _, RegInit => _, RegNext => _, _}

class IOSetupIO extends IOBundle {
  // Index of current FFT N
  val fftIdx = DSPUInt(INPUT,Params.getFFT.nCount - 1)
  // Enable setup
  val enable = DSPBool(INPUT)
  // Done with IO setup
  val done = DSPBool(OUTPUT)

}

class IOCtrl extends DSPModule {

  val setup = new IOSetupIO

  // Used for masking to get coprime mod (when operating in Base N and not binary)
  // i.e. mod 4 is equivalent to a 000...00011 bit mask, except this is a digit mask
  // coprimes -> [coprime, corresponding prime, digit mask]
  val primeDigitsLUT = DSPModule(new IntLUT2D(Params.getIO.coprimes.map(_.map(_._3))), "primeDigits")
  primeDigitsLUT.io.addr := setup.fftIdx
  val primeDigits = primeDigitsLUT.io.dout.cloneType
  primeDigits := RegNext(Mux(setup.enable,primeDigitsLUT.io.dout,primeDigits))

  // Indices indicating order of prime decomposition i.e. (3,2,5) might have indices (1,0,2) if
  // Params.getIO.global has primes stored as (2,3,5)
  // global -> [prime used, prime base (max radix), max coprime]
  // globalPrimes(0) associated with unused (i.e. *1)
  // globalRads(0) associated with unused
  val globalPrimes = List(1) ++ Params.getIO.global.map(_._1)
  val globalRads = List(0) ++ Params.getIO.global.map(_._2)
  val primeIndices = Params.getIO.coprimes.map(_.map{ x =>
    val prime = x._2
    globalPrimes.indexOf(prime)
  })
  val primeIdxLUT = DSPModule(new IntLUT2D(primeIndices), "primeIdx")
  primeIdxLUT.io.addr := setup.fftIdx
  val primeIdx = primeIdxLUT.io.dout.cloneType
  primeIdx := RegNext(Mux(setup.enable,primeIdxLUT.io.dout,primeIdx))


}