package FFT
import dspblocks.fft._
object IOQ {

  /** Calculates the Q needed for PFA + CTA DIT/DIF IO (in-place)
    * Inputs: # of FFT lengths,
    *         coprimes corresponding to the FFT sizes [coprime, corresponding prime, # digits],
    *         global [used prime, max radix associated with used prime, max coprime associated with used prime]
    * Outputs: DIF Q with base, DIT Q with base
    */
  def apply(nCount: Int, coprimesx:List[List[(Int,Int,Int)]], globalx:List[(Int,Int,Int)]):
            Tuple2[List[List[Tuple2[Int,Int]]],List[List[Tuple2[Int,Int]]]] = {
    val ioParams = dspblocks.fft.IOParams(
      coprimes = coprimesx.map(x => x.map { case (a, b, c) => CoprimeInfo(a, b, c) }.toSeq).toSeq,
      global = globalx.map { case (a, b, c) => GlobalPrimeInfo(a, b, c)}
    )
    val out = dspblocks.fft.IOQ(nCount, ioParams)
    val qDIF = out.qDIF.map(x => x.map(y => (y.value, y.base)).toList).toList
    val qDIT = out.qDIT.map(x => x.map(y => (y.value, y.base)).toList).toList
    (qDIF, qDIT)
  }
}