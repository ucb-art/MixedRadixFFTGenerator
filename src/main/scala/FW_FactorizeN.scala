package FFT
import scala.math._
import ChiselDSP.{BaseN, Error}
import dspblocks.fft._

// TODO: Convert my List of Tuples to Maps

object Factorize {

  /** Input: List of supported FFT sizes
    * Output: List Butterfly.rad (global),
    *         List2D Calc.radPow,
    *         List2D Calc.radOrder,
    *         List Calc.maxRad = [maxRad,maxRadIdx],
    *         Int Calc.maxStages,
    *         List2D IO.coprimes = [coprime,prime,numDigits],
    *         List IO.global = [prime,maxRadix,maxCoprime] for each used prime
    * (see Params.scala)
    */
  def apply(sizes: List[Int]): Tuple7[
    List[Int],
    List[List[Int]],
    List[List[Int]],
    List[Tuple2[Int,Int]],
    Int,
    List[List[Tuple3[Int,Int,Int]]],
    List[Tuple3[Int,Int,Int]]
  ] = {

    val temp = dspblocks.fft.FactorizationParams(FFTNs(sizes: _*))
    (
      temp.butterfly.rad.toList,
      temp.calc.radPow.map(_.toList).toList,
      temp.calc.radOrder.map(_.toList).toList,
      temp.calc.maxRad.map(x => (x.maxRad, x.maxRadIdx)).toList,
      temp.calc.maxStages,
      temp.io.coprimes.map(y => y.map(x => (x.coprime, x.associatedPrime, x.numDigits)).toList).toList,
      temp.io.global.map(x => (x.prime, x.maxRadix, x.maxCoprime)).toList
    )

  }
}
