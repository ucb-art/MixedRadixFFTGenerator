package FFT
import scala.math._
import ChiselDSP.Error

object Factorize {

  /** Input: List of supported FFT sizes
    * Output: Butterfly.rad, Calc.radPow, Calc.maxStages, IO.primes, IO.coprimes, IO.maxCoprimes (see Params.scala)
    */
  def apply(sizes: List[Int]): Tuple6[List[Int],List[List[Int]],Int,List[Int],List[List[Int]],List[Int]] = {
    // Get exponents in the set of basex^expx
    val radPow = for (x <- sizes) yield {apply(x)}
    val radPowFlat = radPow.map(_.flatten)
    // Get used radices
    val rad = (radPowFlat.transpose.map(_.sum).zipWithIndex.map{ case (e,i) => {
      if (e != 0) WFTA.getValidRad(i)
      else 0
    }})
    val radOut = rad.filter(_ != 0)
    // Get max # of stages needed
    val maxStages = radPowFlat.map(_.sum).max
    // Get coprime table
    val coprimes = radPow.map(_.zipWithIndex.map{
      case (e1,i1) => e1.zipWithIndex.map {
        case (e2,i2) => math.pow(WFTA.validRad(i1)(i2),e2)
      }.product.toInt
    })
    // Remove unused exponents (when the full column = 0)
    val radPowOut = radPowFlat.map( x => {
      var temp = Array.empty[Int]
      x.zipWithIndex.foreach { case (e, i) => {
        if (rad(i) != 0) temp = temp :+ e
      }}
      temp.toList
    })
    // Get used primes
    val primes = WFTA.validRad.map( x => {
      if (x.intersect(radOut).length != 0) x.min
      else 0
    })
    // Remove unused coprimes (when the full column = 1)
    val coprimesOut = coprimes.map( x => {
      var temp = Array.empty[Int]
      x.zipWithIndex.foreach { case (e, i) => {
        if (primes(i) != 0) temp = temp :+ e
      }}
      temp.toList
    })
    val maxCoprimes = coprimesOut.transpose.map(_.max)
    (radOut,radPowOut,maxStages,primes.filter(_ != 0),coprimesOut,maxCoprimes)
  }
  /** Factor FFTN into powers of valid radices and store powers at the appropriate index */
  def apply(n: Int): List[List[Int]] = {
    // Test if FFTN can be factored by each of the butterfly valid radices (mod = 0)
    // Count # of times it can be factored
    var num = n
    val radPow = for (x <- WFTA.validRad) yield { for ( y <- x) yield {
      var (mod,count) = (0,0)
      while (mod == 0){
        mod = num % y
        if (mod == 0){
          count = count+1
          num = num/y
        }
      }
      count
    }}
    // If number hasn't completely been factorized, then an unsupported radix is required
    if (num != 1) Error("FFT size " + n + " is invalid for allowed butterfly radices.")
    radPow
  }
}
