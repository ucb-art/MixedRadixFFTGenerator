package FFT
import scala.math._
import ChiselDSP.{BaseN, Error}

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

    // Get exponents in the set of basex^expx
    val radPow = for (x <- sizes) yield {apply(x)}
    val radPowFlat = radPow.map(_.flatten)

    // Get globally used radices
    val rad = (radPowFlat.transpose.map(_.sum).zipWithIndex.map{ case (e,i) => {
      if (e != 0) WFTA.getValidRad(i)
      else 0
    }})
    val globalRad = rad.filter(_ != 0)

    // Get max # of stages needed
    val globalMaxStages = radPowFlat.map(_.sum).max

    // Get coprime table
    val coprimes = radPow.map(_.zipWithIndex.map{
      case (e1,i1) => e1.zipWithIndex.map {
        case (e2,i2) => math.pow(WFTA.validRad(i1)(i2),e2)
      }.product.toInt
    })

    // Remove unused exponents (globally when the full column = 0),
    // associate used exponents with corresponding radices,
    // and the max radix used per FFTN + index (index corresponding to aforementioned radices)
    val radPow_rad_maxRad = radPowFlat.map( x => {
      var temp = Array.empty[Tuple2[Int,Int]]
      x.zipWithIndex.foreach { case (e, i) => {
        // If on a per FFT size basis, an exponent is 0, assign radix = 1
        val radTemp = if (e == 0) 1 else rad(i)
        if (rad(i) != 0) temp = temp :+ (e,radTemp)
      }}
      val radPow_rad = temp.toList
      val rads = radPow_rad.unzip._2
      val maxRad = rads.max
      val maxRadIdx = rads.indexOf(maxRad)
      (radPow_rad,(maxRad,maxRadIdx))
    })

    // TODO: Base 2 can be represented easily by Base 4 b/c of binary, but Base 3 can't be represented
    // easily by Base 9 -- therefore it's not easy to expand outward for non-power of 2 bases
    // Also, break this function up for reuse after scheduling

    // Get used primes + corresponding maximum radix used
    val primes_maxRadix = WFTA.validRad.map( x => {
      val intersection = x.intersect(globalRad)
      if (intersection.length != 0) (x.min,intersection.max)
      else (0,0)
    })

    // Remove unused coprimes (globally, when the full coprime column = 1); associated w/ base primes + # of digits
    // for values represented up to the coprime
    val coprimes_primes_digits = coprimes.map( x => {
      var temp = Array.empty[Tuple3[Int,Int,Int]]
      x.zipWithIndex.foreach { case (e, i) => {
        val prime = primes_maxRadix(i)._1
        if (prime != 0) {
          // Digits required to represent up to (but not including) coprime (i.e. for counter)
          val numDigits = BaseN.numDigits(e-1,prime)
          temp = temp :+ (e,prime,numDigits)
        }
      }}
      temp.toList
    })

    // System level: used primes, associated max radix, associated max coprime for each used prime
    val maxCoprimes = coprimes_primes_digits.transpose.map(_.map(_._1).max)
    val tmp = primes_maxRadix.filter(_._1 != 0).zip(maxCoprimes)
    val globalPrimes_maxRadix_maxCoprimes = tmp.map(x => (x._1._1,x._1._2,x._2))

    ( globalRad,
      radPow_rad_maxRad.map(_._1.map(_._1)),
      radPow_rad_maxRad.map(_._1.map(_._2)),
      radPow_rad_maxRad.map(_._2),
      globalMaxStages,
      coprimes_primes_digits,
      globalPrimes_maxRadix_maxCoprimes
    )

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
