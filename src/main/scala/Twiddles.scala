package FFT
import ChiselDSP._

object Twiddles {
  /** Input: powers for each used radix, order of used radices
    *
    * @param coprimesIn list of [coprime,prime,numDigits]
    * @param global list of [prime,maxRadix,maxCoprime]
    * @param radPow powers (exponent) for each used radix
    * @param radOrder order of used radices (base)
    * @param maxStages maximum # of stages for all FFT sizes
    * @return twiddleCountMax main twiddle count max for each calculation stage (CTA)
    *         twiddleSubCountMax twiddle sub count max for each calculation stage (derived from PFA and coprimes)
    *         twiddleLUTScale base multiply amount to scale range of twiddle counts to full twiddle LUT size
    *         twiddles for each radix
    */
  def apply(coprimesIn:List[List[Tuple3[Int,Int,Int]]],
            global:List[Tuple3[Int,Int,Int]],
            radPow:List[List[Int]],
            radOrder:List[List[Int]],
            maxStages:Int): Tuple2[List[List[Int]],List[List[Int]]] = {
    // Gets the radices associated with each calculation stage from radPow and radOrder (decompression),
    // also records the radix of the stage that came before (1 for first stage)
    val stages_prevStages = radPow.zip(radOrder).map{ case (pow,rad) => {
      val stageGen = pow.zip(rad)
      val stagesTemp = stageGen.tail.foldLeft(
        List.fill(stageGen.head._1)(stageGen.head._2)) ((b,a) => b ++ List.fill(a._1)(a._2))
      val padding = List.fill(maxStages-stagesTemp.length)(0)
      val stages = stagesTemp ++ padding
      val prevStages = List(1) ++ stages.init
      stages.zip(prevStages)
    }}

    // Count max for CTA (within a coprime)
    val coprimes_primes = coprimesIn.map( x => x.map( y => (y._1,y._2)))
    val twiddleCountMax = stages_prevStages.zip(coprimes_primes).map{ case (x,coprime_prime) => {

      // For a particular FFTN, gets the coprime corresponding to the current radix
      def rad2Coprime(rad:Int, coprime_prime:List[(Int,Int)]): Int = {
        // Gets base prime associated with radix
        val basePrime = WFTA.validRad.map { x => {
          if (x.contains(rad)) x.min else -1
        }}.filter(_ != -1).head
        coprime_prime.find(_._2 == basePrime).get._1
      }

      val firstRad = x.head._1
      x.tail.scanLeft(rad2Coprime(firstRad,coprime_prime)/firstRad-1)((b,a) => {
        // For radices in a coprime, the previous radix is >= the current radix (also divisible by)
        // However, @ coprime boundaries there will always be a non-zero mod i.e.
        // Stage 1: 0 to N_coprime/R1-1
        // Stage 2: 0 to N_coprime/R1/R2-1
        // Unused stages have 0 counts
        if (a._1 == 0) 0
        else if (a._2 % a._1 == 0) (b + 1) / a._1 - 1
        else rad2Coprime(a._1,coprime_prime)/a._1 - 1
      })
    }}

    // Initial coprime twiddle count renormalization (multiply count)
    // To renormalize twiddle count to the full twiddle LUT range i.e. addr. 2 of 4 --> addr. 4 of 8
    // max coprime / current coprime (same base), except when current coprime < max radix (associated w/
    // the coprime) i.e. no twiddle used
    val twiddleLUTScale = coprimes_primes.map( _.map{ case (coprime,prime) => {
      val (a,maxRad,maxCoprime) = global.find(_._1 == prime).get
      if (coprime < maxRad) 0 else maxCoprime/coprime
    }})




    (twiddleCountMax,twiddleLUTScale)
  }

}