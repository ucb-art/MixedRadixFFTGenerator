package FFT
import ChiselDSP._

object Twiddles {
  /** @param coprimesIn list of [coprime,prime,numDigits]
    * @param global list of [prime,maxRadix,maxCoprime]
    * @param radPow powers (exponent) for each used radix
    * @param radOrder order of used radices (base)
    * @param maxStages maximum # of stages for all FFT sizes
    * @return twiddleCountMax main twiddle count max for each calculation stage (CTA)
    *         twiddleLUTScale base multiply amount to scale range of twiddle counts to full twiddle LUT size
    *         twiddles for each radix
    *         twiddleSubcountMax is the sub count to hold the twiddle value when using PFA
    */
  def apply(coprimesIn:List[List[Tuple3[Int,Int,Int]]],
            global:List[Tuple3[Int,Int,Int]],
            radPow:List[List[Int]],
            radOrder:List[List[Int]],
            maxStages:Int): Tuple4[List[List[Int]],List[List[Int]],List[List[List[ScalaComplex]]],List[List[Int]]] = {

    val stages_prevStages = MemoryAccess.Stages_PrevStages(radPow,radOrder,maxStages)

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

    // Calculate twiddle LUTs
    val twiddles_lengths = global.map{case (prime,maxRadix,maxCoprime) => { twiddleN(maxRadix,maxCoprime)}}
    Status("Twiddle Memory Size: " + twiddles_lengths.unzip._2.sum)

    // Twiddle sub-count max is calculated by the product of the coprimes to the right of the current
    // coprime.
    val twiddleSubcountMax = coprimes_primes.map(_.unzip._1).map(x => {
      val nextCoprimes = x.tail
      (0 until nextCoprimes.length).toList.map(i => nextCoprimes.drop(i).product - 1)
    })

    (twiddleCountMax,twiddleLUTScale,twiddles_lengths.unzip._1,twiddleSubcountMax)

  }

  /** Calculates twiddles associated with each coprime set */
  def twiddleN(rad:Int,maxCoprime:Int): Tuple2[List[List[ScalaComplex]],Int] = {

    // TODO: Can you expand out to radix-8?
    // First stage of coprime calculation requires the most unique twiddles (coprime/radix)
    // Subsequent stages use renormalized addresses
    val twiddleNSize = maxCoprime/rad

    val twiddles = (0 until twiddleNSize).toList.map{k => {
      // Radix-n requires n-1 subsets of twiddles (1 to n-1); n = 0 is trivial
      (0 until rad-1).toList.map{ n => {
        // Twiddle W_N^(nk) = exp(-j*2*pi*n*k/N)
        // exp(i*t) = cos(t) + i*sin(t)
        val t = -2*math.Pi*k*(n+1)/maxCoprime
        Complex(math.cos(t),math.sin(t))
      }}
    }}
    (twiddles,twiddleNSize*(rad-1))
  }

}