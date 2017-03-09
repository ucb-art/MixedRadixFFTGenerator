package dspblocks.fft
import dsptools.numbers.BaseN

// Note: Google "Scala varargs"
case class FFTNs(private val sizes: Int*) {
  // Make sure it's immutable
  def get = sizes.toList.toSeq
  def num = get.length
  def max = get.max
  require(num > 0, "Must have at least one N")
  get foreach { n => require(n > 1, "N must be > 1") }
}
// Index associated with WFTA.getRadIdx(maxRad)
case class MaxRadixInfo(maxRad: Int, maxRadIdx: Int) {
  require(WFTA.getValidRad.contains(maxRad), "Must use valid radix!")
  require(maxRadIdx >= 0, "Radix index must be non-negative")
}

// Ex: Coprime = 2^n, associatedPrime = 2, numDigits = # of Base 'prime' digits needed to represent 
// 0 to (coprime - 1)
case class CoprimeInfo(coprime: Int, associatedPrime: Int, numDigits: Int) {
  require(coprime % associatedPrime == 0 && coprime >= 0, "Coprime must be divisible by associated prime and >= 0")
  require(WFTA.getValidRad.contains(associatedPrime) || associatedPrime == 1, 
    s"Prime $associatedPrime must be in WFTA.groupedValidRad or equal to 1")
  require(numDigits >= 0, "Number of digits should be >= 0")
}
// For a globally used prime, keep track of the associated max radix and max coprime
// associated with it
case class GlobalPrimeInfo(prime: Int, maxRadix: Int, maxCoprime: Int) {
  require(maxCoprime % prime == 0 && maxCoprime >= 0, "Max Coprime must be divisible by associated prime")
  require(maxRadix % prime == 0, "Max Radix must be divisible by associated prime")
  require(WFTA.getValidRad.contains(prime) || prime == 1, s"Prime $prime must be in WFTA.groupedValidRad or equal to 1")
  require(WFTA.getValidRad.contains(maxRadix) || maxRadix == 1, 
    s"Max radix $maxRadix must be in WFTA.groupedValidRad or equal to 1")
}
case class ButterflyParams(rad: Seq[Int]) {
  rad foreach { r => 
    require(WFTA.getValidRad.contains(r), "Butterfly radix must be in WFTA.groupedValidRad")
  }
  def maxRad = rad.max
}
case class CalcParams(
    radPow: Seq[Seq[Int]],
    radOrder: Seq[Seq[Int]],
    maxRad: Seq[MaxRadixInfo],
    maxStages: Int) {
  radPow.flatten foreach { pow => require(pow >= 0, "Power must be non-negative") }
  radOrder.flatten foreach {r => 
    require(WFTA.getValidRad.contains(r) || r == 1, s"Radix $r must be in WFTA.groupedValidRad or equal to 1")
  }
  require(maxStages > 0, "Number of stages should be > 0")
  def radPowCols = radPow.transpose
  def radPowColMax = radPowCols.map(_.max)
  def radOrderCols = radOrder.transpose
  def radOrderColMax = radOrderCols.map(_.max)
  def getMaxRad = maxRad.map(_.maxRad)
  def getMaxRadIdx = maxRad.map(_.maxRadIdx)
}

case class IOParams(
    coprimes: Seq[Seq[CoprimeInfo]],
    global: Seq[GlobalPrimeInfo],
    // Not populated until you get to IOQ
    qDIF: Seq[Seq[IOQ]] = Seq(Seq()),
    qDIT: Seq[Seq[IOQ]] = Seq(Seq())) {
  def coprimeCols = coprimes.transpose
  def qDIFCols = qDIF.transpose
  def qDITCols = qDIT.transpose
  def globalPrime = global.map(_.prime)
  def globalMaxRadix = global.map(_.maxRadix)
  def globalMaxCoprime = global.map(_.maxCoprime)
}
case class FactorizationParams(
    butterfly: ButterflyParams,
    calc: CalcParams,
    io: IOParams) {
}

object FactorizationParams {
  // Input: List of FFT N's
  // Output: See above
  def apply(fftns: FFTNs): FactorizationParams = {
    // Get exponents associated with radices (for each n = row)
    val radPow = fftns.get.map(n => apply(n))
    val radPowFlat = radPow map (_.flatten)

    // Get globally used radices (sum column values, which are associated with radix powers;
    // when radices are used, power > 0 --> sum across all n's > 0)
    // If a radix is not used, the factor = 1
    val fromValidRads = radPowFlat.transpose.map(columnPows => columnPows.sum).zipWithIndex map { 
      case (sumPow, radIdx) if (sumPow != 0) => WFTA.getValidRad(radIdx)
      case _ => 1
    }
    val globalUsedRad = fromValidRads.filter(_ != 1)

    // Get max # of stages needed
    // For each FFTN, sum across the row of powers to get # of stages
    val globalMaxStages = radPowFlat.map(_.sum).max

    // Get coprime table i.e. List of (2^n, 3^m, 5^k) corresponding to all N's
    val coprimes = radPow map(_.zipWithIndex map {
      case (primeGroup, primeGroupIdx) => 
        primeGroup.zipWithIndex map {
          case (pow, radIdx) => 
            math.pow(WFTA.groupedValidRad(primeGroupIdx)(radIdx), pow).toInt
        } product
    })

    val usedPowsRadsMaxRadices = radPowFlat map { case nPowsRow =>
      // Don't include if radix is globally unused (i.e. the whole column is unused)
      val (nUsedPows, nUsedRads) = nPowsRow.zip(fromValidRads) filter { case (pow, rad) => rad != 1 } map {
        case (pow, rad) => 
          // If on a per FFT size basis, an exponent is 0, assign radix = 1 
          // NOTE: assumes no radix shuffling
          (pow, if (pow == 0) 1 else rad)
      } unzip
      // Gets the max radices for each FFTN (+ corresponding column in nUsedRads -> dictates stopping pt)
      val maxRad = nUsedRads.max
      val maxRadIdx = nUsedRads.indexOf(maxRad)
      (nUsedPows, nUsedRads, MaxRadixInfo(maxRad = maxRad, maxRadIdx = maxRadIdx))
    }  
    val nUsedPows = usedPowsRadsMaxRadices map (_._1)
    val nUsedRads = usedPowsRadsMaxRadices map (_._2)
    val maxRadices = usedPowsRadsMaxRadices map (_._3)

    // Get used primes + corresponding maximum radix used
    // Expects that the prime is the smallest value in groupedValidRad 
    // i.e. for (2, 4, 8) it's 2; the biggest used radix corresponding
    // to that prime is found via intersection
    val usedPrimesMaxRadices = WFTA.groupedValidRad map { case primeGroup => 
      val intersection = primeGroup.intersect(globalUsedRad)
      if (intersection.length != 0) (primeGroup.min, intersection.max)
      // TODO: Maybe change to (1, 1) ? 
      else (0, 0)
    } 
    val (usedPrimes, usedMaxRadices) = usedPrimesMaxRadices.unzip

    val coprimeInfo = coprimes map { case nCoprimesRow => 
      // Don't generate for globally unused coprimes
      nCoprimesRow.zip(usedPrimes) filter { case (coprime, prime) => prime != 0 } map {
        case (coprime, prime) => 
          // If, per FFT size, currently checked coprime = 1, associated radix is 1, 0 digits
          if (coprime == 1)  
            CoprimeInfo(coprime = coprime, associatedPrime = 1, numDigits = 0)
          // Digits required to represent up to (but not including) coprime (i.e. for counter)
          else {
            val numDigits = BaseN.numDigits(coprime - 1, prime)
            CoprimeInfo(coprime = coprime, associatedPrime = prime, numDigits = numDigits)
          }
      }
    }

    // GLOBAL System level: (only) used primes, associated max radix, associated max coprime for each used prime
    // Get largest 2^n, 3^m, 5^k used in the system (look for max in a column)
    val maxCoprimes = coprimeInfo.transpose map(coprimeColumns => coprimeColumns.map(infoRow => infoRow.coprime).max)
    val globalPrimeInfo = usedPrimesMaxRadices filter { case (prime, rad) => prime != 0 } zip (maxCoprimes) map { 
      case ((prime, maxRadix), maxCoprime) => 
        GlobalPrimeInfo(prime = prime, maxRadix = maxRadix, maxCoprime = maxCoprime) 
    }

    FactorizationParams(
      butterfly = ButterflyParams(rad = globalUsedRad),
      calc = CalcParams(radPow = nUsedPows, radOrder = nUsedRads, maxRad = maxRadices, maxStages = globalMaxStages),
      io = IOParams(coprimes = coprimeInfo, global = globalPrimeInfo))
  }

  /** Factor FFTN into powers of valid radices and store powers at the appropriate location
      Ex: WFTA.groupedValidRad = Seq(Seq(4, 2), Seq(3))
      Output: Seq(Seq(5, 6), Seq(7))
      Implies N = 4^5 * 2^6 * 3^7 
  */
  def apply(n: Int): Seq[Seq[Int]] = {
    // Test if FFTN can be factored by each of the butterfly valid radices (mod = 0)
    // Count # of times it can be factored
    var unfactorized = n
    val radPow = for (primeGroup <- WFTA.groupedValidRad) yield { for (rad <- primeGroup) yield {
      var (mod, pow) = (0, 0)
      while (mod == 0) {
        mod = unfactorized % rad
        if (mod == 0) {
          pow = pow + 1
          unfactorized = unfactorized / rad
        }
      }
      pow
    }}
    // If number hasn't completely been factorized, then an unsupported radix is required
    require(unfactorized == 1, s"FFT size $n is invalid for allowed butterfly radices.")
    radPow
  }
}