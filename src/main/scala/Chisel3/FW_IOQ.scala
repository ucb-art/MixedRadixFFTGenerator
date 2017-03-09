package dspblocks.fft

case class IOQ(value: Int, base: Int) {
  require(value >= 0, "Q value should be >= 0")
  require(WFTA.getValidRad.contains(base) || base == 1, s"Base $base must be in WFTA.groupedValidRad or equal to 1")
}

object IOQ {
  /** Calculates the Q needed for PFA + CTA DIT/DIF IO (in-place)
    * Outputs: DIF Q with base, DIT Q with base
    */
  def apply(numFFTs: Int, ioParams: IOParams): IOParams = {
    val coprimes = ioParams.coprimes
    val global = ioParams.global
    val (qDIF, qDIT) = (0 until numFFTs).map(rowIdx => 
      // N1, N2, N3 reversed between DIT/DIF
      (apply(coprimes(rowIdx), global), apply(coprimes(rowIdx).reverse, global))
    ).unzip
    ioParams.copy(qDIF = qDIF, qDIT = qDIT)
  }

  /** Extended Euclidean Algorithm
    * ax + by = gcd(a, b)
    * Inputs: a, b
    */
  case class Euclid(gcd: Int, x: Int, y: Int) {
    def toTuple = (gcd, x, y)
  }
  def egcd(a: Int, b: Int): Euclid = {
    if (a == 0) Euclid(b, 0, 1)
    else {
      val (gcd, y, x) = egcd(b % a, a).toTuple
      Euclid(gcd, x - (b / a) * y, y)
    }
  }

  /** Calculate Q values for N = N1 * N2 * N3 * ... where Nx are coprimes -- this is per N */
  def apply(coprimesInfo: Seq[CoprimeInfo], global:Seq[GlobalPrimeInfo]): Seq[IOQ] = {
    val coprimes = coprimesInfo.map(_.coprime)
    val correspondingPrimes = coprimesInfo.map(_.associatedPrime)
    // Base associated with each Q -- note that length of Q = length of coprimes - 1 (take first elements)
    val qBases = correspondingPrimes.map(prime => 
      // When for an FFTN, a coprime = 1 (therefore prime = 1), doing a search in global
      // will not have any results (1 is not in global), therefore force rad = 1 in that case
      // Note that find returns first found result
      global.find(_.prime == prime).getOrElse(GlobalPrimeInfo(prime = prime, maxRadix = 1, maxCoprime = 1)).maxRadix
    ).init

    // # of coprime decomposition equation sets is 1 less than # of coprimes
    (1 until coprimes.length).zip(qBases) map { case (idx, base) => 
      // Factor N into Na, Nb
      // where Na is a coprime and Nb is the product of the other coprimes
      val na = coprimes(idx - 1)
      val nb = coprimes.filterNot(_ == na).product
      val (gcd, p, gcdQ) = egcd(na, nb).toTuple

      // CTA (A = 1; gcd != 1) vs PFA
      // Normal A = p * Nx = q * L * R + 1 --> Q = q * L, where L = product of factors to the left of Na
      // and L * R = Nb
      val right = coprimes.drop(idx).product
      val q = -gcdQ * (if (gcd != 1) 0 else nb / right)
      
      // Q' = (Na-Q) % Na
      val qPrime = (na - q) % na

      val usedQ = {
        if (na == 1) 0                    // Skip if coprime slot unused
        else if (qPrime < 0) qPrime + na  // Remainder can be negative; want only >= 0 modulus, so renormalize
        else qPrime
      }
      IOQ(value = usedQ, base = base)
    }
  }
}