package dspblocks.fft
import dsptools.numbers.MixedRadix
import scala.collection.immutable.ListMap

abstract class FFTType {
  def serialize: String
}
object DIT extends FFTType {
  def serialize = "DIT"
}
object DIF extends FFTType {
  def serialize = "DIF"
}
object UnknownFFTType extends FFTType {
  def serialize = "Unknown"
}

case class BinToBankAddr(
    fftType: FFTType = UnknownFFTType, 
    n: Int = 0, 
    bank: Int, 
    addr: Int) {
  require(n >= 0, "Bin must be >= 0")
  require(bank >= 0, "Bank must be >= 0")
  require(addr >= 0, "Address must be >= 0")
  def getBankAddr = Seq(bank, addr)
}

object BinToBankAddrMap {
  
  def apply(fftType: FFTType, fftParams: FactorizationParams): Seq[BinToBankAddr] = {
    apply(fftType, fftParams.io, fftParams.calc, fftParams.mem)
  }

  def apply(fftType: FFTType, ioParams: IOParams, calcParams: CalcParams, memAccessParams: MemoryAccessParams): Seq[BinToBankAddr] = {
    require(ioParams.numFFTs == 1, 
      "Should only be using bin to bank, address map when you don't require runtime configuration")
    val coprimesTemp = ioParams.getCoprimes.head
    val primesTemp = ioParams.getPrimes.head
    val qDIF = ioParams.getQDIF.head
    val qDIT = ioParams.getQDIT.head
    val (coprimes, primes, q) = fftType match {
      case DIF => (coprimesTemp, primesTemp, qDIF)
      case DIT => (coprimesTemp.reverse, primesTemp.reverse, qDIT)
    }

    // Performs something like:
    // n3' := {n3' + 1 if n3' != N3 - 1; 0 otherwise}
    // n(x < 3)' := {nx' + 1 if nx' != Nx - 1; 0 otherwise} on n(x + 1)' -> 0

    // Note that n3 wraps first, but since Seq starts with 0th index, reverse for convenience
    val coprimesReversed = coprimes.reverse
    val ioIncCountsNPrimeTemp = coprimesReversed.zipWithIndex map { case (nx, idx) =>
      // Original refers to unreversed i.e.
      // 0 0 0
      // 0 0 1
      // 0 0 2
      // 0 1 0
      // 0 1 1
      // 0 1 2
      // 0 2 0
      // 0 2 1
      // 0 2 2
      // 1 0 0
      // Right of current coprime
      val origRightProduct = if (idx == 0) 1 else coprimesReversed.take(idx).product
      val origLeftProduct = coprimesReversed.drop(idx + 1).product
      // Go back to left-most = n0
      Seq.fill(origLeftProduct)(
        (0 until nx).map(Seq.fill(origRightProduct)(_)).flatten
      ).flatten
    }
    // Counts go from top to bottom
    val ioIncCountsNPrime = ioIncCountsNPrimeTemp.reverse.transpose
    val fftn = coprimes.product
    // No R associated with n3' (see paper)
    // Counters are always changing
    // R1 := {0 if WC; else (R1 + Q1') mod N1} where WC is n2' = N2 - 1 & n3' = N3 - 1
    // R2 := {0 if n3' = N3 - 1; else (R2 + Q2') mod N2}
    val ioQCountsRTemp = (0 until coprimes.length - 1).zip(q) map { case (idx, qval) =>
      val rightProduct = coprimes.drop(idx + 1).product
      val coprime = coprimes(idx)
      // Note: Scanleft results in array that's of length (qRepeat.length + 1)
      val qRepeat = Seq.fill(rightProduct - 1)(qval)
      Seq.fill(fftn/rightProduct)(
        qRepeat.scanLeft(0)((accum, qx) => (accum + qx) % coprime)
      ).flatten
    }
    // Match lengths!
    val ioQCountsR = {
      if (ioQCountsRTemp.isEmpty) Seq.fill(fftn)(Seq(0)) 
      else ioQCountsRTemp.transpose map (row => row :+ 0)
    }
    val coprimeCountsBase10 = ioIncCountsNPrime.zip(ioQCountsR) map { case (rowNprime, rowRcount) => 
      rowNprime.zip(rowRcount).zipWithIndex map { case ((nprime, rcount), idx) =>
        (nprime + rcount) % coprimes(idx)
      }
    }

    // TODO: Original BaseN counters should count using prime base (then it should be broken out into 4, 4, 2 etc.)

    def convertCountsToMixedRadix(counts: Seq[Seq[Int]]): Seq[Seq[Int]] = {
      // n1,x .. n1,0 .. n2,x .. n2,0 .. n3,x .. n3,0 for DIF
      // n3,0 .. n3,x .. n2,0 .. n2,x .. n1,0 .. n1,x for DIT
      // Note: Rather than directly swapping lowest digit for highest digit, you're actually
      // changing the order of the mixed radices for DIT; i.e. 4, 4, 2 becomes 2, 4, 4
      // DIF: Base[4, 2] 6 -> 3, 0
      // DIT: Base[2, 4] 6 -> 1, 2
      // AND THEN swapping digit order
      require(calcParams.getStages.length == 1, "Should only have one FFT size to use BinToBankAddrMap")
      val stageInfo = calcParams.getStages.head
      counts.map { case row => 
        // Reverse coprime ordering between DIT/DIF
        val rowWithIndex = fftType match {
          case DIT => row.zipWithIndex.reverse
          case DIF => row.zipWithIndex
        }
        rowWithIndex.map { case (count, idx) => 
          // radices -> highest radix with lowest index
          val radicesTemp = stageInfo.getStagesCorrespondingTo(primes(idx))
          // Reverse radix ordering if DIT when converting to mixed radix with the newly ordered coprimes
          val radices = fftType match {
            case DIT => radicesTemp.reverse
            case DIF => radicesTemp
          }
          // Reverse digit order for DIT
          val outTemp = MixedRadix.toDigitSeqMSDFirst(count, radices, coprimes(idx) - 1)
          fftType match {
            case DIT => outTemp.reverse
            case DIF => outTemp
          }
        }.flatten 
      }
    }

    // This should already correspond to n1, n2, n3, ... (mixed radix)
    val coprimeCounts = convertCountsToMixedRadix(coprimeCountsBase10)

    require(memAccessParams.addressConstants.length == 1, "Should only have one FFT size")
    val addressConstants = memAccessParams.addressConstants.head

    // TODO: # banks doesn't need to = max radix
    val numBanks = calcParams.maxRad.head.maxRad

    // Address = AC0*n0 + AC1*n1 + AC2*n2 + AC3*n3 + ...
    // Bank = (n0 + n1 + n2 + n3 + ...) mod maxRadix
    val out = coprimeCounts.zipWithIndex.map { case (row, idx) =>
      val addr = row.zip(addressConstants).map { case (n, ac) => n * ac }.sum
      val bank = row.sum % numBanks
      BinToBankAddr(fftType = fftType, n = idx, bank = bank, addr = addr)
    }

    // DEBUG
    incCounts += convertCountsToMixedRadix(ioIncCountsNPrime)
    qCounts += convertCountsToMixedRadix(ioQCountsR)
    cpCounts += coprimeCounts
    bankAddr += out

    out
    
  }

  // For single-threaded DEBUG
  // For incCounts, qCounts, cpCounts, 
  // Chisel2 version will not match unless DIF uses reverse and DIT doesn't (for coprime ordering)
  // Also, for incCounts, qCounts, cpCounts, Chisel2 doesn't use outTemp.reverse for DIT 
  // (swapped later for actually generating final n i.e. TB n (but not coprimeCounts) will match cpCounts)
  val incCounts = scala.collection.mutable.ArrayBuffer[Seq[Seq[Int]]]()
  val qCounts = scala.collection.mutable.ArrayBuffer[Seq[Seq[Int]]]()
  val cpCounts = scala.collection.mutable.ArrayBuffer[Seq[Seq[Int]]]()
  val bankAddr = scala.collection.mutable.ArrayBuffer[Seq[BinToBankAddr]]()
  
}