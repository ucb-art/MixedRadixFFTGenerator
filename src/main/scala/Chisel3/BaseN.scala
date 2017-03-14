package dsptools.numbers
import org.scalatest.{FlatSpec, Matchers}

class BaseNSpec extends FlatSpec with Matchers {
  behavior of "BaseN"
  it should "properly convert a decimal into BaseN" in {

    case class BaseNTest(n: Int, rad: Int, res: Seq[Int])

    // Most significant digit first (matched against WolframAlpha)
    val tests = Seq(
      BaseNTest(27, 4, Seq(1, 2, 3)),
      BaseNTest(17, 3, Seq(1, 2, 2)),
      BaseNTest(37, 5, Seq(1, 2, 2))
    )
    tests foreach { case BaseNTest(n, rad, res) =>
      require(BaseN.toDigitSeqMSDFirst(n, rad) == res, s"Base $rad conversion should work!")
      val paddedBaseN = BaseN.toDigitSeqMSDFirst(n, rad, 500)
      require(paddedBaseN == (Seq.fill(paddedBaseN.length - res.length)(0) ++ res), 
        s"Padded base $rad conversion should work!")
    }
  }
}

object BaseN {
  // Support a finite # of bases to make prime check, etc. easier
  // DO NOT CHANGE (unless you know what's going on)
  val supportedBases = Seq(Seq(2, 4), Seq(3), Seq(5), Seq(7), Seq(11))

  /** Converts a decimal representation of the number n into a Seq of
    * Ints representing the base-r interpretation of n 
    * NOTE: Least significant digit is highest indexed (right-most) due to recursion
    */
  private def toDigitSeqInternal(n: Int, r: Int): Seq[Int] = {
    require(n >= 0, "n must be >= 0")
    require(r > 0 && supportedBases.flatten.contains(r), 
      s"r $r must be > 0 and supported by BaseN.supportedBases")
    // Least significant digit is right-most (resolved in this iteration)
    if (n == 0) Nil else toDigitSeqInternal(n / r, r) :+ (n % r)
  }
  def toDigitSeqMSDFirst(n: Int, r: Int): Seq[Int] = {
    // Highest digit first (left-most)
    val temp = toDigitSeqInternal(n, r)
    // Should return non-empty list
    if (temp.isEmpty) Seq(0) else temp
  }
   /** Zero pads Seq[Int] base-r representation */
  def toDigitSeqMSDFirst(n: Int, r: Int, maxn: Int): Seq[Int] = {
    val digitSeq = toDigitSeqMSDFirst(n, r)
    val maxNumDigits = toDigitSeqMSDFirst(maxn, r).length
    val fillDigits = (maxNumDigits - digitSeq.length)
    val padding = List.fill(fillDigits)(0)
    padding ++ digitSeq
  }

  /** Returns # of Base r digits needed to represent the number n */
  def numDigits(n: Int, r: Int): Int = toDigitSeqInternal(n, r).length
}

class MixedRadixSpec extends FlatSpec with Matchers {
  behavior of "MixedRadix"
  it should "properly convert a decimal into MixedRadix" in {

    case class MixedRadixTest(n: Int, rad: Seq[Int], res: Seq[Int])

    // Most significant digit first (matched against WolframAlpha)
    val tests = Seq(
      MixedRadixTest(6, Seq(4, 4, 2), Seq(3, 0)),
      MixedRadixTest(6, Seq(4, 4, 2, 4), Seq(1, 2))
    )
    tests foreach { case MixedRadixTest(n, rad, res) =>
      require(MixedRadix.toDigitSeqMSDFirst(n, rad) == res, s"$rad conversion should work!")
      val paddedMixedRadix = MixedRadix.toDigitSeqMSDFirst(n, rad, 16)
      require(paddedMixedRadix == Seq.fill(paddedMixedRadix.length - res.length)(0) ++ res, 
        s"Padded $rad conversion should work!")
    }
  }
}

object MixedRadix {
  // Least significant digit is highest indexed (right-most)
  private def toDigitSeqInternal(n: Int, radicesLowFirst: Seq[Int]): Seq[Int] = {
    require(n >= 0, "n must be >= 0")
    radicesLowFirst foreach { r => require(r > 0, "r vals must be > 0") }

    if (n != 0 && radicesLowFirst.isEmpty) 
      throw new Exception("N is out of range for the given set of radices!")

    // Start dividing from LSD (resolves LSDs first)
    if (n == 0) Nil 
    else toDigitSeqInternal(n / radicesLowFirst.head, radicesLowFirst.tail) :+ (n % radicesLowFirst.head)
  }

  // Least significant digit is highest indexed (right-most)
  def toDigitSeqMSDFirst(n: Int, radicesHighFirst: Seq[Int]): Seq[Int] = {
    // Assume radix of LSD is high-indexed for radices
    // But toDigitSeqInternal resolves LSDs first, so need to reverse
    val temp = toDigitSeqInternal(n, radicesHighFirst.reverse)
    // Should return non-empty list
    if (temp.isEmpty) Seq(0) else temp
  }
  def toDigitSeqMSDFirst(n: Int, radicesHighFirst: Seq[Int], maxn: Int): Seq[Int] = {
    val digitSeq = toDigitSeqMSDFirst(n, radicesHighFirst)
    val maxNumDigits = toDigitSeqMSDFirst(maxn, radicesHighFirst).length
    val fillDigits = (maxNumDigits - digitSeq.length)
    val padding = List.fill(fillDigits)(0)
    padding ++ digitSeq
  }
  def numDigits(n: Int, radices: Seq[Int]): Int = toDigitSeqInternal(n, radices).length
}