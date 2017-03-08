package dsptools.numbers

object BaseN {
  // Support a finite # of bases to make prime check, etc. easier
  // DO NOT CHANGE (unless you know what's going on)
  val supportedBases = Seq(Seq(2, 4), Seq(3), Seq(5), Seq(7), Seq(11))

  /** Converts a decimal representation of the number n into a Seq of
    * Ints representing the base-r interpretation of n 
    * NOTE: Least significant digit is highest indexed
    */
  private def toIntSeqInternal(n: Int, r: Int): Seq[Int] = {
    require(n >= 0, "n must be >= 0")
    require(r > 0, "r must be > 0")
    if (n == 0) Nil else toIntSeqInternal(n / r, r) :+ (n % r)
  }

  /** Returns # of Base r digits needed to represent the number n */
  def numDigits(n: Int, r: Int): Int = toIntSeqInternal(n, r).length
}