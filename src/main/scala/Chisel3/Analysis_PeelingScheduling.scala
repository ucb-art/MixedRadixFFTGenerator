package dspblocks.fft

// TODO: Clean up code -- a lot of ops being run multiple times for no reason
// = takes an eternity to run analysis :(

case class FFASTParams(fftn: Int, subFFTns: Seq[Int])

object PeelingScheduling {

  // TODO: Remove
  def main(args: Array[String]): Unit = {
    val ffastParams = FFASTParams(fftn = 21600, subFFTns = Seq(675, 800, 864))
    //analyzeConflicts(ffastParams, DIT) 
    //analyzeConflicts(ffastParams, DIF) 
  }

  // TODO: Conflicts A LOT
  // BUT For each possible full FFT bin group (from current stage parallel ops), get # of bank conflicts
  // Can you find different addresses using the same bank that don't have conflicts?
  // There's probably some pattern, just like for the normal FFT
  def analyzeConflicts(ffastParams: FFASTParams, fftType: FFTType) = {
    val subFFTToBanksMap = getSubFFTBankLengths(ffastParams)
    ffastParams.subFFTns foreach { case subFFT => 
      val otherSubFFTs = ffastParams.subFFTns.filter(_ != subFFT)
      // For each sub FFT, figure out how many banks are needed and how long they must be
      val numBanksLengths = subFFTToBanksMap(subFFT)
      val numBanks = numBanksLengths.head
      val addrLength = numBanksLengths.last
      (0 until addrLength) foreach { case addr =>
        // If there are no conflicts, go through addresses one by one
        // For each address, get corresponding sub-FFT bins associated with each bank (bank, address combo)
        // For each address, if you check all banks, you'll see if you have conflicts
        val subFFTBinsForCurrentAddr = (0 until numBanks).map(bank => 
          getBinFromBankAddr(subFFT, fftType, Seq(bank, addr)))
        // Rows: sub-FFT bins associated with current address (different banks)
        // Columns: All aliased FFT bins into a row's sub-FFT Bin (only 1 non-zero for singleton)
        // Peeling off a singleton implies that a sub-FFT bin is associated only 1 non-zero FFT bin
        // BUT you don't know ahead of time which aiased FFT bin it is, so look at all possible combos
        val possibleFFTBinLocs = subFFTBinsForCurrentAddr.map(subFFTBin => 
          getFFTBinsFromSubFFTIdx(ffastParams, subFFT, subFFTBin))
        // From full FFT bins, calculate which sub-bins they fall into for the other sub-FFTs (other stages)
        val usedBanksInOtherStages = otherSubFFTs.map { case otherSubFFT => 
          possibleFFTBinLocs.map(row => row.map { case bin => 
            // Full FFT bin location -> get bank
            binToSubFFTBankAddr(ffastParams, bin, otherSubFFT, fftType).head
          }) 
        }
        // TODO: analyze conflicts
      }
    }
  }

  // Get # of banks required and bank lengths for each sub FFT
  def getSubFFTBankLengths(ffastParams: FFASTParams): Map[Int, Seq[Int]] = {  
    ffastParams.subFFTns.map { case subFFT =>
      val bankLengths = getFFTParams(subFFT).mem.bankLengths
      val numBanks = bankLengths.length
      require(bankLengths.distinct.length == 1, 
        "Since only doing 1 FFT at a time, all banks should be the same length")
      subFFT -> Seq(numBanks, bankLengths.head)
    }.toMap
  }

  // From full-length FFT bin, get the bank & address associated with the sub-FFT bin the signal folds into
  def binToSubFFTBankAddr(ffastParams: FFASTParams, bin: Int, subFFT: Int, fftType: FFTType): Seq[Int] = {
    val subFFTIdx = getSubFFTIdx(ffastParams, subFFT, bin)
    // From sub-FFT index, calculate associated bank + address
    getBankAddrFromBin(subFFT, fftType, subFFTIdx)
  }

  // Get all aliased FFT bins associated with Sub-FFT bin (not something you ever have to do directly)
  def getFFTBinsFromSubFFTIdx(ffastParams: FFASTParams, subFFT: Int, subFFTIdx: Int): Seq[Int] = {
    // Row: FFT bin, Column: Sub FFT
    val map = getBinToSubFFTIdxMap(ffastParams)
    val subFFTLoc = ffastParams.subFFTns.indexOf(subFFT)
    // Row: Sub FFT, Column: FFT bin
    val mapTranspose = map.transpose
    val getMatchingSubFFTIdx = mapTranspose(subFFTLoc).zipWithIndex.filter { 
      case ((subFFTVal, subFFTIdxVal), fftBinVal) => subFFTIdxVal == subFFTIdx
    }
    getMatchingSubFFTIdx map { case ((subFFTVal, subFFTIdxVal), fftBinVal) => fftBinVal }
  }

  def getSubFFTIdx(ffastParams: FFASTParams, subFFT: Int, fullFFTBin: Int): Int = {
    val map = getBinToSubFFTIdxMap(ffastParams)
    map(fullFFTBin)(subFFT)
  }

  // TODO: Should be calculated with mod instead of using a LUT (b/c # of bins is large)
  // Output rows: 0 until full length FFTN
  // Output columns: Sub FFT -> Sub FFT Idx associated with full FFT bin
  def getBinToSubFFTIdxMap(ffastParams: FFASTParams): Seq[Map[Int, Int]] = {
    // Note: One sub-FFT bin results from multiple of the full-length FFT bins
    // aliasing into it
    // X_(NA)[b] = Sum (s.t. b = j % NA) X[j]
    // Rows: FFT bins, Columns associated with subFFTs
    (0 until ffastParams.fftn) map { case bin =>
      ffastParams.subFFTns.map { case subFFT => subFFT -> bin % subFFT }.toMap
    }
  }

  // Potentially used for getting sub FFT bin from 
  // bank/address for singleton estimation (if you can find a way to resolve conflicts)
  // For somewhat parallel peeling, should go from bankAddr to bin (access multiple banks 
  // at the same time) -- In that case, you should bank # of LUTs and map address (LUT address)
  // to bin (LUT data)
  def getBinFromBankAddr(fftn: Int, fftType: FFTType, bankAddr: Seq[Int]): Int = {
    val map = getIOMemBankAddr(fftn, fftType)
    val foundBinToBankAddr = map.find(x => x.getBankAddr == bankAddr).get
    foundBinToBankAddr.n
  }

  // Returns Seq(bank, addr) -- used for FFT IO
  // For fully iterative peeling, can count through bins and get bank/addr
  def getBankAddrFromBin(fftn: Int, fftType: FFTType, bin: Int): Seq[Int] = {
    val map = getIOMemBankAddr(fftn, fftType)
    map(bin).getBankAddr
  }

  def getFFTParams(fftn: Int): FactorizationParams = {
    val fftns = FFTNs(fftn)
    // Parameters sequentially calculated/updated
    // TODO: Rename to UpdateFFTParamsWithMemoryAccessParams, etc.
    MemoryAccessParams(IOQ(FactorizationParams(fftns)))
  }

  // Can convert to LUT for IO (single FFT) since you map bin to memory bank + address
  def getIOMemBankAddr(fftn: Int, fftType: FFTType): Seq[BinToBankAddr] = {
    BinToBankAddrMap(fftType, getFFTParams(fftn))
  }


  def getSubFFTToIOBankAddrMap(ffastParams: FFASTParams, fftType: FFTType): Map[Int, Seq[BinToBankAddr]] = {
    ffastParams.subFFTns.map(subFFT => subFFT -> getIOMemBankAddr(subFFT, fftType)).toMap
  }
}