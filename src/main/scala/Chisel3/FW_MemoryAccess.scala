package dspblocks.fft

case class MemoryAccessParams(
  // Address constants for going from nx to memory bank addresses
  // Row associated with a particular FFTN
  addressConstants: Seq[Seq[Int]],
  // Max number of memory banks needed
  maxNumBanks: Int,
  // Max length of each bank
  bankLengths: Seq[Int]
) {
  addressConstants.flatten foreach { case ac => require(ac >= 0, "Address constants must be >= 0") }
  require(maxNumBanks >= WFTA.getValidRad.min, "# banks must be >= min radix supported by butterfly")
  bankLengths foreach { case l => require (l > 0, "Memory bank lengths must be > 0")}
  require(bankLengths.length == maxNumBanks, "# of bank lengths must equal max # banks")
}

object MemoryAccessParams {

  // For individual FFT, what stages are required
  case class FFTNStageInfo(stages: Seq[Int]) {
    // Records the radix of the stage that came before (1 for first stage)
    def prevStages = Seq(1) ++ stages.init
    stages foreach { s => require(s >= 0, "Stage must be non-negative") }
  }

  /** Gets the radix associated with each calculation stage,
    * also 
    */
  def getStages(calcParams: CalcParams): Seq[FFTNStageInfo] = {
    calcParams.radPow.zip(calcParams.radOrder) map { case (rowPow, rowRad) => 
      val stageGen = rowPow.zip(rowRad)
      // Ex: 2^3 * 3^2
      // stages -> 2, 2, 2, 3, 3 i.e. fill 2 3x and then fill 3 2x
      val stagesTemp = stageGen.tail.foldLeft(
        Seq.fill(stageGen.head._1)(stageGen.head._2)
      )((prev, curr) => prev ++ Seq.fill(curr._1)(curr._2))
      // Max # of stages is fixed -- if current FFT n doesn't require that many stages,
      // you need to pad with 0's
      val stages = stagesTemp ++  Seq.fill(calcParams.maxStages - stagesTemp.length)(0)
      FFTNStageInfo(stages = stages)
    }
  }

  def apply(calcParams: CalcParams, fftns: FFTNs): MemoryAccessParams = {
    val maxRads = calcParams.getMaxRad
    val fftnStages = getStages(calcParams).map(_.stages)
    // Calculates address constants (after evenly dividing out # of banks, the
    // length of each bank is smaller i.e. N/#banks)
    // TODO: Bank != max radix
    val ac = fftnStages.zip(maxRads) map { case (rowStages, rowMaxRad) => 
      // Set the first occurrence of an FFT's max radix in the list of stage radices to 1
      // (essentially tell it to skip over the max radix stage when calculating address constants
      // -- just pass value on the right through)
      // Ax = A(x+1) * r(x+1)
      val idxMaxRad = rowStages.indexOf(rowMaxRad)
      val stagesMod = rowStages.updated(idxMaxRad, 1)
      // Determines # of used stages, and trims list -- if no 0, then all stages are in use
      val numUsedStagesTemp = rowStages.indexOf(0)
      val numUsedStages = if (numUsedStagesTemp == -1) calcParams.maxStages else numUsedStagesTemp
      val usedStagesMod = stagesMod.dropRight(stagesMod.length - numUsedStages)
      val addressConstantTemp = usedStagesMod.tail.scanRight(1)((radix, ac) => ac * radix)
      // Zero out AC associated w/ max radix stage
      val addressConstantShort = addressConstantTemp.updated(idxMaxRad, 0)
      // Pad back to max # of stages
      addressConstantShort ++ Seq.fill(calcParams.maxStages - addressConstantShort.length)(0)
    }
    // TODO: Correct numBanks for multi BFs
    val numBanks = maxRads.max
    val memLengthsForAllNs = fftns.get.zip(maxRads) map { case (rowN, rowMaxRad) => 
      List.fill(rowMaxRad)(rowN / rowMaxRad) ++ List.fill(numBanks - rowMaxRad)(0)
    } 
    val memLengths = memLengthsForAllNs.transpose.map(bankCol => bankCol.max)
    MemoryAccessParams(addressConstants = ac, maxNumBanks = numBanks, bankLengths = memLengths)
  }
}