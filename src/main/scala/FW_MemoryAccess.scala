package FFT

object MemoryAccess{
  /** @param radPow powers (exponent) for each used radix
    * @param radOrder order of used radices (base)
    * @param maxStages maximum # of stages for all FFT sizes
    * @param maxRadIn maximum radix + its index (in radOrder) for all FFT sizes
    * @param fftSizes list of used FFT Ns
    * @return Address constants for going from nx to memory bank addresses,
    *         Max # of memory banks,
    *         Max length of each bank needed
    */
  def apply(radPow:List[List[Int]],radOrder:List[List[Int]],maxStages:Int,maxRadIn:List[(Int,Int)],fftSizes:List[Int]):
      Tuple3[List[List[Int]],Int,List[Int]] = {
    val maxRad = maxRadIn.unzip._1
    val stages = Stages_PrevStages(radPow,radOrder,maxStages).map(_.unzip._1)
    // Calculates address constants (after evenly dividing out # of banks, the
    // length of each bank is smaller i.e. N/#banks)
    // TODO: Bank != max radix
    val ac = stages.zip(maxRad).map{case (stages,maxRad) => {
      // Set the first occurrence of an FFT's max radix in the list of stage radices to 1
      // (essentially tell it to skip over the max radix stage)
      // Ax = A(x+1) * r(x+1)
      val idxMaxRad = stages.indexOf(maxRad)
      val stagesMod = stages.updated(idxMaxRad,1)
      // Determines # of used stages, and trims list
      val numStagesTemp = stages.indexOf(0)
      val numStages = if(numStagesTemp == -1) maxStages else numStagesTemp
      val usedStagesMod = stagesMod.dropRight(stagesMod.length-numStages)
      val addressConstantTemp = usedStagesMod.tail.scanRight(1)((b,a) => b * a)
      // Zero out AC associated w/ max radix stage
      val addressConstantShort = addressConstantTemp.updated(idxMaxRad,0)
      // Pad back to max # of stages
      addressConstantShort ++ List.fill(maxStages-addressConstantShort.length)(0)
    }}
    // TODO: Correct numBanks for multi BFs
    val numBanks = maxRad.max
    val memLengths = fftSizes.zip(maxRad).map{case (n,r) => {
      val pad = numBanks-r
      List.fill(r)(n/r) ++ List.fill(pad)(0)
    }}.transpose.map(_.max)
    (ac,numBanks,memLengths)

  }

  /** @param radPow powers (exponent) for each used radix
    * @param radOrder order of used radices (base)
    * @param maxStages maximum # of stages for all FFT sizes
    * @return Gets the radix associated with each calculation stage from radPow and radOrder (decompression),
    *         also records the radix of the stage that came before (1 for first stage)
    */
  def Stages_PrevStages(radPow:List[List[Int]],radOrder:List[List[Int]],maxStages:Int): List[List[Tuple2[Int,Int]]] = {
    radPow.zip(radOrder).map{ case (pow,rad) => {
      val stageGen = pow.zip(rad)
      val stagesTemp = stageGen.tail.foldLeft(
        List.fill(stageGen.head._1)(stageGen.head._2)) ((b,a) => b ++ List.fill(a._1)(a._2))
      val padding = List.fill(maxStages-stagesTemp.length)(0)
      val stages = stagesTemp ++ padding
      val prevStages = List(1) ++ stages.init
      stages.zip(prevStages)
    }}
  }

}