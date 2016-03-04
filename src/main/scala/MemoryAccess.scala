package FFT

object MemoryAccess{

  def apply(radPow:List[List[Int]],radOrder:List[List[Int]],maxStages:Int,maxRad:List[Int]): List[List[Int]] = {
    val stages = Stages_PrevStages(radPow,radOrder,maxStages).map(x => x.map(_._1))
    // Calculates address constants (after evenly dividing out # of banks, the
    // length of each bank is smaller)
    // TODO: Bank != max radix
    stages.zip(maxRad).map{case (stages,maxRad) => {
      // 0's out the last occurrence of an FFT's max radix in the list of stage radices
      val idxMaxRad = stages.lastIndexOf(maxRad)
      val stagesMod = stages.updated(idxMaxRad,0)
      // Determines # of used stages
      val numStagesTemp = stages.indexOf(0)
      val numStages = if(numStagesTemp == -1) maxStages else numStages


      stagesMod.tail.scanLeft(1)((b,a) => b * a)
    }}
  }

  /** @param radPow powers (exponent) for each used radix
    * @param radOrder order of used radices (base)
    * @param maxStages maximum # of stages for all FFT sizes
    * @return Gets the radices associated with each calculation stage from radPow and radOrder (decompression),
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