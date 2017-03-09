package FFT
import dspblocks.fft._

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
    val radPowNew = radPow.map(_.toSeq).toSeq
    val radOrderNew = radOrder.map(_.toSeq).toSeq
    val maxRadNew = maxRadIn.map(x => MaxRadixInfo(x._1, x._2)).toSeq
    val in = dspblocks.fft.CalcParams(radPow = radPowNew, radOrder = radOrderNew, maxRad = maxRadNew, maxStages = maxStages)
    val out = MemoryAccessParams(in, FFTNs(fftSizes.toSeq: _*))
    (out.addressConstants.map(_.toList).toList, out.maxNumBanks, out.bankLengths.toList)
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