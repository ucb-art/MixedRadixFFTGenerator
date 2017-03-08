package dspblocks.fft

object WFTA {
  // Supported butterfly radices, grouped 
  // NOTE: This is fixed and should not be changed unless WFTA is redesigned (ORDER MATTERS)
  val groupedValidRad = Seq(Seq(4, 2), Seq(3), Seq(5), Seq(7))
  // WFTA stages (WFTA operation broken up into add/multiply stages)
  // Note: This is fixed and should not be changed unless WFTA is redesigned
  val stages = Seq(WFTAAdd, WFTAAdd, WFTAAdd, WFTAMul, WFTAAdd, WFTAAdd, WFTAAdd, WFTAAdd)

  def getValidRad = groupedValidRad.flatten
  def getRadIdx(rad: Int) = getValidRad.indexOf(rad)
}

abstract class WFTAStageType
case object WFTAMul extends WFTAStageType
case object WFTAAdd extends WFTAStageType