package dspblocks.fft
import chisel3._
import chisel3.experimental._

object FFASTTopParams {
  
  // Must support bit growth!
  val adcDataType = FixedPoint(9.W, 8.BP)
  val dspDataType = FixedPoint(20.W, 10.BP)
  val delays = Seq(Seq(0, 1), Seq(6, 9), Seq(12, 19))
  val maxNumPeels = 0

  val ffastParams = FFASTParams(
    fftn = 21600,
    subFFTns = Seq(675, 800, 864),
    delays = delays,
    inputType = DIF,
    sparsityPc = 0.09
  )
  // Interestingly, DIF, DIT doesn't seem to matter much with this architecture...

  import dsptools._

  val dspContext = DspContext(
    // Expects bit growth accounted for
    overflowType = Wrap,
    // Simplest
    trimType = Floor,
    complexUse4Muls = false,
    numMulPipes = 1,
    numAddPipes = 0,
    binaryPointGrowth = 1,
    // Should not be used
    binaryPoint = Some(1),
    numBits = Some(1)
  )

  def fpBP = dspDataType.binaryPoint.get
  def adcBP = adcDataType.binaryPoint.get

  // ns
  val fastClkPeriod = 0.1
  val slowClkExtPeriod = fastClkPeriod * 20
  val inputDelay = 0.02
  // skew 500ps
  val rstMaxDly = 1.25
  val rstMinDly = 0.5
  
  val fastClkPeriodsBeforeMaxFFTPh0ForInternalValid = 2

}