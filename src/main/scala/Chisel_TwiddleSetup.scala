// TODO: Useful macro function for printing out 2D list elements

package FFT
import ChiselDSP._
import Chisel.{Pipe =>_,Complex => _,Mux => _, RegInit => _, RegNext => _, _}

class TwiddleSetupO extends IOBundle {
  // Main twiddle count max for each calculation stage (CTA decomposition -- within a coprime)
  val twiddleCounts = IntLUT2DHelper.getOutputType(Params.getTw.countMax)
  // Counts to hold twiddle (for PFA, based off of subsequent coprimes)
  val subCountMax = IntLUT2DHelper.getOutputType(Params.getTw.subcountMax).map(x => x.getRange.max).max
  val twiddleSubCounts = Vec(Params.getCalc.maxStages,DSPUInt(OUTPUT,subCountMax))
  // Twiddle address renormalization factor at each stage
  val twiddleMuls = Vec(Params.getCalc.maxStages,DSPUInt(OUTPUT,Params.getTw.vals.map(x => x.length).max))
}

class TwiddleSetup extends DSPModule{

  val setupTop = new SetupTopIO
  val generalSetup = (new GeneralSetupO).flip
  val ioSetup = (new IOSetupO).flip
  val o = new TwiddleSetupO

  // TODO: Get rid of cloneType

  // Main twiddle count max for each calculation stage (CTA decomposition -- within a coprime)
  val twiddleCountsLUT = DSPModule(new IntLUT2D(Params.getTw.countMax), "topTwiddleCounts")
  twiddleCountsLUT.io.addr := setupTop.fftIdx
  o.twiddleCounts := RegNext(Mux(setupTop.enable,twiddleCountsLUT.io.dout,o.twiddleCounts))

  // Counts to hold twiddle (for PFA, based off of subsequent coprimes)
  // Note: last "subCount" is always 0 since there aren't any subsequent coprimes
  val twiddleSubCountsLUT = DSPModule(new IntLUT2D(Params.getTw.subcountMax),"subTwiddleCounts")
  twiddleSubCountsLUT.io.addr := setupTop.fftIdx
  val twiddleSubCountsInit = twiddleSubCountsLUT.io.dout.cloneType
  twiddleSubCountsInit := RegNext(Mux(setupTop.enable,twiddleSubCountsLUT.io.dout,twiddleSubCountsInit))
  val twiddleSubCountsShort = Vec(twiddleSubCountsInit :+ DSPUInt(0))

  // Base multiply amount to scale range of twiddle counts to full twiddle LUT size (associated w/ coprime)
  val baseTwiddleRenormLUT = DSPModule(new IntLUT2D(Params.getTw.LUTScale),"baseTwiddleRenorms")
  baseTwiddleRenormLUT.io.addr := setupTop.fftIdx
  val baseTwiddleRenorms = baseTwiddleRenormLUT.io.dout.cloneType
  baseTwiddleRenorms := RegNext(Mux(setupTop.enable,baseTwiddleRenormLUT.io.dout,baseTwiddleRenorms))

  // TODO: Figure out less silly way?

  // For a given stage i, the prime index of the previous stage (i-1)
  val prevStagePrimeIdx = Vec(List(DSPUInt(0)) ++ ioSetup.stagePrimeIdx.init)
  val stagePrimeIdxPrevStagePrimeIdx = ioSetup.stagePrimeIdx.zip(prevStagePrimeIdx)

  // Maps stage to location (in Vec) of baseTwiddleRenorms + twiddleSubCountsShort
  // Note: agnostic to prime ordering
  val primeLoc = Vec(stagePrimeIdxPrevStagePrimeIdx.tail.scanLeft(prevStagePrimeIdx.head)((accum,e) => {
    // "reset" when prime changes
    Mux(e._1 === e._2,accum, accum + DSPUInt(1))
  }))
  // TODO: Address Vec w/ DSPUInt
  // Depending on prime of each stage, associate it with a base renormalization factor
  val stageBaseTwiddleRenorms = Vec(primeLoc.map(loc => {
    baseTwiddleRenorms.zipWithIndex.foldLeft(DSPUInt(0))( (accum,e) => {
      e._1 ? (DSPUInt(e._2) === loc) | accum
    }).pipe(1)
  }))

  // Map sub counts to stages
  o.twiddleSubCounts := Vec(primeLoc.map(loc => {
    twiddleSubCountsShort.zipWithIndex.foldLeft(DSPUInt(0))( (accum,e) => {
      e._1 ? (DSPUInt(e._2) === loc) | accum
    }).pipe(1)
  }))

  // For a given stage i, the radix of the previous stage (i-1)
  val prevStageRad = Vec(List(DSPUInt(1)) ++ generalSetup.stageRad.init)
  val stageRadPrevStageRad = generalSetup.stageRad.zip(prevStageRad)

  // Longest twiddle memory length needed
  val maxTwiddleMemLen = Params.getTw.vals.map(x => x.length).max

  // For DIF: base renormalization due to scaling max coprime to corresponding coprime required for FFTN
  // Subsequent stages associated with the same coprime are renormalized by that base *R1 for stage 2,
  // *R1*R2 for stage 3, etc. (product of previous stages with the same coprime)
  val twiddleRenorms = Vec(stageRadPrevStageRad.tail.scanLeft(prevStageRad.head)((accum,e) => {
    val accumRenorm = (accum * e._2).shorten(maxTwiddleMemLen).pipe(1)
    // "reset" when radix changes
    // TODO: Currently, 4-> 2 is ok, b/c radix 2 stage is always last for coprime 2^N and
    // radix 2 stage doesn't need twiddles, but not valid if generalized (?)
    Mux(e._1 === e._2,accumRenorm, DSPUInt(1))
  }))
  // TODO: Check should I use shorten to maxTwiddleMemLen or maxTwiddleMemLen-1?
  val twiddleMuls = Vec(stageBaseTwiddleRenorms.zip(twiddleRenorms).map{case (base,factor) => {
    (base * factor).shorten(maxTwiddleMemLen).pipe(1)
  }})

  // TODO: Get rid of? Doesn't buy you anything (also, not generalized)
  // Radix 2 always doesn't have any non-trivial twiddles (i.e. 1)
  o.twiddleMuls := {
    if (Params.getBF.rad.contains(2))
      Vec(twiddleMuls.zip(generalSetup.stageRad).map(x => {
        Mux(x._2 === DSPUInt(2),DSPUInt(0),x._1).pipe(1)
      }))
    else twiddleMuls
  }

  // Keep track of how long setup should take (this delay is added on top)
  val setupDelay = o.getMaxOutDelay()
  Status("Twiddle setup module delay: " + setupDelay)

  // TODO: Less conservative delay

}