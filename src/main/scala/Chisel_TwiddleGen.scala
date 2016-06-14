package FFT
import ChiselDSP._
import Chisel.{Pipe =>_,Complex => _,Mux => _, RegInit => _, RegNext => _, Counter => _, _}

class TwiddleGenO[T <: DSPQnm[T]](gen : => T) extends IOBundle {
  val twiddles = Vec(Params.getBF.rad.max-1,Complex(gen).asOutput)
}

class TwiddleGen[T <: DSPQnm[T]](gen : => T) extends GenDSPModule(gen) {

  // twiddleCounts, twiddleSubCounts, twiddleMuls
  val twiddleSetup = (new TwiddleSetupO).flip
  // currStage, we, isDIT, reset
  val calcCtrlFlag = (new CalcCtrlFlags).flip
  // stagePrimeIdx
  val ioSetup = (new IOSetupO).flip

  val o = new TwiddleGenO(gen)

  // TODO: Function-ize, reduce logic
  // Gets associated twiddle counter max, sub counter max, & mul for each stage
  val twiddleCountUsed = twiddleSetup.twiddleCounts.zipWithIndex.foldLeft(DSPUInt(0))((accum,e) => {
    (e._1 ? (calcCtrlFlag.currStage === DSPUInt(e._2))) | accum
  })
  val twiddleSubCountUsed = twiddleSetup.twiddleSubCounts.zipWithIndex.foldLeft(DSPUInt(0))((accum,e) => {
    (e._1 ? (calcCtrlFlag.currStage === DSPUInt(e._2))) | accum
  })
  val twiddleMulUsed = twiddleSetup.twiddleMuls.zipWithIndex.foldLeft(DSPUInt(0))((accum,e) => {
    (e._1 ? (calcCtrlFlag.currStage === DSPUInt(e._2))) | accum
  })


  class TwiddleCounter extends Counter(CountParams(
    countMax = twiddleSetup.twiddleCounts.map(x => x.getRange.max.intValue).max
  ))
  class TwiddleSubCounter extends Counter(CountParams(
    countMax = twiddleSetup.twiddleSubCounts.map(x => x.getRange.max.intValue).max
  ))

  // TODO: Make a vec of counters?
  val twiddleCounter = DSPModule (new TwiddleCounter, "twiddleCounter")
  val twiddleSubCounter = DSPModule (new TwiddleSubCounter, "twiddleSubCounter")
  twiddleCounter.io.max.get := twiddleCountUsed
  twiddleSubCounter.io.max.get := twiddleSubCountUsed
  twiddleCounter.iCtrl.reset := calcCtrlFlag.reset
  twiddleSubCounter.iCtrl.reset := calcCtrlFlag.reset
  twiddleSubCounter.iCtrl.change.get := calcCtrlFlag.we
  twiddleCounter.iCtrl.change.get := twiddleSubCounter.oCtrl.change.get

  // Complete twiddle address
  val twiddleAddr = (twiddleCounter.io.out * twiddleMulUsed).shorten(Params.getTw.addrMax).pipe(
    Params.getDelays.twiddleAddrGen
  )

  // Originally columns associated with twiddles up to radix-1, but want to address column first
  val twiddleList = Params.getTw.vals.map(_.transpose)
  val twiddleLUTs = twiddleList.zipWithIndex.map{ case (primeSet,primeSetIdx) => {
    val setMaxRadix = Params.getIO.global(primeSetIdx)._2
    primeSet.zipWithIndex.map{ case (radixSet,radixSetIdx) => {
      // For each radix, radix-1 twiddle factors fed into butterfly (positions 1 to radix-1)
      val setButterflyInput = radixSetIdx + 1
      DSPModule(new ComplexLUT(radixSet,gen),"twiddleLUT_radix" + setMaxRadix + "_idx" + setButterflyInput)
    }}
  }}

  // Get prime index (associated with List (1) ++ global) for current stage
  val currPrimeIdx = ioSetup.stagePrimeIdx.zipWithIndex.foldLeft(DSPUInt(0))((accum,e) => {
    (e._1 ? (calcCtrlFlag.currStage === DSPUInt(e._2))) | accum
  }).pipe(Params.getDelays.twiddleAddrGen)

  // Assign the right address to the twiddle LUTs (0 if current LUT not needed)
  // TODO: Don't need?
  val assignedTwiddleAddrs = Vec(twiddleLUTs.zipWithIndex.map{ case(lut,i) => {
    twiddleAddr ? (currPrimeIdx === DSPUInt(i+1))
  }})

  // Note: for DIT, twiddle multiplication comes first
  val assignedTwiddleAddrsDIT = assignedTwiddleAddrs
  // For DIF: twiddle multiplication occurs after WFTA -- delay twiddle addressing rather than twiddles themselves
  val assignedTwiddleAddrsDIF = Pipe(assignedTwiddleAddrsDIT,Params.getDelays.wfta)

  // TODO: Check, but last stage doesn't use any twiddles, so even if there's DIT/DIF delay issue, it shouldn't matter
  // noting that DIT/DIF is held through one whole frame

  // Match delay
  // TODO: Make vec?
  val calcDITD = calcCtrlFlag.isDIT.pipe(Params.getDelays.twiddleAddrGen)
  // Delay twiddle addressing to match the time that corresponding data addressing to (internal) memory becomes valid
  val currentTwiddleAddrs = Mux(calcDITD,assignedTwiddleAddrsDIT,assignedTwiddleAddrsDIF)

  // TODO: Check DIT/DIF timing mismatch @ end of stage, but ok b/c last stage doesn't use twiddles?

  // All twiddle LUTs associated with a particular coprime share the same address
  twiddleLUTs.zipWithIndex.foreach{ case(lutset,i) => {
    lutset.foreach{ lut => lut.io.addr := currentTwiddleAddrs(i).shorten(lut.io.addr.getRange)}
  }}

  // Get twiddle outs (all of the same vec lengths -- 0 pad to worst case # of twiddles needed)
  val maxNumTwiddles = Params.getBF.rad.max-1
  val complexZero = Complex(double2T(0.0),double2T(0.0))
  val one = Complex(double2T(1.0),double2T(0.0))
  val twiddleLUTouts = Vec(twiddleLUTs.map(lutset => {
    val lutsetLength = lutset.length
    Vec(
      lutset.map(lut => {
        val out = lut.io.dout.cloneType
        out := lut.io.dout
        out
      }) ++ List.fill(maxNumTwiddles - lutsetLength)(one)
    )
  }))
  // TODO: Why one?

  // TODO: Functions like ?, | for Vec, can this logic be reduced?
  // Selects which twiddles to output based off of which prime is being currently used
  val twiddlesTemp = twiddleLUTouts.zipWithIndex.foldLeft(Vec(maxNumTwiddles,complexZero))((accum,e) => {
    val tcond = currPrimeIdx === DSPUInt(e._2 + 1)
    val currRes = Vec(e._1.map(_ ? tcond))
    Vec(accum.zip(currRes).map{case (a,b) => a | b})
  })
  o.twiddles := Pipe(twiddlesTemp,Params.getDelays.memOutRegDly)

  val delay = o.getMaxOutDelay()
  Status ("Delay through Twiddle Generator module: " + delay)

}