package FFT
import ChiselDSP._
import Chisel.{Pipe =>_,Complex => _,Mux => _, RegInit => _, RegNext => _, _}

// TODO: Add FFT?

// TODO: Get rid of IO that's not used

class SetupTopIO extends IOBundle {
  // Index of current FFT N
  val fftIdx = DSPUInt(INPUT,Params.getFFT.nCount - 1)
  // Enable setup
  val enable = DSPBool(INPUT)
  // Done with IO setup
  val done = DSPBool(OUTPUT)
}

/** Setup outputs */
class GeneralSetupO extends IOBundle {
  val numRad = Params.getBF.rad.length
  val globalMaxRad = Params.getBF.rad.max
  val maxStages = Params.getCalc.maxStages

  // For each unique radix needed, sum of stages needed up to current radix
  val radStageSum = Vec(numRad,DSPUInt(OUTPUT,maxStages))
  // For each prime needed, sum of stages needed up to current prime (Note: length is still the same as radStageSum)
  val primeStageSum = Vec(maxStages,DSPUInt(OUTPUT,maxStages))
  // DIT IO requires knowing the stage sum associated with previous prime
  val prevPrimeStageSum = Vec(maxStages,DSPUInt(OUTPUT,maxStages))
  // Radix needed for each stage
  val stageRad = Vec(maxStages,DSPUInt(OUTPUT,globalMaxRad))
  // Is radix 2 used in current FFT?
  val use2 = DSPBool(OUTPUT)
  // Max radix needed in current FFT
  val maxRad = DSPUInt(OUTPUT,globalMaxRad)
}

class GeneralSetup extends DSPModule {

  val setupTop = new SetupTopIO
  // Per-FFT constants output
  val o = new GeneralSetupO

  // i.e. for N = 4^a1*2^a2*3^b*5^c (base order determined by radOrder), list contains [a1,a2,b,c]
  val radPowLUT = DSPModule(new IntLUT2D(Params.getCalc.radPow), "radPow")
  radPowLUT.io.addr := setupTop.fftIdx
  val radPow = radPowLUT.io.dout.cloneType
  radPow := RegNext(Mux(setupTop.enable,radPowLUT.io.dout,radPow))

  // Order of the calculation radix stages (given by index in {List(1) ++ ButterflyParams.rad} -- idx = 0 if some
  // overall radix is unused for particular FFT)
  val possibleRad = Params.getBF.possibleRad
  val radIdxOrderS = Params.getCalc.radOrder.map(x => x.map(y => possibleRad.indexOf(y) ))
  val radIdxOrderLUT = DSPModule(new IntLUT2D(radIdxOrderS), "radIdxOrder")
  radIdxOrderLUT.io.addr := setupTop.fftIdx
  val radIdxOrder = radIdxOrderLUT.io.dout.cloneType
  radIdxOrder := RegNext(Mux(setupTop.enable,radIdxOrderLUT.io.dout,radIdxOrder))
  // Now using actual radices
  val radOrder = Vec(radIdxOrder.map( x => {
    possibleRad.zipWithIndex.foldLeft(DSPUInt(0))((accum,e) => accum | (DSPUInt(e._1) ? (x === DSPUInt(e._2))))
  }))

  // Ex: sum(0) = power(0)
  // sum(1) = power(0)+power(1)
  // sum(2) = sum(1) + power(2)
  // Keeps track of # of stages required up until current radix
  // Where the last array value represents total # of stages required for FFT calc
  val radStageSumX = Vec(radPow.tail.scanLeft(radPow.head)((accum,e) => (accum + e).pipe(1)))
  o.radStageSum.zip(radStageSumX).foreach{ case (l,r) => {
    l := r.shorten(l.getRange)
  }}

  // Radix associated with each stage (0 if unused for given FFT)
  o.stageRad := Vec((0 until Params.getCalc.maxStages).map(i => {
    // TODO: Check what happens with reverse in HDL?
    o.radStageSum.zip(radOrder).foldRight(DSPUInt(0))((e,accum) => Mux(DSPUInt(i) < e._1,e._2,accum)).pipe(1)
  }))

  // Is 2 used for this FFT?
  // TODO: Package all these foldleft, scanleft, etc's better
  val idx2 = possibleRad.indexOf(2)
  o.use2 := {
    // If 2 not required by any generated FFT sizes
    if (idx2 == -1) DSPBool(false)
    else radIdxOrder.foldLeft(DSPBool(false))((accum,e) => accum | (e === DSPUInt(idx2))).pipe(1)
  }

  // TODO: Generalize?
  // Is 4 used for this FFT?
  val idx4 = possibleRad.indexOf(4)
  val use4 = {
    // If 4 not required by any generated FFT sizes
    if (idx4 == -1) DSPBool(false)
    else radIdxOrder.foldLeft(DSPBool(false))((accum,e) => accum | (e === DSPUInt(idx4)))
  }

  // Max radix for given FFT (max of elements)
  o.maxRad := radOrder.tail.foldLeft(radOrder.head)((accum,e) => Mux(e >= accum,e,accum)).pipe(1)

  // Stage sum associated with previous radix (0 for first)
  val prevRadStageSum = Vec(List(DSPUInt(0)) ++ o.radStageSum.init)

  // Note: this still keeps the same # of outputs as radStageSum, but removes count contribution
  // of radices associated with prime that are != prime (replaces with sum @ corresponding prime)
  val primeStageSumShort = {
    if (possibleRad.contains(2) && possibleRad.contains(4)) {
      Vec(o.radStageSum.zipWithIndex.map { case (e, i) => {
        // Note: 2 will always follow 4 if used
        // TODO: Generalize?
        if (e == o.radStageSum.last) e
        else {
          val is4Loc = radIdxOrder(i) === DSPUInt(possibleRad.indexOf(4))
          val is4LocUse2 = is4Loc & o.use2
          Mux(is4LocUse2, o.radStageSum(i + 1), e)
        }
      }})
    }
    else o.radStageSum
  }

  // Previous prime sum requires updating location for 2 with sum in location for 4
  val prevPrimeStageSumShort = {
    if (possibleRad.contains(2) && possibleRad.contains(4)) {
      Vec(prevRadStageSum.zipWithIndex.map { case (e, i) => {
        // Note: 2 will always follow 4 if used
        // TODO: Generalize?
        if (e == prevRadStageSum.head) e
        else {
          val is2Loc = radIdxOrder(i) === DSPUInt(possibleRad.indexOf(2))
          val is2LocUse4 = is2Loc & use4
          Mux(is2LocUse4, prevRadStageSum(i - 1), e)
        }
      }})
    }
    else prevRadStageSum
  }

  // Map prime stage sum to stages for coprime to its sub-radix decomposition
  val (primeStageSumX,prevPrimeStageSumX) = (0 until Params.getCalc.maxStages).map(i => {
    val cond = o.radStageSum.init.map(x => DSPUInt(i) < x)
    val o1 = {
      // Last stage will always have max stage sum (also default when stage not used)
      if(i == Params.getCalc.maxStages-1) primeStageSumShort.last
      else{
        (0 until o.radStageSum.length-1).foldRight(primeStageSumShort.last)(
          (e,accum) => Mux(cond(e),primeStageSumShort(e),accum)
        )
      }
    }
    val o2 = (0 until o.radStageSum.length-1).foldRight(prevPrimeStageSumShort.last)(
      (e,accum) => Mux(cond(e),prevPrimeStageSumShort(e),accum)
    )
    (o1.pipe(1),o2.pipe(1))
  }).unzip
  o.primeStageSum := Vec(primeStageSumX)
  o.prevPrimeStageSum := Vec(prevPrimeStageSumX)

  // Keep track of how long setup should take (+1 for RegNext on LUT out -- should be consistent throughout)
  val setupDelay = o.getMaxOutDelay() + 1
  setupTop.done := setupTop.enable.pipe(setupDelay)

  Status("General setup takes " + setupDelay + " clocks.")

}