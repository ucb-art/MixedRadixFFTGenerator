package FFT
import ChiselDSP._
import Chisel.{Pipe =>_,Complex => _,Mux => _, RegInit => _, RegNext => _, _}

class SetupIO extends IOBundle {
  // Index of current FFT N
  val fftIdx = DSPUInt(INPUT,Params.getFFT.nCount - 1)
  // Enable setup
  val enable = DSPBool(INPUT)
  // Done with IO setup
  val done = DSPBool(OUTPUT)
}

class GeneralSetup extends DSPModule {

  val setup = new SetupIO

  // i.e. for N = 4^a1*2^a2*3^b*5^c (base order determined by radOrder), list contains [a1,a2,b,c]
  val radPowLUT = DSPModule(new IntLUT2D(Params.getCalc.radPow), "radPow")
  radPowLUT.io.addr := setup.fftIdx
  val radPow = radPowLUT.io.dout.cloneType
  radPow := RegNext(Mux(setup.enable,radPowLUT.io.dout,radPow))

  // Order of the calculation radix stages (given by index in {List(1) ++ ButterflyParams.rad} -- idx = 0 if some
  // overall radix is unused for particular FFT)
  val possibleRad = List(1) ++ Params.getBF.rad
  val radIdxOrderS = Params.getCalc.radOrder.map(x => x.map(y => possibleRad.indexOf(y) ))
  val radIdxOrderLUT = DSPModule(new IntLUT2D(radIdxOrderS), "radIdxOrder")
  radIdxOrderLUT.io.addr := setup.fftIdx
  val radIdxOrder = radIdxOrderLUT.io.dout.cloneType
  radIdxOrder := radIdxOrderLUT.io.dout
  // Now using actual radices
  val radOrderTemp = Vec(radIdxOrder.map( x => {
    possibleRad.zipWithIndex.foldLeft(DSPUInt(0))((accum,e) => accum | (DSPUInt(e._1) ? (x === DSPUInt(e._2))))
  }))
  val radOrder = radOrderTemp.cloneType
  radOrder := RegNext(Mux(setup.enable,radOrderTemp,radOrder))

  // Ex: sum(0) = power(0)
  // sum(1) = power(0)+power(1)
  // sum(2) = sum(1) + power(2)
  // Keeps track of # of stages required up until current radix
  // Where the last array value represents total # of stages required for FFT calc
  val stageSumX = radPow.tail.scanLeft(radPow.head)((accum,e) => (accum + e).pipe(1))
  val stageSum = Vec(stageSumX)

  // Radix associated with each stage (0 if unused for given FFT)
  val stageRad = Vec((0 until Params.getCalc.maxStages).map(i => {
    // TODO: Check what happens with reverse in HDL?
    stageSum.zip(radOrder).foldRight(DSPUInt(0))((e,accum) => Mux(DSPUInt(i) < e._1,e._2,accum)).pipe(1)
  }))

  // Is 2 used for this FFT?
  // TODO: Package all these foldleft, scanleft, etc's better
  val use2 = radIdxOrder.foldLeft(DSPBool(false))((accum,e) => accum | (e === DSPUInt(possibleRad.indexOf(2)))).pipe(1)

  // Max radix for given FFT (max of elements)
  val maxRad = radOrder.tail.foldLeft(radOrder.head)((accum,e) => Mux(e >= accum,e,accum)).pipe(1)

}

// check delay for done
// send out