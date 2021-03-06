package FFT

// ------- Imports START -- DO NOT MODIFY BELOW
import Chisel.{Complex => _, Mux => _, Reg => _, RegNext => _, RegInit => _, Pipe => _, Mem => _,
Module => _, ModuleOverride => _, when => _, switch => _, is => _, unless => _, Round => _,  _}
import ChiselDSP._
// ------- Imports END -- OK TO MODIFY BELOW

// TODO: Radix 2 extension should use rad 4 hardware

/** Processing element IO: twiddle factors, calcDIT -> true if in DIT calculation phase or false if in DIF phase,
  * (WFTA) x = input, y = output, currRad = current butterfly radix
  */
class PEIO[T <: DSPQnm[T]](gen : => T) extends WFTAIO(gen,outDlyMatch=false) {

  // TODO: extend IO???

  // TODO: Check unused x, twiddles are 0ed externally, case out when FFTN <= 7 (no twiddles)
  // TODO: Pass input delays?
  // Double check for rad = 2, twiddles are all 1

  // Twiddle factors (one less than target radix)
  val twiddles = Vec(maxRad-1,Complex(gen).asInput)
  // Flag for indicating calculation phase (either DIT [true] or DIF [false])
  val calcDIT = DSPBool(INPUT)
}

/** Processing element includes both DIT/DIF twiddle multiplication and WFTA butterfly
  * num = butterfly index
  */
class PE[T <: DSPQnm[T]](gen : => T, num: Int = 0) extends GenDSPModule (gen) {

  // Pipeline delay for complex multiplication
  val twiddleDelay = Params.getDelays.twiddle

  CheckDelay.on()

  // Processing element IO
  override val io = new PEIO(gen)

  // WFTA butterfly (note that input delay is always initialized to 0 unless otherwise specified)
  val wfta = DSPModule(new WFTA(gen,num), nameExt = num.toString)

  // Turn off pipeline delay check because of feedback
  CheckDelay.off()

  // Twiddle multiplication occurs after WFTA butterfly for DIF and before butterfly for DIT
  if (Params.getBF.rad.length > 1) {
    val currRadDIT = Pipe(io.currRad.get, twiddleDelay)
    wfta.io.currRad.get.zipWithIndex.foreach { case (e, i) => e := Mux(io.calcDIT, currRadDIT(i), (io.currRad.get)(i)) }
  }
  // WFTA Input Mux - WFTA - Twiddle Input Mux - Twiddle - Output Mux

  val twiddleMulIn = Vec((io.x, wfta.io.y).zipped.map(Mux(io.calcDIT,_,_)))

  // TODO: There can be -1 in twiddles -->should include -1 * -1 overflow

  // If DIF, all inputs to the twiddle multiplication must by delayed by the WFTA output delay
  // amount. (Twiddles delayed in the twiddle block by delaying addressing!)
  val twiddleMulOut = Vec(twiddleMulIn.zipWithIndex.map{case (e,i) => {
    val temp = {
      if (i == 0) e.pipe(twiddleDelay)
      else e * (io.twiddles(i-1),
        aPipe = math.floor(Params.getDelays.addPipe).toInt,
        mPipe = Params.getDelays.mulPipe,
        use4 = Params.getComplex.use4Muls)
    }
    temp.trim(gen.getFracWidth)
  }})

  // TODO: Use shorten instead of shortenTo?
  wfta.io.x.zipWithIndex.foreach{case (e,i) => {
    e :=  Mux(io.calcDIT,twiddleMulOut(i),io.x(i)).shortenTo(gen.getIntWidth)
  }}
  io.y.zipWithIndex.foreach{case (e,i) => e := Mux(io.calcDIT,wfta.io.y(i),twiddleMulOut(i))}

  CheckDelay.on()

  // Total delay through the whole processing element
  val delay = twiddleDelay + wfta.delay
  Status("Total PE delay (WFTA + Twiddles): " + delay)

}