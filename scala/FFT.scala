package FFT

// ------- Imports START -- DO NOT MODIFY BELOW
import Chisel.{Complex => _, Mux => _, Reg => _, RegNext => _, RegInit => _, Pipe => _, Mem => _,
               Module => _, ModuleOverride => _, when => _, switch => _, is => _, unless => _, Round => _,  _}
import ChiselDSP._
// ------- Imports END -- OK TO MODIFY BELOW

/** Module that supports both fixed and floating point testing */
class FFT[T <: DSPQnm[T]](gen : => T) extends GenDSPModule (gen) {

  val wfta = DSPModule(new WFTA(gen,0))
  override val io = new WFTAIO(gen)
  //wfta.io <> io
  //io.x <> wfta.io.x
  //wfta.io.x <> io.x

  /*io.x.zipWithIndex.foreach{
    case (e,i) => {println(i); e <> wfta.io.x(i)}
  }*/
  println("rawr")
  println(Params.getFFT.sizes)
  println(wfta.io.x.length)
  println(wfta.io.x(0).real.dir + "," + io.x(0).real.dir)
  /*for (t <- 0 until io.x.length) {
    println(t)
    wfta.io.x(t) <> io.x(t)
  }*/

  wfta.io.x := io.x
  println("end")
  wfta.io.y <> io.y
  wfta.io.currRad <> io.currRad

  val test = DSPFixed(-1,30)*DSPFixed(-.23,30)//DSPFixed(-3.3,30)*DSPFixed(-1,30)
  debug(test)

}
