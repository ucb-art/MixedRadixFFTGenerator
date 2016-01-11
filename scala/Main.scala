// TODO: Delay variations

package FFT
import ChiselDSP._
import Chisel._

object Main {

  def main(args: Array[String]): Unit = {

    // Suppress warnings
    Warn.suppress = true

    // Generator parameters + fixed/double mode setup
    val (isFixed,p) = Init({GeneratorParams()}, jsonName = "FFT", args = args)

    // All possible subsets of the FFTs the user specified are also generated
    // TODO: Enable all subsets
    //val sizeComb = List(p.fft.sizes)
    val sizeComb = p.fft.sizes.toSet.subsets.map(_.toList).toList.filter(_ != List()).reverse.filter(_ != List(4)).filter(_ != List(3)).filter(_ != List(7)).filter(_ != List(5))

    sizeComb.zipWithIndex.foreach { case (e,i) => {
      val (par,nameExt) = {
        if (e.sorted.sameElements(p.fft.sizes.sorted)) (p , "_out")
        else {
          val newP = p.copy(fft = p.fft.copy(sizes = e))
          Init(newP, args)
          (newP, "")
        }
      }
      // Setup block parameters
      Params(par)

      // Setup module + tester
      val runArgs = args.slice(1, args.length)
      val name = i.toString + nameExt
      if (isFixed)
        Chisel.chiselMainTest(runArgs, () => DSPModule(new FFT({DSPFixed()}), name)) { c => new FFTTests(c) }
      else
        Chisel.chiselMainTest(runArgs, () => DSPModule(new FFT({DSPDbl()}), name)) { c => new FFTTests(c) }

    }}

  }

}
