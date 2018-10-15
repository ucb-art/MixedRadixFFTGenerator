package FFT
import ChiselDSP._
import Chisel.{Complex => _, _}
import scala.collection.mutable.ListBuffer
import net.jcazevedo.moultingyaml._
import net.jcazevedo.moultingyaml.DefaultYamlProtocol._ 

object DebugFFT {

  def main(args: Array[String]): Unit = {
    val fft = new ChiselFFT

    val fftns = List(
      12, 24, 48, 96, 192, 384, 768, 36, 72, 144, 288, 576, 1152, 108, 216, 432, 864, 324, 648, 1296, 972,
      60, 120, 240, 480, 960, 180, 360, 720, 300, 600, 1200, 540, 1080, 900).sorted ++ List(64, 128, 256, 512, 1024, 2048, 1536)

    val saveData = fftns.map { case n =>
      val inVec = (0 until n).map(x => Complex(x.toDouble, 0.0)).toList
      // Reset
      Tracker.observedSection = false
      Tracker.prevStage = 0
      Tracker.data = new ListBuffer[(BigInt, Seq[BigInt], BigInt)]()                                      // Stages, n's, twiddle addresses
      fft.run(args, fftn = Some(n), inVec = Some(inVec), normal = Some(false), genOffset = Some(true), debugMode = true)

      val data: Seq[(Int, Seq[Int], Int)] = Tracker.data.toSeq.map { case (a, b, c) => 
        val newA = a.toInt
        val newB = b.toList.map { case y => y.toInt }
        val newC = c.toInt
        (newA, newB, newC)
      }

      val (stages, ns, twiddleAddrsTemp) = data.unzip3
      
      // TW delay is 4 from address
      val twiddleAddrs = twiddleAddrsTemp.drop(4) ++ Seq.fill(4)(0)

      val maxStage = stages.max

      val evals = (0 to maxStage).map { case stage =>
        val firstIndexOfStage = stages.indexOf(stage)
        val lastIndexOfStage = stages.lastIndexOf(stage)
        val associatedNs = ns.slice(firstIndexOfStage, lastIndexOfStage + 1) 
        val associatedTwiddleAddrs = twiddleAddrs.slice(firstIndexOfStage, lastIndexOfStage + 1)
        val tempOut = associatedNs.zip(associatedTwiddleAddrs)
        if (stage == 0)
          // Pipeline delay for 0th stage is 14?
          stage -> tempOut //.drop(14)
        else if (stage == maxStage)
          stage -> tempOut //.drop(7).dropRight(14)
        else
          // Pipeline delay is 7
          stage -> tempOut //.drop(7)
      }.toMap
      n -> evals
    }.toMap

    scala.tools.nsc.io.File("twiddle_addresses.yaml").writeAll(saveData.toYaml.prettyPrint)
  }

}