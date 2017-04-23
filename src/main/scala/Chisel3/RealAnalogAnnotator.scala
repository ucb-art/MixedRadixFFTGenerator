package dspblocks.fft
// TODO: Stolen from CRAFT ; Relocate!

import chisel3._

import chisel3.experimental._
import barstools.tapeout.transforms._

trait RealAnalogAnnotator extends AnalogAnnotator { self: chisel3.Module =>
  def io: Bundle 

  def annotateReal(): Unit = {
    io.elements.foreach {
      case (_, d) => {
        d match {
          case a: Analog =>
            renameAnalog(a, "  input `ifndef SYNTHESIS real `endif  ")
          case _ =>
        }
      }
    }
  }

}