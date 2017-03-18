package dspblocks.fft
import rocketchiselutil._
import chisel3._
import dsptools.numbers._
import dsptools.numbers.implicits._

class AnalogModel[T <: Data:RealBits](adcDataType: => T, ffastParams: FFASTParams) extends Module {

}