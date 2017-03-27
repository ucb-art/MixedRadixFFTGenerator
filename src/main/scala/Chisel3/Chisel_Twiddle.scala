package dspblocks.fft
import chisel3._
import chisel3.experimental._
import dsptools.numbers._
import dsptools.numbers.implicits._

class TwiddleGenIO[T <: Data:RealBits](dspDataType: => T) extends Bundle {

}



/*
class TwiddleGen[T <: Data:RealBits](dspDataType: => T) extends Module with DelayTracking {

}*/


    io.currentStageToTwiddle.zipWithIndex.map { case (port, idx) => port := isStageBools(idx) }
    io.twiddleCountEnable := reNoDelay