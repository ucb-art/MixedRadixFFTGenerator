// June 9, 2015
// Computes x mod n for x < 2n
// Double check bitwidths

package FFT

import Chisel._
import Node._
import scala.math._

class mod (bitWidth: Int) extends Module {
val io = new Bundle {
  val x = UInt(INPUT, width = bitWidth)
  val n = UInt(INPUT, width = bitWidth)
  val out = UInt(OUTPUT, width = bitWidth)
}

  val sub = io.x - io.n

  when (sub(bitWidth-1) === UInt(0)) {	 	// x bigger than or equal to n
       io.out := sub						// mod output = difference ie 9 mod 5 = 4; 5 mod 5 = 0
  } .otherwise { 							// x smaller than n
       io.out := io.x 						// mod output = x
  }

}
