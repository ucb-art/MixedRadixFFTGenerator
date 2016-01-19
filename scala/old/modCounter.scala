// June 9, 2015
// Fix bitwidth

package FFT 

import Chisel._
import ChiselDSP.{when => _, Reg => _,_}
import Node._
import scala.math._

class modCounter (maxVal: Int) extends DSPModule {

	override val io = new  Bundle {
		val inc = UInt(INPUT)
		val modVal = UInt(INPUT)
		val changeCond = Bool(INPUT)
		val globalReset = Bool(INPUT)
		val wrapCond = Bool(INPUT)
		val out = UInt(OUTPUT,width=Helper.bitWidth(maxVal))
	}
	
	val count = Reg(init=UInt(0,width=Helper.bitWidth(maxVal)))

	val modBitWidth = Helper.bitWidth(2*(maxVal+1)-1)
	val x = Cat(Bits(0,width=1),count) + io.inc
	//val x = UInt(count + io.inc,width=modBitWidth)			// count has max width associated with max of current coprimes-1
																// which should be larger than or = to Q, and add operation takes max of
																// the two widths, but sum is <2*coprime-1
	val xmodn = Module(new mod(modBitWidth))
	xmodn.io.x := x
	xmodn.io.n := io.modVal

	when (io.globalReset === Bool(true)){					// If global reset, reset counter
		count := UInt(0)	
	}.otherwise{
		when (io.changeCond === Bool(true)){				// Otherwise, only update counter when changeCond met
			when (io.wrapCond === Bool(true)){				// If wrapCond met (i.e. counter maxed), reset counter
				count := UInt(0)
			}.otherwise{									// Otherwise, increment by (out + constant) mod N
				count := xmodn.io.out
			}
		}
	}
	io.out := count
}




