// June 1, 2015
// Generic LUT to handle UInt arrays

package FFT 

import Chisel._
import ChiselDSP.{when => _, Reg => _,_}
import Node._
import scala.math._

class UInt2LUT (Array1D:Array[Int]) extends DSPModule {

	var Array1Dx: Array[Int] = Array.empty[Int]
	if (Array1D.isEmpty){
		Array1Dx = Array(0)
	}
	else{
		Array1Dx = Array1D
	}
	
	val LUTsize: Int = Array1Dx.length
	val addrBitWidth: Int = Helper.bitWidth(LUTsize-1)		// Ceil to get address bitwidth
	val maxVal: Int = Array1Dx.max
	val dataBitWidth: Int = Helper.bitWidth(maxVal)			// Ceil to get data bitwidth (from max of array elements)

	override val io = new  IOBundle {
		val addr1 = UInt(INPUT, width=addrBitWidth)
		val addr2 = UInt(INPUT, width=addrBitWidth)
		val dout1 = UInt(OUTPUT, width=dataBitWidth)
		val dout2 = UInt(OUTPUT, width=dataBitWidth)
	}

	// Map 1D array to LUT. If all values of the UInt array are 0, then don't need to make a LUT. 
	if (maxVal == 0){
		io.dout1 := UInt(0, width=1)
		io.dout2 := UInt(0, width=1)
	}
	else{
		val LUT = Vec((0 until LUTsize).map( x => UInt(Array1Dx(x),width=dataBitWidth)))
		io.dout1 := LUT(io.addr1)
		io.dout2 := LUT(io.addr2)
	}
	
}




