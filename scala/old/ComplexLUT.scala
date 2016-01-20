// June 15, 2015
// Generic LUT to handle Complex arrays

package ChiselDSP
import FFT._

import Chisel.{Complex => _, Mux => _, Reg => _, RegNext => _, RegInit => _, Pipe => _, Mem => _,
               Module => _, ModuleOverride => _, when => _, switch => _, is => _, unless => _, Round => _,  _}
//import ChiselDSP._
import Node._
import scala.math._

// Use correct double2T function (SInt or Flo) depending on the function that was passed in

class ComplexLUT[T <: DSPQnm[T]](Array1D:Array[ScalaComplex], gen : => T) extends GenDSPModule (gen) {

	var Array1Dx = Array.empty[ScalaComplex]
	// If the complex array is empty, give it a default value of 0
	if (Array1D.isEmpty){
		Array1Dx = Array(Complex(0.0,0.0))
	}
	else{
		Array1Dx = Array1D
	}
	
	val LUTsize: Int = Array1Dx.length
	val addrBitWidth: Int = Helper.bitWidth(LUTsize-1)		
	
	override val io = new  IOBundle {
		val addr = UInt(INPUT, width=addrBitWidth)
		val dout = Complex(gen).asOutput
	}

	// Convert Scala double data type to appropriate SInt or Flo as real,imag components of Complex
	//val LUT = Vec((0 until LUTsize).map( x =>  Complex(double2T( Array1Dx(x).real ),double2T( Array1Dx(x).imag )) ))
	val LUTr = Vec((0 until LUTsize).map( x =>  {
		val r = double2T( Array1Dx(x).real, Complex.getFixedParams )
		//println(r.getWidth)
		r
	} ))
	val LUTi = Vec((0 until LUTsize).map( x =>  double2T( Array1Dx(x).imag ) ))

	val temp = (0 until LUTsize).map(x => List(Array1Dx(x).real,Array1Dx(x).imag)).transpose
	// only if fixed
	// TODO: not fixed

	val real = LUTr(io.addr)
	val imag = LUTi(io.addr)

	io.dout := Complex(real,imag)

	gen match {
		case f: DSPFixed => {
			val realrange = (DSPFixed.toFixed(temp(0).min,f.getFracWidth),DSPFixed.toFixed(temp(0).max,f.getFracWidth))
			val imagrange = (DSPFixed.toFixed(temp(1).min,f.getFracWidth),DSPFixed.toFixed(temp(1).max,f.getFracWidth))
			// only of inone? TODO

			io.dout.updateLimits(realrange,imagrange)
		}
			case _ =>
	}






}









