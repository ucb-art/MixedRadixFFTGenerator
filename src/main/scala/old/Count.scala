// September 6, 2015
// pipeD in DSP

package DSP
import scala.math._
import Chisel._

object Count{

	// Custom value -> bitwidth needed to support value
	def bw (max: Int) : Int = {
		val temp = log(max)/log(2)
		val up = ceil(temp)
		val bw = if (temp < up) up.toInt else temp.toInt + 1
		if (bw < 0) 1 else bw
	}
	
	def apply(x:Int) : UInt = { if (x != 0) UInt(x,width=bw(x)) else UInt(0,width=1)}
	def apply(x:Int,max:Int) : UInt = UInt(x,width=bw(max))
	def apply (dir: IODirection = null, max:Int) : UInt = UInt(dir,width=bw(max)) 
	
	// Select x or all 0's
	def selU(x:UInt,select:Bool) : UInt = {
	  val numBits = x.getWidth()
	  val selectLong = Fill(numBits,select)
    x & selectLong
	}
	
	// Mux B if sel true; otherwise A
	def muxU(x:UInt,y:UInt,sel:Bool) : UInt = {
	  val (a,b) = matchWidthU(x,y) 
	  selU(a,~sel) | selU(b,sel)
	}
	
	// x mod n
	def mod(x:UInt,n:UInt,nmax:Int) : UInt = {
	  // Note that x <= 2n-1 to work
	  val xwidth = x.getWidth()
	  val nmax_width = bw(2*nmax-1)
	  val pad = nmax_width-xwidth
	  val xnew = Cat(Fill(pad,Bits(0,width=1)),x)
	  val sub = xnew-n
	  val sel = sub(nmax_width-1) === Bits(0,width=1)   // x >= n (result positive) --> select subtraction result, otherwise select x  
    muxU(x,sub(bw(nmax)-1,0),sel)
	}
	
  // match bitwidths
	def matchWidthU(x: UInt, y: UInt) : Tuple2[UInt,UInt] = {
    val diff = y.getWidth() - x.getWidth()
    val zro = Bits(0,width=1)
    if (diff > 0) (Cat(Fill(diff,zro),x.toBits), y) else if (diff < 0) (x, Cat(Fill(-diff,zro),y.toBits)) else (x, y)
  }   
  
	// Add (bitwidth + 1)
	def add1(a:UInt,b:UInt) : UInt = {
	  val zro = Bits(0,width=1)
    val (x,y) = matchWidthU(a,b) 
    val j = Cat(zro,x)
    val k = Cat(zro,y)
    j+k 
	}
	
}


