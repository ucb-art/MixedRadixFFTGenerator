// June 7, 2015
// Expect that if changeCond always true, it should get optimized out

package FFT 
import Chisel._
import DSP._
import Count._
import ChiselDSP.{when => _, Reg => _,_}

class accumulator (bitWidth: Int, resetVal:Int = 0) extends DSPModule {
    
  override val io = new  Bundle {
    val inc = UInt(INPUT)
    val changeCond = Bool(INPUT)
    val globalReset = Bool(INPUT)
    val wrapCond = Bool(INPUT)
    val out = UInt(OUTPUT,width=bitWidth)
  }
  
  val count = Reg(init=UInt(0,width=bitWidth))
  when (io.globalReset){                // If global reset, reset counter
    count:= Count(resetVal)
  }.otherwise{
    when (io.changeCond){               // Otherwise, only update counter when changeCond met
      when (io.wrapCond){               // If wrapCond met (i.e. counter maxed), reset counter
        count := UInt(0)
      }.otherwise{                      // Otherwise, increment by desired amount
        count := count + io.inc
      }
    }
  }
  io.out := count
}




