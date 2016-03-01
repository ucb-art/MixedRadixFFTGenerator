package ChiselDSP
import Chisel._


/** Clk divider IO */
/*class ClkDiv2NIO (maxDivBy: Int, clkMux: Boolean = false) extends IOBundle {
  if (maxDivBy < 1) Error ("Clk divide by value should be >= 1")
  if ((maxDivBy & (maxDivBy-1)) != 0) Error ("ClkDiv2N module can only divide input clock by powers of 2.")

  val clkIn =
}

}

/** Clock divider (outputs are clk/2^N) */
abstract class BaseNCounter (p: BaseNCountParams) extends DSPModule (inputDelay = p.inDelay) {

  override val io = new BaseNCountIO(p)
  va

  */