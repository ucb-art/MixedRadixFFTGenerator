package ChiselDSP
import Chisel._

// TODO: Johnson counter after testing multi-clock domains maxDivBy & (maxDivBy-1) for power of 2 test

/** Clk divider IO */
class ClkDivIO (div: Int) extends IOBundle {
  if (div < 1) Error ("Clk divide by value should be >= 1")
  // val in = Chisel.Clock()
  // val out = in / div
  val slowEn = DSPBool(OUTPUT)
}

/** Clk divider using counters, if divider value is not a multiple of 2, the duty cycle is not 50% */
class ClkDiv(div: Int) extends DSPModule {

  // Uneven duty cycle if div is not even
  val half = div / 2 + div % 2

  override val io = new ClkDivIO(div)

  // Full period of output clock dictated by when the counter wraps
  // If you want something to happen on the rising edge of the output clock, but
  // want to rely on an enable signal, the enable must be high just before
  // the corresponding fast clock rising edge
  if (div == 1) {
    io.slowEn := DSPBool(true)
    // io.out := io.in
  }
  else {
    // val clkOut = RegInit(DSPBool(false),clock=io.in)
    val clkOut = RegInit(DSPBool(false))
    if (div == 2) {
      clkOut := !clkOut
      io.slowEn := clkOut
    }
    else {
      // val count = RegInit(DSPUInt(0,div-1),clock=io.in)
      val count = RegInit(DSPUInt(0, div - 1))
      // TODO: Check phase makes sense?
      // Clock related signals should not have any logic (should come directly from registers)
      val slowEn = (count === DSPUInt(div - 1))
      //io.slowEn := slowEn.reg(init=DSPBool(false),clock = io.in)
      io.slowEn := slowEn.reg(init=DSPBool(false))
      count := Mux(slowEn, DSPUInt(0), (count + DSPUInt(1)).shorten(div - 1))
      // clkOut := Mux((count === DSPUInt(half - 1)) | slowEn, !clkOut, clkOut).reg(init=DSPBool(false), clock = io.in)
      clkOut := Mux((count === DSPUInt(half - 1)) | slowEn, !clkOut, clkOut).reg(init=DSPBool(false))
    }
    // io.out := clkOut
  }

}

// TODO: clock out won't work until Chisel is fixed

