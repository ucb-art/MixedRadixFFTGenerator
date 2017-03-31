package dspblocks.fft
import chisel3._
import chisel3.experimental._

class ClkMuxBlackBox(useBlackBox: Boolean) extends BlackBox {
  val io = IO(new Bundle {
    val sel = Input(Bool())
    val clk0 = Input(Clock())
    val clk1 = Input(Clock())
    val clkOut = Output(Clock())
  })

  if (useBlackBox) 
    setInline("ClkMuxBlackBox.v",
      s"""
      |module ClkMuxBlackBox (
      |  input      sel,
      |  input      clk0,
      |  input      clk1,
      |  output     clkOut);
      |
      |  CKMUX2D8BWP16P90LVT clkMux (
      |    .S(sel),
      |    .I0(clk0),
      |    .I1(clk1),
      |    .Z(clkOut)
      |  );
      |
      |endmodule""".stripMargin
    )
  else 
    setInline("ClkMuxBlackBox.v",
      s"""
      |module ClkMuxBlackBox (
      |  input      sel,
      |  input      clk0,
      |  input      clk1,
      |  output     clkOut);
      |
      |  assign clkOut = sel ? clk1 : clk0; 
      |
      |endmodule""".stripMargin
    )
}