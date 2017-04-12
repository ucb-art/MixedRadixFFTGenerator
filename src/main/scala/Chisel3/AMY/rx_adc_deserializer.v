module rx_adc_deserializer(
    input rx_adc_2400MHz_clk,
    input RX_ADC_RST_ACTHIGH,
    input RX_ADC_DIG_CLK,
    input RX_ADC_SCAN_IN,
    input RX_ADC_SCAN_EN,
    input RX_ADC_MEM_WRITE_EN,
    input [63:0] rx_adc_scan_bank_0,
    input [63:0] rx_adc_scan_bank_1,
    input [63:0] rx_adc_scan_bank_2,
    input [63:0] rx_adc_scan_bank_3,
    input [63:0] rx_adc_scan_bank_4,
    input [63:0] rx_adc_scan_bank_5,
    input [63:0] rx_adc_scan_bank_6,
    input [63:0] rx_adc_scan_bank_7,
    input [63:0] rx_adc_scan_bank_8,
    input [63:0] rx_adc_scan_bank_9,
    input [63:0] rx_adc_scan_bank_10,
    input [63:0] rx_adc_scan_bank_11,
    input [63:0] rx_adc_scan_bank_12,
    input [63:0] rx_adc_scan_bank_13,
    input [63:0] rx_adc_scan_bank_14,
    output RX_ADC_DIG_OUT,
    output RX_ADC_VCO_P_OUT,
    output RX_ADC_VCO_N_OUT,
    output reg rx_adc_mem_clk_out,
    output reg [127:0] rx_adc0_out_sar,
    output reg [127:0] rx_adc0_out_vcop,
    output reg [127:0] rx_adc0_out_vcon,
    output reg [127:0] rx_adc1_out_sar,
    output reg [127:0] rx_adc1_out_vcop,
    output reg [127:0] rx_adc1_out_vcon,
    output reg [127:0] rx_adc2_out_sar,
    output reg [127:0] rx_adc2_out_vcop,
    output reg [127:0] rx_adc2_out_vcon,
    output reg [127:0] rx_adc3_out_sar,
    output reg [127:0] rx_adc3_out_vcop,
    output reg [127:0] rx_adc3_out_vcon,
    output reg [127:0] rx_adc02_out_lsb,
    output reg [127:0] rx_adc13_out_lsb
);

reg [3:0] clk_counter;

always @(posedge rx_adc_2400MHz_clk) begin
    if (~RX_ADC_RST_ACTHIGH) begin
        clk_counter <= clk_counter + 1;
        if (clk_counter== 7) begin
            clk_counter <= 0;
            rx_adc_mem_clk_out <= ~rx_adc_mem_clk_out;
        end
    end
    else if (RX_ADC_RST_ACTHIGH) begin
        clk_counter <= 0;
        rx_adc_mem_clk_out <= 0;
        rx_adc0_out_sar <= 0;
        rx_adc0_out_vcop <= 1;
        rx_adc0_out_vcon <= 2;
        rx_adc1_out_sar <= 3;
        rx_adc1_out_vcop <= 4;
        rx_adc1_out_vcon <= 5;
        rx_adc2_out_sar <= 6;
        rx_adc2_out_vcop <= 7;
        rx_adc2_out_vcon <= 8;
        rx_adc3_out_sar <= 9;
        rx_adc3_out_vcop <= 10;
        rx_adc3_out_vcon <= 11;
        rx_adc02_out_lsb <= 12;
        rx_adc13_out_lsb <= 13;
    end
end

always @(negedge rx_adc_mem_clk_out) begin
    rx_adc0_out_sar <= rx_adc0_out_sar + 1;
    rx_adc0_out_vcop <= rx_adc0_out_vcop + 1;
    rx_adc0_out_vcon <= rx_adc0_out_vcon + 1;
    rx_adc1_out_sar <= rx_adc1_out_sar + 1;
    rx_adc1_out_vcop <= rx_adc1_out_vcop + 1;
    rx_adc1_out_vcon <= rx_adc1_out_vcon + 1;
    rx_adc2_out_sar <= rx_adc2_out_sar + 1;
    rx_adc2_out_vcop <= rx_adc2_out_vcop + 1;
    rx_adc2_out_vcon <= rx_adc2_out_vcon + 1;
    rx_adc3_out_sar <= rx_adc3_out_sar + 1;
    rx_adc3_out_vcop <= rx_adc3_out_vcop + 1;
    rx_adc3_out_vcon <= rx_adc3_out_vcon + 1;
    rx_adc02_out_lsb <= rx_adc02_out_lsb + 1;
    rx_adc13_out_lsb <= rx_adc13_out_lsb + 1;
end

endmodule