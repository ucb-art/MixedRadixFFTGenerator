PRJ=FFT
# inLineMem = Chisel outputs registers; otherwise SRAM black boxes
MEM=--inlineMem
# Run in fixed point mode or Dbl
FIXED = false
# Output Verilog TB file? (mirror of Chisel TB)
VERILOGTB = false
# Output directory for generic 'make vlsi', and more specific (should use) 'make asic', 'make fpga'
VLSI_ROOT = ./build/vlsi/generated-src
ASIC_ROOT = ./build/asic
FPGA_ROOT = ./build/fpga
# Analysis results should be dumped here (you should specify)
ANALYSIS_ROOT = ./build/analysis
# Files you'll look at for debug (Verilog TB, console print out)
DEBUG_ROOT = ./build/debug
# Misc. files used with C++ emulator
TEST_ROOT = ./build/test

# Make Verilog for FPGA
fpga: MEM=--inlineMem
fpga: clean_fpga setup_fpga vlsi
	cp $(VLSI_ROOT)/$(PRJ).v $(FPGA_ROOT)/.
	cp $(ANALYSIS_ROOT)/*.json $(FPGA_ROOT)/.

# Make Verilog for ASIC
asic: MEM=--noInlineMem
asic: clean_asic setup_asic vlsi
	cp $(VLSI_ROOT)/$(PRJ).v $(ASIC_ROOT)/.
	cp $(ANALYSIS_ROOT)/*.json $(ASIC_ROOT)/.
	if [ -f $(VLSI_ROOT)/$(PRJ).conf ]; then \
    	cp $(VLSI_ROOT)/$(PRJ).conf $(ASIC_ROOT)/. ;\
    fi

# Generic Make VLSI (should not use directly)
vlsi: clean_vlsi setup_vlsi clean_analysis setup_analysis
	sbt "run -params_true_false $(MEM) --genHarness --backend v --targetDir $(VLSI_ROOT)"

# Debug without VCD dump (no large files)
debug: clean_test setup_test clean_analysis setup_analysis clean_debug setup_debug
	sbt "run -params_$(FIXED)_$(VERILOGTB) --test --debugMem --genHarness --compile --debug --targetDir $(TEST_ROOT)" | tee $(DEBUG_ROOT)/console_out.txt
	cp $(ANALYSIS_ROOT)/*.json $(DEBUG_ROOT)/.

# Debug with VCD dump
debug_vcd: clean_test setup_test clean_analysis setup_analysis clean_debug setup_debug
	sbt "run -params_$(FIXED)_$(VERILOGTB) --test --debugMem --genHarness --compile --debug --vcd --targetDir $(TEST_ROOT)" | tee $(DEBUG_ROOT)/console_out.txt
	cp $(ANALYSIS_ROOT)/*.json $(DEBUG_ROOT)/.

# Debug with Verilog TB generation
debug_tb: FIXED=true
debug_tb: VERILOGTB=true
debug_tb: debug

# Make ASIC Verilog + TB
asic_tb: asic debug_tb
	mv $(DEBUG_ROOT)/* $(ASIC_ROOT)/.
	rm -rf $(ASIC_ROOT)/constraints.xdc $(ASIC_ROOT)/console_out.txt

# Make FPGA Verilog + TB
fpga_tb: fpga debug_tb
	mv $(DEBUG_ROOT)/* $(FPGA_ROOT)/.
	rm -rf $(FPGA_ROOT)/Makefrag $(FPGA_ROOT)/console_out.txt

setup_%:
	mkdir -p build/$(patsubst setup_%,%,$@)

clean_%:
	rm -rf build/$(patsubst clean_%,%,$@)

clean: clean_asic clean_fpga clean_test clean_debug clean_analysis clean_vlsi
	rm -rf target project build .compile_flags
	rm -rf *.h *.cpp
	rm -rf MatlabScratch/*

.PHONY: fpga_tb asic_tb fpga asic vlsi debug debug_vcd setup_% clean_%