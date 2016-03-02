PRJ = FFT
MEM=--inlineMem
FIXED = true
VERILOGTB = false
VLSI_ROOT = ./build/vlsi/generated-src/

fpga: MEM=--inlineMem
fpga: clean_fpga setup_fpga vlsi
	cp ./build/vlsi/generated-src/*.v ./build/fpga/.
	cp generator_out.json ./build/fpga/.

asic: MEM=--noInlineMem
asic: clean_asic setup_asic vlsi
	cp ./build/vlsi/generated-src/*.v ./build/asic/.
	cp generator_out.json ./build/asic/.
	if [ -f ./build/vlsi/generated-src/$(PRJ).conf ]; then \
		cp ./build/vlsi/generated-src/$(PRJ).conf ./build/asic/. ;\
	fi

vlsi: clean_vlsi setup_vlsi $(VLSI_ROOT)
	sbt "run -params_true_false $(MEM) --genHarness --backend v --targetDir $(VLSI_ROOT)"
$(VLSI_ROOT):
	mkdir -p ./build/vlsi/generated-src

test: clean_test setup_test
	mkdir -p test/generated-src
	sbt "run -params_$(FIXED)_$(VERILOGTB) --test --debugMem --genHarness --compile --targetDir ./build/test" | tee console_out

debug: clean_test setup_test
	sbt "run -params_$(FIXED)_$(VERILOGTB) --test --debugMem --genHarness --compile --debug --targetDir ./build/test" | tee console_out

debug_vcd: clean_test setup_test
	sbt "run -params_$(FIXED)_$(VERILOGTB) --test --debugMem --genHarness --compile --debug --vcd --targetDir ./build/test" | tee console_out

setup_%:
	mkdir -p build/$(patsubst setup_%,%,$@)

clean_%:
	rm -rf build/$(patsubst clean_%,%,$@)

clean: clean_asic clean_fpga clean_test
	rm -rf target project build generator_out.json .compile_flags

.PHONY: fpga asic vlsi test setup_% clean_%
