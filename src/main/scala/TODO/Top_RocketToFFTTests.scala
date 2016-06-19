package FFT
import ChiselDSP._
import Chisel.{Complex => _, _}

class RocketToFFTTests(c: RocketToFFT) extends Tester(c) {


  // bits to complex
  // wait until done functions
  // test with dspmodule wrapper


  var x = BigInt(0)

  //check for ones that can write, cannot write


  write(3,c.memMap("fftIdx").base)
  x = read(c.memMap("fftIdx").base)
  if (x != 3) Error("boo")

  write(1,c.memMap("isFFT").base)
  x = read(c.memMap("isFFT").base)
  if (x != 1) Error("boo")

  step(1)
  write(3,c.memMap("setupDone").base)
  x = read(c.memMap("setupDone").base)
  if (x != 0) Error("boo")
  //while(x != 1) {x = read(c.memMap("setupDone").base)}
  while(x != 1) {x = read(c.memMap("setupDone").base)}

  Status("yay")

  peek(c.kmax)

/*
  write(3,c.memMap("calcDone").base)
  x = read(c.memMap("calcDone").base)
  if (x != 0) Error("boo")

  for (i <- 0 until c.dataMem("toFFT").depth){
    write(i+1,c.memMap("toFFT").base+i)
    x = read(c.memMap("toFFT").base + i)
    if (x != i+1) Error("boo")
  }

*/


  //write(4,4)
  //read(3)
  //read(4)
  //read(0)
  // setup
  //write(2*2048+2,1)
  //var setupDone = read(2*2048+2)
  /*while(setupDone == 0){
    step(1)
    setupDone = read(2*2048+2)
  }*/
  //Status("yay")

  override def step(n:Int) = {
    peek(c.rocketWEs("fftIdx"))
    peek(c.regs("fftIdx").asInstanceOf[Bits])
    super.step(n)
  }

  def write(d:BigInt,a:Int): Unit ={
    var fftReady = peek(c.io.smi.req.ready)
    while(fftReady == 0){
      // Not ready
      step(1)
      fftReady = peek(c.io.smi.req.ready)
    }
    poke(c.io.smi.req.valid,true)
    poke(c.io.smi.req.bits.rw,true)
    poke(c.io.smi.req.bits.addr,a)
    poke(c.io.smi.req.bits.data,d)
    poke(c.io.smi.resp.ready,false)
    step(1)
    poke(c.io.smi.req.valid,false)
    poke(c.io.smi.resp.ready,true)
    var fftValid = peek(c.io.smi.resp.valid)
    while(fftValid == 0){
      // Not valid
      step(1)
      fftValid = peek(c.io.smi.resp.valid)
    }
  }

  def read(a:Int): BigInt ={
    var fftReady = peek(c.io.smi.req.ready)
    while(fftReady == 0){
      // Not ready
      step(1)
      fftReady = peek(c.io.smi.req.ready)
    }
    poke(c.io.smi.req.valid,true)
    poke(c.io.smi.req.bits.rw,false)
    poke(c.io.smi.req.bits.addr,a)
    poke(c.io.smi.resp.ready,false)
    step(1)
    poke(c.io.smi.req.valid,false)
    poke(c.io.smi.resp.ready,true)
    var fftValid = peek(c.io.smi.resp.valid)
    while(fftValid == 0){
      // Not valid
      step(1)
      fftValid = peek(c.io.smi.resp.valid)
    }
    peek(c.io.smi.resp.bits)
  }



}