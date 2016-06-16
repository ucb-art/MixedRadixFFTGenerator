package FFT
import ChiselDSP._
import Chisel.{Complex => _, _}

class RocketToFFTTests(c: RocketToFFT) extends Tester(c) {

  write(3,3)
  write(4,4)
  read(3)
  read(4)
  read(0)

  def write(d:BigInt,a:BigInt): Unit ={
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

  def read(a:BigInt): BigInt ={
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