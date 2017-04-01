
/*

package fft2

import chisel3._
import chisel3.util._
import cde.{Parameters, Field}
import dsptools._
import uncore.tilelink._
import uncore.converters._
import _root_.junctions._
import diplomacy._
import rocketchip._
import dsptools.numbers._
import dsptools.numbers.implicits._
import dspjunctions._
import dspblocks._
import testchipip._

import dspblocks.fft._

trait PeripheryFFT2 extends LazyModule {

  // In bytes (multiple of 4096 = page)
  val scrSize = 4096
  val pDevices: ResourceManager[AddrMapEntry]
  pDevices.add(AddrMapEntry("scr_control", MemSize(scrSize, MemAttr(AddrMapProt.RW))))





  

// update to master



  // add status and control registers here
  // TODO: Name?
  val scrName = "fft-control"
  val scrbuilder = new SCRBuilder(scrName)

}

trait PeripheryFFT2Bundle {
  // Nothin' yet
}


// something super wrong with delays


// TRY WRITE TO ONLY 1 MEMORY


trait PeripheryFFT2Module extends HasPeripheryParameters {
  import chisel3.core.ExplicitCompileOptions.NotStrict

  implicit val p: Parameters
  val pBus: TileLinkRecursiveInterconnect
  def io: Bundle with PeripheryFFT2Bundle
  val outer: PeripheryFFT2

  // Put all of my generator variables here
// hard code

// besst is to set my own dspcontext here

// amy's thing

// comment out for paul
  val ffastMod = Module(new FFASTTopWrapper(vars))
  // where is clk

  ffastMod.scr foreach { case (name, dir, port) =>
    dir match {
      case chisel3.core.Input => 
        scrbuilder.addControl(name, init=0.U)
      case chisel3.core.Output =>
        scrbuilder.addStatus(name)
    }
  }

  // connect SCR stuff here
  // Find out where it's assigning to and then hard code
  val baseAddr = 0x2000
  // TODO: Check that this is right (run rocket-chip)
  val scr: SCRFile = outer.scrbuilder.generate(baseAddr)

  require((scr.numConstrols + scr.numStatues) * 8 < outer.scrSize), "Should fit")

  println(SCRAddressMap(outer.scrName))

  scr.io.tl <> pBus.port("scr_control")
    
// make CONFIG=FFT2Config debug


//comment out forWarnWarnWarnWarnWarnWarnWarnWarnWarnWarnWarn 



  ffastMod.scr foreach { case (name, dir, port) =>
    // dir match
    port := scr.control("name")
  }
  // connect status and control registers here
  //module.thing1 := scr.control("ctrlname")
  //scr.status("statusname") := module.thing2
}

*/
