package dspblocks.fft
import org.scalatest.{FlatSpec, Matchers}

class ThesisSpec extends FlatSpec with Matchers {
  behavior of "Thesis"
  it should "work" in {

    PeelingScheduling.getIOMemBankAddr(8, DIF) foreach {x => println(x) }
    println("xxxx")
    PeelingScheduling.getIOMemBankAddr(8, DIT) foreach {x => println(x) }
    //BinToBankAddrMap.cpCounts foreach {x => x foreach { y => println(y) } }

  }
}

