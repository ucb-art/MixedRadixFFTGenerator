package dspblocks.fft
import chisel3._

abstract class SCRBundle extends Bundle

object SCRHelper {
  def apply(scr: SCRBundle): Seq[(Element, String)] = {
    println("Discovered SCR Ports:")
    val scrPorts = getGroundPorts("scr", scr)
    scrPorts.map { case (element, name) =>
      require(element.getWidth <= 64, "Width must be < 64!")
      val newName = name.stripPrefix("scr_")
      println(s"$newName ${element.getWidth}")
      (element, newName)
    }
  }
  def getGroundPorts(name: String, data: Data): Seq[(Element, String)] = data match {
    case e: Element => Seq(e -> name)
    case b: Record => b.elements.toSeq flatMap { case (n, e) => getGroundPorts(s"${name}_$n", e) }
    case v: Vec[_] => v.zipWithIndex flatMap { case (e, i) => getGroundPorts(s"${name}_$i", e) }
  }
}