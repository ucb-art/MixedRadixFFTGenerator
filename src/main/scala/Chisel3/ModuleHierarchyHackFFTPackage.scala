package dspblocks.fft
import chisel3._
abstract class Module extends chisel3.Module(override_clock = Some(false.B.asClock), override_reset = Some(false.B)) with chisel3.ModuleWithParentInfo
abstract class BlackBox extends chisel3.BlackBox with chisel3.ModuleWithParentInfo with chisel3.util.HasBlackBoxInline