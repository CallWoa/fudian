package fudian.utils.lza_utils

import chisel3._
import chisel3.util._

class LZC(len: Int, zero: Boolean) extends Module {

  val inWidth = len
  val outWidth = (inWidth - 1).U.getWidth

  val io = IO(new Bundle() {
    val in = Input(UInt(inWidth.W))
    val out = Output(UInt(outWidth.W))
  })

  io.out := PriorityEncoder(io.in.asBools.reverse)
}

object LZC {
  def apply(value: UInt): UInt = {
    val lzc = Module(new LZC(value.getWidth, true))
    lzc.io.in := value
    lzc.io.out
  }
  def apply(xs: Seq[Bool]): UInt = {
    apply(Cat(xs.reverse))
  }
}
