package fudian.utils.lza_utils

import chisel3._
import chisel3.util._

class preEncoderIO(val len: Int) extends Bundle {
  val g, s, e= Input(UInt(len.W))
  val f = Output(UInt(len.W))
}
class PreEncoder(val len: Int) extends Module {
  val io = IO(new preEncoderIO(len))
  val (g, s, e) = (io.g, io.s, io.e)
  val f = Wire(Vec(len, Bool()))
  for (i <- 0 until len) {
    if (i == 0) {
      f(i) := (e(i + 1) & g(i)) |
           ((~e(i + 1)) & s(i)) |
              (e(i + 1) & s(i)) |
              (e(i + 1) & s(i))
    } else if(i == len - 1) {
      f(i) := (g(i) & (~s(i - 1))) | (s(i) & (~g(i - 1)))
    }else{
      f(i) := (e(i+1) & g(i) & (~s(i-1))) |
           ((~e(i+1)) & s(i) & (~s(i-1))) |
              (e(i+1) & s(i) & (~g(i-1))) |
           ((~e(i+1)) & g(i) & (~g(i-1)))
    }
  }
  io.f := Cat(f.reverse)
}