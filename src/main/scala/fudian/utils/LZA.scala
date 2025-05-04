package fudian.utils

import chisel3._
import chisel3.util._
import fudian.utils.lza_utils._

class LzaIO(val len: Int) extends Bundle {

  val a, b = Input(UInt(len.W))
  val lzc = Output(UInt(log2Up(len).W))
  val error = Output(Bool())
  val zero = Output(Bool())
}
class LZA(len: Int) extends Module {
  val io = IO(new LzaIO(len))
  val (a, b) = (io.a, io.b)
  val g, s, e = Wire(Vec(len, Bool()))
  for (i <- 0 until len){
    g(i) := a(i) & (~b(i))
    s(i) := (~a(i)) & b(i)
    e(i) := ~(a(i) ^ b(i))
  }

  val preEncoder = Module(new PreEncoder(len))
  preEncoder.io.g := Cat(g.reverse)
  preEncoder.io.s := Cat(s.reverse)
  preEncoder.io.e := Cat(e.reverse)
  val f = preEncoder.io.f

  val errorDetector = Module(new ErrorDetector(len))
  errorDetector.io.g := Cat(g.reverse)
  errorDetector.io.s := Cat(s.reverse)
  errorDetector.io.e := Cat(e.reverse)

  io.error := errorDetector.io.y
  io.lzc := LZC(f)
  io.zero := !f.orR
}

object LZA8 extends App {
  emitVerilog(new LZA(8), Array("--target-dir", "generated"))
}
