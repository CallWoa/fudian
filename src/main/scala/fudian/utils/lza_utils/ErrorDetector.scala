package fudian.utils.lza_utils

import chisel3._
import chisel3.util._

class ErrorDetectorIO(val len: Int) extends Bundle {
  val g, s, e = Input(UInt(len.W))
  val y = Output(Bool())
}

class ErrorDetector(val len: Int) extends Module {
  val io = IO(new ErrorDetectorIO(len))
  val (g, s, e) = (io.g, io.s, io.e)
  val p, n, z = Wire(Vec(len, Bool()))
  for (i <- 0 until len) {
    if (i == len - 1) {
      p(i) := g(i)
      n(i) := s(i)
    } else if(i == len - 2){
      p(i) := (e(i+1) | g(i+1)) & g(i)
      n(i) := (e(i+1) | s(i+1)) & s(i)
    }else{
      p(i) := (e(i+1) | (e(i+2) & g(i+1)) | ((~e(i+2)) & s(i+1))) & g(i)
      n(i) := (e(i+1) | (e(i+2) & s(i+1)) | ((~e(i+2)) & g(i+1))) & s(i)
    }
    z(i) := ~(p(i) | n(i))
  }

  def compress (width: Int, p: UInt, n: UInt, z: UInt): Bool = {
    val nextWidth = width - width / 3
    val node0 = Module(new node)
    node0.io.in(0) := Cat(p(2), p(1), p(0))
    node0.io.in(1) := Cat(n(2), n(1), n(0))
    node0.io.in(2) := Cat(z(2), z(1), z(0))
    var nextColumn_p = node0.io.out(0).asBools
    var nextColumn_n = node0.io.out(1).asBools
    var nextColumn_z = node0.io.out(2).asBools
    for (i <- 1 to (width / 3)) {
      if(i < width/3){
        val node = Module(new node)
        node.io.in(0) := Cat(p(3 * i + 2), p(3 * i + 1), p(3 * i))
        node.io.in(1) := Cat(n(3 * i + 2), n(3 * i + 1), n(3 * i))
        node.io.in(2) := Cat(z(3 * i + 2), z(3 * i + 1), z(3 * i))
        val pout = node.io.out(0).asBools
        val nout = node.io.out(1).asBools
        val zout = node.io.out(2).asBools
        nextColumn_p = nextColumn_p ++ pout
        nextColumn_n = nextColumn_n ++ nout
        nextColumn_z = nextColumn_z ++ zout
      }else if(width % 3 == 2){
        nextColumn_p = nextColumn_p :+ p(width - 2).asBool :+ p(width - 1).asBool
        nextColumn_n = nextColumn_n :+ n(width - 2).asBool :+ n(width - 1).asBool
        nextColumn_z = nextColumn_z :+ z(width - 2).asBool :+ z(width - 1).asBool
      }else if(width % 3 == 1){
        nextColumn_p = nextColumn_p :+ p(width - 1).asBool
        nextColumn_n = nextColumn_n :+ n(width - 1).asBool
        nextColumn_z = nextColumn_z :+ z(width - 1).asBool
      }
    }
    if(nextWidth == 2) (nextColumn_p(0) ^ nextColumn_p(1)) & (~nextColumn_z(0)) & (~nextColumn_z(1))
    else compress(nextWidth, Cat(nextColumn_p.reverse), Cat(nextColumn_n.reverse), Cat(nextColumn_z.reverse))
  }
  val y = compress(len, Cat(p), Cat(n), Cat(z))
  io.y := y
}

