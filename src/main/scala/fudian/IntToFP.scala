package fudian

import chisel3._
import chisel3.util._
import fudian.utils._
import fudian.utils.lza_utils._

class IntToFP_prenorm_in extends Bundle {
  val int = Input(UInt(64.W))
  val sign = Input(Bool())
  val long = Input(Bool())
}

class IntToFP_prenorm_out extends Bundle {
  val norm_int = Output(UInt(63.W))
  val lzc = Output(UInt(6.W))
  val is_zero = Output(Bool())
  val sign = Output(Bool())
}

/**
  * different fp types can share this unit
  */
class IntToFP_prenorm extends Module {

  val io = IO(new Bundle() {
    val in = new IntToFP_prenorm_in
    val out = new IntToFP_prenorm_out
  })

  val (in, signed_int, long_int) = (io.in.int, io.in.sign, io.in.long)

  val in_sign = signed_int && Mux(long_int, in(63), in(31))
  val in_sext = Cat(Fill(32, in(31)), in(31, 0))
  val in_raw = Mux(signed_int && !long_int, in_sext, in)
  val in_abs = Mux(in_sign, (~in_raw).asUInt + 1.U, in_raw)

  val lzc = LZC(in_abs)
  val in_norm = (in_abs << lzc)(62, 0)

  io.out.norm_int := in_norm
  io.out.lzc := lzc
  io.out.is_zero := in === 0.U
  io.out.sign := in_sign

}

class IntToFP_postnorm(val expWidth: Int, val sigWidth: Int) extends Module {
  val io = IO(new Bundle() {
    val in = Flipped(new IntToFP_prenorm_out)
    val rm = Input(UInt(3.W))
    val result = Output(UInt((expWidth + sigWidth).W))
    val fflags = Output(UInt(5.W))
  })
  val (in, lzc, is_zero, in_sign, rm) =
    (io.in.norm_int, io.in.lzc, io.in.is_zero, io.in.sign, io.rm)

  val exp_raw = (63 + FloatPoint.expBias(expWidth)).U(expWidth.W) - lzc
  val sig_raw = in.head(sigWidth - 1) // exclude hidden bit
  val round_bit = in.tail(sigWidth - 1).head(1)
  val sticky_bit = in.tail(sigWidth).orR

  val rounder = Module(new RoundingUnit(sigWidth - 1))
  rounder.io.in := sig_raw
  rounder.io.roundIn := round_bit
  rounder.io.stickyIn := sticky_bit
  rounder.io.signIn := in_sign
  rounder.io.rm := rm

  val ix = rounder.io.inexact
  val fp_exp = Mux(is_zero, 0.U, exp_raw + rounder.io.cout)
  val fp_sig = rounder.io.out

  io.result := Cat(in_sign, fp_exp, fp_sig)
  io.fflags := Cat(0.U(4.W), ix)
}

class IntToFP(val expWidth: Int, val sigWidth: Int) extends Module {
  val io = IO(new IntToFP_prenorm_in {
    val rm = Input(UInt(3.W))
    val result = Output(UInt((expWidth + sigWidth).W))
    val fflags = Output(UInt(5.W))
  })

  val pre_norm = Module(new IntToFP_prenorm)
  val post_norm = Module(new IntToFP_postnorm(expWidth, sigWidth))

  pre_norm.io.in := io
  post_norm.io.in := pre_norm.io.out
  post_norm.io.rm := io.rm

  io.result := post_norm.io.result
  io.fflags := post_norm.io.fflags
}
