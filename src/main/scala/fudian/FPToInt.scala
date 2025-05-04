package fudian

import chisel3._
import chisel3.util._
import fudian.utils._

/**
  *  op: 00 => f -> wu
  *      01 => f -> w
  *      10 => f -> lu
  *      11 => f -> l
  */

class FPToInt(val expWidth: Int, val sigWidth: Int) extends Module {
  val io = IO(new Bundle() {
    val in = Input(UInt((expWidth + sigWidth).W))
    val rm = Input(UInt(3.W))
    val op = Input(UInt(2.W))
    val result = Output(UInt(64.W))
    val fflags = Output(UInt(5.W))
  })
  val isSignedInt = io.op(0)
  val isLongInt = io.op(1)
  val fp_a = FloatPoint.fromUInt(io.in, expWidth, sigWidth)
  val decode_a = fp_a.decode
  val raw_a = RawFloat.fromFP(fp_a, Some(decode_a.expNotZero))
  /*
    fp -> int : 2 ^ (exp - expBias)
    Int32(max) = 1.111xxx111 * 2 ^ 31
    Int64(max) = 1.111xxx111 * 2 ^ 63
  */
  val maxIntExp =
    FloatPoint.expBias(expWidth).U +& Mux(isLongInt, 63.U, 31.U)
  val exp_of = raw_a.exp > maxIntExp
  /*------------------------------------------------------------------------
   Left Shift Path
   ------------------------------------------------------------------------*/
  val lpath_shamt = raw_a.exp - (FloatPoint.expBias(expWidth) + sigWidth - 1).U
  val lpath_iv = !isSignedInt && raw_a.sign
  val lpath_may_of = isSignedInt && (raw_a.exp === maxIntExp)
  val lpath_pos_of = lpath_may_of && !raw_a.sign
  val lpath_neg_of = lpath_may_of && raw_a.sign && raw_a.sig.tail(1).orR
  val lpath_of = lpath_pos_of || lpath_neg_of
  val lpath_max_shamt = 63 - (sigWidth - 1)
  val lpath_max_shmat_width = lpath_max_shamt.U.getWidth
  val lpath_sig_shifted =
    (raw_a.sig << lpath_shamt(lpath_max_shmat_width - 1, 0))(63, 0)
  /*------------------------------------------------------------------------
   Right Shift Path
   ------------------------------------------------------------------------*/
  val rpath_shamt = (FloatPoint.expBias(expWidth) + sigWidth - 1).U - raw_a.exp
  val (rpath_sig_shifted, rpath_sitcky) = ShiftRightJam(
    Cat(raw_a.sig, 0.U(1.W)),
    rpath_shamt
  )
  val rpath_rounder = Module(new RoundingUnit(sigWidth))
  rpath_rounder.io.in := rpath_sig_shifted.head(sigWidth)
  rpath_rounder.io.roundIn := rpath_sig_shifted(0)
  rpath_rounder.io.stickyIn := rpath_sitcky
  rpath_rounder.io.signIn := raw_a.sign
  rpath_rounder.io.rm := io.rm
  val rpath_sig = Cat(
    0.U((64 - sigWidth - 1).W),
    rpath_rounder.io.cout,
    rpath_rounder.io.out
  )
  val rpath_ix = rpath_rounder.io.inexact
  val rpath_iv = !isSignedInt && raw_a.sign && rpath_sig.orR
  val rpath_of = if (sigWidth >= 32) {
    val rpath_exp_inc =
      rpath_rounder.io.r_up && rpath_rounder.io.in(30, 0).andR
    val rpath_exp_eq_31 = raw_a.exp === (FloatPoint.expBias(expWidth) + 31).U
    val rpath_exp_eq_30 = raw_a.exp === (FloatPoint.expBias(expWidth) + 30).U
    val rpath_pos_of = !raw_a.sign && Mux(
      isSignedInt,
      rpath_exp_eq_31 || (rpath_exp_eq_30 && rpath_exp_inc),
      rpath_exp_eq_31 && rpath_exp_inc
    )
    val rpath_neg_of = raw_a.sign && rpath_exp_eq_31 &&
      (rpath_rounder.io.in(30, 0).orR || rpath_rounder.io.r_up)
    // only for int32
    !isLongInt && (rpath_pos_of || rpath_neg_of)
  } else false.B
  /*------------------------------------------------------------------------
   Result Select
   ------------------------------------------------------------------------*/
  val sel_lpath = raw_a.exp >= (FloatPoint.expBias(expWidth) + sigWidth - 1).U
  val of = exp_of || (sel_lpath && lpath_of) || (!sel_lpath && rpath_of)
  val iv = of || (sel_lpath && lpath_iv) || (!sel_lpath && rpath_iv)
  val ix = !iv && !sel_lpath && rpath_ix

  val ResultIntAbs = Mux(sel_lpath, lpath_sig_shifted, rpath_sig)
  val Result = Mux(raw_a.sign && isSignedInt, -ResultIntAbs, ResultIntAbs) & Cat(
    Fill(32, isLongInt),
    ~0.U(32.W)
  )

  val max_int64 = Cat(!isSignedInt, ~0.U(63.W))
  val min_int64 = Cat(isSignedInt, 0.U(63.W))
  val max_int32 = Cat(0.U(32.W), max_int64.head(32))
  val min_int32 = Cat(0.U(32.W), min_int64.head(32))
  val ivSelMax = decode_a.isNaN || !raw_a.sign

  io.result := Mux(
    iv,
    Mux(
      ivSelMax,
      Mux(isLongInt, max_int64, max_int32),
      Mux(isLongInt, min_int64, min_int32)
    ),
    Result
  )
  io.fflags := Cat(iv, false.B, false.B, false.B, ix)
}
