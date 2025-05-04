package fudian

import chisel3._
import chisel3.util._
import utils._
import utils.lza_utils._

/*
  fp32: lut 14 pad 5 iter 2
  fp64: lut 14 pad 6 iter 3
*/
class ReciprocalLUT(input_width: Int, sig_width: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(input_width.W))
    val recp = Output(UInt(sig_width.W))
    val recpSqrt = Output(UInt(sig_width.W))
    val recpSqrt2x = Output(UInt(sig_width.W))
  })

  val tableSize: Int = 1 << input_width
  def generateLUTEntry(i: Int): (UInt, UInt, UInt) = {
    val M = 1.0 + i.toDouble / tableSize

    def quantize(value: Double): UInt = {
      val scaled = (value * (1L << sig_width)).toLong
      if (scaled == (1L << sig_width)) {
        1.U(1.W) ## 0.U((sig_width - 1).W)
      } else {
        0.U(1.W) ## scaled.U(sig_width - 1, 1)
      }
    }

    val recp = quantize(1.0 / M)
    val recpSqrt = quantize(1.0 / math.sqrt(M))
    val recpSqrt2x = quantize(1.0 / math.sqrt(2.0 * M))

    (recp, recpSqrt, recpSqrt2x)
  }

  val (recpTable, recpSqrtTable, recpSqrt2xTable) = (0 until tableSize).map(generateLUTEntry).unzip3

  val recpVec = VecInit(recpTable)
  val recpSqrtVec = VecInit(recpSqrtTable)
  val recpSqrt2xVec = VecInit(recpSqrt2xTable)

  io.recp := recpVec(io.in)
  io.recpSqrt := recpSqrtVec(io.in)
  io.recpSqrt2x := recpSqrt2xVec(io.in)
}

//object ReciprocalLUT extends App {
//  emitVerilog(new ReciprocalLUT(8, 24), Array("--target-dir", "generated"))
//}

object SignExt {
  def apply(a: UInt, len: Int) = {
    val aLen = a.getWidth
    val signBit = a(aLen-1)
    if (aLen >= len) a(len-1,0) else Cat(Fill(len - aLen, signBit), a)
  }
}

class FDIV(val expWidth: Int, val sigWidth: Int) extends Module {
  val io = IO(new Bundle() {
    val in = Flipped(Decoupled(new FPDataIn(expWidth, sigWidth, 2)))
    val out = Decoupled(new FPDataOut(expWidth, sigWidth))
    val isSqrt = Input(Bool())
    val flush = Input(Bool())
  })

  val fp_a = FloatPoint.fromUInt(io.in.bits.src(0), expWidth, sigWidth)
  val fp_b = FloatPoint.fromUInt(io.in.bits.src(1), expWidth, sigWidth)
  val (decode_a, decode_b) = (fp_a.decode, fp_b.decode)
  val raw_a = RawFloat.fromFP(fp_a, Some(decode_a.expNotZero))
  val raw_b = RawFloat.fromFP(fp_b, Some(decode_b.expNotZero))


  val s1_is_sqrt = io.isSqrt
  val s1_rm = io.in.bits.rm
  val s1_res_sign = Mux(s1_is_sqrt, Mux(decode_a.isZero, raw_a.sign, false.B), raw_a.sign ^ raw_b.sign)

  val s1_a_sig_aligned, s1_b_sig_aligned = Wire(UInt(sigWidth.W))
  val a_lzc = LZC(raw_a.sig)
  val b_lzc = LZC(raw_b.sig)
  s1_a_sig_aligned := raw_a.sig << a_lzc
  s1_b_sig_aligned := raw_b.sig << b_lzc
  val a_exp_aligned, b_exp_aligned = Wire(UInt((expWidth+1).W))
  a_exp_aligned := raw_a.exp - Fill(expWidth + 1 - a_lzc.getWidth, 0.U(1.W)) ## a_lzc
  b_exp_aligned := raw_b.exp - Fill(expWidth + 1 - b_lzc.getWidth, 0.U(1.W)) ## b_lzc
  /*------------------------------------------------------------------------
  special case
  *------------------------------------------------------------------------*/
  /*
    Detected at s_idle: divide by 0, invalid(0/0, sqrt minus)
    detected at pre_1: overflow, underflow(except one case)
    detected at rounding: inexact
    detect after rounding: underflow
  */
  val s1_inv = Mux(s1_is_sqrt,
    (fp_a.sign && !decode_a.isZero) || decode_a.isNaN, //sqrt minus or sqrt nan
    (decode_a.isInf && decode_b.isInf) || (decode_b.isZero && decode_a.isZero) || decode_a.isNaN || decode_b.isNaN
  ) //inf/inf or 0/0 or nan/nan
  val inv_flag = Mux(s1_is_sqrt,
    (fp_a.sign && !decode_a.isQNaN && !decode_a.isZero) || decode_a.isSNaN,
    (decode_a.isInf && decode_b.isInf) || (decode_b.isZero && decode_a.isZero) || decode_a.isSNaN || decode_b.isSNaN
  )
  val dbz = decode_b.isZero && !decode_a.isZero && !s1_is_sqrt //div zero
  val zero_div = decode_a.isZero && (!decode_b.isZero || s1_is_sqrt) // exact zero
  val div_inf = !s1_is_sqrt && decode_b.isInf && !decode_a.isInf
  val inf_div = Mux(s1_is_sqrt, decode_a.isInf, decode_a.isInf && !decode_b.isInf)

  // s_pre_1
  // obtain final exp and 0st iter result
  // also obtain iterNum
  val a_lt_b = s1_a_sig_aligned < s1_b_sig_aligned
  val s1_a_eq_b = s1_a_sig_aligned === s1_b_sig_aligned
  val div_exp_diff = (SignExt(a_exp_aligned, expWidth+2) + ~SignExt(b_exp_aligned, expWidth+2)) + (~a_lt_b).asUInt
  val sqrt_exp_diff = SignExt((a_exp_aligned - FloatPoint.expBias(expWidth).U) >> 1, expWidth + 2)
  val s1_exp_diff = Mux(s1_is_sqrt, sqrt_exp_diff, div_exp_diff)

  val exp_underflow = (s1_exp_diff + (FloatPoint.expBias(expWidth) + sigWidth).U).head(1).asBool // inexact zero
  val exp_overflow = ((~s1_exp_diff).asUInt + (FloatPoint.maxNormExp(expWidth) - FloatPoint.expBias(expWidth) + 1).U).head(1).asBool
  val s1_exp_subnormal = (s1_exp_diff + (FloatPoint.expBias(expWidth)-1).U).head(1).asBool && !exp_underflow

  val s1_overflow = exp_overflow && !s1_inv && !dbz && !inf_div
  val s1_underflow = exp_underflow && !s1_inv && !dbz && !zero_div && !div_inf
  val s1_inexact = !zero_div && !s1_inv && !dbz && !div_inf && !inf_div

  val s1_special_fflags = Cat(inv_flag, dbz && !s1_inv && !inf_div, s1_overflow, s1_underflow, s1_inexact)
  val s1_special_exp = Mux(s1_inv || s1_overflow || dbz || inf_div, Fill(expWidth, true.B), 0.U(expWidth.W)) // put inv first
  val s1_special_sig = Mux(s1_inv, 1.U << (sigWidth-2), 0.U((sigWidth-1).W))

  val s1_skip_iter = s1_overflow || exp_underflow || s1_inv || dbz || zero_div || div_inf || inf_div

  val lut_input_width = 14
  val lut = Module(new ReciprocalLUT(lut_input_width, sigWidth))
  lut.io.in := Mux(s1_is_sqrt, s1_a_sig_aligned, s1_b_sig_aligned).tail(1).head(lut_input_width)
  val recp = lut.io.recp
  val recpSqrt = lut.io.recpSqrt
  val recpSqrt2x = lut.io.recpSqrt2x
  val exp_even = a_exp_aligned(0)
  val s1_is_sqrt2x = s1_is_sqrt & !exp_even
  val lut_res = Mux1H(
    Seq(
      !s1_is_sqrt -> recp,
      (s1_is_sqrt & exp_even) -> recpSqrt,
      s1_is_sqrt2x -> recpSqrt2x
    )
  )
  /*------------------------------------------------------------------------
  Pipeline S1 - S2
  *------------------------------------------------------------------------*/
  class Ctrl_S3 extends Bundle{
    val res_sign = Output(Bool())
    val inv = Output(Bool())
    val rm = Output(UInt(3.W))
    val exp_diff = Output(UInt((expWidth+2).W))
    val special_exp = Output(UInt(expWidth.W))
    val special_sig = Output(UInt((sigWidth-1).W))
    val special_fflags = Output(UInt(5.W))
    val exp_subnormal = Output(Bool())
  }
  class FDIV_S1_S2 extends Bundle{
    val a_sig_aligned = Output(UInt(sigWidth.W))
    val b_sig_aligned = Output(UInt(sigWidth.W))
    val a_eq_b = Output(Bool())
    val skip_iter = Output(Bool())
    val is_sqrt = Output(Bool())
    val is_sqrt2x = Output(Bool())

    val ctrl_s3 = new Ctrl_S3
  }
  val iter_in_ready = Wire(Bool())
  val iter_out_valid = Wire(Bool())
  val iter_out_ready = Wire(Bool())
  val pipeline_s1_s2 = Module(new PipelineReg(new FDIV_S1_S2))
  pipeline_s1_s2.io.flush := io.flush
  pipeline_s1_s2.io.in.valid := io.in.valid
  pipeline_s1_s2.io.out.ready := iter_in_ready
  pipeline_s1_s2.io.in.bits.a_eq_b := s1_a_eq_b
  pipeline_s1_s2.io.in.bits.skip_iter := s1_skip_iter
  pipeline_s1_s2.io.in.bits.a_sig_aligned := s1_a_sig_aligned
  pipeline_s1_s2.io.in.bits.b_sig_aligned := s1_b_sig_aligned
  pipeline_s1_s2.io.in.bits.is_sqrt := s1_is_sqrt
  pipeline_s1_s2.io.in.bits.is_sqrt2x := s1_is_sqrt2x
  pipeline_s1_s2.io.in.bits.ctrl_s3.rm := s1_rm
  pipeline_s1_s2.io.in.bits.ctrl_s3.exp_diff := s1_exp_diff
  pipeline_s1_s2.io.in.bits.ctrl_s3.special_exp := s1_special_exp
  pipeline_s1_s2.io.in.bits.ctrl_s3.special_sig := s1_special_sig
  pipeline_s1_s2.io.in.bits.ctrl_s3.special_fflags := s1_special_fflags
  pipeline_s1_s2.io.in.bits.ctrl_s3.res_sign := s1_res_sign
  pipeline_s1_s2.io.in.bits.ctrl_s3.inv := s1_inv
  pipeline_s1_s2.io.in.bits.ctrl_s3.exp_subnormal := s1_exp_subnormal
  val s2_in_valid = pipeline_s1_s2.io.out.valid
  val s2_a_sig_aligned = pipeline_s1_s2.io.out.bits.a_sig_aligned
  val s2_b_sig_aligned = pipeline_s1_s2.io.out.bits.b_sig_aligned
  val s2_is_sqrt = pipeline_s1_s2.io.out.bits.is_sqrt
  val s2_is_sqrt2x = pipeline_s1_s2.io.out.bits.is_sqrt2x
  val s2_a_eq_b = pipeline_s1_s2.io.out.bits.a_eq_b
  val s2_skip_iter = pipeline_s1_s2.io.out.bits.skip_iter
  val s2_ctrl_s3 = pipeline_s1_s2.io.out.bits.ctrl_s3

  /*------------------------------------------------------------------------
  S2 : Iteration Unit
  *------------------------------------------------------------------------*/
  val padding_width = 6
  val gSIterUnit = Module(new GoldSchIterUnit(sigWidth, padding_width))
  gSIterUnit.io.in.bits.n := s1_a_sig_aligned
  gSIterUnit.io.in.bits.d := Mux(s1_is_sqrt, s1_a_sig_aligned, s1_b_sig_aligned)
  gSIterUnit.io.in.bits.r := lut_res
  gSIterUnit.io.in.bits.sqrt := s1_is_sqrt
  gSIterUnit.io.in.bits.sqrt2x := s1_is_sqrt2x
  gSIterUnit.io.in.bits.flush := io.flush
  gSIterUnit.io.in.valid := io.in.valid & !s1_skip_iter & !s1_a_eq_b
  gSIterUnit.io.q.ready := iter_out_ready
  iter_in_ready := gSIterUnit.io.in.ready
  iter_out_valid := gSIterUnit.io.q.valid | (s2_in_valid & (s2_skip_iter | s2_a_eq_b))
  val s2_qa = Mux(s2_a_eq_b, 1.U(1.W) ## 0.U((sigWidth + padding_width - 1).W), gSIterUnit.io.q.bits)

  /*------------------------------------------------------------------------
  Pipeline S2 - S3
  *------------------------------------------------------------------------*/
  class FDIV_S2_S3 extends Bundle{
    val qa = Output(UInt((sigWidth + padding_width).W))
    val a_sig_aligned = Output(UInt(sigWidth.W))
    val b_sig_aligned = Output(UInt(sigWidth.W))
    val skip_iter = Output(Bool())
    val is_sqrt = Output(Bool())
    val is_sqrt2x = Output(Bool())

    val ctrl_s3 = new Ctrl_S3
  }
  val pipeline_s2_s3 = Module(new PipelineReg(new FDIV_S2_S3))
  pipeline_s2_s3.io.flush := io.flush
  pipeline_s2_s3.io.in.valid := iter_out_valid
  pipeline_s2_s3.io.out.ready := io.out.ready
  pipeline_s2_s3.io.in.bits.qa := s2_qa
  pipeline_s2_s3.io.in.bits.a_sig_aligned := s2_a_sig_aligned
  pipeline_s2_s3.io.in.bits.b_sig_aligned := s2_b_sig_aligned
  pipeline_s2_s3.io.in.bits.skip_iter := s2_skip_iter
  pipeline_s2_s3.io.in.bits.is_sqrt := s2_is_sqrt
  pipeline_s2_s3.io.in.bits.is_sqrt2x := s2_is_sqrt2x
  pipeline_s2_s3.io.in.bits.ctrl_s3 := s2_ctrl_s3
  val s3_qa = pipeline_s2_s3.io.out.bits.qa
  val s3_a_sig_aligned = pipeline_s2_s3.io.out.bits.a_sig_aligned
  val s3_b_sig_aligned = pipeline_s2_s3.io.out.bits.b_sig_aligned
  val s3_skip_iter = pipeline_s2_s3.io.out.bits.skip_iter
  val s3_is_sqrt = pipeline_s2_s3.io.out.bits.is_sqrt
  val s3_is_sqrt2x = pipeline_s2_s3.io.out.bits.is_sqrt2x
  val s3_rm = pipeline_s2_s3.io.out.bits.ctrl_s3.rm
  val s3_inv = pipeline_s2_s3.io.out.bits.ctrl_s3.inv
  val s3_exp_diff = pipeline_s2_s3.io.out.bits.ctrl_s3.exp_diff
  val s3_exp_subnormal = pipeline_s2_s3.io.out.bits.ctrl_s3.exp_subnormal
  val s3_res_sign = pipeline_s2_s3.io.out.bits.ctrl_s3.res_sign
  val s3_special_exp = pipeline_s2_s3.io.out.bits.ctrl_s3.special_exp
  val s3_special_sig = pipeline_s2_s3.io.out.bits.ctrl_s3.special_sig
  val s3_special_fflags = pipeline_s2_s3.io.out.bits.ctrl_s3.special_fflags
  val s3_underflow_pre = s3_special_fflags(1)
  val s3_overflow_pre = s3_special_fflags(2)
  iter_out_ready := pipeline_s2_s3.io.in.ready

  /*------------------------------------------------------------------------
  S3 : Goldschmidt Rounding & Normalizing
  *------------------------------------------------------------------------*/
  val qt = Mux(s3_qa.head(1).asBool,
    (s3_qa.head(sigWidth + 2) + 1.U).head(sigWidth + 1) ## 0.U(1.W),
    (s3_qa.head(sigWidth + 3) + 1.U).head(sigWidth + 2)
  )

  val shift_num = Mux(s3_exp_subnormal, (~s3_exp_diff.tail(3)).asUInt + 3.U, 0.U) + s3_qa.head(1)
  val (sig_rounding, sticky) = ShiftRightJam(qt, shift_num)
  val round = sig_rounding(0)
  val guard = sig_rounding(1)

  val rt = (sig_rounding << shift_num)(sigWidth + 1, 0)
  val a = s3_a_sig_aligned ## 0.U(2.W)
  val b = Mux(s3_is_sqrt, rt, s3_b_sig_aligned ## 0.U(2.W))
  val prod = rt * b
  val prod_fixed = Mux(s3_is_sqrt2x,
    prod.head(sigWidth + 1) ## prod.tail(sigWidth + 1).orR,
    prod.tail(1).head(sigWidth + 1) ## prod.tail(sigWidth + 2).orR
  )
  val rmd_is_pos = a > prod_fixed
  val rmd_is_zero = a === prod_fixed
  val rmd_is_neg = !rmd_is_pos & !rmd_is_zero

  val inc = Mux1H(
    Seq(
      (s3_rm === RNE) -> ((rmd_is_pos | (rmd_is_zero & guard)) & round),
      (s3_rm === RMM) -> ((rmd_is_pos | rmd_is_zero) & round),
      (s3_rm === RDN) -> ((round | rmd_is_pos) & s3_res_sign),
      (s3_rm === RUP) -> ((round | rmd_is_pos) & !s3_res_sign),
      false.B -> false.B
    )
  )
  val dec = Mux1H(
    Seq(
      (s3_rm === RDN) -> (!s3_res_sign & rmd_is_neg & !round),
      (s3_rm === RUP) -> (s3_res_sign & rmd_is_neg & !round),
      (s3_rm === RTZ) -> (rmd_is_neg & !round),
      false.B -> false.B
    )
  )

  val sig_trunc = sig_rounding.head(sigWidth + 1)
  val sig_rounded = PriorityMux(
    Seq(
      inc -> (sig_trunc + 1.U),
      dec -> (sig_trunc - 1.U),
      true.B -> sig_trunc
    )
  )

  val inexact = !rmd_is_zero | round
  val cout = (s3_exp_subnormal & sig_rounded.tail(1).head(1).asBool) | sig_rounded.head(1).asBool
  val normal_res_sig = sig_rounded.tail(2)

  val normal_res_exp = Mux(s3_exp_subnormal, 0.U, s3_exp_diff + FloatPoint.expBias(expWidth).U)(expWidth-1, 0)
  val underflow = !normal_res_exp.orR & inexact
  val overflow = normal_res_exp.andR
  val normal_fflags = Cat(0.U(2.W), overflow, underflow, inexact)
  val final_fflags = Mux(s3_skip_iter, s3_special_fflags, normal_fflags)

  // in rmin rmax rminMag
  val noInf = (s3_rm === RTZ || (s3_rm === RDN && !s3_res_sign) || (s3_rm === RUP && s3_res_sign)) && s3_overflow_pre
  val noZero = ((s3_rm === RDN && s3_res_sign) || (s3_rm === RUP && !s3_res_sign)) && s3_underflow_pre
  val specialRmSig = Mux(noInf, Fill(sigWidth-1, 1.U), 1.U((sigWidth - 1).W))
  val specialRmExp = Mux(noInf, FloatPoint.maxNormExp(expWidth).U, 0.U(expWidth.W))

  val res_exp = PriorityMux(
    Seq(
      (noInf || noZero) -> specialRmExp,
      s3_skip_iter -> s3_special_exp,
      (cout && (normal_res_exp =/= FloatPoint.maxNormExp(expWidth).U)) -> (normal_res_exp + 1.U),
      true.B -> normal_res_exp
    )
  )
  val res_sig = PriorityMux(
    Seq(
      (noInf || noZero) -> specialRmSig,
      s3_skip_iter -> s3_special_sig,
      (cout && (normal_res_exp === FloatPoint.maxNormExp(expWidth).U)) -> s3_qa.tail(1).head(sigWidth - 1),
      true.B -> normal_res_sig
    )
  )

  io.in.ready := pipeline_s1_s2.io.in.ready
  io.out.valid := pipeline_s2_s3.io.out.valid
  io.out.bits.result := Cat(Mux(s3_inv, false.B, s3_res_sign), res_exp, res_sig)
  io.out.bits.fflags := final_fflags
}

class GoldSchIterUnit(sigWidth: Int, padding_width: Int = 6) extends Module{
  val io = IO(new Bundle() {
    val in = Flipped(Decoupled(new Bundle() {
      val n = Output(UInt(sigWidth.W))
      val d = Output(UInt(sigWidth.W))
      val r = Output(UInt(sigWidth.W))
      val flush = Output(Bool())
      val sqrt = Output(Bool())
      val sqrt2x = Output(Bool())
    }))
    val q = Decoupled(Output(UInt((sigWidth + padding_width).W)))
  })

  val div_iter_cycle = if(sigWidth > 24) 3 else 2
  val sqrt_iter_cycle = if(sigWidth > 24) 3 else 2
  val cycle = RegInit(0.U(log2Ceil(sqrt_iter_cycle + 1).W))
  val n_reg = Reg(UInt((sigWidth + padding_width + 1).W))
  val d_reg = Reg(UInt((sigWidth + padding_width + 1).W))
  val r_reg = Reg(UInt((sigWidth + padding_width + 1).W))
  val out_valid = Reg(Bool())

  val next_n, next_d, next_r = Wire(UInt((sigWidth + padding_width + 1).W))

  val iter_idle = cycle === 0.U
  val iter_end_pre = Mux(io.in.bits.sqrt, cycle === (sqrt_iter_cycle-1).U, cycle === (div_iter_cycle-1).U)
  val iter_end = Mux(io.in.bits.sqrt, cycle === sqrt_iter_cycle.U, cycle === div_iter_cycle.U)

  when(io.q.ready){
    out_valid := false.B
  }
  when((iter_idle | iter_end) & io.in.fire){
    cycle := 1.U
    n_reg := Mux(io.in.bits.sqrt2x, io.in.bits.n ## 0.U((padding_width+1).W), 0.U(1.W) ## io.in.bits.n ## 0.U(padding_width.W))
    d_reg := Mux(io.in.bits.sqrt2x, io.in.bits.d ## 0.U((padding_width+1).W), 0.U(1.W) ## io.in.bits.d ## 0.U(padding_width.W))
    r_reg := 0.U(1.W) ## io.in.bits.r ## 0.U(padding_width.W)
  }.elsewhen(!iter_idle & io.q.ready){
    when(iter_end){
      cycle := 0.U
    }.otherwise{
      when(iter_end_pre){
        out_valid := true.B
      }
      cycle := cycle + 1.U
      n_reg := next_n
      d_reg := next_d
      r_reg := next_r
    }
  }.otherwise{
    cycle := cycle
    n_reg := n_reg
    d_reg := d_reg
    r_reg := r_reg
  }
  when(io.in.bits.flush){
    cycle := 0.U
    out_valid := false.B
  }
  def fixpoint_mul(a: UInt, b: UInt): UInt = {
    val width = sigWidth + padding_width + 1
    val prod = (a * b)(width * 2 - 1, 0)
    val res = prod.tail(2).head(width - 1) ## prod.tail(width + 1).orR
    res
  }
  val nr = fixpoint_mul(n_reg, r_reg)
  val sq_r = fixpoint_mul(r_reg, r_reg)
  val dr = fixpoint_mul(d_reg, Mux(io.in.bits.sqrt, sq_r, r_reg))

  val cf = (~Mux(io.in.bits.sqrt,
    (dr.head(2) + 1.U) ## dr.tail(2).head(sigWidth + padding_width - 2),
    dr.tail(1)
  )).asUInt + 1.U
  next_n := nr
  next_d := dr
  next_r := 0.U(1.W) ## cf

  io.in.ready := (iter_idle | iter_end) & io.q.ready
  io.q.valid := out_valid
  io.q.bits := next_n.tail(1)
}

//object GoldSchIterUnit extends App {
//  emitVerilog(new GoldSchIterUnit(24), Array("--target-dir", "generated"))
//}

//object FDIV extends App {
//  emitVerilog(new FDIV(8, 24), Array("--target-dir", "generated"))
//}
