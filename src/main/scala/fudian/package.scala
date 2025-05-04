import chisel3._
import chisel3.util._

package object fudian {

  def RNE: UInt = 0.U(3.W)
  def RTZ: UInt = 1.U(3.W)
  def RDN: UInt = 2.U(3.W)
  def RUP: UInt = 3.U(3.W)
  def RMM: UInt = 4.U(3.W)

  class FPDecodeBundle extends Bundle {
    val expNotZero = Bool()
    val expIsZero = Bool()
    val expIsOnes = Bool()
    val sigNotZero = Bool()
    val sigIsZero = Bool()
    val isSubnormal = Bool()
    val isInf = Bool()
    val isZero = Bool()
    val isNaN = Bool()
    val isSNaN = Bool()
    val isQNaN = Bool()
  }

  class FloatPoint(val expWidth: Int, val sigWidth: Int) extends Bundle {
    val sign = Bool()
    val exp = UInt(expWidth.W)
    val sig = UInt((sigWidth - 1).W)
    def decode: FPDecodeBundle = {
      val expNotZero = exp.orR
      val expIsOnes = exp.andR
      val sigNotZero = sig.orR
      val bundle = Wire(new FPDecodeBundle)
      bundle.expNotZero := expNotZero
      bundle.expIsZero := !expNotZero
      bundle.expIsOnes := expIsOnes
      bundle.sigNotZero := sigNotZero
      bundle.sigIsZero := !sigNotZero
      bundle.isSubnormal := bundle.expIsZero && sigNotZero
      bundle.isInf := bundle.expIsOnes && bundle.sigIsZero
      bundle.isZero := bundle.expIsZero && bundle.sigIsZero
      bundle.isNaN := bundle.expIsOnes && bundle.sigNotZero
      bundle.isSNaN := bundle.isNaN && !sig.head(1).asBool
      bundle.isQNaN := bundle.isNaN && sig.head(1).asBool
      bundle
    }
  }
  object FloatPoint {
    def expBias(expWidth: Int): BigInt = {
      (BigInt(1) << (expWidth - 1)) - 1
    }
    def maxNormExp(expWidth: Int): BigInt = {
      (BigInt(1) << expWidth) - 2
    }
    def fromUInt(x: UInt, expWidth: Int, sigWidth: Int): FloatPoint = {
      val fp = Wire(new FloatPoint(expWidth, sigWidth))
      fp.sign := x(expWidth + sigWidth - 1)
      fp.exp := x(expWidth + sigWidth - 2, sigWidth - 1)
      fp.sig := x(sigWidth - 2, 0)
      fp
    }
    //    def defaultNaNUInt(expWidth: Int, pc: Int): UInt = {
    //      Cat(0.U(1.W), Fill(expWidth + 1, 1.U(1.W)), 0.U((pc - 2).W))
    //    }
    //    def defaultNaN(expWidth: Int, pc: Int): FloatPoint = {
    //      fromUInt(defaultNaNUInt(expWidth, pc), expWidth, pc)
    //    }
  }

  class RawFloat(val expWidth: Int, val sigWidth: Int) extends Bundle {
    val sign = Bool()
    val exp = UInt(expWidth.W)
    val sig = UInt(sigWidth.W)
  }

  object RawFloat {
    def fromFP(fp: FloatPoint, expNotZero: Option[Bool] = None, isSubnormal: Option[Bool] = None): RawFloat = {
      val inner = Wire(new RawFloat(fp.expWidth, fp.sigWidth))
      val expNZ = if (expNotZero.isDefined) expNotZero.get else fp.exp.orR
      val isSub = if (isSubnormal.isDefined) isSubnormal.get else fp.sig.orR & (!fp.exp.orR)
      inner.sign := fp.sign
      inner.exp := Mux(isSub, fp.exp + 1.U, fp.exp)
      inner.sig := Cat(expNZ, fp.sig)
      inner
    }

    def fromUInt(x: UInt, expWidth: Int, sigWidth: Int): RawFloat = {
      val fp = FloatPoint.fromUInt(x, expWidth, sigWidth)
      val raw = fromFP(fp)
      raw
    }
  }

  class FPDataIn(val expWidth: Int, val sigWidth: Int, val nelem: Int) extends Bundle {
    val src = Output(Vec(nelem, UInt((expWidth + sigWidth).W)))
    val rm = Output(UInt(3.W))
  }

  class FPDataOut(val expWidth: Int, val sigWidth: Int) extends Bundle {
    val fflags = Output(UInt(5.W))
    val result = Output(UInt((expWidth + sigWidth).W))
  }

  class PipelineReg[T <: Data](gen: T) extends Module {
    val io = IO(new Bundle {
      val in  = Flipped(Decoupled(gen))
      val out = Decoupled(gen)
      val flush = Input(Bool())
    })
    val validReg = RegInit(false.B)
    val dataReg  = Reg(gen)

    when(io.out.ready) {
      validReg := false.B
    }
    when(io.in.fire) {
      validReg := true.B
      dataReg  := io.in.bits
    }
    when(io.flush){
      validReg := false.B
    }
    io.in.ready := !validReg || io.out.ready
    io.out.valid := validReg
    io.out.bits  := dataReg
  }
}