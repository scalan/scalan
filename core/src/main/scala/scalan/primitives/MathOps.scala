package scalan.primitives

import scalan.staged.BaseExp
import scalan.{ScalanStaged, ScalanSeq, Scalan}

trait MathOps { self: Scalan =>
  object Math {
    def ceil(x: Rep[Double]) = math_ceil(x)
    def floor(x: Rep[Double]) = math_floor(x)
    def exp(x: Rep[Double]) = math_exp(x)
    def log(x: Rep[Double]) = math_log(x)
    def sqrt(x: Rep[Double]) = math_sqrt(x)
    def sin(x: Rep[Double]) = math_sin(x)
    def cos(x: Rep[Double]) = math_cos(x)
    def acos(x: Rep[Double]) = math_acos(x)
    def atan(x: Rep[Double]) = math_atan(x)
    def atan2(x: Rep[Double], y: Rep[Double]) = math_atan2(x,y)
    def pow(x: Rep[Double], y: Rep[Double]) = math_pow(x,y)
    def tanh(x: Rep[Double]) = math_tanh(x)
    def abs[A](x: Rep[A])(implicit e: Elem[A], n:Numeric[A]) = math_abs(x)
    def max[A](x: Rep[A], y: Rep[A])(implicit e: Elem[A], n:Ordering[A]) = ordering_max(x,y)
    def min[A](x: Rep[A], y: Rep[A])(implicit e: Elem[A], n:Ordering[A]) = ordering_min(x,y)
    def Pi = math_pi
    def E = math_e
  }
  def math_ceil(x: Rep[Double]) : Rep[Double]
  def math_floor(x: Rep[Double]) : Rep[Double]
  def math_exp(x: Rep[Double]) : Rep[Double]
  def math_log(x: Rep[Double]) : Rep[Double]
  def math_sqrt(x: Rep[Double]) : Rep[Double]
  def math_sin(x: Rep[Double]) : Rep[Double]
  def math_cos(x: Rep[Double]) : Rep[Double]
  def math_acos(x: Rep[Double]) : Rep[Double]
  def math_atan(x: Rep[Double]) : Rep[Double]
  def math_atan2(x: Rep[Double], y: Rep[Double]) : Rep[Double]
  def math_pow(x: Rep[Double], y: Rep[Double]): Rep[Double]
  def math_tanh(x: Rep[Double]): Rep[Double]
  def math_abs[A](x: Rep[A])(implicit e: Elem[A], n:Numeric[A]): Rep[A]
  def math_pi: Rep[Double]
  def math_e: Rep[Double]
}

trait MathOpsSeq extends MathOps { self: ScalanSeq =>
  override def math_ceil(x: Rep[Double]) : Rep[Double] = math.ceil(x)
  override def math_floor(x: Rep[Double]) : Rep[Double] = math.floor(x)
  override def math_exp(x: Rep[Double]) : Rep[Double] = math.exp(x)
  override def math_log(x: Rep[Double]) : Rep[Double] = math.log(x)
  override def math_sqrt(x: Rep[Double]) : Rep[Double] = math.sqrt(x)
  override def math_sin(x: Rep[Double]) : Rep[Double] = math.sin(x)
  override def math_cos(x: Rep[Double]) : Rep[Double] = math.cos(x)
  override def math_acos(x: Rep[Double]) : Rep[Double] = math.acos(x)
  override def math_atan(x: Rep[Double]) : Rep[Double] = math.atan(x)
  override def math_atan2(x: Rep[Double], y: Rep[Double]) : Rep[Double] = math.atan2(x, y)
  override def math_pow(x: Rep[Double], y: Rep[Double]): Rep[Double] = math.pow(x, y)
  override def math_tanh(x: Rep[Double]): Rep[Double] = math.tanh(x)
  override def math_abs[A](x: Rep[A])(implicit e: Elem[A], n:Numeric[A]) : Rep[A] = n.abs(x)
  override def math_pi: Rep[Double] = math.Pi
  override def math_e: Rep[Double] = math.E
}

trait MathOpsExp extends MathOps with BaseExp { self: ScalanStaged =>
  abstract class UnDoubleOp(val opName: String) extends EndoUnOp[Double]

  abstract class BinDoubleOp(val opName: String) extends EndoBinOp[Double]

  case class MathCeil(arg: Rep[Double]) extends UnDoubleOp("Ceil") {
    def copyWith(a: Rep[Double]) = this.copy(arg = a)
  }
  case class MathFloor(arg: Rep[Double]) extends UnDoubleOp("Floor") {
    def copyWith(a: Rep[Double]) = this.copy(arg = a)
  }
  case class MathExp(arg: Rep[Double]) extends UnDoubleOp("Exp") {
    def copyWith(a: Rep[Double]) = this.copy(arg = a)
  }
  case class MathLog(arg: Rep[Double]) extends UnDoubleOp("Log") {
    def copyWith(a: Rep[Double]) = this.copy(arg = a)
  }
  case class MathSqrt(arg: Rep[Double]) extends UnDoubleOp("Sqrt") {
    def copyWith(a: Rep[Double]) = this.copy(arg = a)
  }
  case class MathSin(arg: Rep[Double]) extends UnDoubleOp("Sin") {
    def copyWith(a: Rep[Double]) = this.copy(arg = a)
  }
  case class MathCos(arg: Rep[Double]) extends UnDoubleOp("Cos") {
    def copyWith(a: Rep[Double]) = this.copy(arg = a)
  }
  case class MathAcos(arg: Rep[Double]) extends UnDoubleOp("Acos") {
    def copyWith(a: Rep[Double]) = this.copy(arg = a)
  }
  case class MathAtan(arg: Rep[Double]) extends UnDoubleOp("Atan") {
    def copyWith(a: Rep[Double]) = this.copy(arg = a)
  }
  case class MathAtan2(lhs: Rep[Double], rhs: Rep[Double]) extends BinDoubleOp("Atan2") {
    def copyWith(lhs: Rep[Double], rhs: Rep[Double]) = this.copy(lhs, rhs)
  }
  case class MathPow(lhs: Rep[Double], rhs: Rep[Double]) extends BinDoubleOp("Pow") {
    def copyWith(lhs: Rep[Double], rhs: Rep[Double]) = this.copy(lhs, rhs)
  }
  case class MathTanh(arg: Rep[Double]) extends UnDoubleOp("Tanh") {
    def copyWith(a: Rep[Double]) = this.copy(arg = a)
  }
  case class MathAbs[T:Elem](arg: Exp[T], implicit val n: Numeric[T]) extends NumericUnOp[T]("Abs") {
    def copyWith(a: Rep[T]) = this.copy(arg = a)
  }
  override def math_ceil(x: Rep[Double]) : Rep[Double] = MathCeil(x)
  override def math_floor(x: Rep[Double]) : Rep[Double] = MathFloor(x)
  override def math_exp(x: Rep[Double]) : Rep[Double] = MathExp(x)
  override def math_log(x: Rep[Double]) : Rep[Double] = MathLog(x)
  override def math_sqrt(x: Rep[Double]) : Rep[Double] = MathSqrt(x)
  override def math_sin(x: Rep[Double]) : Rep[Double] = MathSin(x)
  override def math_cos(x: Rep[Double]) : Rep[Double] = MathCos(x)
  override def math_acos(x: Rep[Double]) : Rep[Double] = MathAcos(x)
  override def math_atan(x: Rep[Double]) : Rep[Double] = MathAtan(x)
  override def math_atan2(x: Rep[Double], y: Rep[Double]) : Rep[Double] = MathAtan2(x, y)
  override def math_pow(x: Rep[Double], y: Rep[Double]): Rep[Double] = MathPow(x, y)
  override def math_tanh(x: Rep[Double]): Rep[Double] = MathTanh(x)
  override def math_abs[A](x: Rep[A])(implicit e: Elem[A], n:Numeric[A]) : Rep[A] = MathAbs(x, n)
  override def math_e: Rep[Double] = Const(math.E)
  override def math_pi: Rep[Double] = Const(math.Pi)
}
