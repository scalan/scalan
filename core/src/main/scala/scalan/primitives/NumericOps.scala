package scalan.primitives

import scalan.staged.{ BaseExp }
import scalan.{ ScalanStaged, Scalan, ScalanSeq }

trait NumericOps { self: Scalan =>
  //  implicit def repNumericToNumericOps[T](x: Rep[T])(implicit n: Numeric[T], et: Elem[T]) = new NumericOpsCls(x)
  //  implicit def repNumericIntOps(x: Rep[Int]) = new NumericIntOpsCls(x)
  //  //implicit def numericToNumericOps[T](x: T)(implicit n: Numeric[T], et: Elem[T]) = new NumericOpsCls(x)
  //
  implicit class NumericOpsCls[T](lhs: Rep[T])(implicit val n: Numeric[T], et: Elem[T]) {
    def +(rhs: Rep[T]) = numeric_plus(lhs, rhs)
    def -(rhs: Rep[T]) = numeric_minus(lhs, rhs)
    def *(rhs: Rep[T]) = numeric_times(lhs, rhs)
    def unary_- = numeric_negate(lhs)
    def toFloat = numeric_toFloat(lhs)
    //TODO def abs = Math.abs(lhs)
  }

  implicit class NumericIntOpsCls[T](lhs: Rep[Int]) {
    def /(rhs: Rep[Int]) = numeric_divInt(lhs, rhs)
    def %(rhs: Rep[Int]) = numeric_modInt(lhs, rhs)
  }

  def numeric_plus[T](lhs: Rep[T], rhs: Rep[T])(implicit n: Numeric[T], et: Elem[T]): Rep[T]
  def numeric_minus[T](lhs: Rep[T], rhs: Rep[T])(implicit n: Numeric[T], et: Elem[T]): Rep[T]
  def numeric_times[T](lhs: Rep[T], rhs: Rep[T])(implicit n: Numeric[T], et: Elem[T]): Rep[T]
  def numeric_divInt(lhs: Rep[Int], rhs: Rep[Int]): Rep[Int]
  def numeric_modInt(lhs: Rep[Int], rhs: Rep[Int]): Rep[Int]
  def numeric_negate[T](x: Rep[T])(implicit n: Numeric[T], et: Elem[T]): Rep[T]
  ////  def numeric_abs[T](x: Rep[T])(implicit n: Numeric[T], et: Elem[T]): Rep[T]
  //
  //  //def numeric_signum[T](x: T)(implicit n: Numeric[T]): Rep[Int]
  def numeric_toFloat[T](lhs: Rep[T])(implicit n: Numeric[T], et: Elem[T]): Rep[Float]
}

trait NumericOpsSeq extends NumericOps { self: ScalanSeq =>
  def numeric_plus[T](lhs: Rep[T], rhs: Rep[T])(implicit n: Numeric[T], et: Elem[T]): Rep[T] = n.plus(lhs, rhs)
  def numeric_minus[T](lhs: Rep[T], rhs: Rep[T])(implicit n: Numeric[T], et: Elem[T]): Rep[T] = n.minus(lhs, rhs)
  def numeric_times[T](lhs: Rep[T], rhs: Rep[T])(implicit n: Numeric[T], et: Elem[T]): Rep[T] = n.times(lhs, rhs)
  def numeric_divInt(lhs: Rep[Int], rhs: Rep[Int]): Rep[Int] = lhs / rhs
  def numeric_modInt(lhs: Rep[Int], rhs: Rep[Int]): Rep[Int] = lhs % rhs
  def numeric_negate[T](x: Rep[T])(implicit n: Numeric[T], et: Elem[T]): Rep[T] = n.negate(x)
  //  def numeric_abs[T](x: Rep[T])(implicit n: Numeric[T], et: Elem[T]): Rep[T] = n.abs(x)
  //  //def numeric_signum[T](x: T)(implicit n: Numeric[T]): Rep[Int]
  def numeric_toFloat[T](lhs: Rep[T])(implicit n: Numeric[T], et: Elem[T]): Rep[Float] = n.toFloat(lhs)
}

trait NumericOpsExp extends NumericOps with BaseExp { self: ScalanStaged =>
  abstract class NumericBinOp[T](val opName: String)(implicit val selfType: Elem[T], val numeric: Numeric[T]) extends BinOp[T]

  case class NumericPlus[T: Elem](lhs: Exp[T], rhs: Exp[T], implicit val n: Numeric[T]) extends NumericBinOp[T]("+") {
    def copyWith(l: Rep[T], r: Rep[T]) = this.copy(lhs = l, rhs = r)
  }
  case class NumericMinus[T: Elem](lhs: Exp[T], rhs: Exp[T], implicit val n: Numeric[T]) extends NumericBinOp[T]("-") {
    def copyWith(l: Rep[T], r: Rep[T]) = this.copy(lhs = l, rhs = r)
  }
  case class NumericTimes[T: Elem](lhs: Exp[T], rhs: Exp[T], implicit val n: Numeric[T]) extends NumericBinOp[T]("*") {
    def copyWith(l: Rep[T], r: Rep[T]) = this.copy(lhs = l, rhs = r)
  }
  case class NumericDiv[T: Elem](lhs: Exp[T], rhs: Exp[T], implicit val n: Fractional[T]) extends NumericBinOp[T]("/") {
    def copyWith(l: Rep[T], r: Rep[T]) = this.copy(lhs = l, rhs = r)
  }
  case class NumericDivInt(lhs: Exp[Int], rhs: Exp[Int]) extends BinOp[Int] {
    val selfType = element[Int]
    def copyWith(l: Rep[Int], r: Rep[Int]) = this.copy(lhs = l, rhs = r)
    def opName = "/"
  }
  case class NumericModInt(lhs: Exp[Int], rhs: Exp[Int]) extends BinOp[Int] {
    val selfType = element[Int]
    def copyWith(l: Rep[Int], r: Rep[Int]) = this.copy(lhs = l, rhs = r)
    def opName = "%"
  }

  abstract class NumericUnOp[T](val opName: String)(implicit val selfType: Elem[T], val numeric: Numeric[T]) extends UnOp[T] {
  }
  case class NumericNegate[T: Elem](arg: Exp[T], implicit val n: Numeric[T]) extends NumericUnOp[T]("-") {
    def copyWith(a: Rep[T]) = this.copy(arg = a)
  }

  //  case class NumericAbs[T:Elem](arg: Exp[T], implicit val n: Numeric[T]) extends NumericUnOp[T]("Abs") {
  //     def copyWith(a: Rep[T]) = this.copy(arg = a)
  //  }

  case class NumericToFloat[T: Elem](arg: Exp[T], implicit val n: Numeric[T]) extends Def[Float] with UnOpBase[T, Float] {
    val selfType = element[Float]
    def copyWith(a: Rep[T]) = this.copy(arg = a)
    override def mirror(t: Transformer) = {
      implicit val eT = arg.elem
      copyWith(t(arg))
    }
    def opName = "ToFloat"
  }

  def numeric_plus[T](lhs: Exp[T], rhs: Exp[T])(implicit n: Numeric[T], et: Elem[T]): Rep[T] = NumericPlus(lhs, rhs, n)
  def numeric_minus[T](lhs: Exp[T], rhs: Exp[T])(implicit n: Numeric[T], et: Elem[T]): Rep[T] = NumericMinus(lhs, rhs, n)
  def numeric_times[T](lhs: Exp[T], rhs: Exp[T])(implicit n: Numeric[T], et: Elem[T]): Rep[T] = NumericTimes(lhs, rhs, n)
  def numeric_divInt(lhs: Rep[Int], rhs: Rep[Int]): Rep[Int] = NumericDivInt(lhs, rhs)
  def numeric_modInt(lhs: Rep[Int], rhs: Rep[Int]): Rep[Int] = NumericModInt(lhs, rhs)
  def numeric_toFloat[T](lhs: Rep[T])(implicit n: Numeric[T], et: Elem[T]): Rep[Float] = NumericToFloat(lhs, n)
  def numeric_negate[T](x: Rep[T])(implicit n: Numeric[T], et: Elem[T]): Rep[T] = NumericNegate(x, n)
  //  def numeric_abs[T](x: Rep[T])(implicit n: Numeric[T], et: Elem[T]): Rep[T] = NumericAbs(x, n)
  
  private def isZero[T](x: T, n: Numeric[T]) = x == n.zero
  private def isOne[T](x: T, n: Numeric[T]) = x == n.fromInt(1)
  
  override def rewrite[T](d: Exp[T])(implicit eT: LElem[T]) = d match {
    case Def(d1) => d1 match {
      // constant propagation
      case NumericPlus(Def(Const(x)), Def(Const(y)), n: Numeric[T] @unchecked) => Const(n.plus(x, y))
      case NumericMinus(Def(Const(x)), Def(Const(y)), n: Numeric[T] @unchecked) => Const(n.minus(x, y))
      case NumericTimes(Def(Const(x)), Def(Const(y)), n: Numeric[T] @unchecked) => Const(n.times(x, y))
      case NumericDiv(Def(Const(x)), Def(Const(y)), n: Fractional[T] @unchecked) => Const(n.div(x, y))
      case NumericDivInt(Def(Const(x)), Def(Const(y))) => toRep(x / y)(d1.selfType)
      // single constant propagation
      case NumericPlus(x, Def(Const(zero)), n: Numeric[T] @unchecked) if isZero(zero, n) => x
      case NumericPlus(Def(Const(zero)), x, n: Numeric[T] @unchecked) if isZero(zero, n) => x
      case NumericMinus(x, Def(Const(zero)), n: Numeric[T] @unchecked) if isZero(zero, n) => x
      case NumericMinus(Def(Const(zero)), x, n: Numeric[T] @unchecked) if isZero(zero, n) =>
        numeric_negate(x)(n, d1.selfType)
      case NumericTimes(_, y @ Def(Const(zero)), n: Numeric[T] @unchecked) if isZero(zero, n) => y
      case NumericTimes(y @ Def(Const(zero)), _, n: Numeric[T] @unchecked) if isZero(zero, n) => y
      case NumericTimes(x, Def(Const(one)), n: Numeric[T] @unchecked) if isOne(one, n) => x
      case NumericTimes(Def(Const(one)), x, n: Numeric[T] @unchecked) if isOne(one, n) => x
      case NumericDiv(x @ Def(Const(zero)), _, n: Numeric[T] @unchecked) if isZero(zero, n) => x
      case NumericDiv(x, Def(Const(one)), n: Numeric[T] @unchecked) if isOne(one, n) => x
      case NumericDivInt(x @ Def(Const(0)), _) => x
      case NumericDivInt(x, Def(Const(1))) => x
      case _ => super.rewrite(d)
    }
    case _ => super.rewrite(d)
  }
}

