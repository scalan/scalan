package scalan.primitives

import scala.util.Random
import scalan.common.IdSupply
import scalan.staged.BaseExp
import scalan.{ ScalanExp }

trait NumericOpsExp extends BaseExp { self: ScalanExp =>
  implicit class NumericOpsCls[T](x: Rep[T])(implicit val n: Numeric[T]) {
    def +(y: Rep[T]) = NumericPlus(n)(x.elem).apply(x, y)
    def -(y: Rep[T]) = NumericMinus(n)(x.elem).apply(x, y)
    def *(y: Rep[T]) = NumericTimes(n)(x.elem).apply(x, y)
    def unary_- = NumericNegate(n)(x.elem).apply(x)
    def abs = Math.abs(x)
    def toFloat = NumericToFloat(n).apply(x)
    def toDouble = NumericToDouble(n).apply(x)
    def toInt = NumericToInt(n).apply(x)
    def toLong = NumericToLong(n).apply(x)
    def ceil = Math.ceil(toDouble)
    def floor = Math.floor(toDouble)
  }

  implicit class FractionalOpsCls[T](x: Rep[T])(implicit f: Fractional[T]) {
    def /(y: Rep[T]): Rep[T] = FractionalDivide(f)(x.elem).apply(x, y)
  }

  implicit class IntegralOpsCls[T](x: Rep[T])(implicit i: Integral[T]) {
    def div(y: Rep[T]): Rep[T] = IntegralDivide(i)(x.elem).apply(x, y)
    def mod(y: Rep[T]): Rep[T] = IntegralMod(i)(x.elem).apply(x, y)
    // avoid / due to conflicts
    def /!(y: Rep[T]): Rep[T] = div(y)
    def %(y: Rep[T]): Rep[T] = mod(y)
  }

  case class NumericPlus[T: Elem](n: Numeric[T]) extends EndoBinOp[T]("+", n.plus)

  case class NumericMinus[T: Elem](n: Numeric[T]) extends EndoBinOp[T]("-", n.minus)

  case class NumericTimes[T: Elem](n: Numeric[T]) extends EndoBinOp[T]("*", n.times)

  class DivOp[T: Elem](opName: String, applySeq: (T, T) => T, n: Numeric[T]) extends EndoBinOp[T](opName, applySeq) {
    override def shouldPropagate(lhs: T, rhs: T) = rhs != n.zero
  }

  case class NumericNegate[T: Elem](n: Numeric[T]) extends UnOp[T, T]("-", n.negate)

  case class NumericToDouble[T](n: Numeric[T]) extends UnOp[T,Double]("ToDouble", n.toDouble)

  case class NumericToFloat[T](n: Numeric[T]) extends UnOp[T, Float]("ToFloat", n.toFloat)

  case class NumericToInt[T](n: Numeric[T]) extends UnOp[T,Int]("ToInt", n.toInt)

  case class NumericToLong[T](n: Numeric[T]) extends UnOp[T,Long]("ToLong", n.toLong)

  case class FractionalDivide[T](f: Fractional[T])(implicit elem: Elem[T]) extends DivOp[T]("/", f.div, f)

  case class IntegralDivide[T](i: Integral[T])(implicit elem: Elem[T]) extends DivOp[T]("/", i.quot, i)

  case class IntegralMod[T](i: Integral[T])(implicit elem: Elem[T]) extends DivOp[T]("%", i.rem, i)

  case class NumericRand[T](bound: Exp[T], id: Int = IdSupply.nextId)(implicit val eT: Elem[T]) extends BaseDef[T]

  override def transformDef[A](d: Def[A], t: Transformer) = d match {
    case NumericRand(bound, _) => NumericRand(t(bound))(d.selfType)
    case _ => super.transformDef(d, t)
  }

  def random[T](bound: Rep[T])(implicit n: Numeric[T]): Rep[T] =
    NumericRand(bound)(bound.elem)

  private def isZero[T](x: T, n: Numeric[T]) = x == n.zero
  private def isOne[T](x: T, n: Numeric[T]) = x == n.fromInt(1)
  
  override def rewriteDef[T](d: Def[T]) = d match {
    // scala has problems with type inference here
    // cast to BinOp[a, a] is safe because all matched cases have this type
    case ApplyBinOp(op, x: Exp[a], y) => (op.asInstanceOf[BinOp[a, a]], x.asInstanceOf[Exp[a]], y.asInstanceOf[Exp[a]]) match {
      // x + 0 => x
      case (NumericPlus(n), x, Def(Const(zero))) if isZero(zero, n) => x
      // 0 + x => x
      case (NumericPlus(n), Def(Const(zero)), x) if isZero(zero, n) => x
      // x - 0 => x
      case (NumericMinus(n), x, Def(Const(zero))) if isZero(zero, n) => x
      // 0 - x => -x
      case (NumericMinus(n), Def(Const(zero)), x) if isZero(zero, n) =>
        new NumericOpsCls(x)(n).unary_-
      // _ * 0 => 0
      case (NumericTimes(n), _, y@Def(Const(zero))) if isZero(zero, n) => y
      // 0 * _ => 0
      case (NumericTimes(n), y@Def(Const(zero)), _) if isZero(zero, n) => y
      // x * 1 => x
      case (NumericTimes(n), x, Def(Const(one))) if isOne(one, n) => x
      // 1 * x => x
      case (NumericTimes(n), Def(Const(one)), x) if isOne(one, n) => x
      // 0 / _ => 0
      case (FractionalDivide(n), x@Def(Const(zero)), _) if isZero(zero, n) => x
      // x / 1 => x
      case (FractionalDivide(n), x, Def(Const(one))) if isOne(one, n) => x
      // 0 / _ => 0 (for ints)
      case (IntegralDivide(n), x@Def(Const(zero)), _) if isZero(zero, n) => x
      // x / 1 => x (for ints)
      case (IntegralDivide(n), x, Def(Const(one))) if isOne(one, n) => x
      case _ => super.rewriteDef(d)
    }
    // -(-x) => x
    case ApplyUnOp(_: NumericNegate[_], Def(ApplyUnOp(_: NumericNegate[_], x))) => x
    // (x: Int).toInt => x
    case ApplyUnOp(NumericToInt(_), x) if x.elem == IntElement => x
    // (x: Long).toLong => x
    case ApplyUnOp(NumericToLong(_), x) if x.elem == LongElement => x
    // (x: Float).toFloat => x
    case ApplyUnOp(NumericToFloat(_), x) if x.elem == FloatElement => x
    // (x: Double).toDouble => x
    case ApplyUnOp(NumericToDouble(_), x) if x.elem == DoubleElement => x
    case _ => super.rewriteDef(d)
  }
}
