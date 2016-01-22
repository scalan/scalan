package scalan.primitives

import scala.util.Random
import scalan.common.IdSupply
import scalan.staged.BaseExp
import scalan.{ ScalanExp, Scalan, ScalanStd }

trait NumericOps { self: Scalan =>
  implicit class NumericOpsCls[T](x: Rep[T])(implicit val n: Numeric[T], et: Elem[T]) {
    def +(y: Rep[T]) = NumericPlus(n).apply(x, y)
    def -(y: Rep[T]) = NumericMinus(n).apply(x, y)
    def *(y: Rep[T]) = NumericTimes(n).apply(x, y)
    def unary_- = NumericNegate(n).apply(x)
    def abs = Math.abs(x)
    def toFloat = NumericToFloat(n).apply(x)
    def toDouble = NumericToDouble(n).apply(x)
    def toInt = NumericToInt(n).apply(x)
    def toStr:Rep[String] = NumericToString().apply(x)
    def toLong = NumericToLong(n).apply(x)
    def ceil = Math.ceil(toDouble)
    def floor = Math.floor(toDouble)
  }

  implicit class FractionalOpsCls[T](x: Rep[T])(implicit f: Fractional[T], et: Elem[T]) {
    def /(y: Rep[T]): Rep[T] = FractionalDivide(f).apply(x, y)
  }

  implicit class IntegralOpsCls[T](x: Rep[T])(implicit i: Integral[T], eT: Elem[T]) {
    def div(y: Rep[T]): Rep[T] = IntegralDivide(i).apply(x, y)
    def mod(y: Rep[T]): Rep[T] = IntegralMod(i).apply(x, y)
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

  case class NumericToString[T]() extends UnOp[T,String]("ToString", _.toString)

  case class FractionalDivide[T](f: Fractional[T])(implicit elem: Elem[T]) extends DivOp[T]("/", f.div, f)

  case class IntegralDivide[T](i: Integral[T])(implicit elem: Elem[T]) extends DivOp[T]("/", i.quot, i)

  case class IntegralMod[T](i: Integral[T])(implicit elem: Elem[T]) extends DivOp[T]("%", i.rem, i)

  def random[T](bound: Rep[T])(implicit n: Numeric[T], et: Elem[T]): Rep[T]
}

trait NumericOpsStd extends NumericOps { self: ScalanStd =>
  def random[T](bound: Rep[T])(implicit n: Numeric[T], et: Elem[T]): Rep[T] = {
    (et match {
      case IntElement => Random.nextInt(bound.asInstanceOf[Int])
      case DoubleElement => Random.nextDouble() * bound.asInstanceOf[Double]
      case _ => ???(s"random not implemented for $et")
    }).asInstanceOf[Rep[T]]
  }
}

trait NumericOpsExp extends NumericOps with BaseExp { self: ScalanExp =>
  case class NumericRand[T](bound: Exp[T], id: Int = IdSupply.nextId)(implicit eT: Elem[T]) extends BaseDef[T]

  override def transformDef[A](d: Def[A], t: Transformer) = d match {
    case NumericRand(bound, _) => NumericRand(t(bound))(d.selfType)
    case _ => super.transformDef(d, t)
  }

  def random[T](bound: Rep[T])(implicit n: Numeric[T], et: Elem[T]): Rep[T] =
    NumericRand(bound)(et)

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
        new NumericOpsCls(x)(n, d.selfType.asElem[a]).unary_-
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
    case ApplyUnOp(op1, Def(ApplyUnOp(op2, x))) if
      op1.isInstanceOf[NumericNegate[_]] && op2.isInstanceOf[NumericNegate[_]] => x
    case _ => super.rewriteDef(d)
  }
}
