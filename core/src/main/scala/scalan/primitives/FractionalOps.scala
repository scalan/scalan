package scalan.primitives

import scalan.staged.BaseExp
import scalan.{Scalan, ScalanSeq, ScalanStaged}


trait FractionalOps {
  self: Scalan =>

  implicit class FractionalOpsCls[T](lhs: Rep[T])(implicit f: Fractional[T], et: Elem[T]) {
    def /(rhs: Rep[T]): Rep[T] = fractional_divide(lhs, rhs)
  }

  implicit class IntegralOpsCls[T](lhs: Rep[T])(implicit i: scala.Integral[T], eT: Elem[T]) {
    def %(rhs: Rep[T]): Rep[T] = fractional_mod(lhs, rhs)
  }

  def fractional_divide[T](lhs: Rep[T], rhs: Rep[T])(implicit f: Fractional[T], et: Elem[T]): Rep[T]

  def fractional_mod[T](lhs: Rep[T], rhs: Rep[T])(implicit i: scala.Integral[T], et: Elem[T]): Rep[T]
}

trait FractionalOpsSeq extends FractionalOps { self: ScalanSeq =>
  def fractional_divide[T](lhs: Rep[T], rhs: Rep[T])(implicit f: Fractional[T], et: Elem[T]): Rep[T] = f.div(lhs, rhs)

  def fractional_mod[T](lhs: Rep[T], rhs: Rep[T])(implicit i: scala.Integral[T], et: Elem[T]): Rep[T] = i.rem(lhs, rhs)
}

trait FractionalOpsExp extends FractionalOps with BaseExp { self: ScalanStaged =>
  abstract class FractionalBinOp[T](val opName: String)(implicit elem: Elem[T], val fractional: Fractional[T]) extends EndoBinOp[T]

  case class FractionalDivide[T](lhs: Exp[T], rhs: Exp[T], implicit override val fractional: Fractional[T])(implicit elem: Elem[T]) extends FractionalBinOp[T]("/") {
    def copyWith(l: Rep[T], r: Rep[T]) = this.copy(lhs = l, rhs = r)
  }

  case class FractionalMod[T](lhs: Exp[T], rhs: Exp[T], implicit val i: scala.Integral[T])(implicit elem: Elem[T]) extends EndoBinOp[T] {
    def opName = "%"
    def copyWith(l: Rep[T], r: Rep[T]) = this.copy(lhs = l, rhs = r)
  }

  def fractional_divide[T](lhs: Exp[T], rhs: Exp[T])(implicit f: Fractional[T], et: Elem[T]): Rep[T] = FractionalDivide(lhs, rhs, f)

  def fractional_mod[T](lhs: Rep[T], rhs: Rep[T])(implicit i: scala.Integral[T], et: Elem[T]): Rep[T] = FractionalMod(lhs, rhs, i)

  override def rewrite[T](s: Exp[T])(implicit eT: LElem[T]): Rep[_] = s match {
    case Def(d) => d match {
      case FractionalDivide(Def(Const(x)), Def(Const(y)), f) => {
        val f1: Fractional[T] = f.asInstanceOf[Fractional[T]]
        Const(f1.div(x, y))(s.elem)
      }
      case _ => super.rewrite(s)
    }
    case _ => super.rewrite(s)
  }

}

