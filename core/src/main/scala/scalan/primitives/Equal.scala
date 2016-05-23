package scalan.primitives

import scalan.staged.BaseExp
import scalan.{Scalan, ScalanExp}

trait Equal extends UnBinOps { self: Scalan =>
  case class Equals[A]() extends BinOp[A, Boolean]("==", _ == _)

  implicit class EqualOps[A](x: Rep[A]) {
    def ===(y: Rep[A]): Rep[Boolean] = Equals[A].apply(x, y)
    def !==(y: Rep[A]): Rep[Boolean] = !Equals[A].apply(x, y)
  }
}

trait EqualExp extends Equal with BaseExp { self: ScalanExp =>
  override def rewriteDef[T](d: Def[T]) = d match {
    case ApplyBinOp(_: Equals[_], x, y) if x == y => true
    case ApplyBinOp(_: Equals[_], x, Def(Const(b: Boolean))) if x.elem == BooleanElement =>
      if (b) x else !x.asRep[Boolean]
    case ApplyBinOp(_: Equals[_], Def(Const(b: Boolean)), x) if x.elem == BooleanElement =>
      if (b) x else !x.asRep[Boolean]
    case _ => super.rewriteDef(d)
  }
}
