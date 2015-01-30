package scalan.primitives

import scalan.staged.BaseExp
import scalan.{Scalan, ScalanExp}

trait Equal extends UnBinOps { self: Scalan =>
  case class HashCode[A]() extends UnOp[A, Int]("hashCode", _.hashCode)

  case class Equals[A]() extends BinOp[A, Boolean]("==", _ == _)

  implicit class EqualOps[A](x: Rep[A]) {
    def ===(y: Rep[A]): Rep[Boolean] = Equals[A].apply(x, y)
    def !==(y: Rep[A]): Rep[Boolean] = !Equals[A].apply(x, y)
    def hashcode: Rep[Int] = HashCode[A].apply(x)
  }
}

trait EqualExp extends Equal with BaseExp { self: ScalanExp =>
  override def rewriteDef[T](d: Def[T]) = d match {
    case ApplyBinOp(_: Equals[_], x, y) if x == y => true
    case _ => super.rewriteDef(d)
  }
}
