package scalan.primitives

import scalan.staged.BaseExp
import scalan.{Scalan, ScalanExp}

trait UniversalOps extends UnBinOps { self: Scalan =>
  case class HashCode[A]() extends UnOp[A, Int]("hashCode", _.hashCode)

  case class ToString[A]() extends UnOp[A, String]("toString", _.toString)

  implicit class RepUniversalOps[A](x: Rep[A]) {
    def hashCodeRep: Rep[Int] = HashCode[A]().apply(x)
    def toStringRep = ToString[A]().apply(x)
  }
}

trait UniversalOpsExp extends UniversalOps with BaseExp { self: ScalanExp =>
  override def rewriteDef[T](d: Def[T]) = d match {
    case ApplyUnOp(ToString(), x) if x.elem == StringElement => x
    case _ => super.rewriteDef(d)
  }
}
