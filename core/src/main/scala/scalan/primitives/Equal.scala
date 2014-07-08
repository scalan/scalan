package scalan.primitives

import scalan.staged.{BaseExp}
import scalan.{Scalan, ScalanStaged}
import scalan.ScalanSeq
import scalan.common.OverloadHack

trait Equal { self: Scalan =>
  def equals[A:Elem](a: Rep[A], b: Rep[A]) : Rep[Boolean]
  def notequals[A:Elem](a: Rep[A], b: Rep[A]) : Rep[Boolean]

  implicit class EqualOps[A: Elem](x: Rep[A]) {
    def ===(y: Rep[A]): Rep[Boolean] = self.equals(x, y)
    def !==(y: Rep[A]): Rep[Boolean] = self.notequals(x, y)
  }
  //implicit def extendWithEquals[A: Elem](x: Rep[A]): EqualOps[A] = new EqualOps(x)
}

trait EqualSeq extends Equal  { self: ScalanSeq =>
  def equals[A:Elem](a: Rep[A], b: Rep[A]): Rep[Boolean] = a equals b
  def notequals[A:Elem](a: Rep[A], b: Rep[A]): Rep[Boolean] = !equals(a,b)
}

trait EqualExp extends Equal with BaseExp { self: ScalanStaged =>
  abstract class EqBinOp[T](val opName: String) extends Def[Boolean] with BinOpBase[T, Boolean] {
    val selfType = element[Boolean]
    lazy val uniqueOpId = name(lhs.elem)
    override def mirror(t: Transformer) = {
      implicit val eT = lhs.elem
      copyWith(t(lhs), t(rhs))
    }
  }

  def equals[A:Elem](a: Rep[A], b: Rep[A]): Rep[Boolean] = EqualsClass(a,b)
  def notequals[A:Elem](a: Rep[A], b: Rep[A]): Rep[Boolean] = NotEqual(a,b)

  case class EqualsClass[A](lhs: Exp[A], rhs: Exp[A]) extends EqBinOp[A]("===") {
    def copyWith(l: Rep[A], r: Rep[A]) = this.copy(lhs = l, rhs = r)
  }
  case class NotEqual[A](lhs: Exp[A], rhs: Exp[A]) extends EqBinOp[A]("!=="){
    def copyWith(l: Rep[A], r: Rep[A]) = this.copy(lhs = l, rhs = r)
  }

}

