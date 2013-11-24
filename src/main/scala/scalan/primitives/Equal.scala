package scalan.primitives

import scalan.staged.{ExpressionsBase}
import scalan.{Scalan, ScalanStaged}
import scalan.ScalanSeq
import scalan.common.OverloadHack

trait Equal extends OverloadHack { self: Scalan =>
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

trait EqualExp extends Equal with ExpressionsBase { self: ScalanStaged =>
  abstract class EqBinOp[T](val opName: String) extends Def[Boolean] with BinOpBase[T, Boolean] {
    val elem = element[Boolean]
    override def mirror(t: Transformer) = {
      implicit val eT = lhs.Elem
      copyWith(t(lhs), t(rhs))
    }
  }

  def equals[A:Elem](a: Rep[A], b: Rep[A]): Rep[Boolean] = EqualsClass(a,b)
  def notequals[A:Elem](a: Rep[A], b: Rep[A]): Rep[Boolean] = NotEqual(a,b)

  //FIXME: these methods bacame ambiguous in Seq implementation when declared in Equal
  //def __equal[A,B](a: Rep[A], b: Rep[B])(implicit o: Overloaded1, mA: Elem[A], mB: Elem[B]) : Rep[Boolean] = ??? //equals(a,b)
  //def __equal[A,B](a: Rep[A], b: B)(implicit o: Overloaded2, mA: Elem[A], mB: Elem[B]): Rep[Boolean] = equals(a, b)
  //def __equal[A,B](a: A, b: Rep[B])(implicit o: Overloaded3, mA: Elem[A], mB: Elem[B]): Rep[Boolean] = equals(toRep(a), b)

  //def infix_!=[A,B](a: Rep[A], b: Rep[B])(implicit o: Overloaded1, mA: Elem[A], mB: Elem[B]) : Rep[Boolean] = notequals(a,b)
  //def infix_!=[A,B](a: Rep[A], b: B)(implicit o: Overloaded2, mA: Elem[A], mB: Elem[B]) : Rep[Boolean] = notequals(a, b)
  //def infix_!=[A,B](a: A, b: Rep[B])(implicit o: Overloaded3, mA: Elem[A], mB: Elem[B]) : Rep[Boolean] = notequals(toRep(a), b)

  override def formatDef(d: Def[_]) = d match {
    case EqualsClass(a, b) => "%s == %s".format(a, b)
    case NotEqual(a, b) => "%s != %s".format(a, b)
    case _ => super.formatDef(d)
  }

  case class EqualsClass[A](lhs: Exp[A], rhs: Exp[A]) extends EqBinOp[A]("===") {
    def copyWith(l: Rep[A], r: Rep[A]) = this.copy(lhs = l, rhs = r)
  }
  case class NotEqual[A](lhs: Exp[A], rhs: Exp[A]) extends EqBinOp[A]("!=="){
    def copyWith(l: Rep[A], r: Rep[A]) = this.copy(lhs = l, rhs = r)
  }

}

