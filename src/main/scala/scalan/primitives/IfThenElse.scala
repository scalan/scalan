package scalan.primitives

import scalan.staged.{ExpressionsBase}
import scalan.{ScalanStaged, ScalanSeq, Base, Scalan}

trait IfThenElse extends Base { self: Scalan =>
  def __ifThenElse[T](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T]): Rep[T]

  def IF[T](cond: Rep[Boolean])(thenp: => Rep[T]) = new {
    def ELSE(elsep: => Rep[T]): Rep[T] = __ifThenElse(cond, thenp, elsep)
  }
}

object IfThenElseHack {
  def ifThenElse[T](cond: Boolean, thenp: => T, elsep: => T): T = if (cond) thenp else elsep
}

trait IfThenElseSeq extends IfThenElse { self: ScalanSeq =>
  def __ifThenElse[T](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T]): Rep[T] = IfThenElseHack.ifThenElse(cond, thenp, elsep)
}

trait IfThenElseExp extends IfThenElse with ExpressionsBase { self: ScalanStaged =>

  case class IfThenElse[T:Elem](cond: Exp[Boolean], thenp: Exp[T], elsep: Exp[T]) extends Def[T] {
    override def mirror(t: Transformer) = IfThenElse(t(cond), t(thenp), t(elsep))
  }

  override def __ifThenElse[T](cond: Exp[Boolean], thenp: => Exp[T], elsep: => Exp[T]): Exp[T] = {
    implicit val eT = thenp.Elem
    IfThenElse(cond, thenp, elsep)
  }

  override def rewrite[T](d: Def[T])(implicit eT: Elem[T]) = d match {
    case IfThenElse(Def(Const(true)), t, _) => t
    case IfThenElse(Def(Const(false)), _, e) => e
    case _ => super.rewrite(d)
  }
}


