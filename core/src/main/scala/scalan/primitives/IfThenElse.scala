package scalan.primitives

import scalan.staged.{BaseExp}
import scalan.{ScalanStaged, ScalanSeq, Base, Scalan}

trait IfThenElse extends Base { self: Scalan =>
  def __ifThenElse[T](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T]): Rep[T]

  def IF(cond: Rep[Boolean]): IfBranch = new IfBranch(cond)
  
  class IfBranch(cond: Rep[Boolean]) {
    def apply[T](thenp: => Rep[T]) = THEN(thenp)
    
    def THEN[T](thenp: => Rep[T]) = new ThenBranch[T](cond, thenp)
  }
  
  class ElseIfBranch[T](cond: Rep[Boolean], outer: ThenBranch[T]) {
    def apply(thenp: => Rep[T]) = THEN(thenp)
    
    def THEN(thenp: => Rep[T]) = new ThenBranch[T](cond, thenp) {
      override def ELSE(elsep: => Rep[T]) = outer.elseIf(cond, thenp, elsep)
    }
  }
  
  class ThenBranch[T](cond: Rep[Boolean], thenp: => Rep[T]) {
    def ELSE(elsep: => Rep[T]): Rep[T] = __ifThenElse(cond, thenp, elsep)
    
    def elseIf(cond1: => Rep[Boolean], thenp1: => Rep[T], elsep1: => Rep[T]) = 
      ELSE(__ifThenElse(cond1, thenp1, elsep1))
    
    def ELSEIF(cond1: => Rep[Boolean]) = new ElseIfBranch[T](cond1, this)
  }
}

trait IfThenElseSeq extends IfThenElse { self: ScalanSeq =>
  def __ifThenElse[T](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T]): Rep[T] = if(cond) thenp else elsep
}

trait IfThenElseExp extends IfThenElse with BaseExp { self: ScalanStaged =>

  case class IfThenElse[T](cond: Exp[Boolean], thenp: Exp[T], elsep: Exp[T])(implicit selfType: Elem[T]) extends BaseDef[T] {
    def uniqueOpId = name(selfType)
    override def mirror(t: Transformer) = IfThenElse(t(cond), t(thenp), t(elsep))
  }

  override def __ifThenElse[T](cond: Exp[Boolean], thenp: => Exp[T], elsep: => Exp[T]): Exp[T] = {
    implicit val eT = thenp.elem
    IfThenElse(cond, thenp, elsep)
  }

  override def rewriteDef[T](d: Def[T]) = d match {
    case IfThenElse(Def(Const(true)), t, _) => t
    case IfThenElse(Def(Const(false)), _, e) => e
    case _ => super.rewriteDef(d)
  }
}