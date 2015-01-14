package scalan.primitives

import scalan.staged.{BaseExp}
import scalan.{ScalanExp, ScalanSeq, Base, Scalan}

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

trait IfThenElseExp extends IfThenElse with BaseExp { self: ScalanExp =>

  case class IfThenElse[T](cond: Exp[Boolean], thenp: Exp[T], elsep: Exp[T])(implicit selfType: Elem[T]) extends BaseDef[T] {
    def uniqueOpId = name(selfType)
    override def mirror(t: Transformer) = IfThenElse(t(cond), t(thenp), t(elsep))
  }

  override def __ifThenElse[T](cond: Exp[Boolean], thenp: => Exp[T], elsep: => Exp[T]): Exp[T] = {
    implicit val eT = thenp.elem
    IfThenElse(cond, thenp, elsep)
  }

  implicit class IfThenElseOps[T](tableEntry: TableEntry[T]) {
    def isIfThenElse = tableEntry.rhs match { case IfThenElse(_,_,_) => true case _ => false }
  }

  def liftFromIfThenElse[A,B,C](cond: Rep[Boolean], a: Rep[A], b: Rep[B], iso1: Iso[A,C], iso2: Iso[B,C]): Rep[C] = {
    val ea = iso1.eFrom
    val eb = iso2.eFrom
    implicit val ec = iso1.eTo
    val source = IF (cond) THEN { toLeftSum(a)(eb) } ELSE { toRightSum(b)(ea) }
    val res = SumView(source)(iso1, iso2).self.joinSum
    res
  }

  override def rewriteDef[T](d: Def[T]) = d match {
    case IfThenElse(Def(Const(true)), t, _) => t
    case IfThenElse(Def(Const(false)), _, e) => e

    case IfThenElse(cond, Def(UnpackableDef(a, iso1: Iso[a, c])), Def(UnpackableDef(b, iso2: Iso[b, _]))) =>
      liftFromIfThenElse(cond, a.asRep[a], b.asRep[b], iso1, iso2)

    case IfThenElse(cond, a, Def(UnpackableDef(b, iso2: Iso[b, d]))) =>
      liftFromIfThenElse(cond, a, b.asRep[b], identityIso(a.elem), iso2)

    case IfThenElse(cond, Def(UnpackableDef(a, iso1: Iso[a, c])), b) =>
      liftFromIfThenElse(cond, a.asRep[a], b, iso1, identityIso(b.elem))

    case Tup(Def(IfThenElse(c1, t1, e1)), Def(IfThenElse(c2, t2, e2))) if c1 == c2 =>
      IF (c1) THEN { Pair(t1, t2) } ELSE { Pair(e1, e2) }

    case apply: Apply[a, b] => apply.f match {
      case Def(IfThenElse(c, t, e)) => {
        IF (c) { t.asRep[a=>b](apply.arg) } ELSE { e.asRep[a=>b](apply.arg) }
      }
      case _ => super.rewriteDef(d)
    }

    case _ => super.rewriteDef(d)
  }
}