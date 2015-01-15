package scalan.primitives

import scalan.staged.BaseExp
import scalan.{ScalanExp, Scalan}

trait LogicalOps { self: Scalan =>
  val And = new EndoBinOp[Boolean]("&&", _ && _)

  val Or = new EndoBinOp[Boolean]("||", _ || _)

  val Not = new EndoUnOp[Boolean]("!", !_)

  val BooleanToInt = new UnOp[Boolean, Int]("ToInt", if (_) 1 else 0)

  implicit class RepBooleanOps(value: Rep[Boolean]) {
    def &&(y: Rep[Boolean]): Rep[Boolean] = And(value, y)
    def ||(y: Rep[Boolean]): Rep[Boolean] = Or(value, y)
    def unary_!() : Rep[Boolean] = Not(value)
    def toInt: Rep[Int] = BooleanToInt(value)
  }
}

trait LogicalOpsExp extends LogicalOps with BaseExp { self: ScalanExp =>
  override def rewriteDef[A](d: Def[A]) = d match {
    case ApplyBinOp(op, lhs, rhs) =>
      op.asInstanceOf[BinOp[_, _]] match {
        case _: Equals[_] if lhs.elem == BoolElement && rhs.elem == BoolElement =>
          matchBoolConsts(d, lhs, rhs, x => x, x => !x.asRep[Boolean])
        case And =>
          matchBoolConsts(d, lhs, rhs, x => x, _ => false)
        case Or =>
          matchBoolConsts(d, lhs, rhs, _ => true, x => x)
        case _ => super.rewriteDef(d)
      }
    case ApplyUnOp(o1, Def(ApplyUnOp(o2, x))) if o1 == Not && o2 == Not => x
    case _ => super.rewriteDef(d)
  }

  @inline
  private def matchBoolConsts(d: Def[_], lhs: Exp[_], rhs: Exp[_], ifTrue: Exp[_] => Exp[_], ifFalse: Exp[_] => Exp[_]): Exp[_] =
    lhs match {
      case Def(Const(b: Boolean)) => if (b) ifTrue(rhs) else ifFalse(rhs)
      case _ => rhs match {
        case Def(Const(b: Boolean)) => if (b) ifTrue(lhs) else ifFalse(lhs)
        case _ => super.rewriteDef(d)
      }
    }
}
