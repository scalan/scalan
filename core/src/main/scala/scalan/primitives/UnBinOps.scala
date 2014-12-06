package scalan.primitives

import scalan.{ScalanSeq, Scalan, ScalanExp}
import scalan.staged.BaseExp

trait UnBinOps { self: Scalan =>
  class UnOp[A, R](val opName: String, val applySeq: A => R)(implicit val eResult: Elem[R]) {
    override def toString = opName

    def apply(arg: Rep[A]) = applyUnOp(this, arg)

    def shouldPropagate(arg: A) = true
  }

  class BinOp[A, R](val opName: String, val applySeq: (A, A) => R)(implicit val eResult: Elem[R]) {
    override def toString = opName

    def apply(lhs: Rep[A], rhs: Rep[A]) = applyBinOp(this, lhs, rhs)

    // ideally shouldn't be necessary, but
    // we curently can't handle division by zero properly
    def shouldPropagate(lhs: A, rhs: A) = true
  }

  type EndoUnOp[A] = UnOp[A, A]
  type EndoBinOp[A] = BinOp[A, A]

  def applyUnOp[A, R](op: UnOp[A, R], arg: Rep[A]): Rep[R]

  def applyBinOp[A, R](op: BinOp[A, R], lhs: Rep[A], rhs: Rep[A]): Rep[R]
}

trait UnBinOpsSeq extends UnBinOps { self: ScalanSeq =>
  def applyUnOp[A, R](op: UnOp[A, R], arg: Rep[A]): Rep[R] =
    op.applySeq(arg)

  def applyBinOp[A, R](op: BinOp[A, R], lhs: Rep[A], rhs: Rep[A]): Rep[R] =
    op.applySeq(lhs, rhs)
}

trait UnBinOpsExp extends BaseExp with UnBinOps { self: ScalanExp =>

  case class ApplyUnOp[A, R](op: UnOp[A, R], arg: Exp[A]) extends BaseDef[R]()(op.eResult) {
    override def toString = s"$op($arg)"
    lazy val uniqueOpId = name(arg.elem)
    override def mirror(t: Transformer) = ApplyUnOp(op, t(arg))
  }

  case class ApplyBinOp[A, R](op: BinOp[A, R], lhs: Exp[A], rhs: Exp[A]) extends BaseDef[R]()(op.eResult) {
    override def toString = s"$op($lhs, $rhs)"
    lazy val uniqueOpId = name(lhs.elem, rhs.elem)
    override def mirror(t: Transformer) = ApplyBinOp(op, t(lhs), t(rhs))
  }

  def applyUnOp[A, R](op: UnOp[A, R], arg: Rep[A]): Rep[R] = ApplyUnOp(op, arg)

  def applyBinOp[A, R](op: BinOp[A, R], lhs: Rep[A], rhs: Rep[A]): Rep[R] = ApplyBinOp(op, lhs, rhs)

  override def rewriteDef[T](d: Def[T]): Exp[_] = d match {
    case ApplyUnOp(op: UnOp[a, T], Def(Const(arg))) if op.shouldPropagate(arg) =>
      toRep(op.applySeq(arg.asInstanceOf[a]))(d.selfType)
    case ApplyBinOp(op: BinOp[a, T], Def(Const(lhs)), Def(Const(rhs))) if op.shouldPropagate(lhs, rhs) =>
      toRep(op.applySeq(lhs.asInstanceOf[a], rhs.asInstanceOf[a]))(d.selfType)
    case _ => super.rewriteDef(d)
  }
}