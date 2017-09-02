package scalan.primitives

import scalan.{ScalanExp}
import scalan.staged.BaseExp

trait UnBinOpsExp extends BaseExp { self: ScalanExp =>

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

  case class ApplyUnOp[A, R](op: UnOp[A, R], arg: Exp[A]) extends BaseDef[R]()(op.eResult) {
    override def toString = s"$op($arg)"
  }

  case class ApplyBinOp[A, R](op: BinOp[A, R], lhs: Exp[A], rhs: Exp[A]) extends BaseDef[R]()(op.eResult) {
    override def toString = s"$op($lhs, $rhs)"
  }

  def applyUnOp[A, R](op: UnOp[A, R], arg: Rep[A]): Rep[R] = ApplyUnOp(op, arg)

  def applyBinOp[A, R](op: BinOp[A, R], lhs: Rep[A], rhs: Rep[A]): Rep[R] = ApplyBinOp(op, lhs, rhs)

  override def rewriteDef[T](d: Def[T]): Exp[_] =
    currentPass.config.constantPropagation match {
      case false => super.rewriteDef(d)
      case true =>
        d match {
          case ApplyUnOp(op: UnOp[a, T @unchecked], Def(Const(arg))) if op.shouldPropagate(arg) =>
            toRep(op.applySeq(arg.asInstanceOf[a]))(d.selfType)
          case ApplyBinOp(op: BinOp[a, T @unchecked], Def(Const(lhs)), Def(Const(rhs))) if op.shouldPropagate(lhs, rhs) =>
            toRep(op.applySeq(lhs.asInstanceOf[a], rhs.asInstanceOf[a]))(d.selfType)
          case _ => super.rewriteDef(d)
        }
    }

  // allows use of context bounds in classes extending UnOp/BinOp.
  // Note that this must be overridden if some transformation _is_ needed (i.e. if the class contains Rep[_] somewhere)
  override protected def transformProductParam(x: Any, t: Transformer) = x match {
    case (_: UnOp[_, _]) | (_: BinOp[_, _]) => x
    case _ => super.transformProductParam(x, t)
  }
}