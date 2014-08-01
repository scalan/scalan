package scalan.primitives

import scalan.{ScalanStaged, ScalanSeq, Scalan}
import scalan.staged.{BaseExp}

trait OrderingOps { self: Scalan =>
  implicit def repOrderingToOrderingOps[T](x: Rep[T])(implicit n: Ordering[T], et: Elem[T]) = new OrderingOpsCls(x)
  implicit def OrderingToOrderingOps[T](x: T)(implicit n: Ordering[T], et: Elem[T]) = new OrderingOpsCls(toRep(x))

  class OrderingOpsCls[T](lhs: Rep[T])(implicit val n: Ordering[T], et: Elem[T]) {
    def <(rhs: Rep[T]) = ordering_lt(lhs, rhs)
    def <=(rhs: Rep[T]) = ordering_lteq(lhs, rhs)
    def >(rhs: Rep[T]) = ordering_gt(lhs, rhs)
    def >=(rhs: Rep[T]) = ordering_gteq(lhs, rhs)
    def max(rhs: Rep[T]): Rep[T] = ordering_max(lhs, rhs)
    def min(rhs: Rep[T]): Rep[T] = ordering_min(lhs, rhs)
  }

  def ordering_lt[T](lhs: Rep[T], rhs: Rep[T])(implicit n: Ordering[T]): Rep[Boolean]
  def ordering_lteq[T](lhs: Rep[T], rhs: Rep[T])(implicit n: Ordering[T]): Rep[Boolean]
  def ordering_gt[T](lhs: Rep[T], rhs: Rep[T])(implicit n: Ordering[T]): Rep[Boolean]
  def ordering_gteq[T](lhs: Rep[T], rhs: Rep[T])(implicit n: Ordering[T]): Rep[Boolean]
  def ordering_max[T](lhs: Rep[T], rhs: Rep[T])(implicit n: Ordering[T], et: Elem[T]): Rep[T]
  def ordering_min[T](lhs: Rep[T], rhs: Rep[T])(implicit n: Ordering[T], et: Elem[T]): Rep[T]
}

trait OrderingOpsSeq extends OrderingOps { self: ScalanSeq =>
  def ordering_lt[T](lhs: Rep[T], rhs: Rep[T])(implicit n: Ordering[T]): Rep[Boolean] = n.lt(lhs, rhs)
  def ordering_lteq[T](lhs: Rep[T], rhs: Rep[T])(implicit n: Ordering[T]): Rep[Boolean] = n.lteq(lhs, rhs)
  def ordering_gt[T](lhs: Rep[T], rhs: Rep[T])(implicit n: Ordering[T]): Rep[Boolean] = n.gt(lhs, rhs)
  def ordering_gteq[T](lhs: Rep[T], rhs: Rep[T])(implicit n: Ordering[T]): Rep[Boolean] = n.gteq(lhs, rhs)
  def ordering_max[T](lhs: Rep[T], rhs: Rep[T])(implicit n: Ordering[T], et: Elem[T]): Rep[T] = n.max(lhs, rhs)
  def ordering_min[T](lhs: Rep[T], rhs: Rep[T])(implicit n: Ordering[T], et: Elem[T]): Rep[T] = n.min(lhs, rhs)
}

trait OrderingOpsExp extends OrderingOps with BaseExp { self: ScalanStaged =>
  abstract class OrderingBinOp[T](ordering: Ordering[T], val opName: String) extends BinOp[T, Boolean]

  case class OrderingLT[T](lhs: Exp[T], rhs: Exp[T], implicit val n: Ordering[T]) extends OrderingBinOp[T](n, "<") {
    def copyWith(l: Rep[T], r: Rep[T]) = this.copy(lhs = l, rhs = r)
  }
  case class OrderingLTEQ[T](lhs: Exp[T], rhs: Exp[T], implicit val n: Ordering[T]) extends OrderingBinOp[T](n, "<=") {
    def copyWith(l: Rep[T], r: Rep[T]) = this.copy(lhs = l, rhs = r)
  }
  case class OrderingGT[T](lhs: Exp[T], rhs: Exp[T], implicit val n: Ordering[T]) extends OrderingBinOp[T](n, ">") {
    def copyWith(l: Rep[T], r: Rep[T]) = this.copy(lhs = l, rhs = r)
  }
  case class OrderingGTEQ[T](lhs: Exp[T], rhs: Exp[T], implicit val n: Ordering[T]) extends OrderingBinOp[T](n, ">=") {
    def copyWith(l: Rep[T], r: Rep[T]) = this.copy(lhs = l, rhs = r)
  }
  case class OrderingMax[T](lhs: Exp[T], rhs: Exp[T])(implicit val n: Ordering[T], elem: Elem[T]) extends EndoBinOp[T] {
    def copyWith(l: Rep[T], r: Rep[T]) = this.copy(lhs = l, rhs = r)
    def opName = "max"
  }
  case class OrderingMin[T](lhs: Exp[T], rhs: Exp[T])(implicit val n: Ordering[T], elem: Elem[T]) extends EndoBinOp[T] {
    def copyWith(l: Rep[T], r: Rep[T]) = this.copy(lhs = l, rhs = r)
    def opName = "min"
  }

  def ordering_lt[T](lhs: Exp[T], rhs: Exp[T])(implicit n: Ordering[T]): Rep[Boolean] = OrderingLT(lhs,rhs,n)
  def ordering_lteq[T](lhs: Exp[T], rhs: Exp[T])(implicit n: Ordering[T]): Rep[Boolean] = OrderingLTEQ(lhs,rhs,n)
  def ordering_gt[T](lhs: Exp[T], rhs: Exp[T])(implicit n: Ordering[T]): Rep[Boolean] = OrderingGT(lhs,rhs,n)
  def ordering_gteq[T](lhs: Exp[T], rhs: Exp[T])(implicit n: Ordering[T]): Rep[Boolean] = OrderingGTEQ(lhs,rhs,n)
  def ordering_max[T](lhs: Exp[T], rhs: Exp[T])(implicit n: Ordering[T], et: Elem[T]): Rep[T] = OrderingMax(lhs,rhs)
  def ordering_min[T](lhs: Exp[T], rhs: Exp[T])(implicit n: Ordering[T], et: Elem[T]): Rep[T] = OrderingMin(lhs,rhs)

  override def rewrite[T](s: Exp[T]) = s match {
    case Def(d) => d match {
      case OrderingLT(Def(Const(x)), Def(Const(y)), n) => Const(n.lt(x, y))
      case OrderingGT(Def(Const(x)), Def(Const(y)), n) => Const(n.gt(x,y))
      case OrderingLTEQ(Def(Const(x)), Def(Const(y)), n) => Const(n.lteq(x,y))
      case OrderingGTEQ(Def(Const(x)), Def(Const(y)), n) => Const(n.gteq(x,y))
      case _ => super.rewrite(s)
    }
    case _ => super.rewrite(s)
  }
}
