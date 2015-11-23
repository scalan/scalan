package scalan.primitives

import scalan.{ScalanExp, ScalanSeq, Scalan}
import scalan.staged.{BaseExp}

trait OrderingOps { self: Scalan =>
  implicit def repOrderingToOrderingOps[T](x: Rep[T])(implicit n: Ordering[T], et: Elem[T]) = new OrderingOpsCls(x)
  implicit def OrderingToOrderingOps[T](x: T)(implicit n: Ordering[T], et: Elem[T]) = new OrderingOpsCls(toRep(x))

  class OrderingOpsCls[T](lhs: Rep[T])(implicit val n: Ordering[T], et: Elem[T]) {
    def <(rhs: Rep[T]) = OrderingLT(n).apply(lhs,rhs)
    def <=(rhs: Rep[T]) = OrderingLTEQ(n).apply(lhs,rhs)
    def >(rhs: Rep[T]) = OrderingGT(n).apply(lhs,rhs)
    def >=(rhs: Rep[T]) = OrderingGTEQ(n).apply(lhs,rhs)
    def max(rhs: Rep[T]): Rep[T] = OrderingMax(n).apply(lhs,rhs)
    def min(rhs: Rep[T]): Rep[T] = OrderingMin(n).apply(lhs,rhs)
    def compare(rhs: Rep[T]): Rep[Int] = OrderingCompare(n).apply(lhs,rhs)
  }

  case class OrderingLT[T](ord: Ordering[T]) extends BinOp[T, Boolean]("<", ord.lt)

  case class OrderingLTEQ[T](ord: Ordering[T]) extends BinOp[T, Boolean]("<=", ord.lteq)

  case class OrderingGT[T](ord: Ordering[T]) extends BinOp[T, Boolean](">", ord.gt)

  case class OrderingGTEQ[T](ord: Ordering[T]) extends BinOp[T, Boolean](">=", ord.gteq)

  case class OrderingMax[T: Elem](ord: Ordering[T]) extends BinOp[T, T]("max", ord.max)

  case class OrderingMin[T: Elem](ord: Ordering[T]) extends BinOp[T, T]("min", ord.min)

  case class OrderingCompare[T: Elem](ord: Ordering[T])(implicit val eT: Elem[T]) extends BinOp[T, Int]("compare", ord.compare)
}
