package scalan.arrays

import scalan.staged.BaseExp
import scalan.ScalanExp

trait JoinPrimitives extends BaseExp { self: ScalanExp =>

  case class CommonInnerJoin[A, B, K, R](xs: Exp[Array[A]], ys: Exp[Array[B]], a: Rep[A => K], b: Rep[B => K], f: Rep[((A, B)) => R])
                                        (implicit val ordK: Ordering[K], val nK: Numeric[K], val st: Elem[Array[(K, R)]],
                                         val eA: Elem[A], val eB: Elem[B], val eK: Elem[K], val eR: Elem[R]) extends ArrayDef[(K, R)]

  case class CommonOuterJoin[A, B, K, R](xs: Exp[Array[A]], ys: Exp[Array[B]], a: Rep[A => K], b: Rep[B => K], f: Rep[((A, B)) => R], f1: Rep[A => R], f2: Rep[B => R])
                                        (implicit val ordK: Ordering[K], val nK: Numeric[K], val st: Elem[Array[(K, R)]],
                                         val eA: Elem[A], val eB: Elem[B], val eK: Elem[K], val eR: Elem[R]) extends ArrayDef[(K, R)]

  case class PairedInnerJoin[K, B, C, R](xs: Exp[Array[(K, B)]], ys: Exp[Array[(K, C)]], f: Exp[((B, C)) => R])
                                        (implicit val ordK: Ordering[K], val nK: Numeric[K], val eK: Elem[K],
                                         val eB: Elem[B], val eC: Elem[C], val eR: Elem[R]) extends ArrayDef[(K, R)]

  case class PairedOuterJoin[K, B, C, R](xs: Exp[Array[(K, B)]], ys: Exp[Array[(K, C)]], f: Exp[((B, C)) => R], f1: Exp[B => R], f2: Exp[C => R])
                                        (implicit val ordK: Ordering[K], val nK: Numeric[K], val eK: Elem[K],
                                         val eB: Elem[B], val eC: Elem[C], val eR: Elem[R]) extends ArrayDef[(K, R)]

}
