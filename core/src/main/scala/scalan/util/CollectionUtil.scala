package scalan.util

import scala.collection.{Seq, mutable}
import mutable.{ArrayBuffer, HashMap}

object CollectionUtil {

  def foldRight[A,B](xs: Seq[A])(proj: A => B)(f: (A,B) => B): B =
    xs.foldRight[B](null.asInstanceOf[B]) { case (a, b) =>
      b match {
        case null => proj(a)
        case _ => f(a, b)
      }
    }

  def createMultiMap[K,V](kvs: Seq[(K,V)]): Map[K, ArrayBuffer[V]] = {
    val res = HashMap.empty[K, ArrayBuffer[V]]
    for ((k,v) <- kvs) {
      if (res.contains(k))
        res(k) += v
      else
        res += k -> ArrayBuffer(v)
    }
    res.toMap
  }

  def joinSeqs[O, I, K](outer: Seq[O], inner: Seq[I])(outKey: O=>K, inKey: I=>K): Seq[(O,I)] = {
    val kvs = createMultiMap(inner.map(i => (inKey(i), i)))
    val res = outer.flatMap(o => {
      val ko = outKey(o)
      kvs(ko).map(i => (o,i))
    })
    res
  }

  def outerJoin[K, L, R, O]
        (left: Map[K, L], right: Map[K, R])
        (l: (K,L) => O, r: (K,R) => O, inner: (K,L,R) => O): Map[K,O] = {
    val res = HashMap.empty[K, O]
    val lks = left.keySet
    val rks = right.keySet
    val leftOnly = lks diff rks
    val rightOnly = rks diff lks
    val both = lks intersect rks
    for (lk <- leftOnly)
      res += lk -> l(lk, left(lk))
    for (rk <- rightOnly)
      res += rk -> r(rk, right(rk))
    for (k <- both)
      res += k -> inner(k, left(k), right(k))
    res.toMap
  }

}
