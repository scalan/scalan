package scalan.util

import scala.collection.{Seq, mutable}
import mutable.{HashMap, ArrayBuffer}
import scala.collection.generic.CanBuildFrom

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

  def outerJoinSeqs[O, I, K, R]
      (outer: Seq[O], inner: Seq[I])
      (outKey: O=>K, inKey: I=>K)
      (projO: (K,O) => R, projI: (K,I) => R, proj:(K,O,I) => R): Seq[(K,R)] = {
    val res = ArrayBuffer.empty[(K,R)]
    val kis = inner.map(i => (inKey(i), i))
    val kvs = createMultiMap(kis)
    val outerKeys = mutable.Set.empty[K]
    for (o <- outer) {
      val ko = outKey(o)
      outerKeys += ko
      if (!kvs.contains(ko))
        res += ((ko, projO(ko, o)))
      else
        for (i <- kvs(ko))
          res += ((ko, proj(ko, o, i)))
    }
    for ((k,i) <- kis if !outerKeys.contains(k))
      res += ((k, projI(k, i)))
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

  def join[K,V,R](ks: List[K], kv: Map[K,V])(f: (K,V) => R): List[R] = {
    val vs = ks.map(k => kv.get(k) match {
      case Some(v) => v
      case None => sys.error(s"Cannot find value for key $k")
    })
    (ks zip vs).map(f.tupled)
  }

  implicit class AnyOps[A](x: A) {
    def zipWithExpandedBy[B](f: A => List[B]): List[(A,B)] = {
      val ys = f(x)
      List.fill(ys.length)(x) zip ys
    }
    def traverseDepthFirst(f: A => List[A]): List[A] = {
      var all: List[A] = Nil
      var stack = List(x)
      while (stack.nonEmpty) {
        val h = stack.head
        stack = stack.tail

        var next = f(h).reverse
        while (next.nonEmpty) {
          stack = next.head :: stack
          next = next.tail
        }
        all = h :: all
      }
      all.reverse
    }
  }

  implicit class TraversableOps[A, Source[X] <: Traversable[X]](xs: Source[A]) {
    def mapFirst[B](f: A => Option[B]): Option[B] = {
      for (x <- xs) {
        val y = f(x)
        if (y.isDefined) return y
      }
      return None
    }
    def filterMap[B](f: A => Option[B])(implicit cbf: CanBuildFrom[Source[A], B, Source[B]]): Source[B] = {
       val b = cbf()
       for (x <- xs) {
         f(x) match {
           case Some(y) =>
             b += y
           case None =>
         }
       }
       b.result()
    }
    def distinctBy[K](key: A => K)(implicit cbf: CanBuildFrom[Source[A], A, Source[A]]): Source[A] = {
      val keys = mutable.Set[K]()
      val b = cbf()
      for (x <- xs) {
        val k = key(x)
        if (!keys.contains(k)) {
          b += x
          keys += k
        }
      }
      b.result()
    }
  }
}


