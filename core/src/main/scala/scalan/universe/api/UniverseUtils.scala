package scalan.universe.api

object UniverseUtils {
  @inline def hashCode2(o1: Any, o2: Any) = 41 * (41 + o1.hashCode) + o2.hashCode
  @inline def hashCode3(o1: Any, o2: Any, o3: Any) =
    41 * (41 * (41 + o1.hashCode) + o2.hashCode) + o3.hashCode

  def genTuplesIter[A,B](xs: List[A])(f: A => Iterator[B]): Iterator[Iterator[B]] = xs match {
    case h :: t =>
      val tailTuples = genTuples(t)(f)
      val tails = for {
        h1 <- f(h)
        t1 <- tailTuples
      } yield Iterator(h1) ++ t1
      tails
    case Nil => Iterator(Nil.toIterator)
  }

  def mapTraversablesToList[A,B](xs: TraversableOnce[A])(f: A => TraversableOnce[B]): List[List[B]] = {
    val iters = xs.map(f)
    iters.map(_.toList).toList
  }

  def genTuples[A,B](xs: List[A])(f: A => Iterator[B]): List[List[B]] = {
    val iter = genTuplesIter(xs)(f)
    iter.map(_.toList).toList
  }

  def join[K,V,R](ks: List[K], kv: Map[K,V])(f: (K,V) => R): List[R] = {
    val vs = ks.map(k => kv.get(k) match {
      case Some(v) => v
      case None => sys.error(s"Cannot find value for key $k")
    })
    (ks zip vs).map(f.tupled)
  }

  def traverseDepthFirst[T](t: T)(f: T => List[T]): List[T] = {
    var all: List[T] = Nil
    var queue = List(t)
    while (queue.nonEmpty) {
      val h = queue.head
      queue = queue.tail

      var next = f(h).reverse
      while (next.nonEmpty) {
        queue = next.head :: queue
        next = next.tail
      }
      all = h :: all
    }
    all.reverse
  }

}
