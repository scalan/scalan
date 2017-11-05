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


}
