package scalan.effects

import scalan.monads.MonadsDsl

trait StateExamples extends MonadsDsl { self =>
  implicit val F = stateMonad[Int]
  import F.toMonadic

  def zipArrayWithIndex[A:Elem](as: Rep[Array[A]]): Rep[Array[(Int,A)]] =
    as.foldLeft(F.unit(SArray.empty[(Int, A)]))({ p =>
      val Pair(acc, a) = p
      for {
        xs <- acc
        n  <- State.get[Int]
        _  <- State.set(n + 1)
      } yield xs.append(Pair(n, a))
    }).run(0)._1

  lazy val zipArrayWithIndexW = fun { xs: Arr[Double] => zipArrayWithIndex(xs) }

  def zipCollectionWithIndex[A:Elem](as: Coll[A])(implicit C: CollectionManager): Coll[(Int,A)] =
    as.arr.foldLeft(F.unit(C.empty[(Int, A)]))({ p =>
      val Pair(acc, a) = p
      for {
        xs <- acc
        n  <- State.get[Int]
        _  <- State.set(n + 1)
      } yield xs.append(Pair(n, a))
    }).run(0)._1

  lazy val zipCollectionWithIndexW = fun { xs: Arr[Double] =>
    implicit val C = Collection.manager
    zipCollectionWithIndex(Collection(xs)).arr
  }

  class BoxedCollectionManager extends CollectionManager {
    def apply[T: Elem](arr: Rep[Array[T]]): Coll[T] = fromArray(arr)
    def fromArray[T: Elem](arr: Rep[Array[T]]): Coll[T] = CollectionOverArray(arr)
    def fromList[T: Elem](list: Rep[List[T]]): Coll[T] = CollectionOverList(list)
    def replicate[T: Elem](len: Rep[Int], v: Rep[T]): Coll[T] = CollectionOverArray(SArray.replicate(len, v))
    def empty[T: Elem]: Coll[T] = CollectionOverArray(SArray.empty[T])
    def singleton[T: Elem](v: Rep[T]): Coll[T] = CollectionOverArray(SArray.replicate(1, v))
    def indexRange(l: Rep[Int]): Coll[Int] = CollectionOverArray(SArray.rangeFrom0(l))
  }

  lazy val zipCollectionWithIndexW2 = fun { xs: Arr[Double] =>
    implicit val C = new BoxedCollectionManager
    zipCollectionWithIndex(Collection(xs)).arr
  }

  def zipCollectionWithIndex3[A:Elem](as: Coll[A])(implicit C: CollectionManager): Coll[(Int,A)] =
    as.foldLeft[State[Int,Collection[(Int, A)]]](F.unit(C.empty[(Int, A)]), fun { p =>
      val Pair(acc, a) = p
      for {
        xs <- acc
        n  <- State.get[Int]
        _  <- State.set(n + 1)
      } yield xs.append(Pair(n, a))
    }).run(0)._1

  lazy val zipCollectionWithIndexW3 = fun { xs: Arr[Double] =>
    implicit val C = Collection.manager
    zipCollectionWithIndex3(Collection(xs)).arr
  }
}
