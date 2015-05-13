package scalan.collections

import scala.annotation.unchecked.uncheckedVariance
import scalan._
import scalan.arrays.ArrayOps
import scalan.common.Default
import scalan.common.OverloadHack.Overloaded1

trait Collections extends ArrayOps with ListOps { self: ScalanCommunityDsl =>

  type Coll[+A] = Rep[Collection[A]]
  trait Collection[@uncheckedVariance +A] extends Reifiable[Collection[A @uncheckedVariance]] {
    implicit def eItem: Elem[A @uncheckedVariance]
    def length: Rep[Int]
    def arr: Rep[Array[A @uncheckedVariance]]
    def lst: Rep[List[A @uncheckedVariance]]
    def seq: Rep[SSeq[A] @uncheckedVariance] = SSeq(arr)
    def apply(i: Rep[Int]): Rep[A]
    @OverloadId("many")
    def apply(indices: Coll[Int])(implicit o: Overloaded1): Coll[A]
    def mapBy[B: Elem](f: Rep[A => B @uncheckedVariance]): Coll[B] //= Collection(arr.mapBy(f))
    def zip[B: Elem](ys: Coll[B]): PairColl[A @uncheckedVariance, B] // = PairCollectionSOA(self, ys)
    def slice(offset: Rep[Int], length: Rep[Int]): Coll[A]
    def reduce(implicit m: RepMonoid[A @uncheckedVariance]): Rep[A] //= arr.reduce(m)
    def update (idx: Rep[Int], value: Rep[A @uncheckedVariance]): Coll[A]
    def updateMany (idxs: Coll[Int], vals: Coll[A @uncheckedVariance]): Coll[A]
    def indexes: Coll[Int] = Collection.indexRange(length)
    def filterBy(f: Rep[A @uncheckedVariance => Boolean]): Coll[A]
    def flatMapBy[B: Elem](f: Rep[A @uncheckedVariance => Collection[B]]): Coll[B] // = Collection(arr.flatMap {in => f(in).arr} )
    def append(value: Rep[A @uncheckedVariance]): Coll[A]  // = Collection(arr.append(value))
    /*def scan(implicit m: RepMonoid[A @uncheckedVariance]): Rep[(Collection[A], A)] = {
      val arrScan = arr.scan(m)
      (Collection(arrScan._1), arrScan._2)
    } */
  }

  def emptyColl[A: Elem]: Coll[A] = element[Collection[A]].defaultRepValue
  type Segments1 = PairCollection[Int, Int]

  trait CollectionManager {
    def apply[T: Elem](arr: Rep[Array[T]]): Coll[T]
    def fromArray[T: Elem](arr: Rep[Array[T]]): Coll[T]
    def fromList[T: Elem](arr: Rep[List[T]]): Coll[T]
    def replicate[T: Elem](len: Rep[Int], v: Rep[T]): Coll[T]
    def empty[T: Elem]: Coll[T]
    def singleton[T: Elem](v: Rep[T]): Coll[T]
    def indexRange(l: Rep[Int]): Coll[Int]
  }

  trait CollectionCompanion extends TypeFamily1[Collection] with CollectionManager {
    def manager: CollectionManager = this
    def apply[T: Elem](arr: Rep[Array[T]]): Coll[T] = fromArray(arr)

    def fromArray[T: Elem](arr: Rep[Array[T]]): Coll[T] = {
      element[T] match {
        case pairE: PairElem[a, b] =>
          implicit val ea = pairE.eFst
          implicit val eb = pairE.eSnd
          val ps = arr.asRep[Array[(a, b)]]
          val as = fromArray(ps.map { _._1 })
          val bs = fromArray(ps.map { _._2 })
          as zip bs //PairCollectionSOA[a,b](as, bs)
        // TODO add cases for NestedCollectionFlat and View
        //        case viewE: ViewElem[a, b] =>
        //          CollectionOverArray[b](arr.asRep[Array[b]])
        case e => CollectionOverArray(arr)
      }
    }

    def fromList[T: Elem](arr: Rep[List[T]]): Coll[T] = {
      element[T] match {
        case baseE: BaseElem[a] =>
          CollectionOverList[a](arr.asRep[List[a]])
        case pairE: PairElem[a, b] =>
          implicit val ea = pairE.eFst
          implicit val eb = pairE.eSnd
          val ps = arr.asRep[List[(a, b)]]
          val as = fromList(ps.map { _._1 })
          val bs = fromList(ps.map { _._2 })
          as zip bs //PairCollectionSOA[a,b](as, bs)
        case viewE: ViewElem[a, b] =>
          // TODO
          CollectionOverList[b](arr.asRep[List[b]])
        case e => ???(s"Element is $e")
      }
    }

    def replicate[T: Elem](len: Rep[Int], v: Rep[T]): Coll[T] = {
      element[T] match {
        case baseE: BaseElem[a] =>
          CollectionOverArray[a](array_replicate(len, v.asRep[a]))
        case pairElem: PairElem[a ,b] => {
          implicit val ea = pairElem.eFst
          implicit val eb = pairElem.eSnd
          val ps = v.asRep[(a, b)]
          val as = replicate(len, ps._1)
          val bs = replicate(len, ps._2)
          as zip bs
        }
        case viewElem: ViewElem[a, b] =>
          CollectionOverArray(SArray.replicate(len, v))
        case e => ???(s"Element is $e")
      }
    }

    def empty[T: Elem]: Coll[T] = {
      element[T] match {
        case baseE: BaseElem[a] =>
          CollectionOverArray[a](SArray.empty[T])
        case pairElem: PairElem[a ,b] => {
          implicit val ea = pairElem.eFst
          implicit val eb = pairElem.eSnd
          val as = empty[a]
          val bs = empty[b]
          as zip bs
        }
        case viewElem: ViewElem[a, b] =>
          CollectionOverArray(SArray.empty[T])
        case e => ???(s"Element is $e")
      }
    }

    def singleton[T: Elem](v: Rep[T]): Coll[T] = {
      element[T] match {
        case paE: CollectionElem[_, _] => ???
        case _ => replicate(toRep(1), v)
      }
    }
    def indexRange(l: Rep[Int]): Coll[Int] = CollectionOverArray(array_rangeFrom0(l))
  }

  abstract class UnitCollection(val length: Rep[Int]) extends Collection[Unit] {
    def eItem = UnitElement
    def arr = SArray.replicate(length, ())
    def lst = SList.replicate(length, ())
    def apply(i: Rep[Int]) = ()
    def mapBy[B: Elem](f: Rep[Unit => B @uncheckedVariance]): Coll[B] = Collection(arr.mapBy(f))
    @OverloadId("many")
    def apply(indices: Coll[Int])(implicit o: Overloaded1): Coll[Unit] = UnitCollection(indices.length)
    def slice(offset: Rep[Int], length: Rep[Int]) = UnitCollection(length)
    def reduce(implicit m: RepMonoid[Unit @uncheckedVariance]): Rep[Unit] = ()
    def zip[B: Elem](ys: Coll[B]): PairColl[Unit, B] = PairCollectionSOA(self, ys)
    def update (idx: Rep[Int], value: Rep[Unit]): Coll[Unit] = self
    def updateMany (idxs: Coll[Int], vals: Coll[Unit]): Coll[Unit] = self
    def filterBy(f: Rep[Unit => Boolean]): Coll[Unit] = Collection(arr.filterBy(f))
    def flatMapBy[B: Elem](f: Rep[Unit => Collection[B]]): Coll[B] = Collection(arr.flatMap {in => f(in).arr} )
    def append(value: Rep[Unit]): Coll[Unit]  = Collection(arr.append(value))
  }
  trait UnitCollectionCompanion extends ConcreteClass0[UnitCollection]

  abstract class CollectionOverArray[A](val arr: Rep[Array[A]])(implicit val eItem: Elem[A]) extends Collection[A] {
    def lst = arr.toList
    def length = arr.length
    def apply(i: Rep[Int]) = arr(i)
    def mapBy[B: Elem](f: Rep[A => B @uncheckedVariance]): Coll[B] = Collection(arr.mapBy(f))
    def slice(offset: Rep[Int], length: Rep[Int]) = {
      val sl = arr.slice(offset, length)
      CollectionOverArray(sl)
    }
    @OverloadId("many")
    def apply(indices: Coll[Int])(implicit o: Overloaded1): Coll[A] = CollectionOverArray(arr(indices.arr))
    def reduce(implicit m: RepMonoid[A @uncheckedVariance]): Rep[A] = arr.reduce(m)
    def zip[B: Elem](ys: Coll[B]): PairColl[A, B] = PairCollectionSOA(self, ys)
    def update (idx: Rep[Int], value: Rep[A]): Coll[A] = CollectionOverArray(arr.update(idx, value))
    def updateMany (idxs: Coll[Int], vals: Coll[A]): Coll[A] = CollectionOverArray(arr.updateMany(idxs.arr, vals.arr))
    def filterBy(f: Rep[A @uncheckedVariance => Boolean]): Coll[A] = CollectionOverArray(arr.filterBy(f))
    def flatMapBy[B: Elem](f: Rep[A @uncheckedVariance => Collection[B]]): Coll[B] = Collection(arr.flatMap {in => f(in).arr})
    def append(value: Rep[A @uncheckedVariance]): Coll[A]  = CollectionOverArray(arr.append(value))
  }
  trait CollectionOverArrayCompanion extends ConcreteClass1[CollectionOverArray]

  abstract class CollectionOverList[A](val lst: Rep[List[A]])(implicit val eItem: Elem[A]) extends Collection[A] {
    def length = lst.length
    def apply(i: Rep[Int]) = lst(i)
    def arr = lst.toArray
    def mapBy[B: Elem](f: Rep[A => B @uncheckedVariance]): Coll[B] = CollectionOverList(lst.mapBy(f))
    def slice(offset: Rep[Int], length: Rep[Int]) = CollectionOverList(lst.slice(offset, length))
    @OverloadId("many")
    def apply(indices: Coll[Int])(implicit o: Overloaded1): Coll[A] = CollectionOverList(lst(indices.arr))
    def reduce(implicit m: RepMonoid[A @uncheckedVariance]): Rep[A] = lst.reduce(m)
    def zip[B: Elem](ys: Coll[B]): PairColl[A, B] = PairCollectionSOA(self, ys)
    def update (idx: Rep[Int], value: Rep[A]): Coll[A] = ???
    def updateMany (idxs: Coll[Int], vals: Coll[A]): Coll[A] = ???
    def filterBy(f: Rep[A @uncheckedVariance => Boolean]): Coll[A] = CollectionOverList(lst.filterBy(f))
    def flatMapBy[B: Elem](f: Rep[A @uncheckedVariance => Collection[B]]): Coll[B] =
      CollectionOverList(lst.flatMap { in => f(in).lst } )
    def append(value: Rep[A @uncheckedVariance]): Coll[A]  = CollectionOverList(value :: lst)
  }
  trait CollectionOverListCompanion extends ConcreteClass1[CollectionOverList]

  abstract class CollectionOverSeq[A](override val seq: Rep[SSeq[A]])(implicit val eItem: Elem[A]) extends Collection[A] {
    def arr = seq.toArray
    def lst = seq.toList
    def length = seq.size
    def apply(i: Rep[Int]) = seq(i)
    def slice(offset: Rep[Int], length: Rep[Int]) = CollectionOverSeq(seq.slice(offset, offset + length))
    @OverloadId("many")
    def apply(indices: Coll[Int])(implicit o: Overloaded1): Coll[A] = {
      CollectionOverSeq(SSeq(indices.arr.map(i => seq(i))))
    }
    def mapBy[B: Elem](f: Rep[A => B @uncheckedVariance]): Coll[B] = ???
    def reduce(implicit m: RepMonoid[A @uncheckedVariance]): Rep[A] = ???
    def zip[B: Elem](ys: Coll[B]): PairColl[A, B] = ???
    def update (idx: Rep[Int], value: Rep[A]): Coll[A] = ???
    def updateMany (idxs: Coll[Int], vals: Coll[A]): Coll[A] = ???
    def filterBy(f: Rep[A @uncheckedVariance => Boolean]): Coll[A] = ???
    def flatMapBy[B: Elem](f: Rep[A @uncheckedVariance => Collection[B]]): Coll[B] = ???
    def append(value: Rep[A @uncheckedVariance]): Coll[A]  = ???
  }
  trait CollectionOverSeqCompanion extends ConcreteClass1[CollectionOverSeq]

  trait PairCollection[A,B] extends Collection[(A,B)] {
    implicit def eA: Elem[A]
    implicit def eB: Elem[B]
    def as: Rep[Collection[A]]
    def bs: Rep[Collection[B]]
    @OverloadId("many")
    override def apply(indices: Coll[Int])(implicit o: Overloaded1): Rep[PairCollection[A, B]]
    def coll: Coll[(A, B)]
  }
  type PairColl[A, B] = Rep[PairCollection[A, B]]

  abstract class PairCollectionSOA[A, B](val as: Rep[Collection[A]], val bs: Rep[Collection[B]])(implicit val eA: Elem[A], val eB: Elem[B])
    extends PairCollection[A, B] {
    lazy val eItem = element[(A, B)]
    def arr = (as.arr zip bs.arr)
    def lst = (as.lst zip bs.lst)
    def coll: Coll[(A, B)] = self
    def apply(i: Rep[Int]) = (as(i), bs(i))
    def length = as.length
    def slice(offset: Rep[Int], length: Rep[Int]) =
      PairCollectionSOA(as.slice(offset, length), bs.slice(offset, length))
    @OverloadId("many")
    def apply(indices: Coll[Int])(implicit o: Overloaded1): PairColl[A, B] = as(indices) zip bs(indices)
    def mapBy[C: Elem](f: Rep[(A,B) @uncheckedVariance => C]): Coll[C] = Collection(arr.mapBy(f))   // TODO: this should be done in another way
    def reduce(implicit m: RepMonoid[(A,B) @uncheckedVariance]): Rep[(A,B)] = arr.reduce(m)  // TODO: this should be done in another way
    def zip[C: Elem](ys: Coll[C]): PairColl[(A, B),C] = PairCollectionSOA(self, ys)
    def update (idx: Rep[Int], value: Rep[(A, B)]): PairColl[A, B] =
      PairCollectionSOA(as.update(idx, value._1), bs.update(idx, value._2))
    def updateMany (idxs: Coll[Int], vals: Coll[(A, B)]): PairColl[A, B] =
      PairCollectionSOA(as.updateMany(idxs, vals.as), bs.updateMany(idxs, vals.bs))
    def filterBy(f: Rep[(A,B) @uncheckedVariance => Boolean]) =
      CollectionOverArray(arr.filterBy(f))
    def flatMapBy[C: Elem](f: Rep[(A,B) @uncheckedVariance => Collection[C]]): Coll[C] =
      Collection(arr.flatMap {in => f(in).arr})
    def append(value: Rep[(A,B) @uncheckedVariance]): Coll[(A,B)]  = PairCollectionSOA(as.append(value._1), bs.append(value._2))
  }

  trait PairCollectionSOACompanion extends ConcreteClass2[PairCollectionSOA]

  abstract class PairCollectionAOS[A, B](val coll: Rep[Collection[(A,B)]])(implicit val eA: Elem[A], val eB: Elem[B])
    extends PairCollection[A,B] {
    lazy val eItem = element[(A, B)]
    def arr = coll.arr
    def lst = coll.lst
    override def seq = coll.seq
    def as = coll.map(_._1)
    def bs = coll.map(_._2)
    def apply(i: Rep[Int]) = coll(i)
    def length = coll.length
    def slice(offset: Rep[Int], length: Rep[Int]) =
      PairCollectionAOS(coll.slice(offset, length))
    @OverloadId("many")
    def apply(indices: Coll[Int])(implicit o: Overloaded1): PairColl[A, B] =
      PairCollectionAOS(coll(indices))
    def mapBy[C: Elem](f: Rep[(A,B) @uncheckedVariance => C]): Coll[C] = coll.mapBy(f)
    def reduce(implicit m: RepMonoid[(A,B) @uncheckedVariance]): Rep[(A,B)] = coll.reduce(m)
    def zip[C: Elem](ys: Coll[C]): PairColl[(A, B),C] = PairCollectionSOA(self, ys)
    def update (idx: Rep[Int], value: Rep[(A,B)]): Coll[(A,B)] = PairCollectionAOS(coll.update(idx, value))
    def updateMany (idxs: Coll[Int], vals: Coll[(A,B)]): Coll[(A,B)] = PairCollectionAOS(coll.updateMany(idxs, vals))
    def filterBy(f: Rep[(A,B) @uncheckedVariance => Boolean]) = coll.filterBy(f)
    def flatMapBy[C: Elem](f: Rep[(A,B) @uncheckedVariance => Collection[C]]): Coll[C] = coll.flatMapBy(f)
    def append(value: Rep[(A,B) @uncheckedVariance]): Coll[(A,B)]  = PairCollectionAOS(coll.append(value))
  }
  trait PairCollectionAOSCompanion extends ConcreteClass2[PairCollectionAOS] {
    def fromArray[A: Elem, B: Elem](arr: Arr[(A, B)]) = PairCollectionAOS(CollectionOverArray(arr))
  }

  trait NestedCollection[A] extends Collection[Collection[A]] {
    implicit def eA: Elem[A]
    def values: Rep[Collection[A]]
    def segments: Rep[PairCollection[Int, Int]]
    def segOffsets = segments.as
    def segLens = segments.bs
    @OverloadId("many")
    override def apply(indices: Coll[Int])(implicit o: Overloaded1): Rep[NestedCollection[A]]
  }
  //type NColl[A] = Rep[NestedCollectionFlat[A]]
  type NColl[A] = Rep[NestedCollection[A]]

  abstract class NestedCollectionFlat[A](val values: Coll[A], val segments: PairColl[Int, Int])(implicit val eA: Elem[A])
    extends NestedCollection[A] {
    lazy val eItem = collectionElement(eA)
    def length = segments.length
    //def segOffsets = segments.asInstanceOf[Rep[PairCollectionSOA[Int,Int]]].as
    //def segLens = segments.asInstanceOf[Rep[PairCollectionSOA[Int,Int]]].bs
    def apply(i: Rep[Int]) = {
      val Pair(offset, length) = segments(i)
      values.slice(offset, length)
    }

    def arr = segments.arr.map { seg =>
      val Pair(offset,length) = seg
      values.slice(offset, length)
    }
    def lst = arr.toList
    def slice(offset: Rep[Int], length: Rep[Int]): NColl[A] = ???
    @OverloadId("many")
    def apply(indices: Coll[Int])(implicit o: Overloaded1): NColl[A] = {
      val newValues = segments(indices).flatMap { in =>
        values.slice(in._1, in._2)
      }
      val newSegments = {
        val newLens = segments(indices).bs
        val newOffsArr = newLens.arr.scan._1
        Collection(newOffsArr) zip newLens
      }
      NestedCollectionFlat(newValues, newSegments)
    }
    def mapBy[B: Elem](f: Rep[Collection[A] => B @uncheckedVariance]): Coll[B] = Collection(arr.mapBy(f))
    def reduce(implicit m: RepMonoid[Collection[A @uncheckedVariance]]): Coll[A] = arr.reduce(m)
    def zip[B: Elem](ys: Coll[B]): PairColl[Collection[A], B] = PairCollectionSOA(self, ys)
    def update (idx: Rep[Int], value: Rep[Collection[A]]): NColl[A] = ???
    def updateMany (idxs: Coll[Int], vals: Coll[Collection[A]]): NColl[A] = ???
    def filterBy(f: Rep[Collection[A @uncheckedVariance] => Boolean]): NColl[A] = ???
    def flatMapBy[B: Elem](f: Rep[Collection[A @uncheckedVariance] => Collection[B]]): Coll[B] = Collection(arr.flatMap {in => f(in).arr} )
    def append(value: Rep[Collection[A @uncheckedVariance]]): NColl[A]  = ??? //Collection(arr.append(value))
  }
  trait NestedCollectionFlatCompanion extends ConcreteClass1[NestedCollectionFlat] {
    def fromJuggedArray[T: Elem](arr: Rep[Array[Array[T]]]): Rep[NestedCollectionFlat[T]] = {
      val lens: Arr[Int] = arr.map(i => i.length)
      val positions = lens.scan._1
      val segments = Collection.fromArray(positions).zip(Collection.fromArray(lens))
      val flat_arr = arr.flatMap {i => i}
      NestedCollectionFlat(Collection.fromArray(flat_arr), segments)
    }
  }

  implicit def convertCollectionElem[A](e: Elem[Collection[A]]): CollectionElem[A, _] =
    e.asInstanceOf[CollectionElem[A, _]]

  implicit class CollectionOfPairsExtensions[A, B](coll: Coll[(A, B)]) {
    val collElem = convertCollectionElem(coll.selfType1)

    def asPairColl: PairColl[A, B] =
      PairCollectionAOS(coll)(collElem.eItem.eFst, collElem.eItem.eSnd)

    def as: Coll[A] = collElem match {
      case _: PairCollectionElem[_, _, _] => coll.asRep[PairCollection[A, B]].as
      case _ => coll.map(_._1)(collElem.eItem.eFst)
    }

    def bs: Coll[B] = coll.selfType1 match {
      case _: PairCollectionElem[_, _, _] => coll.asRep[PairCollection[A, B]].bs
      case _ => coll.map(_._2)(collElem.eItem.eSnd)
    }
  }
}

trait CollectionsDsl extends impl.CollectionsAbs { self: ScalanCommunityDsl =>
  implicit class CollectionExtensions[A](coll: Coll[A]) {
    implicit def eItem: Elem[A] = coll.selfType1.eItem

    def map[B: Elem](f: Rep[A] => Rep[B]): Coll[B] = coll.mapBy(fun(f))

    def filter(f: Rep[A] => Rep[Boolean]): Coll[A] = coll.filterBy(fun(f))

    def flatMap[B: Elem](f: Rep[A] => Coll[B]): Coll[B] = coll.flatMapBy(fun(f))
  }
}

trait CollectionsDslSeq extends impl.CollectionsSeq { self: ScalanCommunityDslSeq => }

trait CollectionsDslExp extends impl.CollectionsExp { self: ScalanCommunityDslExp =>
  override def rewriteDef[T](d: Def[T]) = d match {
    case ExpPairCollectionAOS(pairColl @ Def(_: PairCollection[_, _])) => pairColl
    case _ => super.rewriteDef(d)
  }
}
