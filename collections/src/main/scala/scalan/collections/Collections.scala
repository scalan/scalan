package scalan.collections

import java.lang.reflect.Method

import scala.annotation.unchecked.uncheckedVariance
import scalan._
import scalan.arrays.ArrayOps
import scalan.common.OverloadHack.Overloaded1
import scala.collection.mutable
import scala.annotation.tailrec
import scala.reflect.runtime.universe._

trait Collections { self: CollectionsDsl =>

  type Coll[+Item] = Rep[Collection[Item]]
  trait Collection[@uncheckedVariance +Item] extends Def[Collection[Item @uncheckedVariance]] {
    implicit def eItem: Elem[Item @uncheckedVariance]
    def length: Rep[Int]
    def arr: Rep[Array[Item @uncheckedVariance]]
    def lst: Rep[List[Item @uncheckedVariance]]
    def seq: Rep[SSeq[Item] @uncheckedVariance] = SSeq(arr)
    def apply(i: Rep[Int]): Rep[Item]
    @OverloadId("many")
    def apply(indices: Coll[Int])(implicit o: Overloaded1): Coll[Item]
    def mapBy[B: Elem](f: Rep[Item => B @uncheckedVariance]): Coll[B] //= Collection(arr.mapBy(f))
    def zip[B: Elem](ys: Coll[B]): PairColl[Item @uncheckedVariance, B] // = PairCollectionSOA(self, ys)
    def slice(offset: Rep[Int], length: Rep[Int]): Coll[Item]
    def reduce(implicit m: RepMonoid[Item @uncheckedVariance]): Rep[Item] //= arr.reduce(m)
    def update (idx: Rep[Int], value: Rep[Item @uncheckedVariance]): Coll[Item]
    def updateMany (idxs: Coll[Int], vals: Coll[Item @uncheckedVariance]): Coll[Item]
    def indexes: Coll[Int] = Collection.indexRange(length)
    def filterBy(f: Rep[Item @uncheckedVariance => Boolean]): Coll[Item]
    def flatMapBy[B: Elem](f: Rep[Item @uncheckedVariance => Collection[B]]): Coll[B] // = Collection(arr.flatMap {in => f(in).arr} )
    def append(value: Rep[Item @uncheckedVariance]): Coll[Item]  // = Collection(arr.append(value))
    def foldLeft[S: Elem](init: Rep[S], f: Rep[((S, Item)) => S]): Rep[S] = arr.fold(init, f)
    def sortBy[O: Elem](by: Rep[Item => O])(implicit o: Ordering[O]): Coll[Item]
    /*def scan(implicit m: RepMonoid[A @uncheckedVariance]): Rep[(Collection[A], A)] = {
      val arrScan = arr.scan(m)
      (Collection(arrScan._1), arrScan._2)
    } */
  }

  def emptyColl[A: Elem]: Coll[A] = element[Collection[A]].defaultRepValue
  type Segments1 = PairCollection[Int, Int]

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
    def sortBy[O: Elem](by: Rep[Unit => O])(implicit o: Ordering[O]): Coll[Unit] = ???
  }
  trait UnitCollectionCompanion extends ConcreteClass0[UnitCollection]

  abstract class CollectionOverArray[Item](val arr: Rep[Array[Item]])(implicit val eItem: Elem[Item]) extends Collection[Item] {
    def lst = arr.toList
    def length = arr.length
    def apply(i: Rep[Int]) = arr(i)
    def mapBy[B: Elem](f: Rep[Item => B @uncheckedVariance]): Coll[B] = Collection(arr.mapBy(f))
    def slice(offset: Rep[Int], length: Rep[Int]) = {
      val sl = arr.slice(offset, length)
      CollectionOverArray(sl)
    }
    @OverloadId("many")
    def apply(indices: Coll[Int])(implicit o: Overloaded1): Coll[Item] = CollectionOverArray(arr(indices.arr))
    def reduce(implicit m: RepMonoid[Item @uncheckedVariance]): Rep[Item] = arr.reduce(m)
    def zip[B: Elem](ys: Coll[B]): PairColl[Item, B] = PairCollectionSOA(self, ys)
    def update (idx: Rep[Int], value: Rep[Item]): Coll[Item] = CollectionOverArray(arr.update(idx, value))
    def updateMany (idxs: Coll[Int], vals: Coll[Item]): Coll[Item] = CollectionOverArray(arr.updateMany(idxs.arr, vals.arr))
    def filterBy(f: Rep[Item @uncheckedVariance => Boolean]): Coll[Item] = CollectionOverArray(arr.filterBy(f))
    def flatMapBy[B: Elem](f: Rep[Item @uncheckedVariance => Collection[B]]): Coll[B] = Collection(arr.flatMap {in => f(in).arr})
    def append(value: Rep[Item @uncheckedVariance]): Coll[Item]  = CollectionOverArray(arr.append(value))
    def sortBy[O: Elem](means: Rep[Item => O])(implicit o: Ordering[O]): Coll[Item] = CollectionOverArray(arr.sortBy(means))
  }
  trait CollectionOverArrayCompanion extends ConcreteClass1[CollectionOverArray]

  abstract class CollectionOverList[Item](val lst: Rep[List[Item]])(implicit val eItem: Elem[Item]) extends Collection[Item] {
    def length = lst.length
    def apply(i: Rep[Int]) = lst(i)
    def arr = lst.toArray
    def mapBy[B: Elem](f: Rep[Item => B @uncheckedVariance]): Coll[B] = CollectionOverList(lst.mapBy(f))
    def slice(offset: Rep[Int], length: Rep[Int]) = CollectionOverList(lst.slice(offset, length))
    @OverloadId("many")
    def apply(indices: Coll[Int])(implicit o: Overloaded1): Coll[Item] = CollectionOverList(lst(indices.arr))
    def reduce(implicit m: RepMonoid[Item @uncheckedVariance]): Rep[Item] = lst.reduce(m)
    def zip[B: Elem](ys: Coll[B]): PairColl[Item, B] = PairCollectionSOA(self, ys)
    def update (idx: Rep[Int], value: Rep[Item]): Coll[Item] = ???
    def updateMany (idxs: Coll[Int], vals: Coll[Item]): Coll[Item] = ???
    def filterBy(f: Rep[Item @uncheckedVariance => Boolean]): Coll[Item] = CollectionOverList(lst.filterBy(f))
    def flatMapBy[B: Elem](f: Rep[Item @uncheckedVariance => Collection[B]]): Coll[B] =
      CollectionOverList(lst.flatMap { in => f(in).lst } )
    def append(value: Rep[Item @uncheckedVariance]): Coll[Item]  = CollectionOverList(value :: lst)
    def sortBy[O: Elem](by: Rep[Item => O])(implicit o: Ordering[O]): Coll[Item] = ???
  }
  trait CollectionOverListCompanion extends ConcreteClass1[CollectionOverList]

  abstract class CollectionOverSeq[Item](override val seq: Rep[SSeq[Item]])(implicit val eItem: Elem[Item]) extends Collection[Item] {
    def arr = seq.toArray
    def lst = seq.toList
    def length = seq.size
    def apply(i: Rep[Int]) = seq(i)
    def slice(offset: Rep[Int], length: Rep[Int]) = CollectionOverSeq(seq.slice(offset, offset + length))
    @OverloadId("many")
    def apply(indices: Coll[Int])(implicit o: Overloaded1): Coll[Item] = {
      CollectionOverSeq(SSeq(indices.arr.map(i => seq(i))))
    }
    def mapBy[B: Elem](f: Rep[Item => B @uncheckedVariance]): Coll[B] = CollectionOverSeq(seq.map(f))
    def reduce(implicit m: RepMonoid[Item @uncheckedVariance]): Rep[Item] = {
      val r = fun { p: Rep[(Item,Item)] => val Pair(x,y) = p; m.append(x,y) }
      seq.reduce(r)
    }
    def zip[B: Elem](ys: Coll[B]): PairColl[Item, B] = PairCollectionSOA(CollectionOverSeq(seq), ys)
    def update (idx: Rep[Int], value: Rep[Item]): Coll[Item] = ???
    def updateMany (idxs: Coll[Int], vals: Coll[Item]): Coll[Item] = ???
    def filterBy(f: Rep[Item @uncheckedVariance => Boolean]): Coll[Item] = ???
    def flatMapBy[B: Elem](f: Rep[Item @uncheckedVariance => Collection[B]]): Coll[B] = ???
    def append(value: Rep[Item @uncheckedVariance]): Coll[Item]  = ???
    def sortBy[O: Elem](by: Rep[Item => O])(implicit o: Ordering[O]): Coll[Item] = ???
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
    def innerJoin[C, R](other: PairColl[A, C], f: Rep[((B, C)) => R])(implicit ordK: Ordering[A], eR: Elem[R], eB: Elem[B], eC: Elem[C]): PairColl[A, R]
    def outerJoin[C, R](other: PairColl[A, C], f: Rep[((B, C)) => R], f1: Rep[B => R], f2: Rep[C => R])(implicit ordK: Ordering[A], eR: Elem[R], eB: Elem[B], eC: Elem[C]): PairColl[A, R]
    def innerMult(other: PairColl[A, B])(implicit ordK: Ordering[A], nA: Numeric[A], nB: Numeric[B]) = {
      innerJoin[B, B](other, (b1: Rep[(B, B)]) => b1._1 * b1._2)
    }
    def outerSum(other: PairColl[A, B])(implicit ordK: Ordering[A], nA: Numeric[A], nB: Numeric[B]) = {
      outerJoin[B, B](other, (b1: Rep[(B, B)]) => b1._1 + b1._2, (b: Rep[B]) => b, (b: Rep[B]) => b)
    }
    def outerSubtr(other: PairColl[A, B])(implicit ordK: Ordering[A], nA: Numeric[A], nB: Numeric[B]) = {
      outerJoin[B, B](other, (b1: Rep[(B, B)]) => b1._1 - b1._2, (b: Rep[B]) => b, (b: Rep[B]) => b)
    }
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
    def innerJoin[C, R](other: PairColl[A, C], f: Rep[((B, C)) => R])
                       (implicit ordK: Ordering[A], eR: Elem[R], eB: Elem[B], eC: Elem[C]): PairColl[A, R] = {
      val innerJoined = Collection(pairColl_innerJoin[A, B, C, R](this, other, f))
      PairCollectionSOA(innerJoined.as, innerJoined.bs)
    }
    def outerJoin[C, R](other: PairColl[A, C], f: Rep[((B, C)) => R], f1: Rep[B => R], f2: Rep[C => R])
                       (implicit ordK: Ordering[A], eR: Elem[R], eB: Elem[B], eC: Elem[C]) = {
      val outerJoined = Collection(pairColl_outerJoin[A, B, C, R](this, other, f, f1, f2))
      PairCollectionSOA(outerJoined.as, outerJoined.bs)
    }
    def sortBy[O: Elem](means: Rep[((A, B)) => O])(implicit o: Ordering[O]): PairColl[A, B] = {
      val sorted = Collection(arr.sortBy(means))
      PairCollectionSOA(sorted.as, sorted.bs)
    }
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
    def innerJoin[C, R](other: PairColl[A, C], f: Rep[((B, C)) => R])
                       (implicit ordK: Ordering[A], eR: Elem[R], eB: Elem[B], eC: Elem[C]) = {
      PairCollectionAOS.fromArray(pairColl_innerJoin[A, B, C, R](this, other, f))
    }
    def outerJoin[C, R](other: PairColl[A, C], f: Rep[((B, C)) => R], f1: Rep[B => R], f2: Rep[C => R])
                       (implicit ordK: Ordering[A], eR: Elem[R], eB: Elem[B], eC: Elem[C]) = {
      PairCollectionAOS.fromArray(pairColl_outerJoin[A, B, C, R](this, other, f, f1, f2))
    }
    def sortBy[O: Elem](means: Rep[((A, B)) => O])(implicit o: Ordering[O]): PairColl[A, B] = PairCollectionAOS(Collection(arr.sortBy(means)))
  }
  trait PairCollectionAOSCompanion extends ConcreteClass2[PairCollectionAOS] {
    def fromArray[A: Elem, B: Elem](arr: Arr[(A, B)]) = PairCollectionAOS(CollectionOverArray(arr))
  }

  trait NestedCollection[A] extends Collection[Collection[A]] {
    implicit def eA: Elem[A]
    def values: Rep[Collection[A]]
    def nestedValues: Rep[Collection[Collection[A]]]
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
    def nestedValues = segments.map { case Pair(o, l) => values.slice(o,l) }

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
    def sortBy[O: Elem](by: Rep[Collection[A] => O])(implicit o: Ordering[O]): NColl[A] = ???
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

  abstract class CompoundCollection[A](val nestedValues: Coll[Collection[A]])(implicit val eA: Elem[A])
    extends NestedCollection[A] {
    lazy val eItem = collectionElement(eA)
    def segments = {
      val offsets = Collection(nestedValues.map(_.length).arr.scan._1)
      val lengths = nestedValues.map(_.length)
      offsets zip lengths
    }
    def values = {
      nestedValues.flatMap(row => row)
    }
    def length = nestedValues.length
    def apply(i: Rep[Int]) = nestedValues(i)
    def arr = nestedValues.arr
    def lst = arr.toList
    def slice(offset: Rep[Int], length: Rep[Int]): NColl[A] = CompoundCollection(nestedValues.slice(offset, length))
    @OverloadId("many")
    def apply(indices: Coll[Int])(implicit o: Overloaded1): NColl[A] = {
      CompoundCollection(nestedValues(indices))
    }
    def mapBy[B: Elem](f: Rep[Collection[A] => B @uncheckedVariance]): Coll[B] = Collection(arr.mapBy(f))
    def reduce(implicit m: RepMonoid[Collection[A @uncheckedVariance]]): Coll[A] = arr.reduce(m)
    def zip[B: Elem](ys: Coll[B]): PairColl[Collection[A], B] = PairCollectionSOA(self, ys)
    def update (idx: Rep[Int], value: Rep[Collection[A]]): NColl[A] = CompoundCollection(nestedValues.update(idx, value))
    def updateMany (idxs: Coll[Int], vals: Coll[Collection[A]]): NColl[A] = ???
    def filterBy(f: Rep[Collection[A @uncheckedVariance] => Boolean]): NColl[A] = ???
    def flatMapBy[B: Elem](f: Rep[Collection[A @uncheckedVariance] => Collection[B]]): Coll[B] =
      nestedValues.flatMap { in => f(in) }
    def append(value: Rep[Collection[A @uncheckedVariance]]): NColl[A]  = ??? //Collection(arr.append(value))
    def sortBy[O: Elem](by: Rep[Collection[A] => O])(implicit o: Ordering[O]): NColl[A] = ???
  }
  trait CompoundCollectionCompanion extends ConcreteClass1[CompoundCollection] {
    def fromJuggedArray[T: Elem](xss: Rep[Array[Array[T]]]): Rep[CompoundCollection[T]] = {
      val nestedValues = Collection(xss.map(xs => Collection(xs)))
      CompoundCollection(nestedValues)
    }
  }

  abstract class FuncCollection[A,B,Env]
      (val env1: Coll[Env], val indexedFunc: Rep[((Int, A)) => B])
      (implicit val eA: Elem[A], val eB: Elem[B], val eEnv: Elem[Env])
    extends Collection[A => B] {
    lazy val eItem = element[A => B]

    def arr = ???
    def lst = ???
    def length = env1.length
    def apply(i: Rep[Int]) = fun { x: Rep[A] => indexedFunc(Pair(i, x)) }

    def mapBy[R: Elem](f: Rep[((A => B)) => R @uncheckedVariance]): Coll[R] = {
      val range = SArray.rangeFrom0(env1.length)
      Collection(range.mapBy(fun { i: Rep[Int] =>
        val itemFunc = apply(i)
        f(itemFunc)
      }))
    }

    @OverloadId("many")
    def apply(indices: Coll[Int])(implicit o: Overloaded1): Coll[A=>B] = {
      indices.mapBy(fun { i: Rep[Int] => apply(i) })
    }

    def slice(offset: Rep[Int], length: Rep[Int]) = ???
    def reduce(implicit m: RepMonoid[A=>B @uncheckedVariance]): Rep[A=>B] = ???
    def zip[C: Elem](ys: Coll[C]): PairColl[A=>B, C] = PairCollectionSOA(self, ys)
    def update (idx: Rep[Int], value: Rep[A=>B]): Coll[A=>B] = ???
    def updateMany (idxs: Coll[Int], vals: Coll[A=>B]): Coll[A=>B] = ???
    def filterBy(f: Rep[(A=>B) => Boolean]): Coll[A=>B] = ???
    def flatMapBy[R: Elem](f: Rep[((A=>B)) => Collection[R]]): Coll[R] = ???
    def append(value: Rep[A=>B]): Coll[A=>B]  = ???
    def sortBy[O: Elem](by: Rep[(A=>B) => O])(implicit o: Ordering[O]): Coll[A=>B] = ???
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

  abstract class StructItemCollection[Val, Schema <: Struct]
        (val struct: Rep[Schema])
        (implicit val eVal: Elem[Val], val eSchema: Elem[Schema])
    extends Collection[StructItem[Val,Schema]] {
    def eItem = element[StructItem[Val,Schema]]

    private def itemSymbols: Seq[Rep[StructItem[Val,Schema]]] = {
      val len = eSchema.fields.length
      val syms = Seq.tabulate(len) { i =>
        val item = struct.getItem(i).asRep[StructItem[Val, Schema]]
        item
      }
      syms
    }

    def length = eSchema.fields.length
    def arr: Arr[StructItem[Val,Schema]] = SArray.fromSyms(itemSymbols)
    def lst = arr.toList
    def apply(i: Rep[Int]) = struct.getItem(i).asRep[StructItem[Val,Schema]]

    @OverloadId("many")
    def apply(indices: Coll[Int])(implicit o: Overloaded1) = ???

    def mapBy[B: Elem](f: Rep[(StructItem[Val, Schema]) => B]) = {
      val syms = itemSymbols.map { item =>
        f(item)
      }
      Collection(SArray.fromSyms(syms))
    }

    def zip[B: Elem](ys: Coll[B]): PairColl[StructItem[Val,Schema], B] = {
      def error = !!!(s"Argument is not StructItemCollection: $ys", self)
      ys.selfType1 match {
        case ce: CollectionElem[b,_] => ce.eItem match {
          case eSI: StructItemElem[v, s, _] =>
            if (eSchema != eSI.eSchema)
              !!!(s"Can zip StructItemCollections only with the same Schema but found $eSchema and ${eSI.eSchema}")
            PairCollectionSOA(self, ys)(eItem, ce.eItem.asElem[B])
          case _ => error
        }
        case _ => error
      }
    }

    def slice(offset: Rep[Int], length: Rep[Int]) = ???

    def reduce(implicit m: RepMonoid[StructItem[Val, Schema]]) = ???

    def update(idx: Rep[Int], value: Rep[StructItem[Val, Schema]]) = ???

    def updateMany(idxs: Coll[Int], vals: Coll[StructItem[Val, Schema]]) = ???

    def filterBy(f: Rep[(StructItem[Val, Schema]) => Boolean]) = ???

    def flatMapBy[B: Elem](f: Rep[(StructItem[Val, Schema]) => Collection[B]]) = ???

    def append(value: Rep[StructItem[Val, Schema]]) = ???

    def sortBy[O: Elem](by: Rep[(StructItem[Val, Schema]) => O])(implicit o: Ordering[O]) = ???
  }
}

trait CollectionsDsl extends impl.CollectionsAbs with SeqsDsl {

  trait CollectionFunctor extends Functor[Collection] {
    def tag[A](implicit evA: WeakTypeTag[A]) = weakTypeTag[Collection[A]]
    def lift[A](implicit evA: Elem[A]) = element[Collection[A]]
    def unlift[T](implicit eFT: Elem[Collection[T]]) = eFT.asInstanceOf[CollectionElem[T,_]].eItem
    def getElem[T](fa: Rep[Collection[T]]) = fa.selfType1
    def unapply[A](e: Elem[_]) = e match {
      case te: CollectionElem[_, _] => Some(te.asElem[Collection[A]])
      case _ => None
    }
    def map[A:Elem,B:Elem](xs: Rep[Collection[A]])(f: Rep[A] => Rep[B]) = xs.map(f)
  }
  implicit val collectionContainer: Functor[Collection] = new CollectionFunctor {}

  implicit class CollectionExtensions[A](coll: Coll[A]) {
    implicit def eItem: Elem[A] = coll.selfType1.eItem

    def map[B: Elem](f: Rep[A] => Rep[B]): Coll[B] = coll.mapBy(fun(f))

    def filter(f: Rep[A] => Rep[Boolean]): Coll[A] = coll.filterBy(fun(f))

    def flatMap[B: Elem](f: Rep[A] => Coll[B]): Coll[B] = coll.flatMapBy(fun(f))

    def :+(x: Rep[A]) = coll.append(x)
  }

  trait CollectionManager {
    def apply[T: Elem](arr: Rep[Array[T]]): Coll[T]
    def fromArray[T: Elem](arr: Rep[Array[T]]): Coll[T]
    def fromList[T: Elem](arr: Rep[List[T]]): Coll[T]
    def replicate[T: Elem](len: Rep[Int], v: Rep[T]): Coll[T]
    def empty[T: Elem]: Coll[T]
    def singleton[T: Elem](v: Rep[T]): Coll[T]
    def indexRange(l: Rep[Int]): Coll[Int]
  }

  def pairColl_innerJoin[K, B, C, R](xs: PairColl[K, B], ys: PairColl[K, C], f: Rep[((B, C)) => R])
                                    (implicit ordK: Ordering[K], selfType: Elem[Array[(K, R)]],
                                     eK: Elem[K], eR: Elem[R], eB: Elem[B], eC: Elem[C]): Rep[Array[(K, R)]]

  def pairColl_outerJoin[K, B, C, R](xs: PairColl[K, B], ys: PairColl[K, C], f: Rep[((B, C)) => R], f1: Rep[B => R], f2: Rep[C => R])
                                    (implicit ordK: Ordering[K], selfType: Elem[Array[(K, R)]],
                                     eK: Elem[K], eR: Elem[R], eB: Elem[B], eC: Elem[C]): Rep[Array[(K, R)]]
}

trait CollectionsDslStd extends impl.CollectionsStd with SeqsDslStd {

  def pairColl_innerJoin[K, B, C, R](xs: PairColl[K, B], ys: PairColl[K, C], f: Rep[((B, C)) => R])
                                    (implicit ordK: Ordering[K], selfType: Elem[Array[(K, R)]],
                                     eK: Elem[K], eR: Elem[R], eB: Elem[B], eC: Elem[C]): Rep[Array[(K, R)]] = {

    val xIter = xs.arr.iterator
    val yIter = ys.arr.iterator

    val buffer = mutable.ArrayBuffer[(K, R)]()

    @tailrec
    def go(keyX: K, keyY: K, valueX: B, valueY: C) {
      val cmp = ordK.compare(keyX, keyY)
      if (cmp == 0) {
        // keyX == keyY
        val value = f((valueX, valueY))
        buffer.append((keyX, value))
        if (xIter.hasNext && yIter.hasNext) {
          val (keyX1, valueX1) = xIter.next()
          val (keyY1, valueY1) = yIter.next()
          go(keyX1, keyY1, valueX1, valueY1)
        }
      } else if (cmp < 0) {
        // keyX < keyY
        if (xIter.hasNext) {
          val (keyX1, valueX1) = xIter.next()
          go(keyX1, keyY, valueX1, valueY)
        }
      } else {
        // keyY < keyX
        if (yIter.hasNext) {
          val (keyY1, valueY1) = yIter.next()
          go(keyX, keyY1, valueX, valueY1)
        }
      }
    }
    if (xIter.hasNext && yIter.hasNext) {
      val (keyX1, valueX1) = xIter.next()
      val (keyY1, valueY1) = yIter.next()
      go(keyX1, keyY1, valueX1, valueY1)
    }
    buffer.toArray
  }

  def pairColl_outerJoin[K, B, C, R](xs: PairColl[K, B], ys: PairColl[K, C], f: Rep[((B, C)) => R], f1: Rep[B => R], f2: Rep[C => R])
                                    (implicit ordK: Ordering[K], selfType: Elem[Array[(K, R)]],
                                     eK: Elem[K], eR: Elem[R], eB: Elem[B], eC: Elem[C]): Rep[Array[(K, R)]] = {
    val xIter = xs.arr.iterator
    val yIter = ys.arr.iterator

    val buffer = mutable.ArrayBuffer[(K, R)]()

    // called only when yIter is empty
    def finishX() {
      xIter.foreach { kv => buffer.append((kv._1, f1(kv._2))) }
    }

    // called only when xIter is empty
    def finishY() {
      yIter.foreach { kv => buffer.append((kv._1, f2(kv._2))) }
    }

    @tailrec
    def go(keyX: K, keyY: K, valueX: B, valueY: C) {
      val cmp = ordK.compare(keyX, keyY)
      if (cmp == 0) {
        // keyX == keyY
        val value = f((valueX, valueY))
        buffer.append((keyX, value))
        (xIter.hasNext, yIter.hasNext) match {
          case (true, true) =>
            val (keyX1, valueX1) = xIter.next()
            val (keyY1, valueY1) = yIter.next()
            go(keyX1, keyY1, valueX1, valueY1)
          case (true, false) =>
            finishX()
          case (false, true) =>
            finishY()
          case _ => {}
        }
      } else if (cmp < 0) {
        // keyX < keyY
        val value = f1(valueX)
        buffer.append((keyX, value))
        if (xIter.hasNext) {
          val (keyX1, valueX1) = xIter.next()
          go(keyX1, keyY, valueX1, valueY)
        } else {
          buffer.append((keyY, f2(valueY)))
          finishY()
        }
      } else {
        // keyY < keyX
        val value = f2(valueY)
        buffer.append((keyY, value))
        if (yIter.hasNext) {
          val (keyY1, valueY1) = yIter.next()
          go(keyX, keyY1, valueX, valueY1)
        } else {
          buffer.append((keyX, f1(valueX)))
          finishX()
        }
      }
    }
    (xIter.hasNext, yIter.hasNext) match {
      case (true, true) =>
        val (keyX1, valueX1) = xIter.next()
        val (keyY1, valueY1) = yIter.next()
        go(keyX1, keyY1, valueX1, valueY1)
      case (true, false) =>
        finishX()
      case (false, true) =>
        finishY()
      case _ => {}
    }
    buffer.toArray
  }
}

trait CollectionsDslExp extends impl.CollectionsExp with SeqsDslExp {

  override def rewriteDef[T](d: Def[T]) = d match {
//    case ExpPairCollectionAOS(pairColl @ Def(_: PairCollection[_, _])) => pairColl
    case _ => super.rewriteDef(d)
  }

  override protected def getResultElem(receiver: Exp[_], m: Method, args: List[AnyRef]): Elem[_] = receiver.elem match {
    case e: StructItemCollectionElem[v,s] => m.getName match {
      case "apply" => structItemElement(e.eVal, e.eSchema.asElem[Struct])
      case "zip" =>
        val eA = structItemElement(e.eVal, e.eSchema.asElem[Struct])
        val eB = args(0).asInstanceOf[Coll[Any]].elem.eItem
        pairCollectionElement(eA, eB)
      case _ => super.getResultElem(receiver, m, args)
    }
    case e: PairCollectionElem[a,b,_] => m.getName match {
      case "apply" => e.eItem
      case _ => super.getResultElem(receiver, m, args)
    }
    case e: CollectionElem[t, _] => e.eItem match {
      case pe: PairElem[a,b] => m.getName match {
        case "as" => collectionElement(pe.eFst)
        case "bs" => collectionElement(pe.eSnd)
        case _ => super.getResultElem(receiver, m, args)
      }
      case _ => super.getResultElem(receiver, m, args)
    }

    case e: StructItemElem[v,s,_] => m.getName match {
      case "value" => e.eVal
      case _ => super.getResultElem(receiver, m, args)
    }
    case _ => super.getResultElem(receiver, m, args)
  }

  def pairColl_innerJoin[K, B, C, R](xs: PairColl[K, B], ys: PairColl[K, C], f: Rep[((B, C)) => R])
                                    (implicit ordK: Ordering[K], selfType: Elem[Array[(K, R)]],
                                     eK: Elem[K], eR: Elem[R], eB: Elem[B], eC: Elem[C]): Rep[Array[(K, R)]] = {
    ArrayInnerJoin(xs.arr, ys.arr, f)
  }

  def pairColl_outerJoin[K, B, C, R](xs: PairColl[K, B], ys: PairColl[K, C], f: Rep[((B, C)) => R], f1: Rep[B => R], f2: Rep[C => R])
                                    (implicit ordK: Ordering[K], selfType: Elem[Array[(K, R)]], eK: Elem[K], eR: Elem[R], eB: Elem[B], eC: Elem[C]): Rep[Array[(K, R)]] = {
    ArrayOuterJoin(xs.arr, ys.arr, f, f1, f2)
  }

  case class ArrayInnerJoin[K, B, C, R](xs: Exp[Array[(K, B)]], ys: Exp[Array[(K, C)]], f: Exp[((B, C)) => R])
                                       (implicit val ordK: Ordering[K], val selfType: Elem[Array[(K, R)]],
                                        val eK: Elem[K], val eR: Elem[R], val eB: Elem[B], val eC: Elem[C])
    extends Def[Array[(K, R)]] {
  }

  case class ArrayOuterJoin[K, B, C, R](xs: Exp[Array[(K, B)]], ys: Exp[Array[(K, C)]], f: Rep[((B, C)) => R],
                                        f1: Rep[B => R], f2: Rep[C => R])
                                       (implicit val ordK: Ordering[K], val selfType: Elem[Array[(K, R)]],
                                        val eK: Elem[K], val eR: Elem[R], val eB: Elem[B], val eC: Elem[C])
    extends Def[Array[(K, R)]] {
  }
}
