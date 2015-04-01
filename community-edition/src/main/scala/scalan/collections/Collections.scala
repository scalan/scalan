package scalan.collections

import scala.annotation.unchecked.uncheckedVariance
import scalan._
import scalan.arrays.ArrayOps
import scalan.common.Default
import scalan.common.OverloadHack.Overloaded1

trait Collections extends ArrayOps with ListOps { self: ScalanCommunityDsl =>

  type Coll[+A] = Rep[Collection[A]]
  trait Collection[@uncheckedVariance +A] extends Reifiable[Collection[A @uncheckedVariance]] {
    implicit def elem: Elem[A @uncheckedVariance]
    def length: Rep[Int]
    def arr: Rep[Array[A @uncheckedVariance]]
    def lst: Rep[List[A @uncheckedVariance]]
    def seq: Rep[SSeq[A] @uncheckedVariance] = SSeq(arr)
    def apply(i: Rep[Int]): Rep[A]
    @OverloadId("many")
    def apply(indices: Coll[Int])(implicit o: Overloaded1): Coll[A]
    def map[B: Elem](f: Rep[A @uncheckedVariance] => Rep[B]): Coll[B] //= Collection(arr.map(f))
    def mapBy[B: Elem](f: Rep[A => B @uncheckedVariance]): Coll[B] //= Collection(arr.mapBy(f))
    def zip[B: Elem](ys: Coll[B]): Coll[(A, B)] // = PairCollection(self, ys)
    def slice(offset: Rep[Int], length: Rep[Int]): Rep[Collection[A]]
    def reduce(implicit m: RepMonoid[A @uncheckedVariance]): Rep[A] //= arr.reduce(m)
    def update (idx: Rep[Int], value: Rep[A @uncheckedVariance]): Coll[A]
    def updateMany (idxs: Coll[Int], vals: Coll[A @uncheckedVariance]): Coll[A]
    def indexes: Coll[Int] = Collection.indexRange(length)
    def filter(f: Rep[A @uncheckedVariance] => Rep[Boolean]): Coll[A]
    def flatMap[B: Elem](f: Rep[A @uncheckedVariance] => Coll[B]): Coll[B] // = Collection(arr.flatMap {in => f(in).arr} )
    def append(value: Rep[A @uncheckedVariance]): Coll[A]  // = Collection(arr.append(value))
    /*def scan(implicit m: RepMonoid[A @uncheckedVariance]): Rep[(Collection[A], A)] = {
      val arrScan = arr.scan(m)
      (Collection(arrScan._1), arrScan._2)
    } */
  }

  def emptyColl[A: Elem]: Coll[A] = element[Collection[A]].defaultRepValue
  type Segments1 = Collection[(Int, Int)]

  trait CollectionCompanion extends TypeFamily1[Collection] {
    def defaultOf[A](implicit ea: Elem[A]): Default[Rep[Collection[A]]] = ea match {
      case UnitElement => Default.defaultVal(UnitCollection(0).asRep[Collection[A]])
      case baseE: BaseElem[a] => BaseCollection.defaultOf[a](baseE)
      case pairE: PairElem[a, b] => PairCollection.defaultOf[a, b](pairE.eFst, pairE.eSnd)
      case e => ???(s"Element is $e")
    }

    def apply[T: Elem](arr: Rep[Array[T]]): Coll[T] = fromArray(arr)
    def fromArray[T: Elem](arr: Rep[Array[T]]): Coll[T] = {
      element[T] match {
        case baseE: BaseElem[a] =>
          BaseCollection[a](arr.asRep[Array[a]])
        case pairE: PairElem[a, b] =>
          implicit val ea = pairE.eFst
          implicit val eb = pairE.eSnd
          val ps = arr.asRep[Array[(a, b)]]
          val as = fromArray(ps.map { _._1 })
          val bs = fromArray(ps.map { _._2 })
          as zip bs //PairCollection[a,b](as, bs)
        case viewE: ViewElem[a, b] =>
          // TODO
          BaseCollection[b](arr.asRep[Array[b]])
        case e => ???(s"Element is $e")
      }
    }
    def fromList[T: Elem](arr: Rep[List[T]]): Coll[T] = {
      element[T] match {
        case baseE: BaseElem[a] =>
          ListCollection[a](arr.asRep[List[a]])
        case pairE: PairElem[a, b] =>
          implicit val ea = pairE.eFst
          implicit val eb = pairE.eSnd
          val ps = arr.asRep[List[(a, b)]]
          val as = fromList(ps.map { _._1 })
          val bs = fromList(ps.map { _._2 })
          as zip bs //PairCollection[a,b](as, bs)
        case viewE: ViewElem[a, b] =>
          // TODO
          ListCollection[b](arr.asRep[List[b]])
        case e => ???(s"Element is $e")
      }
    }

    def replicate[T: Elem](len: Rep[Int], v: Rep[T]): Coll[T] = {
      element[T] match {
        case baseE: BaseElem[a] =>
          BaseCollection[a](array_replicate(len, v.asRep[a]))
        case pairElem: PairElem[a ,b] => {
          implicit val ea = pairElem.eFst
          implicit val eb = pairElem.eSnd
          val ps = v.asRep[(a, b)]
          val as = replicate(len, ps._1)
          val bs = replicate(len, ps._2)
          as zip bs
        }
        case viewElem: ViewElem[a, b] =>
          BaseCollection(SArray.replicate(len, v))
        case e => ???(s"Element is $e")
      }
    }

    def singleton[T: Elem](v: Rep[T]): Coll[T] = {
      element[T] match {
        case paE: CollectionElem[_, _] => ???
        case _ => replicate(toRep(1), v)
      }
    }
    def indexRange(l: Rep[Int]): Coll[Int] = BaseCollection(array_rangeFrom0(l))
  }

  abstract class UnitCollection(val length: Rep[Int]) extends Collection[Unit] {
    def elem = UnitElement
    def arr = SArray.replicate(length, ())
    def lst = SList.replicate(length, ())
    def apply(i: Rep[Int]) = ()
    def map[B: Elem](f: Rep[Unit] => Rep[B]): Coll[B] = Collection(arr.map(f))
    def mapBy[B: Elem](f: Rep[Unit => B @uncheckedVariance]): Coll[B] = Collection(arr.mapBy(f))
    @OverloadId("many")
    def apply(indices: Coll[Int])(implicit o: Overloaded1): Coll[Unit] = UnitCollection(indices.length)
    def slice(offset: Rep[Int], length: Rep[Int]) = UnitCollection(length)
    def reduce(implicit m: RepMonoid[Unit @uncheckedVariance]): Rep[Unit] = ()
    def zip[B: Elem](ys: Coll[B]): Coll[(Unit, B)] = PairCollection(self, ys)
    def update (idx: Rep[Int], value: Rep[Unit]): Coll[Unit] = self
    def updateMany (idxs: Coll[Int], vals: Coll[Unit]): Coll[Unit] = self //UnitCollection(arr.updateMany(idxs.arr, vals.arr))
    def filter(f: Rep[Unit] => Rep[Boolean]): Coll[Unit] = Collection(arr.filter(f))
    def flatMap[B: Elem](f: Rep[Unit] => Coll[B]): Coll[B] = Collection(arr.flatMap {in => f(in).arr} )
    def append(value: Rep[Unit]): Coll[Unit]  = Collection(arr.append(value))
  }
  trait UnitCollectionCompanion extends ConcreteClass0[UnitCollection] {
    def defaultOf = Default.defaultVal(UnitCollection(0))
  }

  abstract class BaseCollection[A](val arr: Rep[Array[A]])(implicit val eA: Elem[A]) extends Collection[A] {
    def elem = eA
    def lst = arr.toList
    def length = arr.length
    def apply(i: Rep[Int]) = arr(i)
    def map[B: Elem](f: Rep[A @uncheckedVariance] => Rep[B]): Coll[B] = Collection(arr.map(f))
    def mapBy[B: Elem](f: Rep[A => B @uncheckedVariance]): Coll[B] = Collection(arr.mapBy(f))
    def slice(offset: Rep[Int], length: Rep[Int]) = {
      val sl = arr.slice(offset, length)
      BaseCollection(sl)
    }
    @OverloadId("many")
    def apply(indices: Coll[Int])(implicit o: Overloaded1): Coll[A] = BaseCollection(arr(indices.arr))
    def reduce(implicit m: RepMonoid[A @uncheckedVariance]): Rep[A] = arr.reduce(m)
    def zip[B: Elem](ys: Coll[B]): Coll[(A, B)] = PairCollection(self, ys)
    def update (idx: Rep[Int], value: Rep[A]): Coll[A] = BaseCollection(arr.update(idx, value))
    def updateMany (idxs: Coll[Int], vals: Coll[A]): Coll[A] = BaseCollection(arr.updateMany(idxs.arr, vals.arr))
    def filter(f: Rep[A @uncheckedVariance] => Rep[Boolean]): Coll[A] = BaseCollection(arr.filter(f))
    def flatMap[B: Elem](f: Rep[A @uncheckedVariance] => Coll[B]): Coll[B] = BaseCollection(arr.flatMap {in => f(in).arr} )
    def append(value: Rep[A @uncheckedVariance]): Coll[A]  = BaseCollection(arr.append(value))
  }
  trait BaseCollectionCompanion extends ConcreteClass1[BaseCollection] {
    def defaultOf[A](implicit ea: Elem[A]) = {
      implicit val ct = ea.classTag
      Default.defaultVal(BaseCollection(Array.empty[A]))
    }
  }

  abstract class ListCollection[A](val lst: Rep[List[A]])(implicit val eA: Elem[A]) extends Collection[A] {
    def elem = eA
    def length = lst.length
    def apply(i: Rep[Int]) = lst(i)
    def arr = lst.toArray
    def map[B: Elem](f: Rep[A @uncheckedVariance] => Rep[B]): Coll[B] = ListCollection(lst.map(f))
    def mapBy[B: Elem](f: Rep[A => B @uncheckedVariance]): Coll[B] = ListCollection(lst.mapBy(f))
    def slice(offset: Rep[Int], length: Rep[Int]) = ListCollection(lst.slice(offset, length))
    @OverloadId("many")
    def apply(indices: Coll[Int])(implicit o: Overloaded1): Coll[A] = ListCollection(lst(indices.arr))
    def reduce(implicit m: RepMonoid[A @uncheckedVariance]): Rep[A] = lst.reduce(m)
    def zip[B: Elem](ys: Coll[B]): Coll[(A, B)] = PairCollection(self, ys)
    def update (idx: Rep[Int], value: Rep[A]): Coll[A] = ???
    def updateMany (idxs: Coll[Int], vals: Coll[A]): Coll[A] = ???
    def filter(f: Rep[A @uncheckedVariance] => Rep[Boolean]): Coll[A] = ListCollection(lst.filter(f))
    def flatMap[B: Elem](f: Rep[A @uncheckedVariance] => Coll[B]): Coll[B] = ListCollection(lst.flatMap {in => f(in).arr} )
    def append(value: Rep[A @uncheckedVariance]): Coll[A]  = ListCollection(value :: lst)
  }
  trait ListCollectionCompanion extends ConcreteClass1[ListCollection] {
    def defaultOf[A](implicit ea: Elem[A]) =
      Default.defaultVal(ListCollection(List.empty[A]))
  }

  abstract class CollectionOnSeq[A](override val seq: Rep[SSeq[A]])(implicit val eA: Elem[A]) extends Collection[A] {
    def elem = eA
    def arr = seq.toArray
    def lst = seq.toList
    def length = seq.size
    def apply(i: Rep[Int]) = seq(i)
    def slice(offset: Rep[Int], length: Rep[Int]) = CollectionOnSeq(seq.slice(offset, offset + length))
    @OverloadId("many")
    def apply(indices: Coll[Int])(implicit o: Overloaded1): Coll[A] = {
      CollectionOnSeq(SSeq(indices.arr.map(i => seq(i))))
    }
    def map[B: Elem](f: Rep[A @uncheckedVariance] => Rep[B]): Coll[B] = ???
    def mapBy[B: Elem](f: Rep[A => B @uncheckedVariance]): Coll[B] = ???
    def reduce(implicit m: RepMonoid[A @uncheckedVariance]): Rep[A] = ???
    def zip[B: Elem](ys: Coll[B]): Coll[(A, B)] = ???
    def update (idx: Rep[Int], value: Rep[A]): Coll[A] = ???
    def updateMany (idxs: Coll[Int], vals: Coll[A]): Coll[A] = ???
    def filter(f: Rep[A @uncheckedVariance] => Rep[Boolean]): Coll[A] = ???
    def flatMap[B: Elem](f: Rep[A @uncheckedVariance] => Coll[B]): Coll[B] = ???
    def append(value: Rep[A @uncheckedVariance]): Coll[A]  = ???
  }
  trait CollectionOnSeqCompanion extends ConcreteClass1[CollectionOnSeq] {
    def defaultOf[A](implicit ea: Elem[A]) =
      Default.defaultVal(CollectionOnSeq(SSeq.empty[A]))
  }

  trait IPairCollection[A,B] extends Collection[(A,B)] {
    implicit def eA: Elem[A]
    implicit def eB: Elem[B]
    def as: Rep[Collection[A]]
    def bs: Rep[Collection[B]]
  }

  implicit def convertPairColl[A:Elem,B:Elem](x: Coll[(A,B)]): Rep[IPairCollection[A, B]] = x.asRep[IPairCollection[A, B]]
  implicit def eIPairColl[A: Elem, B:Elem]: Elem[IPairCollection[A,B]] = element[PairCollection[A,B]].asElem[IPairCollection[A,B]]

  abstract class PairCollection[A, B](val as: Rep[Collection[A]], val bs: Rep[Collection[B]])(implicit val eA: Elem[A], val eB: Elem[B])
    extends IPairCollection[A, B] {
    lazy val elem = element[(A, B)]
    def mapPairs[R: Elem](f: (Rep[A], Rep[B]) => Rep[R]): Coll[R] = {
      map { (p: Rep[(A, B)]) =>
        val Pair(a, b) = p
        f(a, b)
      }
    }
    def arr = (as.arr zip bs.arr)
    def lst = (as.lst zip bs.lst)
    def apply(i: Rep[Int]) = (as(i), bs(i))
    def length = as.length
    def slice(offset: Rep[Int], length: Rep[Int]) =
      PairCollection(as.slice(offset, length), bs.slice(offset, length))
    @OverloadId("many")
    def apply(indices: Coll[Int])(implicit o: Overloaded1): Coll[(A, B)] = as(indices) zip bs(indices)
    def map[C: Elem](f: Rep[(A,B) @uncheckedVariance] => Rep[C]): Coll[C] =  Collection (arr.map (f) )
    def mapBy[C: Elem](f: Rep[(A,B) @uncheckedVariance => C]): Coll[C] = Collection(arr.mapBy(f))   // TODO: this should be done in another way
    def reduce(implicit m: RepMonoid[(A,B) @uncheckedVariance]): Rep[(A,B)] = arr.reduce(m)  // TODO: this should be done in another way
    def zip[C: Elem](ys: Coll[C]): Coll[((A, B),C)] = PairCollection(self, ys)
    def update (idx: Rep[Int], value: Rep[(A,B)]): Coll[(A,B)] = PairCollection(as.update(idx, value._1), bs.update(idx, value._2))
    def updateMany (idxs: Coll[Int], vals: Coll[(A,B)]): Coll[(A,B)] = {
      val cvals = vals.asRep[IPairCollection[A,B]]
      PairCollection(as.updateMany(idxs, cvals.as), bs.updateMany(idxs, cvals.bs))
    }
    def filter(f: Rep[(A,B) @uncheckedVariance] => Rep[Boolean]): Coll[(A,B)] = Collection(arr.filter(f))
    def flatMap[C: Elem](f: Rep[(A,B) @uncheckedVariance] => Coll[C]): Coll[C] = Collection(arr.flatMap {in => f(in).arr} )
    def append(value: Rep[(A,B) @uncheckedVariance]): Coll[(A,B)]  = PairCollection(as.append(value._1), bs.append(value._2))
  }

  trait PairCollectionCompanion extends ConcreteClass2[PairCollection] with CollectionCompanion {
    def defaultOf[A, B](implicit ea: Elem[A], eb: Elem[B]) = {
      val as = BaseCollection.defaultOf[A].value
      val bs = BaseCollection.defaultOf[B].value
      Default.defaultVal(PairCollection(as, bs))
    }
  }

  abstract class CollectionOfPairs[A, B](val arr: Rep[Array[(A,B)]])(implicit val eA: Elem[A], val eB: Elem[B])
    extends IPairCollection[A,B] {
    lazy val elem = element[(A, B)]
    def lst = arr.toList
    def as = Collection.fromArray(arr.map(_._1))
    def bs = Collection.fromArray(arr.map(_._2))
    def apply(i: Rep[Int]) = arr(i)
    def length = arr.length
    def slice(offset: Rep[Int], length: Rep[Int]) =
      CollectionOfPairs(arr.slice(offset, length))
    @OverloadId("many")
    def apply(indices: Coll[Int])(implicit o: Overloaded1): Coll[(A, B)] =
      CollectionOfPairs(arr(indices.arr))
    def map[C: Elem](f: Rep[(A,B) @uncheckedVariance] => Rep[C]): Coll[C] =  Collection(arr.map (f) )
    def mapBy[C: Elem](f: Rep[(A,B) @uncheckedVariance => C]): Coll[C] = Collection(arr.mapBy(f))   // TODO: this should be done in another way
    def reduce(implicit m: RepMonoid[(A,B) @uncheckedVariance]): Rep[(A,B)] = arr.reduce(m)  // TODO: this should be done in another way
    def zip[C: Elem](ys: Coll[C]): Coll[((A, B),C)] = PairCollection(self, ys)
    def update (idx: Rep[Int], value: Rep[(A,B)]): Coll[(A,B)] = CollectionOfPairs(arr.update(idx, value))
    def updateMany (idxs: Coll[Int], vals: Coll[(A,B)]): Coll[(A,B)] = CollectionOfPairs(arr.updateMany(idxs.arr, vals.arr))
    def filter(f: Rep[(A,B) @uncheckedVariance] => Rep[Boolean]): Coll[(A,B)] = CollectionOfPairs(arr.filter(f))
    def flatMap[C: Elem](f: Rep[(A,B) @uncheckedVariance] => Coll[C]): Coll[C] = Collection(arr.flatMap {in => f(in).arr} )
    def append(value: Rep[(A,B) @uncheckedVariance]): Coll[(A,B)]  = CollectionOfPairs(arr.append(value))
  }
  trait CollectionOfPairsCompanion extends ConcreteClass2[CollectionOfPairs] with CollectionCompanion {
    def defaultOf[A, B](implicit ea: Elem[A], eb: Elem[B]) = {
      Default.defaultVal(CollectionOfPairs(SArray.empty[(A,B)]))
    }
  }

  trait INestedCollection[A] extends Collection[Collection[A]] {
    implicit def eA: Elem[A]
    def values: Rep[Collection[A]]
    def segments: Rep[Collection[(Int, Int)]]
    def segOffsets = convertPairColl(segments).as
    def segLens = convertPairColl(segments).bs
  }
  //type NColl[A] = Rep[NestedCollection[A]]
  type NColl[A] = Rep[INestedCollection[A]]
  implicit def convertNestedColl[A:Elem](x: Coll[Collection[A]]): Rep[INestedCollection[A]] = x.asRep[INestedCollection[A]]
  implicit def eINestedColl[A: Elem]: Elem[INestedCollection[A]] = element[NestedCollection[A]].asElem[INestedCollection[A]]

  // TODO rename back to FlatNestedCollection after unification with Scalan
  abstract class NestedCollection[A](val values: Coll[A], val segments: Coll[(Int, Int)])(implicit val eA: Elem[A])
    extends INestedCollection[A] {
    lazy val elem = collectionElement(eA)
    def length = segments.length
    //def segOffsets = segments.asInstanceOf[Rep[PairCollection[Int,Int]]].as
    //def segLens = segments.asInstanceOf[Rep[PairCollection[Int,Int]]].bs
    def apply(i: Rep[Int]) = {
      val Pair(offset, length) = segments(i)
      values.slice(offset, length)
    }

    def arr = segments.arr.map { seg =>
      val Pair(offset,length) = seg
      values.slice(offset, length)
    }
    def lst = arr.toList
    def slice(offset: Rep[Int], length: Rep[Int]) = ???
    @OverloadId("many")
    def apply(indices: Coll[Int])(implicit o: Overloaded1): NColl[A] = {
      val newValues = segments(indices).flatMap { in =>
        values.slice(in._1, in._2)
      }
      val newSegments = {
        val newLens = segments(indices).map{_._2}
        val newOffsArr = newLens.arr.scan._1
        Collection(newOffsArr) zip newLens
      }
      NestedCollection(newValues, newSegments)
    }
    def map[B: Elem](f: Rep[Collection[A @uncheckedVariance]] => Rep[B]): Coll[B] =  Collection(arr.map(f))
    def mapBy[B: Elem](f: Rep[Collection[A] => B @uncheckedVariance]): Coll[B] = Collection(arr.mapBy(f))
    def reduce(implicit m: RepMonoid[Collection[A @uncheckedVariance]]): Coll[A] = arr.reduce(m)
    def zip[B: Elem](ys: Coll[B]): Coll[(Collection[A],B)] = PairCollection(self, ys)
    def update (idx: Rep[Int], value: Rep[Collection[A]]): NColl[A] = ???
    def updateMany (idxs: Coll[Int], vals: Coll[Collection[A]]): NColl[A] = ???
    def filter(f: Rep[Collection[A @uncheckedVariance]] => Rep[Boolean]): NColl[A] = ???
    def flatMap[B: Elem](f: Rep[Collection[A @uncheckedVariance]] => Coll[B]): Coll[B] = Collection(arr.flatMap {in => f(in).arr} )
    def append(value: Rep[Collection[A @uncheckedVariance]]): NColl[A]  = ??? //Collection(arr.append(value))
  }
  trait NestedCollectionCompanion extends ConcreteClass1[NestedCollection] {
    def defaultOf[A](implicit ea: Elem[A]) = Default.defaultVal(NestedCollection(element[Collection[A]].defaultRepValue, element[Segments1].defaultRepValue))
    def createNestedCollection[A:Elem](vals: Coll[A], segments: Coll[(Int,Int)]) = NestedCollection(vals, segments)

    def fromJuggedArray[T: Elem](arr: Rep[Array[Array[T]]]): Rep[NestedCollection[T]] = {
      val lens: Arr[Int] = arr.map(i => i.length)
      val positions = lens.scan._1
      val segments = positions.zip(lens)
      val flat_arr = arr.flatMap {i => i}
      mkNestedCollection(Collection.fromArray(flat_arr), Collection.fromArray(segments))
    }
  }

}

trait CollectionsDsl extends impl.CollectionsAbs { self: ScalanCommunityDsl => }

trait CollectionsDslSeq extends impl.CollectionsSeq { self: ScalanCommunityDslSeq => }

trait CollectionsDslExp extends impl.CollectionsExp { self: ScalanCommunityDslExp => }
