package scalan.collection

import scalan._
import scalan.arrays.ArrayOps
import scalan.common.Default
import scalan.common.OverloadHack.Overloaded1
import scala.annotation.unchecked.uncheckedVariance

trait Collections extends ArrayOps with ListOps { self: CollectionsDsl =>

  type Coll[+A] = Rep[Collection[A]]
  trait Collection[@uncheckedVariance +A] extends Reifiable[Collection[A @uncheckedVariance]] {
    implicit def elem: Elem[A @uncheckedVariance]
    def length: Rep[Int]
    def arr: Rep[Array[A @uncheckedVariance]]
    def apply(i: Rep[Int]): Rep[A]
    @OverloadId("many")
    def apply(indices: Arr[Int])(implicit o: Overloaded1): Coll[A]
    def map[B: Elem](f: Rep[A @uncheckedVariance] => Rep[B]): Coll[B] //= Collection(arr.map(f))
    def mapBy[B: Elem](f: Rep[A => B @uncheckedVariance]): Coll[B] //= Collection(arr.mapBy(f))
    def zip[B: Elem](ys: Coll[B]): Coll[(A, B)] // = PairCollection(self, ys)
    def slice(offset: Rep[Int], length: Rep[Int]): Rep[Collection[A]]
    def reduce(implicit m: RepMonoid[A @uncheckedVariance]): Rep[A] //= arr.reduce(m)
    /*def scan(implicit m: RepMonoid[A @uncheckedVariance]): Rep[(Collection[A], A)] = {
      val arrScan = arr.scan(m)
      (Collection(arrScan._1), arrScan._2)
    } */
  }

  implicit def defaultCollectionElement[A:Elem]: Elem[Collection[A]] = element[A] match {
    case _: BaseElem[_] => element[BaseCollection[A]].asElem[Collection[A]]
    case pe: PairElem[a, b] =>
      implicit val ea = pe.eFst
      implicit val eb = pe.eSnd
      element[PairCollection[a, b]].asElem[Collection[A]]
    case viewE: ViewElem[_, _] => element[BaseCollection[A]].asElem[Collection[A]]
    case e => ???(s"Element is $e")
  }

  type Segments = Collection[(Int, Int)]

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
          BaseCollection(Array.replicate(len, v))
        case e => ???(s"Element is $e")
      }
    }

    def singleton[T: Elem](v: Rep[T]): Coll[T] = {
      element[T] match {
        case paE: CollectionElem[_, _, _] => ???
        case _ => replicate(toRep(1), v)
      }
    }
  }

  abstract class UnitCollection(val len: Rep[Int]) extends Collection[Unit] {
    def elem = UnitElement
    def arr = Array.replicate(len, ())
    def length = len
    def apply(i: Rep[Int]) = ()
    def map[B: Elem](f: Rep[Unit] => Rep[B]): Coll[B] = Collection(arr.map(f))
    def mapBy[B: Elem](f: Rep[Unit => B @uncheckedVariance]): Coll[B] = Collection(arr.mapBy(f))
    @OverloadId("many")
    def apply(indices: Arr[Int])(implicit o: Overloaded1): Coll[Unit] = UnitCollection(indices.length)
    def slice(offset: Rep[Int], length: Rep[Int]) = UnitCollection(length)
    def reduce(implicit m: RepMonoid[Unit @uncheckedVariance]): Rep[Unit] = ()
    def zip[B: Elem](ys: Coll[B]): Coll[(Unit, B)] = PairCollection(self, ys)
  }
  trait UnitCollectionCompanion extends ConcreteClass0[UnitCollection] {
    def defaultOf = Default.defaultVal(UnitCollection(0))
  }

  abstract class BaseCollection[A](val arr: Rep[Array[A]])(implicit val eA: Elem[A]) extends Collection[A] {
    def elem = eA
    def length = arr.length
    def apply(i: Rep[Int]) = arr(i)
    def map[B: Elem](f: Rep[A @uncheckedVariance] => Rep[B]): Coll[B] = BaseCollection(arr.map(f))
    def mapBy[B: Elem](f: Rep[A => B @uncheckedVariance]): Coll[B] = BaseCollection(arr.mapBy(f))
    def slice(offset: Rep[Int], length: Rep[Int]) = BaseCollection(arr.slice(offset, length))
    @OverloadId("many")
    def apply(indices: Arr[Int])(implicit o: Overloaded1): Coll[A] = BaseCollection(arr(indices))
    def reduce(implicit m: RepMonoid[A @uncheckedVariance]): Rep[A] = arr.reduce(m)
    def zip[B: Elem](ys: Coll[B]): Coll[(A, B)] = PairCollection(self, ys)
  }
  trait BaseCollectionCompanion extends ConcreteClass1[BaseCollection] {
    def defaultOf[A](implicit ea: Elem[A]) =
      Default.defaultVal(BaseCollection(Array.empty[A]))
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
    def apply(indices: Arr[Int])(implicit o: Overloaded1): Coll[A] = ListCollection(lst(indices))
    def reduce(implicit m: RepMonoid[A @uncheckedVariance]): Rep[A] = ???
    def zip[B: Elem](ys: Coll[B]): Coll[(A, B)] = PairCollection(self, ys)
  }
  trait ListCollectionCompanion extends ConcreteClass1[ListCollection] {
    def defaultOf[A](implicit ea: Elem[A]) =
      Default.defaultVal(ListCollection(List.empty[A]))
  }

  // TODO We shouldn't need this anymore. Check if recursive types like Tree in EE work without it
  // 
  //  abstract class EmptyArray[A](implicit val eA: Elem[A]) extends Collection[A] {
  //    def elem = eA
  //    def length = 0
  //    // TODO should throw an exception? Need support for exceptions first
  //    def apply(i: Rep[Int]) = eA.defaultRepValue
  //    def slice(offset: Rep[Int], length: Rep[Int]) = self
  //    def apply(indices: Arr[Int])(implicit o: Overloaded1): Coll[A] = self
  //  }
  //  trait EmptyArrayCompanion extends ConcreteClass1[EmptyArray] {
  //    def defaultOf[A](implicit ea: Elem[A]) = Default.defaultVal(EmptyArray[A])
  //  }

  abstract class PairCollection[A, B](val as: Rep[Collection[A]], val bs: Rep[Collection[B]])(implicit val eA: Elem[A], val eB: Elem[B])
    extends Collection[(A, B)] {
    lazy val elem = element[(A, B)]
    def mapPairs[R: Elem](f: (Rep[A], Rep[B]) => Rep[R]): Coll[R] = {
      map { (p: Rep[(A, B)]) =>
        val Pair(a, b) = p
        f(a, b)
      }
    }
    def arr = (as.arr zip bs.arr)
    def apply(i: Rep[Int]) = (as(i), bs(i))
    def length = as.length
    def slice(offset: Rep[Int], length: Rep[Int]) =
      PairCollection(as.slice(offset, length), bs.slice(offset, length))
    @OverloadId("many")
    def apply(indices: Arr[Int])(implicit o: Overloaded1): Coll[(A, B)] = as(indices) zip bs(indices)
    def map[C: Elem](f: Rep[(A,B) @uncheckedVariance] => Rep[C]): Coll[C] =  BaseCollection (arr.map (f) )
    def mapBy[C: Elem](f: Rep[(A,B) @uncheckedVariance => C]): Coll[C] = BaseCollection(arr.mapBy(f))   // TODO: this should be done in another way
    def reduce(implicit m: RepMonoid[(A,B) @uncheckedVariance]): Rep[(A,B)] = arr.reduce(m)  // TODO: this should be done in another way
    def zip[C: Elem](ys: Coll[C]): Coll[((A, B),C)] = PairCollection(self, ys)
  }

  trait PairCollectionCompanion extends ConcreteClass2[PairCollection] with CollectionCompanion {
    def defaultOf[A, B](implicit ea: Elem[A], eb: Elem[B]) = {
      val as = BaseCollection.defaultOf[A].value
      val bs = BaseCollection.defaultOf[B].value
      Default.defaultVal(PairCollection(as, bs))
    }
  }

  // TODO rename back to FlatNestedCollection after unification with Scalan
  abstract class NestedCollection[A](val values: Rep[Collection[A]], val segments: Rep[Collection[(Int, Int)]])(implicit val eA: Elem[A])
    extends Collection[Collection[A]] {
    lazy val elem = defaultCollectionElement(eA)
    def length = segments.length
    def apply(i: Rep[Int]) = {
      val Pair(offset, length) = segments(i)
      values.slice(offset, length)
    }

    def arr = segments.arr.map { seg =>
      val Pair(offset,length) = seg
      values.slice(offset, length)
    }
    def slice(offset: Rep[Int], length: Rep[Int]) = ???
    @OverloadId("many")
    def apply(indices: Arr[Int])(implicit o: Overloaded1): Coll[Collection[A]] = ???
    def map[B: Elem](f: Rep[Collection[A @uncheckedVariance]] => Rep[B]): Coll[B] =  Collection(arr.map(f))
    def mapBy[B: Elem](f: Rep[Collection[A] => B @uncheckedVariance]): Coll[B] = Collection(arr.mapBy(f))
    def reduce(implicit m: RepMonoid[Collection[A @uncheckedVariance]]): Coll[A] = arr.reduce(m)
    def zip[B: Elem](ys: Coll[B]): Coll[(Collection[A],B)] = PairCollection(self, ys)
  }
  trait NestedCollectionCompanion extends ConcreteClass1[NestedCollection] {
    def defaultOf[A](implicit ea: Elem[A]) = Default.defaultVal(NestedCollection(element[Collection[A]].defaultRepValue, element[Segments].defaultRepValue))
  }

}

trait CollectionsDsl extends impl.CollectionsAbs

trait CollectionsDslSeq extends impl.CollectionsSeq

trait CollectionsDslExp extends impl.CollectionsExp
