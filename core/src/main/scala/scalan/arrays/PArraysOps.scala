package scalan.arrays

import scalan.common.Default
import scalan._
import scalan.common.OverloadHack.Overloaded1

trait PArraysOps { scalan: PArraysDsl =>
  import TagImplicits._
  
  implicit def defaultPArrayElement[A:Elem]: Elem[PArray[A]] = element[A] match {
    case _: BaseElem[_] => element[BaseArray[A]].asElem[PArray[A]]
    case pe: PairElem[a, b] =>
      implicit val ea = pe.ea
      implicit val eb = pe.eb
      element[PairArray[a, b]].asElem[PArray[A]]
    case viewE: ViewElem[_, _] => element[BaseArray[A]].asElem[PArray[A]]
    case e => ???(s"Element is $e")
  }

  trait PArrayOps[T] extends PArray[T] {
    def map[R:Elem](f: Rep[T] => Rep[R]) = {
      val fun_f = fun(f)
      mapBy(fun_f)
    }
    def mapBy[R:Elem](f: Rep[T=>R]) = {
      PArray(arr.map(f))
    }
    def zip[U:Elem](ys: PA[U]) = PairArray(self, ys)
    def reduce(implicit m: RepMonoid[T]) = arr.sum
  }
  trait PArrayCompanionOps extends PArrayCompanion {
    def defaultOf[A](implicit ea: Elem[A]): Default[Rep[PArray[A]]] = ea match {
      case baseE: BaseElem[a] => BaseArray.defaultOf[a](baseE)
      case pairE: PairElem[a,b] => PairArray.defaultOf[a,b](pairE.ea, pairE.eb)
      case e => ???(s"Element is $e")
    }

    def apply[T:Elem](arr: Rep[Array[T]]): PA[T] = fromArray(arr)
    def fromArray[T:Elem](arr: Rep[Array[T]]): PA[T] = element[T] match {
      case baseE: BaseElem[a] =>
        BaseArray[a](arr.asRep[Array[a]])
      case pairE: PairElem[a,b] =>
        implicit val ea = pairE.ea
        implicit val eb = pairE.eb
        val ps = arr.asRep[Array[(a,b)]]
        val as = fromArray(ps.map(fun { _._1 }))
        val bs = fromArray(ps.map(fun { _._2 }))
        as zip bs //PairArray[a,b](as, bs)
      case viewE: ViewElem[a, b] =>
        // TODO
        BaseArray[b](arr.asRep[Array[b]])
      case e => ???(s"Element is $e")
    }

    def replicate[T:Elem](len: Rep[Int], v: Rep[T]): PA[T] = element[T] match {
      case baseE: BaseElem[a] =>
        BaseArray[a](array_replicate(len, v.asRep[a]))
      case e => ???(s"Element is $e")
    }

    def singleton[T:Elem](v: Rep[T]): PA[T] = element[T] match {
      case paE: PArrayElem[tFrom,tTo] => ???
      case _ => replicate(toRep(1), v)
    }
  }

  //-------------------------------  BaseType ----------------------------------
  trait BaseArrayOps[A] extends PArrayOps[A] {
    implicit def eA: Elem[A]
    def length = arr.length
    def elem = eA
    def apply(i: Rep[Int]) = arr(i)
    def slice(offset: Rep[Int], length: Rep[Int]) = BaseArray(arr.slice(offset, length))
    def apply(indices: Arr[Int])(implicit o: Overloaded1): PA[A] = BaseArray(arr(indices))
  }
  trait BaseArrayCompanionOps extends BaseArrayCompanion {
    def defaultOf[A](implicit ea: Elem[A]) = Default.defaultVal(BaseArray(Default.defaultOf[Rep[Array[A]]]))
  }

  //-------------------------------  Tuple2Type ----------------------------------
  trait PairArrayOps[A,B] extends PArrayOps[(A,B)] {
    implicit def eA: Elem[A]
    implicit def eB: Elem[B]
    lazy val elem = element[(A,B)]
    def as: PA[A]
    def bs: PA[B]
    def mapPairs[R:Elem](f: (Rep[A],Rep[B]) => Rep[R]): PA[R] = {
      map({ (p: Rep[(A,B)]) => { val Pair(a,b) = p; f(a,b)}})
    }
    def arr = as.arr zip bs.arr
    def apply(i: Rep[Int]) = (as(i), bs(i))
    def length = as.length
    def slice(offset: Rep[Int], length: Rep[Int]) = PairArray(as.slice(offset, length), bs.slice(offset, length))
    def apply(indices: Arr[Int])(implicit o: Overloaded1): PA[(A, B)] = 
      PairArray(as(indices), bs(indices))
  }
  trait PairArrayCompanionOps extends PairArrayCompanion {
    def defaultOf[A,B](implicit ea: Elem[A], eb: Elem[B]) = {
      val as = PArray.defaultOf[A].value
      val bs = PArray.defaultOf[B].value
      Default.defaultVal(PairArray(as, bs))
    }
  }
  
  type Segments = PArray[(Int, Int)]
  
  trait FlatNestedArrayOps[A] extends PArrayOps[PArray[A]] {
    implicit def eA: Elem[A]
    lazy val elem = defaultPArrayElement(eA)
    def values: Rep[PArray[A]]
    def segments: Rep[PArray[(Int, Int)]]
    def length = segments.length
    def apply(i: Rep[Int]) = {
      val Pair(offset, length) = segments(i)
      values.slice(offset, length)
    }// arr(i)
    def arr: Rep[Array[PArray[A]]] = {
      ??? // TODO
    }
    def slice(offset: Rep[Int], length: Rep[Int]) = ??? // TODO
    def apply(indices: Arr[Int])(implicit o: Overloaded1): PA[PArray[A]] = ???
  }
  trait FlatNestedArrayCompanionOps extends FlatNestedArrayCompanion {
    def defaultOf[A](implicit ea: Elem[A]) = Default.defaultVal(FlatNestedArray(element[PArray[A]].defaultRepValue, element[Segments].defaultRepValue))
  }
}

trait PArraysDsl extends ScalanDsl with PArraysAbs with PArraysOps { }

trait PArraysDslSeq extends PArraysDsl with PArraysSeq with ScalanSeqImplementation

trait PArraysDslExp extends PArraysDsl with PArraysExp with ScalanStaged