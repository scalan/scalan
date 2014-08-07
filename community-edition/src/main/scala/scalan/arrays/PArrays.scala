package scalan.arrays

import scalan._
import scalan.common.Default
import scalan.common.OverloadHack.Overloaded1
import scala.annotation.unchecked.uncheckedVariance
import scalan.community.{ScalanCommunity, ScalanCommunityStaged, ScalanCommunitySeq}

trait PArrays extends ArrayOps { self: PArraysDsl =>

  type PA[+A] = Rep[PArray[A]]
  trait PArray[@uncheckedVariance +A] extends Reifiable[PArray[A @uncheckedVariance]] {
    implicit def elem: Elem[A @uncheckedVariance]
    def length: Rep[Int]
    def arr: Rep[Array[A @uncheckedVariance]]
    def apply(i: Rep[Int]): Rep[A]
    def apply(indices: Arr[Int])(implicit o: Overloaded1): PA[A]
    def map[B: Elem](f: Rep[A @uncheckedVariance] => Rep[B]): PA[B] = PArray(arr.map(f))
    def mapBy[B: Elem](f: Rep[A => B @uncheckedVariance]): PA[B] = PArray(arr.mapBy(f))
    def zip[B: Elem](ys: PA[B]): PA[(A, B)] = PairArray(self, ys)
    def slice(offset: Rep[Int], length: Rep[Int]): Rep[PArray[A]]
    def reduce(implicit m: RepMonoid[A @uncheckedVariance]): Rep[A] = arr.reduce(m)
    def scan(implicit m: RepMonoid[A @uncheckedVariance]): Rep[(PArray[A], A)] = {
      val arrScan = arr.scan(m)
      (PArray(arrScan._1), arrScan._2)
    }
  }
  
  implicit def defaultPArrayElement[A:Elem]: Elem[PArray[A]] = element[A] match {
    case _: BaseElem[_] => element[BaseArray[A]].asElem[PArray[A]]
    case pe: PairElem[a, b] =>
      implicit val ea = pe.eFst
      implicit val eb = pe.eSnd
      element[PairArray[a, b]].asElem[PArray[A]]
    case viewE: ViewElem[_, _] => element[BaseArray[A]].asElem[PArray[A]]
    case e => ???(s"Element is $e")
  }

  type Segments = PArray[(Int, Int)]

  trait PArrayCompanion extends TypeFamily1[PArray] {
    def defaultOf[A](implicit ea: Elem[A]): Default[Rep[PArray[A]]] = ea match {
      case baseE: BaseElem[a] => BaseArray.defaultOf[a](baseE)
      case pairE: PairElem[a, b] => PairArray.defaultOf[a, b](pairE.eFst, pairE.eSnd)
      case e => ???(s"Element is $e")
    }

    def apply[T: Elem](arr: Rep[Array[T]]): PA[T] = fromArray(arr)
    def fromArray[T: Elem](arr: Rep[Array[T]]): PA[T] = {
      element[T] match {
        case baseE: BaseElem[a] =>
          BaseArray[a](arr.asRep[Array[a]])
        case pairE: PairElem[a, b] =>
          implicit val ea = pairE.eFst
          implicit val eb = pairE.eSnd
          val ps = arr.asRep[Array[(a, b)]]
          val as = fromArray(ps.map { _._1 })
          val bs = fromArray(ps.map { _._2 })
          as zip bs //PairArray[a,b](as, bs)
        case viewE: ViewElem[a, b] =>
          // TODO
          BaseArray[b](arr.asRep[Array[b]])
        case e => ???(s"Element is $e")
      }
    }

    def replicate[T: Elem](len: Rep[Int], v: Rep[T]): PA[T] = {
      element[T] match {
        case baseE: BaseElem[a] =>
          BaseArray[a](array_replicate(len, v.asRep[a]))
        case pairElem: PairElem[a ,b] => {
          implicit val ea = pairElem.eFst
          implicit val eb = pairElem.eSnd
          val ps = v.asRep[(a, b)]
          val as = replicate(len, ps._1)
          val bs = replicate(len, ps._2)
          as zip bs
        }
        case viewElem: ViewElem[a, b] =>
          BaseArray(Array.replicate(len, v))
        case e => ???(s"Element is $e")
      }
    }

    def singleton[T: Elem](v: Rep[T]): PA[T] = {
      element[T] match {
        case paE: PArrayElem[tFrom, tTo] => ???
        case _ => replicate(toRep(1), v)
      }
    }
  }

  abstract class BaseArray[A](val arr: Rep[Array[A]])(implicit val eA: Elem[A]) extends PArray[A] {
    def length = arr.length
    def elem = eA
    def apply(i: Rep[Int]) = arr(i)
    def slice(offset: Rep[Int], length: Rep[Int]) = BaseArray(arr.slice(offset, length))
    def apply(indices: Arr[Int])(implicit o: Overloaded1): PA[A] = BaseArray(arr(indices))
  }
  trait BaseArrayCompanion extends ConcreteClass1[BaseArray] {
    def defaultOf[A](implicit ea: Elem[A]) =
      Default.defaultVal(BaseArray(Default.defaultOf[Rep[Array[A]]]))
  }

  abstract class PairArray[A, B](val as: Rep[PArray[A]], val bs: Rep[PArray[B]])(implicit val eA: Elem[A], val eB: Elem[B])
    extends PArray[(A, B)] {
    lazy val elem = element[(A, B)]
    def mapPairs[R: Elem](f: (Rep[A], Rep[B]) => Rep[R]): PA[R] = {
      map { (p: Rep[(A, B)]) =>
        val Pair(a, b) = p
        f(a, b)
      }
    }
    def arr = as.arr zip bs.arr
    def apply(i: Rep[Int]) = (as(i), bs(i))
    def length = as.length
    def slice(offset: Rep[Int], length: Rep[Int]) =
      PairArray(as.slice(offset, length), bs.slice(offset, length))
    def apply(indices: Arr[Int])(implicit o: Overloaded1): PA[(A, B)] =
      PairArray(as(indices), bs(indices))
  }
  trait PairArrayCompanion extends ConcreteClass2[PairArray] with PArrayCompanion {
    def defaultOf[A, B](implicit ea: Elem[A], eb: Elem[B]) = {
      val as = PArray.defaultOf[A].value
      val bs = PArray.defaultOf[B].value
      Default.defaultVal(PairArray(as, bs))
    }
  }

  abstract class FlatNestedArray[A](val values: Rep[PArray[A]], val segments: Rep[PArray[(Int, Int)]])(implicit val eA: Elem[A])
    extends PArray[PArray[A]] {
    lazy val elem = defaultPArrayElement(eA)
    def length = segments.length
    def apply(i: Rep[Int]) = {
      val Pair(offset, length) = segments(i)
      values.slice(offset, length)
    }
    def arr: Rep[Array[PArray[A]]] = ???
    def slice(offset: Rep[Int], length: Rep[Int]) = ???
    def apply(indices: Arr[Int])(implicit o: Overloaded1): PA[PArray[A]] = ???
  }
  trait FlatNestedArrayCompanion extends ConcreteClass1[FlatNestedArray] {
    def defaultOf[A](implicit ea: Elem[A]) = Default.defaultVal(FlatNestedArray(element[PArray[A]].defaultRepValue, element[Segments].defaultRepValue))
  }

}

trait PArraysDsl extends ScalanCommunity with impl.PArraysAbs with PArrays { }

trait PArraysDslSeq extends PArraysDsl with impl.PArraysSeq with ScalanCommunitySeq

trait PArraysDslExp extends PArraysDsl with impl.PArraysExp with ScalanCommunityStaged
