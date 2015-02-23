package scalan.parrays

import scalan._
import scalan.arrays.ArrayOps
import scalan.common.Default
import scalan.common.OverloadHack.Overloaded1
import scala.annotation.unchecked.uncheckedVariance

trait PArrays extends ArrayOps { self: ScalanCommunityDsl =>

  type PA[+A] = Rep[PArray[A]]
  trait PArray[@uncheckedVariance +A] extends Reifiable[PArray[A @uncheckedVariance]] {
    implicit def elem: Elem[A @uncheckedVariance]
    def length: Rep[Int]
    def arr: Rep[Array[A @uncheckedVariance]]
    def seq: Rep[SSeq[A] @uncheckedVariance] = SSeq(arr)
    def apply(i: Rep[Int]): Rep[A]
    @OverloadId("many")
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
    def zipWithIndex: PA[(A, Int)] = zip(PArray.fromArray(SArray.rangeFrom0(length)))
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
      case UnitElement => Default.defaultVal(UnitArray(0).asRep[PArray[A]])
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
          BaseArray(SArray.replicate(len, v))
        case e => ???(s"Element is $e")
      }
    }

    def singleton[T: Elem](v: Rep[T]): PA[T] = {
      element[T] match {
        case paE: PArrayElem[_, _, _] => ???
        case _ => replicate(toRep(1), v)
      }
    }
  }

  abstract class UnitArray(val length: Rep[Int]) extends PArray[Unit] {
    def elem = UnitElement
    def arr = SArray.replicate(length, ())
    def apply(i: Rep[Int]) = ()
    @OverloadId("many")
    def apply(indices: Arr[Int])(implicit o: Overloaded1): PA[Unit] = UnitArray(indices.length)
    def slice(offset: Rep[Int], length: Rep[Int]) = UnitArray(length)
  }
  trait UnitArrayCompanion extends ConcreteClass0[UnitArray] {
    def defaultOf = Default.defaultVal(UnitArray(0))
  }

  abstract class BaseArray[A](val arr: Rep[Array[A]])(implicit val eA: Elem[A]) extends PArray[A] {
    def elem = eA
    def length = arr.length
    def apply(i: Rep[Int]) = arr(i)
    def slice(offset: Rep[Int], length: Rep[Int]) = BaseArray(arr.slice(offset, length))
    @OverloadId("many")
    def apply(indices: Arr[Int])(implicit o: Overloaded1): PA[A] = BaseArray(arr(indices))
  }
  trait BaseArrayCompanion extends ConcreteClass1[BaseArray] {
    def defaultOf[A](implicit ea: Elem[A]) =
      Default.defaultVal(BaseArray(SArray.empty[A]))
  }

  abstract class ArrayOnSeq[A](override val seq: Rep[SSeq[A]])(implicit val eA: Elem[A]) extends PArray[A] {
    def elem = eA
    def arr = seq.toArray
    def length = seq.size
    def apply(i: Rep[Int]) = seq(i)
    def slice(offset: Rep[Int], length: Rep[Int]) = ArrayOnSeq(seq.slice(offset, offset + length))
    @OverloadId("many")
    def apply(indices: Arr[Int])(implicit o: Overloaded1): PA[A] = {
      ArrayOnSeq(SSeq(indices.map(i => seq(i))))
    }
  }
  trait ArrayOnSeqCompanion extends ConcreteClass1[ArrayOnSeq] {
    def defaultOf[A](implicit ea: Elem[A]) =
      Default.defaultVal(ArrayOnSeq(SSeq.empty[A]))
  }

// TODO We shouldn't need this anymore. Check if recursive types like Tree in EE work without it
// 
//  abstract class EmptyArray[A](implicit val eA: Elem[A]) extends PArray[A] {
//    def elem = eA
//    def length = 0
//    // TODO should throw an exception? Need support for exceptions first
//    def apply(i: Rep[Int]) = eA.defaultRepValue
//    def slice(offset: Rep[Int], length: Rep[Int]) = self
//    def apply(indices: Arr[Int])(implicit o: Overloaded1): PA[A] = self
//  }
//  trait EmptyArrayCompanion extends ConcreteClass1[EmptyArray] {
//    def defaultOf[A](implicit ea: Elem[A]) = Default.defaultVal(EmptyArray[A])
//  }
  trait IPairArray[A,B] extends PArray[(A,B)] {
    implicit def eA: Elem[A]
    implicit def eB: Elem[B]
    def as: Rep[PArray[A]]
    def bs: Rep[PArray[B]]
  }

  abstract class PairArray[A, B](val as: Rep[PArray[A]], val bs: Rep[PArray[B]])(implicit val eA: Elem[A], val eB: Elem[B])
    extends IPairArray[A,B] {
    lazy val elem = element[(A, B)]
    def arr = as.arr zip bs.arr
    def apply(i: Rep[Int]) = (as(i), bs(i))
    def length = as.length
    def slice(offset: Rep[Int], length: Rep[Int]) =
      PairArray(as.slice(offset, length), bs.slice(offset, length))
    @OverloadId("many")
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

  abstract class ArrayOfPairs[A, B](val arr: Rep[Array[(A,B)]])(implicit val eA: Elem[A], val eB: Elem[B])
    extends IPairArray[A,B] {
    lazy val elem = element[(A, B)]
    def as = PArray.fromArray(arr.map(_._1))
    def bs = PArray.fromArray(arr.map(_._2))
    def apply(i: Rep[Int]) = arr(i)
    def length = arr.length
    def slice(offset: Rep[Int], length: Rep[Int]) =
      ArrayOfPairs(arr.slice(offset, length))
    @OverloadId("many")
    def apply(indices: Arr[Int])(implicit o: Overloaded1): PA[(A, B)] =
      ArrayOfPairs(arr(indices))
  }
  trait ArrayOfPairsCompanion extends ConcreteClass2[ArrayOfPairs] with PArrayCompanion {
    def defaultOf[A, B](implicit ea: Elem[A], eb: Elem[B]) = {
      Default.defaultVal(ArrayOfPairs(SArray.empty[(A,B)]))
    }
  }

  trait INestedArray[A] extends PArray[PArray[A]] {
    implicit def eA: Elem[A]
    def values: Rep[PArray[A]]
    def segments: Rep[PArray[(Int, Int)]]
  }
  // TODO rename back to FlatNestedArray after unification with Scalan
  abstract class NestedArray[A](val values: Rep[PArray[A]], val segments: Rep[PArray[(Int, Int)]])(implicit val eA: Elem[A])
    extends INestedArray[A] {
    lazy val elem = defaultPArrayElement(eA)
    def length = segments.length
    def apply(i: Rep[Int]) = {
      val Pair(offset, length) = segments(i)
      values.slice(offset, length)
    }
    def arr: Rep[Array[PArray[A]]] = ???
    def slice(offset: Rep[Int], length: Rep[Int]) = ???
    @OverloadId("many")
    def apply(indices: Arr[Int])(implicit o: Overloaded1): PA[PArray[A]] = ???
  }
  trait NestedArrayCompanion extends ConcreteClass1[NestedArray] {
    def defaultOf[A](implicit ea: Elem[A]) = Default.defaultVal(NestedArray(element[PArray[A]].defaultRepValue, element[Segments].defaultRepValue))
  }

}

trait PArraysDsl extends impl.PArraysAbs { self: ScalanCommunityDsl => }

trait PArraysDslSeq extends impl.PArraysSeq { self: ScalanCommunityDslSeq => }

trait PArraysDslExp extends impl.PArraysExp { self: ScalanCommunityDslExp => }
