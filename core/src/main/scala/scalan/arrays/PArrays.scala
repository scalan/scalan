package scalan.arrays

import scalan._
import scalan.common.Default

trait PArrays extends Base with PArraysOps { self: PArraysDsl =>

  type PA[T] = Rep[PArray[T]]
  trait PArray[T] extends UserType[PArray[T]] {
    implicit def eT: Elem[T]
    def length: Rep[Int]
    def arr: Rep[Array[T]]
    def map[R:Elem](f: (Rep[T] => Rep[R])): PA[R]
    def mapBy[R:Elem](f: (Rep[T=>R])): PA[R]
    def zip[U:Elem](ys: PA[U]): PA[(T,U)]
  }
  trait PArrayCompanion extends TypeFamily1[PArray]{
    def defaultOf[A](implicit ea: Elem[A]): Default[Rep[PArray[A]]]
    def apply[T:Elem](arr: Rep[Array[T]]): PA[T]
    def fromArray[T:Elem](arr: Rep[Array[T]]): PA[T]
    def replicate[T:Elem](len: Rep[Int], v: Rep[T]): PA[T]
    def singleton[T:Elem](v: Rep[T]): PA[T]
  }

  abstract class BaseArray[T](val arr: Rep[Array[T]])(implicit val eA: Elem[T])
    extends PArray[T]
    with BaseArrayOps[T] { self: BaseArrayOps[T] =>
  }
  trait BaseArrayCompanion extends ConcreteClass1[BaseArray] {
    def defaultOf[A](implicit ea: Elem[A]): Default[Rep[BaseArray[A]]]
  }

  abstract class PairArray[A, B](val as: PA[A], val bs: PA[B])(implicit val eA: Elem[A], val eB: Elem[B])
    extends PArray[(A, B)]
    with PairArrayOps[A,B] { self: PairArrayOps[A,B] =>
  }
  trait PairArrayCompanion extends ConcreteClass2[PairArray] {
    def defaultOf[A,B](implicit ea: Elem[A], eb: Elem[B]): Default[Rep[PairArray[A,B]]]
  }

}


