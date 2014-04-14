package scalan.arrays

import scalan._
import scalan.common.Default

trait PArrays extends Base with PArraysOps { self: PArraysDsl =>

  type PA[A] = Rep[PArray[A]]
  trait PArray[A] extends UserType[PArray[A]] {
    implicit def elem: Elem[A]
    def length: Rep[Int]
    def arr: Rep[Array[A]]
    def map[B:Elem](f: (Rep[A] => Rep[B])): PA[B]
    def mapBy[B:Elem](f: (Rep[A=>B])): PA[B]
    def zip[B:Elem](ys: PA[B]): PA[(A,B)]
  }
  trait PArrayCompanion extends TypeFamily1[PArray]{
    def defaultOf[A](implicit ea: Elem[A]): Default[Rep[PArray[A]]]
    def apply[A:Elem](arr: Rep[Array[A]]): PA[A]
    def fromArray[A:Elem](arr: Rep[Array[A]]): PA[A]
    def replicate[A:Elem](len: Rep[Int], v: Rep[A]): PA[A]
    def singleton[A:Elem](v: Rep[A]): PA[A]
  }

  abstract class BaseArray[A](val arr: Rep[Array[A]])(implicit val eA: Elem[A])
    extends PArray[A]
    with BaseArrayOps[A] {
  }
  trait BaseArrayCompanion extends ConcreteClass1[BaseArray] {
    def defaultOf[A](implicit ea: Elem[A]): Default[Rep[BaseArray[A]]]
  }

  abstract class PairArray[A, B](val as: Rep[PArray[A]], val bs: Rep[PArray[B]])(implicit val eA: Elem[A], val eB: Elem[B])
    extends PArray[(A, B)]
    with PairArrayOps[A,B] {
  }
  trait PairArrayCompanion extends ConcreteClass2[PairArray] {
    def defaultOf[A,B](implicit ea: Elem[A], eb: Elem[B]): Default[Rep[PairArray[A,B]]]
  }

}


