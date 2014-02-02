package scalan.arrays

import scala.annotation.implicitNotFound
import scala.annotation.unchecked.uncheckedVariance
import scalan.common.{DefaultOf, Common}
import Common._
import scala.language.implicitConversions
import scalan._

trait PArrays extends Base with PArraysOps { self: PArraysDsl =>

  type PA[T] = Rep[PArray[T]]
  trait PArray[T] extends UserType[PArray[T]] {
    implicit def eT: Elem[T]
    def length: Rep[Int]
    def arr: Rep[Array[T]]
    def map[R:Elem](f: (Rep[T] => Rep[R])): PA[R]
  }

  abstract class BaseArray[T](val arr: Rep[Array[T]])(implicit val eA: Elem[T])
    extends PArray[T]
    with BaseArrayOps[T] { self: BaseArrayOps[T] =>
  }

  abstract class PairArray[A, B](val as: PA[A], val bs: PA[B])(implicit val eA: Elem[A], val eB: Elem[B])
    extends PArray[(A, B)]
    with PairArrayOps[A,B] { self: PairArrayOps[A,B] =>
  }

}


