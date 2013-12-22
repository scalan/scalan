package scalan.rx

import scalan._
import scalan.common.Common
import scala.language.implicitConversions

trait Reactive extends ScalanDsl with ReactiveOps { self: ReactiveDsl =>

  type Obs[A] = Rep[Observable[A]]
  trait Observable[A] extends UserType {
    implicit def eA: Elem[A]
    def value: Rep[A]
    def index: Rep[Int]
    def completed: Rep[Boolean]
    def map[B:Elem](f: Rep[A=>B]): Obs[B]
    def zip[B](that: Obs[B]): Obs[(A, B)]
  }

  abstract class ObservableImpl[A]
      (val value: Rep[A], val index: Rep[Int], val completed: Rep[Boolean])(implicit val eA: Elem[A])
    extends Observable[A] with ObservableImplOps[A] { self: ObservableImplOps[A] =>
    def map[B: Elem](f: Rep[A => B]): Obs[B] = ???
    def zip[B](that: Obs[B]): Obs[(A, B)] = ???
  }

}








