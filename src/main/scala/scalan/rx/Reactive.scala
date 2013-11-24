/**
 * Author: Alexander Slesarenko
 * Date: 1/26/13
 */
package scalan.rx

import scalan._
import scalan.common.Common
import scala.language.{implicitConversions}

trait Reactive extends ScalanDsl {

  // declaration of abstract domain object (this is just interface)
  type Obs[A] = Rep[Observable[A]]
  trait Observable[A] {
    implicit def eA: Elem[A]
    def value: Rep[A]
    def index: Rep[Int]
    def completed: Rep[Boolean]
    def map[B:Elem](f: Rep[A=>B]): Obs[B]
    def zip[B](that: Obs[B]): Obs[(A, B)]
  }

  //TODO find a way how to get rid of this boilerplate
  // single proxy for each type family
  implicit def proxyObservable[T:Elem](p: Obs[T]): Observable[T] = {
    implicit val mA = element[T].manifest;
    proxyOps[Observable[T], Observable[T]](p)
  }

  // One of concrete implementations of domain object along with its state representation.
  // Each implementation requires
  // 1) implementation of methods
  class ObservableImpl[A]
      (val value: Rep[A], val index: Rep[Int], val completed: Rep[Boolean])(implicit val eA: Elem[A]) extends Observable[A] {
    def map[B: Elem](f: Rep[A => B]) = ???
    def zip[B](that: Obs[B]) = ???
  }
  // 2) state representation type
  type ObservableImplData[A] = (A, (Int, Boolean))

  // 3) companion object with Iso, constructor and deconstructor
  object ObservableImpl {
    abstract class Iso[T](implicit val eT: Elem[T]) extends IsoBase[ObservableImplData[T], ObservableImpl[T]] {
      override def fromStaged = { case ObservableImpl(v,i,c) => Pair(v, Pair(i,c)) }
      override def toStaged = (p: Rep[(T, (Int, Boolean))]) => {
        val Pair(v, Pair(i, c)) = p
        ObservableImpl(v, i, c)
      }
      def manifest = { implicit val mT = eT.manifest; Predef.manifest[ObservableImpl[T]] }
      def zero = Common.zero(ObservableImpl(eT.zero.zero, -1, true))
    }

    def apply[T:Elem](value: Rep[T], index: Rep[Int], completed: Rep[Boolean]): Rep[ObservableImpl[T]]  = mkObservableImpl(value, index, completed)
    def unapply[T:Elem](p: Rep[ObservableImpl[T]]) = unmkObservableImpl(p)
  }

  // 4) implicit resolution of Iso
  implicit def isoObservableImpl[T:Elem]: Iso[ObservableImplData[T], ObservableImpl[T]]

  // 5) smart constructor and deconstructor
  def mkObservableImpl[T:Elem](value: Rep[T], index: Rep[Int], completed: Rep[Boolean]): Rep[ObservableImpl[T]]
  def unmkObservableImpl[T:Elem](p: Rep[ObservableImpl[T]]): Option[(Rep[T], Rep[Int], Rep[Boolean])]
}

trait ReactiveSeq extends Reactive { self: ScalanSeq =>
  implicit def isoObservableImpl[T:Elem]:Iso[ObservableImplData[T], ObservableImpl[T]]
    = new ObservableImpl.Iso[T] with SeqIso[ObservableImplData[T], ObservableImpl[T]]

  def mkObservableImpl[T:Elem](value: Rep[T], index: Rep[Int], completed: Rep[Boolean])
    = new ObservableImpl[T](value, index, completed)
  def unmkObservableImpl[T:Elem](p: Rep[ObservableImpl[T]])
    = Some((p.value, p.index, p.completed))
}

trait ReactiveExp extends Reactive with ProxyExp with ViewsExp { self: ScalanStaged =>

  case class ExpObservableImpl[A]
      (override val value: Rep[A], override val index: Rep[Int], override val completed: Rep[Boolean])
      (implicit override val eA: Elem[A])
    extends ObservableImpl[A](value, index, completed) with UserType[ObservableImpl[A]] {
    def elem = element[ObservableImpl[A]]
    override def mirror(t: Transformer): Rep[_] = ExpObservableImpl(t(value), t(index), t(completed))
  }
  addUserType(manifest[ExpObservableImpl[Any]])

  def mkObservableImpl[T:Elem](value: Rep[T], index: Rep[Int], completed: Rep[Boolean])
    = ExpObservableImpl(value, index, completed)
  def unmkObservableImpl[T:Elem](p: Rep[ObservableImpl[T]])
    = Some((p.value, p.index, p.completed))

  implicit def isoObservableImpl[T:Elem]: Iso[ObservableImplData[T], ObservableImpl[T]]
    = new ObservableImpl.Iso[T] with StagedIso[ObservableImplData[T], ObservableImpl[T]]
}


