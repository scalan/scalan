
package scalan.rx

import scalan._
import scalan.common.Common
import scala.language.implicitConversions


trait ReactiveDsl extends Reactive {

  // single proxy for each type family
  implicit def proxyObservable[A:Elem](p: Obs[A]): Observable[A] = {
    implicit val mA = element[A].manifest;
    proxyOps[Observable[A], Observable[A]](p)
  }

  trait ObservableElem[From,To] extends ViewElem[From, To]

  // elem for concrete class
  trait ObservableImplElem[A] extends ObservableElem[ObservableImplData[A], ObservableImpl[A]]

  // state representation type
  type ObservableImplData[A] = (A, (Int, Boolean))

  // 3) companion object with Iso, constructor and deconstructor
  object ObservableImpl {
    abstract class Iso[A:Elem] extends IsoBase[ObservableImplData[A], ObservableImpl[A]] {
      override def fromStaged = { case ObservableImpl(value, index, completed) => Pair(value, Pair(index, completed)) }
      override def toStaged = (p: Rep[(A, (Int, Boolean))]) => {
        val Pair(value, Pair(index, completed)) = p
        ObservableImpl(value, index, completed)
      }
      def manifest = { 
        implicit val mA = element[A].manifest
        Predef.manifest[ObservableImpl[A]] 
      }
      def zero = Common.zero[Rep[ObservableImpl[A]]](ObservableImpl(element[A].zero.zero, 0, false))
    }

    def apply[A:Elem](value: Rep[A], index: Rep[Int], completed: Rep[Boolean]): Rep[ObservableImpl[A]]  = mkObservableImpl(value, index, completed)
    def unapply[A:Elem](p: Rep[ObservableImpl[A]]) = unmkObservableImpl(p)
  }

  // 4) implicit resolution of Iso
  implicit def isoObservableImpl[A:Elem]: Iso[ObservableImplData[A], ObservableImpl[A]]

  // 5) smart constructor and deconstructor
  def mkObservableImpl[A:Elem](value: Rep[A], index: Rep[Int], completed: Rep[Boolean]): Rep[ObservableImpl[A]]
  def unmkObservableImpl[A:Elem](p: Rep[ObservableImpl[A]]): Option[(Rep[A], Rep[Int], Rep[Boolean])]

}
