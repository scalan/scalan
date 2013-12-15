
package scalan.rx

import scalan._
import scalan.common.Common
import scala.language.implicitConversions


trait ReactiveAbs extends Reactive
{ self: ReactiveDsl =>

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
  object ObservableImpl extends ObservableImplCompanion {
    abstract class Iso[A](implicit eA: Elem[A])
           extends IsoBase[ObservableImplData[A], ObservableImpl[A]] {
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

    def apply[A](p: Rep[ObservableImplData[A]])(implicit eA: Elem[A]): Rep[ObservableImpl[A]]
        = isoObservableImpl(eA).toStaged(p)
    def apply[A](p: Observable[A])(implicit eA: Elem[A]): Rep[ObservableImpl[A]]
        = mkObservableImpl(p.value, p.index, p.completed)
    def apply[A]
          (value: Rep[A], index: Rep[Int], completed: Rep[Boolean])
          (implicit eA: Elem[A]): Rep[ObservableImpl[A]]
        = mkObservableImpl(value, index, completed)
    def unapply[A:Elem](p: Rep[ObservableImpl[A]]) = unmkObservableImpl(p)
  }

  implicit def proxyObservableImpl[A:Elem](p: Rep[ObservableImpl[A]]): ObservableImplOps[A] = {
    implicit val mA = element[A].manifest;
    proxyOps[ObservableImplOps[A], ObservableImplOps[A]](p)
  }

  implicit def extendObservableImpl[A](p: Rep[ObservableImpl[A]])(implicit eA: Elem[A]) = new {
    def toData: Rep[ObservableImplData[A]] = isoObservableImpl(eA).fromStaged(p)
  }

  // 4) implicit resolution of Iso
  implicit def isoObservableImpl[A](implicit eA: Elem[A]): Iso[ObservableImplData[A], ObservableImpl[A]]

  // 5) smart constructor and deconstructor
  def mkObservableImpl[A](value: Rep[A], index: Rep[Int], completed: Rep[Boolean])(implicit eA: Elem[A]): Rep[ObservableImpl[A]]
  def unmkObservableImpl[A:Elem](p: Rep[ObservableImpl[A]]): Option[(Rep[A], Rep[Int], Rep[Boolean])]

}


trait ReactiveSeq extends ReactiveAbs
{ self: ScalanSeq with ReactiveDsl =>

  case class SeqObservableImpl[A]
      (override val value: Rep[A], override val index: Rep[Int], override val completed: Rep[Boolean])
      (implicit override val eA: Elem[A])
    extends ObservableImpl[A](value, index, completed) with ObservableImplOps[A] {
  }


  implicit def isoObservableImpl[A](implicit eA: Elem[A]):Iso[ObservableImplData[A], ObservableImpl[A]]
    = new ObservableImpl.Iso[A] with SeqIso[ObservableImplData[A], ObservableImpl[A]] { i =>
        // should use i as iso reference
        override lazy val eTo = new SeqViewElem[ObservableImplData[A], ObservableImpl[A]]
                                    with ObservableImplElem[A] { val iso = i }
      }


  def mkObservableImpl[A]
      (value: Rep[A], index: Rep[Int], completed: Rep[Boolean])
      (implicit eA: Elem[A])
      = new ObservableImpl[A](value, index, completed) with ObservableImplOps[A]
  def unmkObservableImpl[A:Elem](p: Rep[ObservableImpl[A]])
    = Some((p.value, p.index, p.completed))

}


trait ReactiveExp extends ReactiveAbs with ProxyExp with ViewsExp
{ self: ScalanStaged with ReactiveDsl =>

  case class ExpObservableImpl[A]
      (override val value: Rep[A], override val index: Rep[Int], override val completed: Rep[Boolean])
      (implicit override val eA: Elem[A])
    extends ObservableImpl[A](value, index, completed) with ObservableImplOps[A] with UserType[ObservableImpl[A]] {
    def elem = element[ObservableImpl[A]]
    override def mirror(t: Transformer): Rep[_] = ExpObservableImpl[A](t(value), t(index), t(completed))
  }
  addUserType(manifest[ExpObservableImpl[Any]])


  def mkObservableImpl[A]
      (value: Rep[A], index: Rep[Int], completed: Rep[Boolean])
      (implicit eA: Elem[A])
      = new ExpObservableImpl[A](value, index, completed) with ObservableImplOps[A]
  def unmkObservableImpl[A:Elem](p: Rep[ObservableImpl[A]])
    = Some((p.value, p.index, p.completed))


  implicit def isoObservableImpl[A](implicit eA: Elem[A]):Iso[ObservableImplData[A], ObservableImpl[A]]
    = new ObservableImpl.Iso[A] with StagedIso[ObservableImplData[A], ObservableImpl[A]] { i =>
        // should use i as iso reference
        override lazy val eTo = new StagedViewElem[ObservableImplData[A], ObservableImpl[A]]
                                    with ObservableImplElem[A] { val iso = i }
      }

}
