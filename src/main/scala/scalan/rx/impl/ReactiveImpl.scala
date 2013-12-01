
package scalan.rx

import scalan._
import scalan.common.Common
import scala.language.implicitConversions


trait ReactiveSeq extends ReactiveDsl { self: ScalanSeq =>

  implicit def isoObservableImpl[A:Elem]:Iso[ObservableImplData[A], ObservableImpl[A]]
    = new ObservableImpl.Iso[A] with SeqIso[ObservableImplData[A], ObservableImpl[A]] { i =>
        // should use i as iso reference
        override lazy val eTo = new SeqViewElem[ObservableImplData[A], ObservableImpl[A]]
                                    with ObservableImplElem[A] { val iso = i }
      }


  def mkObservableImpl[A:Elem](value: Rep[A], index: Rep[Int], completed: Rep[Boolean])
    = new ObservableImpl[A](value, index, completed)
  def unmkObservableImpl[A:Elem](p: Rep[ObservableImpl[A]])
    = Some((p.value, p.index, p.completed))

}


trait ReactiveExp extends ReactiveDsl with ProxyExp with ViewsExp { self: ScalanStaged =>

  case class ExpObservableImpl[A]
      (override val value: Rep[A], override val index: Rep[Int], override val completed: Rep[Boolean])
      (implicit override val eA: Elem[A])
    extends ObservableImpl[A](value, index, completed) with UserType[ObservableImpl[A]] {
    def elem = element[ObservableImpl[A]]
    override def mirror(t: Transformer): Rep[_] = ExpObservableImpl[A](t(value), t(index), t(completed))
  }
  addUserType(manifest[ExpObservableImpl[Any]])


  def mkObservableImpl[A:Elem](value: Rep[A], index: Rep[Int], completed: Rep[Boolean])
    = new ExpObservableImpl[A](value, index, completed)
  def unmkObservableImpl[A:Elem](p: Rep[ObservableImpl[A]])
    = Some((p.value, p.index, p.completed))


  implicit def isoObservableImpl[A:Elem]:Iso[ObservableImplData[A], ObservableImpl[A]]
    = new ObservableImpl.Iso[A] with StagedIso[ObservableImplData[A], ObservableImpl[A]] { i =>
        // should use i as iso reference
        override lazy val eTo = new StagedViewElem[ObservableImplData[A], ObservableImpl[A]]
                                    with ObservableImplElem[A] { val iso = i }
      }

}
