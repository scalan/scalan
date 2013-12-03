package scalan.rx

import scalan._
import scalan.common.Common
import scala.language.implicitConversions

trait ReactiveSeq extends ReactiveDsl { self: ScalanSeq =>

  implicit def isoObservableImpl[T:Elem]:Iso[ObservableImplData[T], ObservableImpl[T]]
  = new ObservableImpl.Iso[T] with SeqIso[ObservableImplData[T], ObservableImpl[T]] { i =>
    // should use i as iso reference
    override lazy val eTo = new SeqViewElem[ObservableImplData[T], ObservableImpl[T]]
      with ObservableImplElem[T] { val iso = i }
  }

  def mkObservableImpl[T:Elem](value: Rep[T], index: Rep[Int], completed: Rep[Boolean])
  = new ObservableImpl[T](value, index, completed)
  def unmkObservableImpl[T:Elem](p: Rep[ObservableImpl[T]])
  = Some((p.value, p.index, p.completed))
}

trait ReactiveExp extends ReactiveDsl with ProxyExp with ViewsExp { self: ScalanStaged =>

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
    = new ObservableImpl.Iso[T] with StagedIso[ObservableImplData[T], ObservableImpl[T]] { i =>
        override lazy val eTo = new StagedViewElem[ObservableImplData[T], ObservableImpl[T]]
                                    with ObservableImplElem[T] { val iso = i }
      }
}
