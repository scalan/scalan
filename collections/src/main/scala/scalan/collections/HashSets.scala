package scalan.collections

import scala.collection.immutable.HashSet
import scalan._

trait HashSets extends Base with TypeWrappers { self: HashSetsDsl =>

  type RHS[A] = Rep[SHashSet[A]]

  @ContainerType
  trait SHashSet[A] extends TypeWrapper[HashSet[A], SHashSet[A]] { self =>
    implicit def eA: Elem[A]
    @External def +(elem: Rep[A]): Rep[SHashSet[A]]
    @External def map[B:Elem](f: Rep[A => B]): Rep[SHashSet[B]]
    @External def fold(z: Rep[A])(f : Rep[((A,A)) => A]): Rep[A]
  }
  trait SHashSetCompanion extends ExCompanion1[SHashSet]  {
    @External def empty[A:Elem]: Rep[SHashSet[A]]
  }

  def DefaultOfHashSet[A:Elem]: Rep[HashSet[A]] = HashSet.empty[A]
}

trait HashSetsDslStd extends impl.HashSetsStd {
  trait SHashSetImplDecls[A] extends SHashSetImpl[A] {
    override def map[B:Elem](f: Rep[A => B]): Rep[SHashSet[B]] = SHashSetImpl(wrappedValue.map(f))
    //override def +(elem: Rep[A]): Rep[HashSet[A]] = value + elem
  }
}
