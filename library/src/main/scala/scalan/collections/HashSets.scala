package scalan.collections

import scala.collection.immutable.HashSet
import scalan._
import scalan.common.Default

trait HashSets extends Base with TypeWrappers { self: ScalanCommunityDsl =>

  type RHS[A] = Rep[SHashSet[A]]

  @ContainerType
  trait SHashSet[A] extends TypeWrapper[HashSet[A], SHashSet[A]] { self =>
    implicit def eA: Elem[A]
    def wrappedValue: Rep[HashSet[A]]
    @External def +(elem: Rep[A]): Rep[SHashSet[A]]
    @External def map[B:Elem](f: Rep[A => B]): Rep[SHashSet[B]]
    @External def fold(z: Rep[A])(f : Rep[((A,A)) => A]): Rep[A]
  }
  trait SHashSetCompanion extends ExCompanion1[SHashSet]  {
    @External def empty[A:Elem]: Rep[SHashSet[A]]
  }

  def DefaultOfHashSet[A:Elem]: Default[HashSet[A]] = Default.defaultVal(HashSet.empty[A]) //SHashSet.defaultVal
}

trait HashSetsDslSeq extends impl.HashSetsSeq { self: ScalanCommunityDslSeq =>
  trait SeqSHashSet[A] extends SHashSetImpl[A] {
    override def map[B:Elem](f: Rep[A => B]): Rep[SHashSet[B]] = SHashSetImpl(wrappedValue.map(f))
    //override def +(elem: Rep[A]): Rep[HashSet[A]] = value + elem
  }
}
