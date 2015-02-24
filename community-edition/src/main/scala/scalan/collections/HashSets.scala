package scalan.collections

import scala.collection.immutable.HashSet
import scalan._
import scalan.common.Default
import scala.reflect.runtime.universe._

trait HashSets extends Base with BaseTypes { self: ScalanCommunityDsl =>

  type RHS[A] = Rep[HashSet[A]]
  trait SHashSet[A] extends BaseTypeEx[HashSet[A], SHashSet[A]] { self =>
    implicit def eA: Elem[A]
    def wrappedValueOfBaseType: Rep[HashSet[A]]
    @External def +(elem: Rep[A]): Rep[HashSet[A]]
    @External def map[B:Elem](f: Rep[A => B]): Rep[HashSet[B]]
    @External def fold(z: Rep[A])(f : Rep[((A,A)) => A]): Rep[A]
  }
  trait SHashSetCompanion extends ExCompanion1[HashSet]  {
    @External def empty[A:Elem]: Rep[HashSet[A]]
  }

  def DefaultOfHashSet[A:Elem]: Default[HashSet[A]] = Default.defaultVal(HashSet.empty[A]) //SHashSet.defaultVal
}

trait HashSetsDsl extends impl.HashSetsAbs { self: ScalanCommunityDsl => }
trait HashSetsDslSeq extends impl.HashSetsSeq { self: ScalanCommunityDslSeq =>
  trait SeqSHashSet[A] extends SHashSetImpl[A] {
    override def map[B:Elem](f: Rep[A => B]): Rep[HashSet[B]] = wrappedValueOfBaseType.map(f)
    //override def +(elem: Rep[A]): Rep[HashSet[A]] = value + elem
  }
}
trait HashSetsDslExp extends impl.HashSetsExp { self: ScalanCommunityDslExp => }
