package scalan.collection

import scala.collection.immutable.HashSet
import scalan._
import scalan.common.Default
import scala.reflect.runtime.universe._

trait HashSets extends Base with BaseTypes { self: HashSetsDsl =>

  type RHS[A] = Rep[HashSet[A]]
  trait SHashSet[A] extends BaseTypeEx[HashSet[A], SHashSet[A]] { self =>
    implicit def eA: Elem[A]
    @External def +(elem: Rep[A]): Rep[HashSet[A]]
    @External def map[B:Elem](f: Rep[A => B]): Rep[HashSet[B]]
  }
  trait SHashSetCompanion extends ExCompanion1[HashSet]  {
    @External def empty[A:Elem]: Rep[HashSet[A]]
  }
  implicit def DefaultOfHashSet[A:Elem]: Default[HashSet[A]] = Default.defaultVal(HashSet.empty[A]) //SHashSet.defaultVal
}

trait HashSetsDsl extends impl.HashSetsAbs
trait HashSetsDslSeq extends impl.HashSetsSeq {
  trait SeqSHashSet[A] extends SHashSetImpl[A] {
    override def map[B:Elem](f: Rep[A => B]): Rep[HashSet[B]] = value.map(f)
    //override def +(elem: Rep[A]): Rep[HashSet[A]] = value + elem
  }
}
trait HashSetsDslExp extends impl.HashSetsExp
