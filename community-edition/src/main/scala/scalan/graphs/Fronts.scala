package scalan.graphs
import scala.annotation.unchecked.uncheckedVariance
import scalan.common.Default
import scalan._
import scalan.collection.{CollectionsDslExp, CollectionsDslSeq, CollectionsDsl}

/**
 * Created by afilippov on 2/27/15.
 */

trait Fronts extends ScalanCommunityDsl with CollectionsDsl { self: FrontsDsl =>
  trait Front extends Reifiable[Front] {
    def total: Rep[Int]

    def contains(v: Rep[Int]): Rep[Boolean]

    def append(v: Rep[Int]): Rep[Front]

    def set: Rep[Collection[Int]]
  }

  trait FrontCompanion {
    def defaultOf = BaseFront.defaultOf

    def fromStartNode(start: Rep[Int], len: Rep[Int]) = {
      val bits = PBitSet.empty(len).add(start)
      val set = Collection.singleton(start)
      BaseFront(set, bits)
    }

    def fromHashSet(hs: Rep[SHashSet[Int]]) = HsFront(hs)
  }

  abstract class BaseFront(val set: Rep[Collection[Int]], val bits: Rep[PBitSet]) extends Front {
    def total = bits.length

    def contains(v: Rep[Int]) = bits.contains(v)

    def append(v: Rep[Int]) = {
      BaseFront(set.append(v), bits.add(v))
    }
  }

  implicit def defaultFrontElement: Elem[Front] = {
    element[BaseFront].asElem[Front]
  }

  trait BaseFrontCompanion extends ConcreteClass0[Front] {
    def defaultOf = Default.defaultVal(BaseFront(element[Collection[Int]].defaultRepValue, element[PBitSet].defaultRepValue))
  }

  abstract class HsFront(val hashSet: Rep[SHashSet[Int]]) extends Front {
    def total = ???

    def contains(v: Rep[Int]) = ???

    def append(v: Rep[Int]): Rep[HsFront] = ???

    def set: Rep[Collection[Int]] = ??? //Collection(hashSet.floor)
  }

  trait HsFrontCompanion extends ConcreteClass0[Front] {
    def defaultOf = Default.defaultVal(HsFront(element[SHashSet[Int]].defaultRepValue))
  }

}
trait FrontsDsl extends impl.FrontsAbs
trait FrontsDslSeq extends impl.FrontsSeq
trait FrontsDslExp extends impl.FrontsExp

