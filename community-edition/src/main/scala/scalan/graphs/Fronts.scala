package scalan.graphs
import scala.annotation.unchecked.uncheckedVariance
import scalan.collections.CollectionsDsl
import scalan.common.Default
import scalan._

/**
 * Created by afilippov on 2/27/15.
 */

trait Fronts extends ScalanCommunityDsl { self: FrontsDsl =>
  trait Front extends Reifiable[Front] {
    //def total: Rep[Int]

    def contains(v: Rep[Int]): Rep[Boolean]

    def append(v: Rep[Int]): Rep[Front]

    def set: Rep[Collection[Int]]
  }

  trait FrontCompanion {
    def defaultOf = BaseFront.defaultOf

    def emptyBaseFront(len: Rep[Int]) = {
      val bits = PBitSet.empty(len)
      val set = BaseCollection(SArray.empty[Int])
      BaseFront(set, bits)
    }
    def emptyListBasedFront(len: Rep[Int]) = {
      val bits = PBitSet.empty(len)
      val set = ListCollection(SList.empty[Int])
      ListFront(set, bits)
    }
    def emptyCollBasedFront(len: Rep[Int]) = {
      val bits = PBitSet.empty(len)
      val set = ListCollection(SList.empty[Int])
      CollectionFront(set, bits)
    }
    def emptyMapBasedFront(len: Rep[Int]) = {
      MapBasedFront(MMap.empty[Int,Unit])
    }
    def fromStartNode(start: Rep[Int], len: Rep[Int]) = {
      val bits = PBitSet.empty(len).add(start)
      val set = Collection.singleton(start).convertTo[BaseCollection[Int]]
      BaseFront(set, bits)
    }

    def fromStartNodeMap(start: Rep[Int], len: Rep[Int]) = {
      val empty = MMap.empty[Int,Unit]
      MapBasedFront(empty).append(start)
    }
  }

  abstract class BaseFront(val set: Rep[BaseCollection[Int]], val bits: Rep[PBitSet]) extends Front {
    def contains(v: Rep[Int]) = bits.contains(v)

    def append(v: Rep[Int]) = {
      BaseFront(set.append(v).convertTo[BaseCollection[Int]], bits.add(v))
    }
  }

  implicit def defaultFrontElement: Elem[Front] = {
    element[BaseFront].asElem[Front]
  }

  trait BaseFrontCompanion extends ConcreteClass0[Front] {
    def defaultOf = Default.defaultVal(BaseFront(element[BaseCollection[Int]].defaultRepValue, element[PBitSet].defaultRepValue))
  }

  abstract class ListFront(val set: Rep[ListCollection[Int]], val bits: Rep[PBitSet]) extends Front {
    //def total = bits.length

    def contains(v: Rep[Int]) = bits.contains(v)

    def append(v: Rep[Int]) = {
      ListFront(set.append(v).convertTo[ListCollection[Int]], bits.add(v))
    }
  }

  trait ListFrontCompanion extends ConcreteClass0[Front] {
    def defaultOf = Default.defaultVal(ListFront(element[ListCollection[Int]].defaultRepValue, element[PBitSet].defaultRepValue))
  }

  abstract class CollectionFront(val set: Rep[Collection[Int]], val bits: Rep[PBitSet]) extends Front {
    def contains(v: Rep[Int]) = bits.contains(v)

    def append(v: Rep[Int]) = {
      CollectionFront(set.append(v), bits.add(v))
    }
  }

  trait CollectionFrontCompanion extends ConcreteClass0[Front] {
    def defaultOf = Default.defaultVal(
      CollectionFront(element[Collection[Int]].defaultRepValue,
                      element[PBitSet].defaultRepValue))
  }

  abstract class MapBasedFront(val mmap: Rep[MMap[Int,Unit]]) extends Front {
    def contains(v: Rep[Int]) = {
      mmap.contains(v)
    }

    def append(v: Rep[Int]): Rep[MapBasedFront] = MapBasedFront(mmap.update(v, ()) | mmap)

    def set: Rep[Collection[Int]] = Collection(mmap.keys)
  }

  trait MapBasedFrontCompanion extends ConcreteClass0[Front] {
    def defaultOf = Default.defaultVal(MapBasedFront(element[MMap[Int, Unit]].defaultRepValue))
  }



}
trait FrontsDsl extends impl.FrontsAbs
trait FrontsDslSeq extends impl.FrontsSeq
trait FrontsDslExp extends impl.FrontsExp

