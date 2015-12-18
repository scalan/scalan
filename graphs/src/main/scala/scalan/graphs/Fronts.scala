package scalan.graphs
import scalan.collections.{BitSetsDsl, BitSetsDslExp, BitSetsDslSeq, CollectionsDsl}
import scalan._

/**
 * Created by afilippov on 2/27/15.
 */

trait Fronts extends CollectionsDsl with BitSetsDsl { self: FrontsDsl =>
  trait Front extends Def[Front] {
    //def total: Rep[Int]

    def contains(v: Rep[Int]): Rep[Boolean]

    def append(v: Rep[Int]): Rep[Front]

    def set: Rep[Collection[Int]]
  }

  trait FrontCompanion {
    def emptyBaseFront(len: Rep[Int]) = {
      val bits = BitSet.empty(len)
      val set = CollectionOverArray(SArray.empty[Int])
      BaseFront(set, bits)
    }
    def emptyListBasedFront(len: Rep[Int]) = {
      val bits = BitSet.empty(len)
      val set = CollectionOverList(SList.empty[Int])
      ListFront(set, bits)
    }
    def emptyCollBasedFront(len: Rep[Int]) = {
      val bits = BitSet.empty(len)
      val set = CollectionOverList(SList.empty[Int])
      CollectionFront(set, bits)
    }
    def emptyMapBasedFront(len: Rep[Int]) = {
      MapBasedFront(MMap.empty[Int,Unit])
    }
    def fromStartNode(start: Rep[Int], len: Rep[Int]) = {
      val bits = BitSet.empty(len).add(start)
      val set = Collection.singleton(start).convertTo[CollectionOverArray[Int]]
      BaseFront(set, bits)
    }

    def fromStartNodeMap(start: Rep[Int], len: Rep[Int]) = {
      val empty = MMap.empty[Int,Unit]
      MapBasedFront(empty).append(start)
    }
  }

  abstract class BaseFront(val set: Rep[CollectionOverArray[Int]], val bits: Rep[BitSet]) extends Front {
    def contains(v: Rep[Int]) = bits.contains(v)

    def append(v: Rep[Int]) = {
      BaseFront(set.append(v).convertTo[CollectionOverArray[Int]], bits.add(v))
    }
  }

  trait BaseFrontCompanion extends ConcreteClass0[Front]

  abstract class ListFront(val set: Rep[CollectionOverList[Int]], val bits: Rep[BitSet]) extends Front {
    //def total = bits.length

    def contains(v: Rep[Int]) = bits.contains(v)

    def append(v: Rep[Int]) = {
      ListFront(set.append(v).convertTo[CollectionOverList[Int]], bits.add(v))
    }
  }

  trait ListFrontCompanion extends ConcreteClass0[Front]

  abstract class CollectionFront(val set: Rep[Collection[Int]], val bits: Rep[BitSet]) extends Front {
    def contains(v: Rep[Int]) = bits.contains(v)

    def append(v: Rep[Int]) = {
      CollectionFront(set.append(v), bits.add(v))
    }
  }

  trait CollectionFrontCompanion extends ConcreteClass0[Front]

  abstract class MapBasedFront(val mmap: Rep[MMap[Int,Unit]]) extends Front {
    def contains(v: Rep[Int]) = {
      mmap.contains(v)
    }

    def append(v: Rep[Int]): Rep[MapBasedFront] = MapBasedFront(mmap.update(v, ()) | mmap)

    def set: Rep[Collection[Int]] = Collection(mmap.keys)
  }

  trait MapBasedFrontCompanion extends ConcreteClass0[Front]
}

trait FrontsDslSeq extends impl.FrontsSeq with BitSetsDslSeq
trait FrontsDslExp extends impl.FrontsExp with BitSetsDslExp
