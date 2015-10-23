package scalan.collections

/**
 * Author: Alexander Slesarenko
 * Date: 1/26/13
 */
import scalan._
import scalan.common.OverloadHack.Overloaded1

trait BitSets  { self: ScalanCommunityDsl =>
  type BS = Rep[BitSet]
  trait BitSet extends Def[BitSet] {
    def bits: Rep[Collection[Boolean]]
    def union(that: Rep[BitSet]) = {
      val flags = (bits zip that.bits) map { case Pair(x,y) => x || y }
      BitSet(flags)
    }
    def length: Rep[Int] = bits.length
    def contains(n: Rep[Int]): Rep[Boolean] = bits(n)
    def add(n: Rep[Int]): Rep[BitSet] = BitSet(bits.update(n, true))
    @OverloadId("many")
    def add(ns: Coll[Int])(implicit o: Overloaded1): Rep[BitSet] = BitSet(bits.updateMany(ns, Collection.replicate(ns.length, true)))
  }
  trait BitSetCompanion {
    def apply(flags: Coll[Boolean]): Rep[BitSet] = BoolCollBitSet(flags)
    @OverloadId("many")
    def apply(flags: Rep[Array[Boolean]])(implicit o: Overloaded1): Rep[BitSet] =
      BitSet(Collection.fromArray(flags))
    def empty(range: Rep[Int]) = BitSet(Collection.replicate(range, false))
  }

  abstract class BoolCollBitSet(val bits: Rep[Collection[Boolean]]) extends BitSet {
  }
  trait BoolCollBitSetCompanion
}
