package scalan.collections

/**
 * Created by afilippov on 2/18/15.
 */
/**
 * Author: Alexander Slesarenko
 * Date: 1/26/13
 */
import scalan._
import scala.reflect.runtime.universe._
import scalan.collection.CollectionsDsl
import scalan.common.Default
import scalan.common.OverloadHack.Overloaded1
import scalan.{ScalanCommunitySeq, ScalanCommunityExp, ScalanCommunity}

trait BitSets  { self: ScalanCommunity with CollectionsDsl =>
  trait PBitSetOps {
    def bits: Coll[Boolean]
    def union(that: Rep[PBitSet]) = {
      val flags = (bits zip that.bits) map { case Pair(x,y) => x || y }
      PBitSet(flags)
    }
    def contains(n: Rep[Int]): Rep[Boolean] = bits(n)
    def add(n: Rep[Int]): Rep[PBitSet] = PBitSet(bits.update(n, true))
    def add(ns: Coll[Int])(implicit o: Overloaded1): Rep[PBitSet] = PBitSet(bits.updateMany(ns, Collection.replicate(ns.length, true)))
  }

  class PBitSet(val bits: Coll[Boolean]) extends PBitSetOps
  class PBitSetIso extends Iso[Collection[Boolean], PBitSet] {
    override def from(p: Rep[PBitSet]) = p match { case PBitSet(bits) => bits }
    override def to(p: Coll[Boolean]) = PBitSet(p)
    def tag = weakTypeTag[PBitSet]
    lazy val defaultRepTo = Default.defaultVal(PBitSet(emptyColl[Boolean]))
    lazy val eTo = new ViewElem(this) {}
  }
  object PBitSet {
    def apply(flags: Coll[Boolean]): Rep[PBitSet]  = mkPBitSet(flags)
    def apply(flags: Rep[Array[Boolean]])(implicit o: Overloaded1): Rep[PBitSet] =
      mkPBitSet(Collection.fromArray(flags))
    def unapply(p: Rep[PBitSet]) = unmkPBitSet(p)
    def empty(range: Rep[Int]) = PBitSet(Collection.replicate(range, false))
  }

  implicit val isoPBitSet: Iso[Collection[Boolean], PBitSet]
  implicit def repToPointProxyOps(p: Rep[PBitSet]): PBitSetOps = proxyOps[PBitSetOps](p)
  def mkPBitSet(flags: Coll[Boolean]): Rep[PBitSet]
  def unmkPBitSet(p: Rep[PBitSet]): Option[Coll[Boolean]]
}

trait BitSetsSeq extends BitSets { self: ScalanCommunitySeq =>
  implicit lazy val isoPBitSet:Iso[Collection[Boolean], PBitSet] = new PBitSetIso
  def mkPBitSet(flags: Coll[Boolean]) = new PBitSet(flags)
  def unmkPBitSet(p: Rep[PBitSet]) = Some(p.bits)
}

trait BitSetsExp extends BitSets { self: ScalanCommunityExp =>

  case class ExpBitSet(override val bits: Coll[Boolean])
    extends PBitSet(bits) with Def[PBitSet] {
    def uniqueOpId = ""
    lazy val selfType = element[PBitSet]
    override def mirror(t: Transformer) = ExpBitSet(t(bits))
  }

  object ExpBitSet {
    //    addRewriteRules({
    //      case MethodCall(Def(ExpBitSet(bits)), "bits", _) => bits
    //    })
  }
  def mkPBitSet(flags: Coll[Boolean]) = ExpBitSet(flags)
  def unmkPBitSet(p: Rep[PBitSet]) = Some(p.bits)

  implicit lazy val isoPBitSet:Iso[Collection[Boolean], PBitSet] = new PBitSetIso
}

//trait BitSetsDsl extends BitSets

//trait BitSetsDslSeq extends BitSetsSeq

//trait BitSetsDslExp extends BitSetsExp



