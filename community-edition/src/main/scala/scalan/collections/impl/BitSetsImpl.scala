package scalan.collections
package impl

import scalan._
import scalan.common.OverloadHack.Overloaded1
import scala.reflect.runtime.universe._
import scala.reflect._
import scalan.common.Default

// Abs -----------------------------------
trait BitSetsAbs extends Scalan with BitSets {
  self: ScalanCommunityDsl =>
  // single proxy for each type family
  implicit def proxyBitSet(p: Rep[BitSet]): BitSet = {
    implicit val tag = weakTypeTag[BitSet]
    proxyOps[BitSet](p)(TagImplicits.typeTagToClassTag[BitSet])
  }

  class BitSetElem[To <: BitSet]
    extends EntityElem[To] {
    def isEntityType = true
    def tag = { assert(this.isInstanceOf[BitSetElem[_]]); weakTypeTag[BitSet].asInstanceOf[WeakTypeTag[To]]}
    override def convert(x: Rep[Reifiable[_]]) = convertBitSet(x.asRep[BitSet])
    def convertBitSet(x : Rep[BitSet]): Rep[To] = {
      assert(x.selfType1.isInstanceOf[BitSetElem[_]])
      x.asRep[To]
    }
    def getDefaultRep: Rep[To] = ???
  }

  def bitSetElement =
    new BitSetElem[BitSet]()

  trait BitSetCompanionElem extends CompanionElem[BitSetCompanionAbs]
  implicit lazy val BitSetCompanionElem: BitSetCompanionElem = new BitSetCompanionElem {
    lazy val tag = weakTypeTag[BitSetCompanionAbs]
    protected def getDefaultRep = BitSet
  }

  abstract class BitSetCompanionAbs extends CompanionBase[BitSetCompanionAbs] with BitSetCompanion {
    override def toString = "BitSet"
  }
  def BitSet: Rep[BitSetCompanionAbs]
  implicit def proxyBitSetCompanion(p: Rep[BitSetCompanion]): BitSetCompanion = {
    proxyOps[BitSetCompanion](p)
  }

  // elem for concrete class
  class BoolCollBitSetElem(val iso: Iso[BoolCollBitSetData, BoolCollBitSet])
    extends BitSetElem[BoolCollBitSet]
    with ViewElem[BoolCollBitSetData, BoolCollBitSet] {
    override def convertBitSet(x: Rep[BitSet]) = BoolCollBitSet(x.bits)
    override def getDefaultRep = super[ViewElem].getDefaultRep
    override lazy val tag = super[ViewElem].tag
  }

  // state representation type
  type BoolCollBitSetData = Collection[Boolean]

  // 3) Iso for concrete class
  class BoolCollBitSetIso
    extends Iso[BoolCollBitSetData, BoolCollBitSet] {
    override def from(p: Rep[BoolCollBitSet]) =
      unmkBoolCollBitSet(p) match {
        case Some((bits)) => bits
        case None => !!!
      }
    override def to(p: Rep[Collection[Boolean]]) = {
      val bits = p
      BoolCollBitSet(bits)
    }
    lazy val tag = {
      weakTypeTag[BoolCollBitSet]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[BoolCollBitSet]](BoolCollBitSet(element[Collection[Boolean]].defaultRepValue))
    lazy val eTo = new BoolCollBitSetElem(this)
  }
  // 4) constructor and deconstructor
  abstract class BoolCollBitSetCompanionAbs extends CompanionBase[BoolCollBitSetCompanionAbs] with BoolCollBitSetCompanion {
    override def toString = "BoolCollBitSet"

    def apply(bits: Rep[Collection[Boolean]]): Rep[BoolCollBitSet] =
      mkBoolCollBitSet(bits)
    def unapply(p: Rep[BoolCollBitSet]) = unmkBoolCollBitSet(p)
  }
  def BoolCollBitSet: Rep[BoolCollBitSetCompanionAbs]
  implicit def proxyBoolCollBitSetCompanion(p: Rep[BoolCollBitSetCompanionAbs]): BoolCollBitSetCompanionAbs = {
    proxyOps[BoolCollBitSetCompanionAbs](p)
  }

  class BoolCollBitSetCompanionElem extends CompanionElem[BoolCollBitSetCompanionAbs] {
    lazy val tag = weakTypeTag[BoolCollBitSetCompanionAbs]
    protected def getDefaultRep = BoolCollBitSet
  }
  implicit lazy val BoolCollBitSetCompanionElem: BoolCollBitSetCompanionElem = new BoolCollBitSetCompanionElem

  implicit def proxyBoolCollBitSet(p: Rep[BoolCollBitSet]): BoolCollBitSet =
    proxyOps[BoolCollBitSet](p)

  implicit class ExtendedBoolCollBitSet(p: Rep[BoolCollBitSet]) {
    def toData: Rep[BoolCollBitSetData] = isoBoolCollBitSet.from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoBoolCollBitSet: Iso[BoolCollBitSetData, BoolCollBitSet] =
    new BoolCollBitSetIso

  // 6) smart constructor and deconstructor
  def mkBoolCollBitSet(bits: Rep[Collection[Boolean]]): Rep[BoolCollBitSet]
  def unmkBoolCollBitSet(p: Rep[BoolCollBitSet]): Option[(Rep[Collection[Boolean]])]
}

// Seq -----------------------------------
trait BitSetsSeq extends BitSetsDsl with ScalanSeq {
  self: ScalanCommunityDslSeq =>
  lazy val BitSet: Rep[BitSetCompanionAbs] = new BitSetCompanionAbs with UserTypeSeq[BitSetCompanionAbs, BitSetCompanionAbs] {
    lazy val selfType = element[BitSetCompanionAbs]
  }

  case class SeqBoolCollBitSet
      (override val bits: Rep[Collection[Boolean]])

    extends BoolCollBitSet(bits)
        with UserTypeSeq[BitSet, BoolCollBitSet] {
    lazy val selfType = element[BoolCollBitSet].asInstanceOf[Elem[BitSet]]
  }
  lazy val BoolCollBitSet = new BoolCollBitSetCompanionAbs with UserTypeSeq[BoolCollBitSetCompanionAbs, BoolCollBitSetCompanionAbs] {
    lazy val selfType = element[BoolCollBitSetCompanionAbs]
  }

  def mkBoolCollBitSet
      (bits: Rep[Collection[Boolean]]): Rep[BoolCollBitSet] =
      new SeqBoolCollBitSet(bits)
  def unmkBoolCollBitSet(p: Rep[BoolCollBitSet]) =
    Some((p.bits))
}

// Exp -----------------------------------
trait BitSetsExp extends BitSetsDsl with ScalanExp {
  self: ScalanCommunityDslExp =>
  lazy val BitSet: Rep[BitSetCompanionAbs] = new BitSetCompanionAbs with UserTypeDef[BitSetCompanionAbs, BitSetCompanionAbs] {
    lazy val selfType = element[BitSetCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  case class ExpBoolCollBitSet
      (override val bits: Rep[Collection[Boolean]])

    extends BoolCollBitSet(bits) with UserTypeDef[BitSet, BoolCollBitSet] {
    lazy val selfType = element[BoolCollBitSet].asInstanceOf[Elem[BitSet]]
    override def mirror(t: Transformer) = ExpBoolCollBitSet(t(bits))
  }

  lazy val BoolCollBitSet: Rep[BoolCollBitSetCompanionAbs] = new BoolCollBitSetCompanionAbs with UserTypeDef[BoolCollBitSetCompanionAbs, BoolCollBitSetCompanionAbs] {
    lazy val selfType = element[BoolCollBitSetCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object BoolCollBitSetMethods {
  }

  object BoolCollBitSetCompanionMethods {
  }

  def mkBoolCollBitSet
    (bits: Rep[Collection[Boolean]]): Rep[BoolCollBitSet] =
    new ExpBoolCollBitSet(bits)
  def unmkBoolCollBitSet(p: Rep[BoolCollBitSet]) =
    Some((p.bits))

  object BitSetMethods {
    object bits {
      def unapply(d: Def[_]): Option[Rep[BitSet]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BitSetElem[_]] && method.getName == "bits" =>
          Some(receiver).asInstanceOf[Option[Rep[BitSet]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[BitSet]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object union {
      def unapply(d: Def[_]): Option[(Rep[BitSet], Rep[BitSet])] = d match {
        case MethodCall(receiver, method, Seq(that, _*), _) if receiver.elem.isInstanceOf[BitSetElem[_]] && method.getName == "union" =>
          Some((receiver, that)).asInstanceOf[Option[(Rep[BitSet], Rep[BitSet])]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[BitSet], Rep[BitSet])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object length {
      def unapply(d: Def[_]): Option[Rep[BitSet]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BitSetElem[_]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[BitSet]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[BitSet]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object contains {
      def unapply(d: Def[_]): Option[(Rep[BitSet], Rep[Int])] = d match {
        case MethodCall(receiver, method, Seq(n, _*), _) if receiver.elem.isInstanceOf[BitSetElem[_]] && method.getName == "contains" =>
          Some((receiver, n)).asInstanceOf[Option[(Rep[BitSet], Rep[Int])]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[BitSet], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object add {
      def unapply(d: Def[_]): Option[(Rep[BitSet], Rep[Int])] = d match {
        case MethodCall(receiver, method, Seq(n, _*), _) if receiver.elem.isInstanceOf[BitSetElem[_]] && method.getName == "add"&& method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, n)).asInstanceOf[Option[(Rep[BitSet], Rep[Int])]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[BitSet], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object add_many {
      def unapply(d: Def[_]): Option[(Rep[BitSet], Coll[Int])] = d match {
        case MethodCall(receiver, method, Seq(ns, _*), _) if receiver.elem.isInstanceOf[BitSetElem[_]] && method.getName == "add" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "many" } =>
          Some((receiver, ns)).asInstanceOf[Option[(Rep[BitSet], Coll[Int])]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[BitSet], Coll[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object BitSetCompanionMethods {
    object apply {
      def unapply(d: Def[_]): Option[Coll[Boolean]] = d match {
        case MethodCall(receiver, method, Seq(flags, _*), _) if receiver.elem.isInstanceOf[BitSetCompanionElem] && method.getName == "apply"&& method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some(flags).asInstanceOf[Option[Coll[Boolean]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Coll[Boolean]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_many {
      def unapply(d: Def[_]): Option[Rep[Array[Boolean]]] = d match {
        case MethodCall(receiver, method, Seq(flags, _*), _) if receiver.elem.isInstanceOf[BitSetCompanionElem] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "many" } =>
          Some(flags).asInstanceOf[Option[Rep[Array[Boolean]]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Array[Boolean]]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object empty {
      def unapply(d: Def[_]): Option[Rep[Int]] = d match {
        case MethodCall(receiver, method, Seq(range, _*), _) if receiver.elem.isInstanceOf[BitSetCompanionElem] && method.getName == "empty" =>
          Some(range).asInstanceOf[Option[Rep[Int]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Int]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}
