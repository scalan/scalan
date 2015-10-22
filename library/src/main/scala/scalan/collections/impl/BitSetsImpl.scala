package scalan.collections

import scalan._
import scalan.common.OverloadHack.Overloaded1
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait BitSetsAbs extends BitSets with scalan.Scalan {
  self: ScalanCommunityDsl =>

  // single proxy for each type family
  implicit def proxyBitSet(p: Rep[BitSet]): BitSet = {
    proxyOps[BitSet](p)(scala.reflect.classTag[BitSet])
  }

  // familyElem
  class BitSetElem[To <: BitSet]
    extends EntityElem[To] {
    lazy val parent: Option[Elem[_]] = None
    lazy val entityDef: STraitOrClassDef = {
      val module = getModules("BitSets")
      module.entities.find(_.name == "BitSet").get
    }
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map()
    }
    override def isEntityType = true
    override lazy val tag = {
      weakTypeTag[BitSet].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[BitSet] => convertBitSet(x) }
      tryConvert(element[BitSet], this, x, conv)
    }

    def convertBitSet(x: Rep[BitSet]): Rep[To] = {
      x.selfType1 match {
        case _: BitSetElem[_] => x.asRep[To]
        case e => !!!(s"Expected $x to have BitSetElem[_], but got $e")
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def bitSetElement: Elem[BitSet] =
    cachedElem[BitSetElem[BitSet]]()

  implicit case object BitSetCompanionElem extends CompanionElem[BitSetCompanionAbs] {
    lazy val tag = weakTypeTag[BitSetCompanionAbs]
    protected def getDefaultRep = BitSet
  }

  abstract class BitSetCompanionAbs extends CompanionDef[BitSetCompanionAbs] with BitSetCompanion {
    def selfType = BitSetCompanionElem
    override def toString = "BitSet"
  }
  def BitSet: Rep[BitSetCompanionAbs]
  implicit def proxyBitSetCompanion(p: Rep[BitSetCompanion]): BitSetCompanion =
    proxyOps[BitSetCompanion](p)

  // elem for concrete class
  class BoolCollBitSetElem(val iso: Iso[BoolCollBitSetData, BoolCollBitSet])
    extends BitSetElem[BoolCollBitSet]
    with ConcreteElem[BoolCollBitSetData, BoolCollBitSet] {
    override lazy val parent: Option[Elem[_]] = Some(bitSetElement)
    override lazy val entityDef = {
      val module = getModules("BitSets")
      module.concreteSClasses.find(_.name == "BoolCollBitSet").get
    }
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map()
    }

    override def convertBitSet(x: Rep[BitSet]) = BoolCollBitSet(x.bits)
    override def getDefaultRep = BoolCollBitSet(element[Collection[Boolean]].defaultRepValue)
    override lazy val tag = {
      weakTypeTag[BoolCollBitSet]
    }
  }

  // state representation type
  type BoolCollBitSetData = Collection[Boolean]

  // 3) Iso for concrete class
  class BoolCollBitSetIso
    extends Iso[BoolCollBitSetData, BoolCollBitSet] {
    override def from(p: Rep[BoolCollBitSet]) =
      p.bits
    override def to(p: Rep[Collection[Boolean]]) = {
      val bits = p
      BoolCollBitSet(bits)
    }
    lazy val eTo = new BoolCollBitSetElem(this)
  }
  // 4) constructor and deconstructor
  class BoolCollBitSetCompanionAbs extends CompanionDef[BoolCollBitSetCompanionAbs] with BoolCollBitSetCompanion {
    def selfType = BoolCollBitSetCompanionElem
    override def toString = "BoolCollBitSet"

    def apply(bits: Rep[Collection[Boolean]]): Rep[BoolCollBitSet] =
      mkBoolCollBitSet(bits)
  }
  object BoolCollBitSetMatcher {
    def unapply(p: Rep[BitSet]) = unmkBoolCollBitSet(p)
  }
  lazy val BoolCollBitSet: Rep[BoolCollBitSetCompanionAbs] = new BoolCollBitSetCompanionAbs
  implicit def proxyBoolCollBitSetCompanion(p: Rep[BoolCollBitSetCompanionAbs]): BoolCollBitSetCompanionAbs = {
    proxyOps[BoolCollBitSetCompanionAbs](p)
  }

  implicit case object BoolCollBitSetCompanionElem extends CompanionElem[BoolCollBitSetCompanionAbs] {
    lazy val tag = weakTypeTag[BoolCollBitSetCompanionAbs]
    protected def getDefaultRep = BoolCollBitSet
  }

  implicit def proxyBoolCollBitSet(p: Rep[BoolCollBitSet]): BoolCollBitSet =
    proxyOps[BoolCollBitSet](p)

  implicit class ExtendedBoolCollBitSet(p: Rep[BoolCollBitSet]) {
    def toData: Rep[BoolCollBitSetData] = isoBoolCollBitSet.from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoBoolCollBitSet: Iso[BoolCollBitSetData, BoolCollBitSet] =
    cachedIso[BoolCollBitSetIso]()

  // 6) smart constructor and deconstructor
  def mkBoolCollBitSet(bits: Rep[Collection[Boolean]]): Rep[BoolCollBitSet]
  def unmkBoolCollBitSet(p: Rep[BitSet]): Option[(Rep[Collection[Boolean]])]

  registerModule(scalan.meta.ScalanCodegen.loadModule(BitSets_Module.dump))
}

// Seq -----------------------------------
trait BitSetsSeq extends BitSetsDsl with scalan.ScalanSeq {
  self: ScalanCommunityDslSeq =>
  lazy val BitSet: Rep[BitSetCompanionAbs] = new BitSetCompanionAbs with Def[BitSetCompanionAbs] {
  }

  case class SeqBoolCollBitSet
      (override val bits: Rep[Collection[Boolean]])

    extends BoolCollBitSet(bits)
        with Def[BoolCollBitSet] {
    lazy val selfType = element[BoolCollBitSet]
  }

  def mkBoolCollBitSet
      (bits: Rep[Collection[Boolean]]): Rep[BoolCollBitSet] =
      new SeqBoolCollBitSet(bits)
  def unmkBoolCollBitSet(p: Rep[BitSet]) = p match {
    case p: BoolCollBitSet @unchecked =>
      Some((p.bits))
    case _ => None
  }
}

// Exp -----------------------------------
trait BitSetsExp extends BitSetsDsl with scalan.ScalanExp {
  self: ScalanCommunityDslExp =>
  lazy val BitSet: Rep[BitSetCompanionAbs] = new BitSetCompanionAbs with Def[BitSetCompanionAbs] {
  }

  case class ExpBoolCollBitSet
      (override val bits: Rep[Collection[Boolean]])

    extends BoolCollBitSet(bits) with Def[BoolCollBitSet] {
    lazy val selfType = element[BoolCollBitSet]
  }

  object BoolCollBitSetMethods {
  }

  object BoolCollBitSetCompanionMethods {
  }

  def mkBoolCollBitSet
    (bits: Rep[Collection[Boolean]]): Rep[BoolCollBitSet] =
    new ExpBoolCollBitSet(bits)
  def unmkBoolCollBitSet(p: Rep[BitSet]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: BoolCollBitSetElem @unchecked =>
      Some((p.asRep[BoolCollBitSet].bits))
    case _ =>
      None
  }

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
        case MethodCall(receiver, method, Seq(n, _*), _) if receiver.elem.isInstanceOf[BitSetElem[_]] && method.getName == "add" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
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
        case MethodCall(receiver, method, Seq(flags, _*), _) if receiver.elem == BitSetCompanionElem && method.getName == "apply" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
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
        case MethodCall(receiver, method, Seq(flags, _*), _) if receiver.elem == BitSetCompanionElem && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "many" } =>
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
        case MethodCall(receiver, method, Seq(range, _*), _) if receiver.elem == BitSetCompanionElem && method.getName == "empty" =>
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

object BitSets_Module {
  val packageName = "scalan.collections"
  val name = "BitSets"
  val dump = "H4sIAAAAAAAAALVVPYwbRRR+Xt+dz/YpF1yA7hrCYYISgX1CQimuQHeOE0Vy7k7ZgJCJIs2tx86E2Zm9nfHJpkhBCV2UFqH06dJEQqJBSIiCCgESNVUgiiIgFYg3s+Ndb5QlabLFanb27Xvv+3mzd/6ARRXDSRUQTkQrpJq0fLveVrrpd4VmenpRDsacnqXDT1++F1wUO8qD1T4sXSPqrOJ9qCaL7iRK1z497EGViIAqLWOl4bWerdAOJOc00EyKNgvDsSYHnLZ7TOmtHiwcyMH0EG5AqQfHAymCmGrqdzhRiiq3v0xNRyx9rtrn6V6U1RBtg6I9h+JyTJjG9rHG8ST+Eo38qZBiGmo45lrbi0xbGFNhYSRjPStRwXTX5GD2uCAIbkCjd50ckTaWGLV9HTMxwi/rEQk+JiO6iyEmfAEbVpQPL08j+1zuQU3RQyToQhhxuzOJAAAVeMc20cr4aaX8tAw/TZ/GjHD2CTEv92M5mUJylcoAkwhTvPWMFLMMtCsGzc+uBB899uuhZz6emFYqFuESJnq1wA1WCuTxu0s31aPzt894UOtDjantA6VjEuh5yR1bdSKE1LbnlEASj1CtjSK1bJVtjHnCEtVAhhERmMlRuYI6cRYwbYLN3opTp4D6io7oLLQ0iUop3hMFeK1vOoTz/ftrb7/xe/dDD7x8iSqm9NH48SyphqUdhkxry6i5VR25xWVSwG/efzD4dhOueClNLuvzKYMpFtUvP9V/PPWeB8t96+NznIz6yJTqchruxR0pdB+W5RGNkzeVI8LN6qlKVQZ0SMZcO/7mgZcRuIYThRMXUcPKlnV3aUZAPTHorhS0eW6/+bf//a07xn8xrCRvkhH8l53559djQ22tqdEDTCvb0qqGMk6uY8Pt1Dqp31OaXi8SNKL7MQvxADmi737z1fsPv95dtJo2HNIPCB/TZJwd0Ay06WVxSLhC4JUdKTklIhN4/m6w1hJEvgzpSxuP2NXbn2sra2mSPzv2Dq5j81v2u7X/UXh2hv3V3/T+XPv5Sw+qKCRSE5Koufmck/cCpwlSw2e3dWTsFcOUkSgZi8581fXs2GnYJR7G+fAsKmF3TvpTkPdBGcnJ7zx9Ep1EWemTLl0BgNXCxvPDvv5EnXZ+s2JMY4Px7Gs4kbOjWrlWYtgoMIDv6EeYNx5/sXv6h7u/WevWjJA4UULn/lDOsnlGGkk+xBKOBf4G8Uc01zWOmtHY9v0fyE6spRgIAAA="
}
}

trait BitSetsDsl extends impl.BitSetsAbs {self: ScalanCommunityDsl =>}
trait BitSetsDslSeq extends impl.BitSetsSeq {self: ScalanCommunityDslSeq =>}
trait BitSetsDslExp extends impl.BitSetsExp {self: ScalanCommunityDslExp =>}
