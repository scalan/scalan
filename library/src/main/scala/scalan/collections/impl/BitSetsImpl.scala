package scalan.collections

import scalan._
import scalan.common.OverloadHack.Overloaded1
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}

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
    val parent: Option[Elem[_]] = None
    override def isEntityType = true
    override lazy val tag = {
      weakTypeTag[BitSet].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Reifiable[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[BitSet] => convertBitSet(x) }
      tryConvert(element[BitSet], this, x, conv)
    }

    def convertBitSet(x : Rep[BitSet]): Rep[To] = {
      assert(x.selfType1 match { case _: BitSetElem[_] => true; case _ => false })
      x.asRep[To]
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def bitSetElement: Elem[BitSet] =
    new BitSetElem[BitSet]

  implicit case object BitSetCompanionElem extends CompanionElem[BitSetCompanionAbs] {
    lazy val tag = weakTypeTag[BitSetCompanionAbs]
    protected def getDefaultRep = BitSet
  }

  abstract class BitSetCompanionAbs extends CompanionBase[BitSetCompanionAbs] with BitSetCompanion {
    override def toString = "BitSet"
  }
  def BitSet: Rep[BitSetCompanionAbs]
  implicit def proxyBitSetCompanion(p: Rep[BitSetCompanion]): BitSetCompanion =
    proxyOps[BitSetCompanion](p)

  // elem for concrete class
  class BoolCollBitSetElem(val iso: Iso[BoolCollBitSetData, BoolCollBitSet])
    extends BitSetElem[BoolCollBitSet]
    with ConcreteElem[BoolCollBitSetData, BoolCollBitSet] {
    override val parent: Option[Elem[_]] = Some(bitSetElement)

    override def convertBitSet(x: Rep[BitSet]) = BoolCollBitSet(x.bits)
    override def getDefaultRep = super[ConcreteElem].getDefaultRep
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
    lazy val defaultRepTo: Rep[BoolCollBitSet] = BoolCollBitSet(element[Collection[Boolean]].defaultRepValue)
    lazy val eTo = new BoolCollBitSetElem(this)
  }
  // 4) constructor and deconstructor
  abstract class BoolCollBitSetCompanionAbs extends CompanionBase[BoolCollBitSetCompanionAbs] with BoolCollBitSetCompanion {
    override def toString = "BoolCollBitSet"

    def apply(bits: Rep[Collection[Boolean]]): Rep[BoolCollBitSet] =
      mkBoolCollBitSet(bits)
  }
  object BoolCollBitSetMatcher {
    def unapply(p: Rep[BitSet]) = unmkBoolCollBitSet(p)
  }
  def BoolCollBitSet: Rep[BoolCollBitSetCompanionAbs]
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
    new BoolCollBitSetIso

  // 6) smart constructor and deconstructor
  def mkBoolCollBitSet(bits: Rep[Collection[Boolean]]): Rep[BoolCollBitSet]
  def unmkBoolCollBitSet(p: Rep[BitSet]): Option[(Rep[Collection[Boolean]])]

  registerModule(scalan.meta.ScalanCodegen.loadModule(BitSets_Module.dump))
}

// Seq -----------------------------------
trait BitSetsSeq extends BitSetsDsl with scalan.ScalanSeq {
  self: ScalanCommunityDslSeq =>
  lazy val BitSet: Rep[BitSetCompanionAbs] = new BitSetCompanionAbs with UserTypeSeq[BitSetCompanionAbs] {
    lazy val selfType = element[BitSetCompanionAbs]
  }

  case class SeqBoolCollBitSet
      (override val bits: Rep[Collection[Boolean]])

    extends BoolCollBitSet(bits)
        with UserTypeSeq[BoolCollBitSet] {
    lazy val selfType = element[BoolCollBitSet]
  }
  lazy val BoolCollBitSet = new BoolCollBitSetCompanionAbs with UserTypeSeq[BoolCollBitSetCompanionAbs] {
    lazy val selfType = element[BoolCollBitSetCompanionAbs]
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
  lazy val BitSet: Rep[BitSetCompanionAbs] = new BitSetCompanionAbs with UserTypeDef[BitSetCompanionAbs] {
    lazy val selfType = element[BitSetCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  case class ExpBoolCollBitSet
      (override val bits: Rep[Collection[Boolean]])

    extends BoolCollBitSet(bits) with UserTypeDef[BoolCollBitSet] {
    lazy val selfType = element[BoolCollBitSet]
    override def mirror(t: Transformer) = ExpBoolCollBitSet(t(bits))
  }

  lazy val BoolCollBitSet: Rep[BoolCollBitSetCompanionAbs] = new BoolCollBitSetCompanionAbs with UserTypeDef[BoolCollBitSetCompanionAbs] {
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
  val dump = "H4sIAAAAAAAAALVVz28bRRR+3vxwbEdN6wtKLi3BLSoqdoSEipQDSh0XIZkkyhaETFVpsh47E2ZnNjvjyObQP6CIS8UVoR649cYRiQtCQhw4IYrEuacCQhXQExVvZse73qpLe2EPq9nZt+997/u+N3v3N1hQMVxQAeFENEOqSdO36y2lG35HaKYn78j+iNNtOtg+uvdG/MXhJx6s9GDxkKhtxXtQSRadcZSufXrchQoRAVVaxkrDi11boRVIzmmgmRQtFoYjTQ44bXWZ0ptdmD+Q/ckx3IRSF04HUgQx1dRvc6IUVW5/iRpELH2u2OfJbpTVEC3TRWumi2sxYRrhY43TSfw+jfyJkGISajjloO1GBhbGlFkYyVhPS5Qx3aHsTx/nBcENqHePyAlpYYlhy9cxE0P8shaR4EMypDsYYsLnEbCifHBtEtnnuS5UFT1Ggt4OI253xhEAoAKvWRDNjJ9myk/T8NPwacwIZx8R83IvluMJJFdpDmAcYYpLz0gxzUA7ot+4dT344JFfCz3z8dhAKdsOFzHR2QI3WCmQx+/2b6uHb9257EG1B1Wmtg6UjkmgZyV3bNWIEFJbzCmBJB6iWutFatkqWxjzhCUqgQwjIjCTo3IZdeIsYNoEm71lp04B9WUd0WloaRyV0n7PFfRrfdMmnO89WH31/K+d9z3w8iUqmNJH48fTpBoWrzBkWltGza3iyC0ukzb88oPf+99uwHUvpcllfT5lMMWC+vmn2o8X3/RgqWd9fJWTYQ+ZUh1Ow924LYXuwZI8oXHypnxCuFk9Valynw7IiGvH32zjc9i4hnOFExdRw8qmdXdpSkAtMeiOFLRxda/xt//9p3eN/2JYTt4kI/iYXf7nl1MDba2p0QNMKwtpRcMcTq5jw+1U26nfU5peKhI0onsxC/EAOaGvf/PVu398vbNgNa27Tt8jfESTcXaNZk0bLAsDwhU2Xr4iJadEZALP3k2v1aQjX4b0zPpDduPOx9rKWhrnz47dgyMEv2m/W/0Phadn2F+9De/P1Xufe1BBIZGakESNjeecvP9xmiA1fHZbQ8ZeMEwZiZKxaM9WXcuOnbpd4mGcD8+iEnZnpL8IeR9U9ikbMHPY5fefPo9OqAzABZe0oI2VQvj5kV97ok4rv1k21rHBeALWndTZga0clBjWC2zgOxHQCTcffbbzyg9f3rcGrho5ca6Ezv2nnHHzjNSTfNhLOBL4M8Tf0QxqHDijtMX9L3i6bOAeCAAA"
}
}

trait BitSetsDsl extends impl.BitSetsAbs {self: ScalanCommunityDsl =>}
trait BitSetsDslSeq extends impl.BitSetsSeq {self: ScalanCommunityDslSeq =>}
trait BitSetsDslExp extends impl.BitSetsExp {self: ScalanCommunityDslExp =>}
