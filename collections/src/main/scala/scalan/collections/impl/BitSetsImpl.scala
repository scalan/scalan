package scalan.collections

import scalan._
import scalan.common.OverloadHack.Overloaded1
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait BitSetsAbs extends scalan.ScalanDsl with BitSets {
  self: CollectionsDsl with BitSetsDsl =>

  // single proxy for each type family
  implicit def proxyBitSet(p: Rep[BitSet]): BitSet = {
    proxyOps[BitSet](p)(scala.reflect.classTag[BitSet])
  }

  // familyElem
  class BitSetElem[To <: BitSet]
    extends EntityElem[To] {
    lazy val parent: Option[Elem[_]] = None
    lazy val typeArgs = TypeArgs()
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
        case e => !!!(s"Expected $x to have BitSetElem[_], but got $e", x)
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
  implicit def proxyBitSetCompanionAbs(p: Rep[BitSetCompanionAbs]): BitSetCompanionAbs =
    proxyOps[BitSetCompanionAbs](p)

  abstract class AbsBoolCollBitSet
      (bits: Rep[Collection[Boolean]])
    extends BoolCollBitSet(bits) with Def[BoolCollBitSet] {
    lazy val selfType = element[BoolCollBitSet]
  }
  // elem for concrete class
  class BoolCollBitSetElem(val iso: Iso[BoolCollBitSetData, BoolCollBitSet])
    extends BitSetElem[BoolCollBitSet]
    with ConcreteElem[BoolCollBitSetData, BoolCollBitSet] {
    override lazy val parent: Option[Elem[_]] = Some(bitSetElement)
    override lazy val typeArgs = TypeArgs()

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
    extends EntityIso[BoolCollBitSetData, BoolCollBitSet] with Def[BoolCollBitSetIso] {
    override def from(p: Rep[BoolCollBitSet]) =
      p.bits
    override def to(p: Rep[Collection[Boolean]]) = {
      val bits = p
      BoolCollBitSet(bits)
    }
    lazy val eFrom = element[Collection[Boolean]]
    lazy val eTo = new BoolCollBitSetElem(self)
    lazy val selfType = new BoolCollBitSetIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class BoolCollBitSetIsoElem() extends Elem[BoolCollBitSetIso] {
    def getDefaultRep = reifyObject(new BoolCollBitSetIso())
    lazy val tag = {
      weakTypeTag[BoolCollBitSetIso]
    }
    lazy val typeArgs = TypeArgs()
  }
  // 4) constructor and deconstructor
  class BoolCollBitSetCompanionAbs extends CompanionDef[BoolCollBitSetCompanionAbs] with BoolCollBitSetCompanion {
    def selfType = BoolCollBitSetCompanionElem
    override def toString = "BoolCollBitSet"

    @scalan.OverloadId("fromFields")
    def apply(bits: Rep[Collection[Boolean]]): Rep[BoolCollBitSet] =
      mkBoolCollBitSet(bits)

    def unapply(p: Rep[BitSet]) = unmkBoolCollBitSet(p)
  }
  lazy val BoolCollBitSetRep: Rep[BoolCollBitSetCompanionAbs] = new BoolCollBitSetCompanionAbs
  lazy val BoolCollBitSet: BoolCollBitSetCompanionAbs = proxyBoolCollBitSetCompanion(BoolCollBitSetRep)
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
    reifyObject(new BoolCollBitSetIso())

  // 6) smart constructor and deconstructor
  def mkBoolCollBitSet(bits: Rep[Collection[Boolean]]): Rep[BoolCollBitSet]
  def unmkBoolCollBitSet(p: Rep[BitSet]): Option[(Rep[Collection[Boolean]])]

  registerModule(BitSets_Module)
}

// Std -----------------------------------
trait BitSetsStd extends scalan.ScalanDslStd with BitSetsDsl {
  self: CollectionsDsl with BitSetsDslStd =>

  lazy val BitSet: Rep[BitSetCompanionAbs] = new BitSetCompanionAbs {
  }

  case class StdBoolCollBitSet
      (override val bits: Rep[Collection[Boolean]])
    extends AbsBoolCollBitSet(bits) {
  }

  def mkBoolCollBitSet
    (bits: Rep[Collection[Boolean]]): Rep[BoolCollBitSet] =
    new StdBoolCollBitSet(bits)
  def unmkBoolCollBitSet(p: Rep[BitSet]) = p match {
    case p: BoolCollBitSet @unchecked =>
      Some((p.bits))
    case _ => None
  }
}

// Exp -----------------------------------
trait BitSetsExp extends scalan.ScalanDslExp with BitSetsDsl {
  self: CollectionsDsl with BitSetsDslExp =>

  lazy val BitSet: Rep[BitSetCompanionAbs] = new BitSetCompanionAbs {
  }

  case class ExpBoolCollBitSet
      (override val bits: Rep[Collection[Boolean]])
    extends AbsBoolCollBitSet(bits)

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

object BitSets_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAALVWTYgcRRSumdnd2ZlZNuuIxOzFuE7iDzqzGCTCorI/E3+Y7A7pxMgkRGq7ayYVq6vLrtqlx0O8BdSbiAfBQ0TxEgTxpuBFAyLiwatnT1EJORgQFF9VV//Msp3owTk0VdWv33vf9733aq79hqZliI5KFzPM2z5RuO2Y9apULafLFVXjk4G3w8gGGV544aM/z/lvHSyjhQGauYjlhmQDVIsX3Uika0d5PVTD3CVSBaFU6MGeidBxA8aIq2jAO9T3dxTeZqTTo1Kt9NDUduCNX0eXUamHFtyAuyFRxFlnWEoi7fks0RnRdF8z+/GWyGLwjkbRyaE4HWKqIH2IsRDbnyLCGfOAj32F5m1qW0KnBTYNEgnA8KIvmAlT6aEq9UUQqiRqFSJcDLxkO8UxHKBm7xLexR2IOuo4KqR8pJ0J7L6GR2QTTLT5FGCQhA1PjwWxzhtSeRPxIoEQAlWeNIm1M87aKWdtzVnLISHFjL6B9ct+GERjFP9KFYQiAS4ev4uLxAPpcq/19nn33G2n4Zf1x5FOpWoSmgFHDxRUiJEHuP3u1Lvy1vNXj5dRfYDqVK5uSxViV+XLwNLVwJwHyuScMojDESi4VKSgibIKNnvKpOYGvsAcPFku50AoRl2qtLE+m7PyFHBfVYIkpqVIlFK8hwvwmlpax4z1bxx64siv3VfKqDwZogYuHWiGMHGq0MwaBaaVYVQ/apbc4jAp4Idv/O5dX0bnyylN1uu/UwZcNJ/+4MsjpP9ZGc0OTCGfYHhkNNI8bBDpDtBssEvC+Ly6i5le7atT1SNDvMOUZS8PuwKwFTpc2IOCaE5WTG2XEviNuDw3A05aJ/qtP5zv37umqy9Ec/GbuCn/psf/+nl+qExhKqgAqqRJ6YBCFehly4U9qa+n1Z6S9FCRnIL0Q+rDSNklT33z1ZmbX29OG0WbFunLmO2QuJst0Ay0zmV6iJkE4NW1IGAE80ze/FNjrceInMAn9yzdoheuvqOMqKVocnRsbV+C5FfMd4fuoG8y1T6/cuW+mx+/eq/pvFmgxseitfwf+i5pk/+xr1Ba+vFEaWZ7/VgEIg9qArVyca+s5xNY3PslTO1J88wqJj1XEY+iyfKoAGeTJ/u3Zy3aN1+zP2o93x3WgUI4k3NhcU/0ZXNog4WoXVAEG8RlOCSevjyID5dbLO+x9587+9L9Z8+YApvzjFH8Jm3R/a/ik1ismIvjkTtcHGDU6voC/hjA4ti3z/z05g+ffmJ6M8OvO8KAg4hNm3zmTKbAlgqAObaeQKzLtz/cfOzHL34xfVnXlQnjgqeXcb4fJ3Wdzw0CuGAznfSQsNkl5zHzMFt0Iec0flY/1v4BHLab5ycJAAA="
}
}

trait BitSetsDsl extends impl.BitSetsAbs {self: CollectionsDsl with BitSetsDsl =>}
trait BitSetsDslStd extends impl.BitSetsStd {self: CollectionsDsl with BitSetsDslStd =>}
trait BitSetsDslExp extends impl.BitSetsExp {self: CollectionsDsl with BitSetsDslExp =>}
