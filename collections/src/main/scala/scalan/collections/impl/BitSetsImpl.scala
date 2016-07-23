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
    def isEntityType = true
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
  val dump = "H4sIAAAAAAAAALVWTYgcRRSumdnd2ZlZNpsR0ezFuE4SE3VmMUiERWV/JqJMZod0YmQMSm13zaRidVfZVbv0eIjgIQf1JOJB8BBQvARBvCmIYAQREczVs6cYCTmYk+Kr6prunmU70YNzaKqqX7/3vu9779VcvYmmZYgOSxczHDR9onDTMetVqRpOO1BUjU5xb5uRDTL45bFvefXt97pFtNBHMxew3JCsjyrxoh2JZO0or4MqOHCJVDyUCj3cMRFaLmeMuIryoEV9f1vhLUZaHSrVSgdNbXFv9Aa6hAodtODywA2JIs46w1ISac9nic6IJvuK2Y82RRojaGkUrQyKMyGmCtKHGAux/WkinFHAg5Gv0LxNbVPotMCmRiIBGF7wBTNhSh1Upr7goRpHLUOEC9wbb6cCDAeo3rmId3ALog5bjgppMNTOBHZfx0PSBRNtPgUYJGGDMyNBrPOaVN5EvEgghECVJ01izZSzZsJZU3PWcEhIMaNvYv2yF/JohOJfoYRQJMDF4/dwMfZA2oHXeOe8+8odp+YX9ceRTqVsEpoBRw/lVIiRB7j94fT78vbzV04UUbWPqlSubkkVYldly8DSVcNBwJXJOWEQh0NQcClPQRNlFWx2lUnF5b7AAXiyXM6BUIy6VGljfTZn5cnhvqwEGZsWIlFI8B7MwWtqaR0z1rtx4IlDv7dfLqLiZIgKuHSgGcKxU4Vm1igwrQyj+lGx5OaHSQAfufGH9/0yOl9MaLJe/50y4KL+9EdfHSK9z4totm8K+STDQ6OR5mGDSLePZvkOCePz8g5merWnTmWPDPA2U5a9LOwSwFboYG4PCqI5WTG1XRjDr8Xl2eUBaZzsNf50fvzgqq6+EM3Fb+Km/Jue+OvX+YEyhamgAqiSJqV9CpWgly0X9qS6nlR7QtIjeXIK0gupDyNlhzz13ddnb33TnTaK1i3SlzDbJnE3W6ApaJ3L9AAzCcDLa5wzgoNU3uxTY63GiBzuk/1Lt+mrV95VRtRCNDk6NrcuQvIr5rsDd9F3PNW+uHz5/lufvHaf6bxZoMbHorH8H/pu3Cb/Y1+hpPTjiVJP9/qxCEQ+oAnUysW9sp5NYHH3lzC1J81Tq5j0TEUcRZPlUQLOJk/2bs9KtGe+Zn/Yer43rH25cCbnwuKu6Mvm0AYLUTOnCDaIy3BIPH15EB8ut1je4x8+d+7FB8+dNQU25xmj+E3SontfxaewWDEXx6N3uTjAqNH2BfwxgMXxa89cf+unzz41vZni1x1hwEHEuk0+dSYTYEs5wBxbTyDWpTsfd4/9/OVvpi+rujJhXATJZZztx0ld5zODAC7YVCc9JGx24/OYeZgtupAzGj+rH2v/AAaM1KcnCQAA"
}
}

trait BitSetsDsl extends impl.BitSetsAbs {self: CollectionsDsl with BitSetsDsl =>}
trait BitSetsDslStd extends impl.BitSetsStd {self: CollectionsDsl with BitSetsDslStd =>}
trait BitSetsDslExp extends impl.BitSetsExp {self: CollectionsDsl with BitSetsDslExp =>}
