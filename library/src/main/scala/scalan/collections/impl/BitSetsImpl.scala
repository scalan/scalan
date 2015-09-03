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
    cachedElem[BitSetElem[BitSet]]()

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
    override lazy val parent: Option[Elem[_]] = Some(bitSetElement)
    override lazy val entityDef = {
      val module = getModules("BitSets")
      module.concreteSClasses.find(_.name == "BoolCollBitSet").get
    }
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map()
    }

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
    cachedIso[BoolCollBitSetIso]()

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
  val dump = "H4sIAAAAAAAAALVVv48bRRR+3vvhs33KJZYidNcQDhOUKLFPSFGKK6KL4yAkc3e6DREyUaS59diZMDuztzM+2RT5A6BDtAilT0cZKU0UCVFQ8UuioYEmgFAUSBXEm9nxrjfKhjRssZqdffve977ve7N3focFFcNJFRBORDOkmjR9u95SuuF3hGZ68q7sjzi9RAdPf25+9+vgwnEPVnqweIOoS4r3oJIsOuMoXfv0oAsVIgKqtIyVhte6tkIrkJzTQDMpWiwMR5rsc9rqMqU3uzC/L/uTA7gFpS4cDaQIYqqp3+ZEKarc/hI1iFj6XLHPk50oqyFapovWTBdXYsI0wscaR5P4PRr5EyHFJNRwxEHbiQwsjCmzMJKxnpYoY7obsj99nBcEN6DevUkOSQtLDFu+jpkY4pe1iAQfkiHdxhATPo+AFeWDK5PIPs91oaroARL0ThhxuzOOAAAVeMuCaGb8NFN+moafhk9jRjj7iJiXu7EcTyC5SnMA4whTnPmPFNMMtCP6jY+vBR888WuhZz4eGyhl2+EiJnq1wA1WCuTxq71P1aO3b5/3oNqDKlNb+0rHJNCzkju2akQIqS3mlEASD1Gt9SK1bJUtjHnGEpVAhhERmMlRuYw6cRYwbYLN3rJTp4D6so7oNLQ0jkppvycK+rW+aRPOdx+unn3jt877Hnj5EhVM6aPx42lSDYsXGTKtLaPmVnHkFpdJG37z4R/9BxtwzUtpcllfThlMsaB+/L727akLHiz1rI8vczLsIVOqw2m4E7el0D1Ykoc0Tt6UDwk3q+cqVe7TARlx7fibbXwOG9dwonDiImpY2bTuLk0JqCUG3ZaCNi7vNv72v/7sjvFfDMvJm2QE/2Hnn/50ZKCtNTV6gGllIa1omMPJdWy4nWo79XtK0+tFgkZ0N2YhHiCH9Nz9u+/9eW97wWpad51eJXxEk3F2jWZNGywLA8IVNl6+KCWnRGQCz95Nr9WkI1+G9Nj6I3b99ifayloa58+Onf2bCH7Tfrf6AoWnZ9hfvQ3v8eoPX3hQQSGRmpBEjY2XnLz/cZogNXx2W0PGXjFMGYmSsWjPVl3Ljp26XeJhnA/PohJ2Z6Q/BXkfVPYoGzBz2OX3nz+PTqgMwEmXtKCNlUL4+ZFfe6ZOK79ZNtaxwXgC1p3U2YGtHJQY1gts4DsR0Am3nny+ffqbL3+xBq4aOXGuhM79p5xx84zUk3zYSzgS+DPE39EMahw4o7TF/S9ehh/oHggAAA=="
}
}

trait BitSetsDsl extends impl.BitSetsAbs {self: ScalanCommunityDsl =>}
trait BitSetsDslSeq extends impl.BitSetsSeq {self: ScalanCommunityDslSeq =>}
trait BitSetsDslExp extends impl.BitSetsExp {self: ScalanCommunityDslExp =>}
