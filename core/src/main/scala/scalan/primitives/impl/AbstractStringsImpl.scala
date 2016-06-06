package scalan.primitives

import scalan._
import scalan.common.Default
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait AbstractStringsAbs extends scalan.Scalan with AbstractStrings {
  self: AbstractStringsDsl =>

  // single proxy for each type family
  implicit def proxyAString(p: Rep[AString]): AString = {
    proxyOps[AString](p)(scala.reflect.classTag[AString])
  }

  // familyElem
  class AStringElem[To <: AString]
    extends EntityElem[To] {
    lazy val parent: Option[Elem[_]] = None
    lazy val typeArgs = TypeArgs()
    override def isEntityType = true
    override lazy val tag = {
      weakTypeTag[AString].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[AString] => convertAString(x) }
      tryConvert(element[AString], this, x, conv)
    }

    def convertAString(x: Rep[AString]): Rep[To] = {
      x.selfType1 match {
        case _: AStringElem[_] => x.asRep[To]
        case e => !!!(s"Expected $x to have AStringElem[_], but got $e", x)
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def aStringElement: Elem[AString] =
    cachedElem[AStringElem[AString]]()

  implicit case object AStringCompanionElem extends CompanionElem[AStringCompanionAbs] {
    lazy val tag = weakTypeTag[AStringCompanionAbs]
    protected def getDefaultRep = AString
  }

  abstract class AStringCompanionAbs extends CompanionDef[AStringCompanionAbs] with AStringCompanion {
    def selfType = AStringCompanionElem
    override def toString = "AString"
  }
  def AString: Rep[AStringCompanionAbs]
  implicit def proxyAStringCompanionAbs(p: Rep[AStringCompanionAbs]): AStringCompanionAbs =
    proxyOps[AStringCompanionAbs](p)

  abstract class AbsSString
      (wrappedValue: Rep[String])
    extends SString(wrappedValue) with Def[SString] {
    lazy val selfType = element[SString]
  }
  // elem for concrete class
  class SStringElem(val iso: Iso[SStringData, SString])
    extends AStringElem[SString]
    with ConcreteElem[SStringData, SString] {
    override lazy val parent: Option[Elem[_]] = Some(aStringElement)
    override lazy val typeArgs = TypeArgs()

    override def convertAString(x: Rep[AString]) = SString(x.wrappedValue)
    override def getDefaultRep = SString("")
    override lazy val tag = {
      weakTypeTag[SString]
    }
  }

  // state representation type
  type SStringData = String

  // 3) Iso for concrete class
  class SStringIso
    extends EntityIso[SStringData, SString] with Def[SStringIso] {
    override def from(p: Rep[SString]) =
      p.wrappedValue
    override def to(p: Rep[String]) = {
      val wrappedValue = p
      SString(wrappedValue)
    }
    lazy val eFrom = element[String]
    lazy val eTo = new SStringElem(self)
    lazy val selfType = new SStringIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class SStringIsoElem() extends Elem[SStringIso] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new SStringIso())
    lazy val tag = {
      weakTypeTag[SStringIso]
    }
  }
  // 4) constructor and deconstructor
  class SStringCompanionAbs extends CompanionDef[SStringCompanionAbs] with SStringCompanion {
    def selfType = SStringCompanionElem
    override def toString = "SString"

    @scalan.OverloadId("fromFields")
    def apply(wrappedValue: Rep[String]): Rep[SString] =
      mkSString(wrappedValue)

    def unapply(p: Rep[AString]) = unmkSString(p)
  }
  lazy val SStringRep: Rep[SStringCompanionAbs] = new SStringCompanionAbs
  lazy val SString: SStringCompanionAbs = proxySStringCompanion(SStringRep)
  implicit def proxySStringCompanion(p: Rep[SStringCompanionAbs]): SStringCompanionAbs = {
    proxyOps[SStringCompanionAbs](p)
  }

  implicit case object SStringCompanionElem extends CompanionElem[SStringCompanionAbs] {
    lazy val tag = weakTypeTag[SStringCompanionAbs]
    protected def getDefaultRep = SString
  }

  implicit def proxySString(p: Rep[SString]): SString =
    proxyOps[SString](p)

  implicit class ExtendedSString(p: Rep[SString]) {
    def toData: Rep[SStringData] = isoSString.from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoSString: Iso[SStringData, SString] =
    reifyObject(new SStringIso())

  // 6) smart constructor and deconstructor
  def mkSString(wrappedValue: Rep[String]): Rep[SString]
  def unmkSString(p: Rep[AString]): Option[(Rep[String])]

  abstract class AbsCString
      (wrappedValue: Rep[String])
    extends CString(wrappedValue) with Def[CString] {
    lazy val selfType = element[CString]
  }
  // elem for concrete class
  class CStringElem(val iso: Iso[CStringData, CString])
    extends AStringElem[CString]
    with ConcreteElem[CStringData, CString] {
    override lazy val parent: Option[Elem[_]] = Some(aStringElement)
    override lazy val typeArgs = TypeArgs()

    override def convertAString(x: Rep[AString]) = CString(x.wrappedValue)
    override def getDefaultRep = CString("")
    override lazy val tag = {
      weakTypeTag[CString]
    }
  }

  // state representation type
  type CStringData = String

  // 3) Iso for concrete class
  class CStringIso
    extends EntityIso[CStringData, CString] with Def[CStringIso] {
    override def from(p: Rep[CString]) =
      p.wrappedValue
    override def to(p: Rep[String]) = {
      val wrappedValue = p
      CString(wrappedValue)
    }
    lazy val eFrom = element[String]
    lazy val eTo = new CStringElem(self)
    lazy val selfType = new CStringIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class CStringIsoElem() extends Elem[CStringIso] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new CStringIso())
    lazy val tag = {
      weakTypeTag[CStringIso]
    }
  }
  // 4) constructor and deconstructor
  class CStringCompanionAbs extends CompanionDef[CStringCompanionAbs] with CStringCompanion {
    def selfType = CStringCompanionElem
    override def toString = "CString"

    @scalan.OverloadId("fromFields")
    def apply(wrappedValue: Rep[String]): Rep[CString] =
      mkCString(wrappedValue)

    def unapply(p: Rep[AString]) = unmkCString(p)
  }
  lazy val CStringRep: Rep[CStringCompanionAbs] = new CStringCompanionAbs
  lazy val CString: CStringCompanionAbs = proxyCStringCompanion(CStringRep)
  implicit def proxyCStringCompanion(p: Rep[CStringCompanionAbs]): CStringCompanionAbs = {
    proxyOps[CStringCompanionAbs](p)
  }

  implicit case object CStringCompanionElem extends CompanionElem[CStringCompanionAbs] {
    lazy val tag = weakTypeTag[CStringCompanionAbs]
    protected def getDefaultRep = CString
  }

  implicit def proxyCString(p: Rep[CString]): CString =
    proxyOps[CString](p)

  implicit class ExtendedCString(p: Rep[CString]) {
    def toData: Rep[CStringData] = isoCString.from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoCString: Iso[CStringData, CString] =
    reifyObject(new CStringIso())

  // 6) smart constructor and deconstructor
  def mkCString(wrappedValue: Rep[String]): Rep[CString]
  def unmkCString(p: Rep[AString]): Option[(Rep[String])]

  registerModule(AbstractStrings_Module)
}

// Std -----------------------------------
trait AbstractStringsStd extends scalan.ScalanStd with AbstractStringsDsl {
  self: AbstractStringsDslStd =>
  lazy val AString: Rep[AStringCompanionAbs] = new AStringCompanionAbs {
  }

  case class StdSString
      (override val wrappedValue: Rep[String])
    extends AbsSString(wrappedValue) {
  }

  def mkSString
    (wrappedValue: Rep[String]): Rep[SString] =
    new StdSString(wrappedValue)
  def unmkSString(p: Rep[AString]) = p match {
    case p: SString @unchecked =>
      Some((p.wrappedValue))
    case _ => None
  }

  case class StdCString
      (override val wrappedValue: Rep[String])
    extends AbsCString(wrappedValue) {
  }

  def mkCString
    (wrappedValue: Rep[String]): Rep[CString] =
    new StdCString(wrappedValue)
  def unmkCString(p: Rep[AString]) = p match {
    case p: CString @unchecked =>
      Some((p.wrappedValue))
    case _ => None
  }
}

// Exp -----------------------------------
trait AbstractStringsExp extends scalan.ScalanExp with AbstractStringsDsl {
  self: AbstractStringsDslExp =>
  lazy val AString: Rep[AStringCompanionAbs] = new AStringCompanionAbs {
  }

  case class ExpSString
      (override val wrappedValue: Rep[String])
    extends AbsSString(wrappedValue)

  object SStringMethods {
  }

  object SStringCompanionMethods {
  }

  def mkSString
    (wrappedValue: Rep[String]): Rep[SString] =
    new ExpSString(wrappedValue)
  def unmkSString(p: Rep[AString]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: SStringElem @unchecked =>
      Some((p.asRep[SString].wrappedValue))
    case _ =>
      None
  }

  case class ExpCString
      (override val wrappedValue: Rep[String])
    extends AbsCString(wrappedValue)

  object CStringMethods {
  }

  object CStringCompanionMethods {
  }

  def mkCString
    (wrappedValue: Rep[String]): Rep[CString] =
    new ExpCString(wrappedValue)
  def unmkCString(p: Rep[AString]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CStringElem @unchecked =>
      Some((p.asRep[CString].wrappedValue))
    case _ =>
      None
  }

  object AStringMethods {
    object wrappedValue {
      def unapply(d: Def[_]): Option[Rep[AString]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AStringElem[_]] && method.getName == "wrappedValue" =>
          Some(receiver).asInstanceOf[Option[Rep[AString]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[AString]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object AStringCompanionMethods {
    object apply {
      def unapply(d: Def[_]): Option[Rep[String]] = d match {
        case MethodCall(receiver, method, Seq(msg, _*), _) if receiver.elem == AStringCompanionElem && method.getName == "apply" =>
          Some(msg).asInstanceOf[Option[Rep[String]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[String]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}

object AbstractStrings_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAALVWTYgcRRR+85OdnR+SzUjA7MV1drIRjTOLQSIsKsPsRJTJ7JDeGBmDoaa7dtKxurvsql17PETwkIN6EvEgeAgoXoIg3hREUCGIBPSac05RCTmYk2L9dPf0jNvZ9eAciurqN++97/vee9XXf4cDzIcVZiKC3IaDOWoYat9ivG50XG7z8RnP2iZ4HW/98sR3Xumd93tZWBjA3CXE1hkZQFFvOgGN9wa3ulBErokZ93zG4dGuitA0PUKwyW3PbdqOs83RkOBm12Z8rQv5oWeN34ArkOnCgum5po85NtoEMYZZeD6PZUZ2/FxUz+MNOonhNiWKZgLFpo9sLtIXMRa0/VlMjbHruWOHw8EwtQ0q0xI2ZRxQgeFFhxIVJteFgu1Qz+dR1IKIcMmzose8i8QBVLuX0Q5qiqijpsF92x1JZxSZr6MR7gkTaZ4XGBgmW5tjikPnZcatqXgBBQChylMqscaEs0bMWUNyVjewbyNiv4Xky77vBWPQv0wOIKDCxYk9XEQecMe16u9eMF+9b5SdrPxzIFMpqITmhKNHUipEySO4vXH2A3bvhWunslAaQMlmrSHjPjJ5sgxCusrIdT2uco4ZRP5IKFhLU1BFaQmbmTIpmp5DkSs8hVxWhFDENm0ujeVZJZQnhfsCpzgyzQQ0E+NdSsGraqmNCOnfOfrksd86r2QhOx2iKFwaohn8yCmHQkvXg6JULsWQ3fQ4MeLjd/6wflyFC9mYp9Dt/qQRLqrPfPz1Mdz/IgvzA1XJpwkaKZEkEeuYmQOY93awr88LO4jI3a5CFSy8hbYJD+lL4s4J3ByWUpuQYknKmiruTAS/rOuz57m4frpf/9P46cPrsvx8qOg3uiv/tk/9devgFleVyaHypo8oxdbLiGzrUXCIQ040dczJcpp8FPd92xEjZAc//f035+5+2zugFKyGwJTLiVr5JEYZOlurcZibGGglJ3qWdNqG5+DDtXv2a9fe40q5TDA9IDaGl0VDrqn/HX2AiNHs+vLq1SN3P734kOqv+aHNHUTrq/+hu6Jm+B+7B+L61nOjOnmWy6Kgb8HQ1LWTkRdn/yI6xphQnHhdjho01Dy1sZSvhO2RuOiUxz1LSC5L/9ZXrstqXdkP2PY+wbZnweqYifxXYBp4TpTEvqiYImQm5eVdXVc6QZzw6p5s7Aa7lQp7ZhYuziT7nDoMc/OhkdIT69gkyMeWvDGxI250Xe0nP3r+/EsPnz+n+q1iKSP9Jh5Lu39/nEF0Td2Wjz3gthRG9Y5DxdeQ2Jz84dlf3/7588/UPJowwOFQdO9pkCLy4RAEjcYOi/HVUvAZYZcJja/c/6T3+M2vbqsZVZL9KialG3+IJGfTtIrVmUTEB0aCbA552cqJKujLZfMfoci9wg8KAAA="
}
}

trait AbstractStringsDsl extends impl.AbstractStringsAbs
trait AbstractStringsDslStd extends impl.AbstractStringsStd
trait AbstractStringsDslExp extends impl.AbstractStringsExp
