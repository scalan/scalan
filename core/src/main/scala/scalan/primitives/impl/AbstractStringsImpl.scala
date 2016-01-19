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
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map()
    }
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
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map()
    }

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
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map()
    }

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

// Seq -----------------------------------
trait AbstractStringsSeq extends scalan.ScalanSeq with AbstractStringsDsl {
  self: AbstractStringsDslSeq =>
  lazy val AString: Rep[AStringCompanionAbs] = new AStringCompanionAbs {
  }

  case class SeqSString
      (override val wrappedValue: Rep[String])
    extends AbsSString(wrappedValue) {
  }

  def mkSString
    (wrappedValue: Rep[String]): Rep[SString] =
    new SeqSString(wrappedValue)
  def unmkSString(p: Rep[AString]) = p match {
    case p: SString @unchecked =>
      Some((p.wrappedValue))
    case _ => None
  }

  case class SeqCString
      (override val wrappedValue: Rep[String])
    extends AbsCString(wrappedValue) {
  }

  def mkCString
    (wrappedValue: Rep[String]): Rep[CString] =
    new SeqCString(wrappedValue)
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
  val dump = "H4sIAAAAAAAAALVVTWhcRRz/70ey2Q+aNFKwuRg3a0TR3ShIDwFl2W5FWJPQV0XW0jL7drKdOm9mMjOJux567EFv4lWw4EXoRTyJIIII4sGTiODZU1VKD/akODPvY9+ueSYefIfhzcx//h+/3/83c/c3WFASNpWPKGLNAGvU9Nx/W+mG12Wa6MmrfHhI8UW8/yj/6qPnPln7PA/LfVi8gdRFRftQDn+6Y5H8e/igB2XEfKw0l0rD4z0XoeVzSrGvCWctEgSHGg0obvWI0ts9KA74cHIAtyDXgxWfM19ijb0ORUphFa0vYZsRSeZlN5/simkM1rJVtFJVXJGIaJO+ibES2l/GwpswziaBhjNRarvCpmVsSiQQXOo4RMm4u8GH8bTIkFmA1d5NdIRaJsSo5WlJ2MicrArkv4VGeMeYWPOiSVhhun9lIty80IOKwgcGoFcCQd3KWACAYeB5l0Rzik8zwadp8Wl4WBJEyTvIbu5JPp5A+OUKAGNhXDxzgovYA+6yYePdq/6bD71qkLeHxzaVkqtw0Th6LKMbHBUGx28vv68evHznQh4qfagQ1R4oLZGv05RHaFURY1y7nBMAkRwZtupZbLkobWMz1xJlnwcCMeMpgrJmeKLEJ9oa27VaxE4G9CUtcGyaG4tcUu96Rr2ubzqI0r1755994tfuG3nIz4YoG5eeaXwZO9VQaoft4CC1QzlCNztOUvGT934ffrMFV/MJTpHb01FjXCyon36s/vDUS3lY6rtGvkTRqG+gUl2Kg13Z4Uz3YYkfYRnulI4QtX/HUlUa4n10SHUEYLrygqlcw3qm5AS2sGy79s7FAFTDDt3hDDcu7TX+8L774K5tQAm1cCfU4F/kwp8/n9nXrjc11N6WSAg8fB3Rw1D4yxoKRsIJKhtZBAq8J0lgLowj/MLXX7x2/8udBcfhalSYcznlq5iu0YbO1+saFqcGIZdTRith2h4P8Nn6A3LtznvacZcbz94Qu4ObRpLb7tz5f6Exvqk+vX373P2Prz/iFLY0IDpAorH1H/QVy+F/1A8kHR7eHKvTuR3WDHwrXghdJx15bf6I0Yw3hTi1XY0lGnGeKS3nK2V7Lmk65/HEFrLD+j/5teOGGzdPU2znlMV25osNY6by34TZwgumJU4FxQwgcylvHOu61h0nCW+diMZxZbczy567Ddfmkn1xdtFAsxy/JOEh80qcjYQiYhmrqBYJ9QwNeVHXGsxuPfxw5+nvP/vFab5i+9/cPCx52dNan0VldS4R82KnktdQtNJw6f8NR9KtDkEJAAA="
}
}

trait AbstractStringsDsl extends impl.AbstractStringsAbs
trait AbstractStringsDslSeq extends impl.AbstractStringsSeq
trait AbstractStringsDslExp extends impl.AbstractStringsExp
