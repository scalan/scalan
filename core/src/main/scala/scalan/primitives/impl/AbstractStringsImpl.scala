package scalan.primitives

import scalan._
import scalan.common.Default
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait AbstractStringsAbs extends AbstractStrings with scalan.Scalan {
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
        case e => !!!(s"Expected $x to have AStringElem[_], but got $e")
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
  implicit def proxyAStringCompanion(p: Rep[AStringCompanion]): AStringCompanion =
    proxyOps[AStringCompanion](p)

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
    extends Iso[SStringData, SString] {
    override def from(p: Rep[SString]) =
      p.wrappedValue
    override def to(p: Rep[String]) = {
      val wrappedValue = p
      SString(wrappedValue)
    }
    lazy val eTo = new SStringElem(this)
  }
  // 4) constructor and deconstructor
  class SStringCompanionAbs extends CompanionDef[SStringCompanionAbs] with SStringCompanion {
    def selfType = SStringCompanionElem
    override def toString = "SString"

    def apply(wrappedValue: Rep[String]): Rep[SString] =
      mkSString(wrappedValue)
  }
  object SStringMatcher {
    def unapply(p: Rep[AString]) = unmkSString(p)
  }
  lazy val SString: Rep[SStringCompanionAbs] = new SStringCompanionAbs
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
    cachedIso[SStringIso]()

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
    extends Iso[CStringData, CString] {
    override def from(p: Rep[CString]) =
      p.wrappedValue
    override def to(p: Rep[String]) = {
      val wrappedValue = p
      CString(wrappedValue)
    }
    lazy val eTo = new CStringElem(this)
  }
  // 4) constructor and deconstructor
  class CStringCompanionAbs extends CompanionDef[CStringCompanionAbs] with CStringCompanion {
    def selfType = CStringCompanionElem
    override def toString = "CString"

    def apply(wrappedValue: Rep[String]): Rep[CString] =
      mkCString(wrappedValue)
  }
  object CStringMatcher {
    def unapply(p: Rep[AString]) = unmkCString(p)
  }
  lazy val CString: Rep[CStringCompanionAbs] = new CStringCompanionAbs
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
    cachedIso[CStringIso]()

  // 6) smart constructor and deconstructor
  def mkCString(wrappedValue: Rep[String]): Rep[CString]
  def unmkCString(p: Rep[AString]): Option[(Rep[String])]

  registerModule(AbstractStrings_Module)
}

// Seq -----------------------------------
trait AbstractStringsSeq extends AbstractStringsDsl with scalan.ScalanSeq {
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
trait AbstractStringsExp extends AbstractStringsDsl with scalan.ScalanExp {
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
  val dump = "H4sIAAAAAAAAALVVTWwbRRR+Xjtx/KMmjQRSciG4JggEdoSEeggSMq6LkNwk6haETIU0Xk/cKbOzk51JsDn0wBFuiCtCvffGBQmJC0JCHDghQOLMqYBQBfRUxJvZH6/dbptL9zCanzfvve/73pu99QcsqRC2lUc4ES2fatJy7byjdNPtCc309FIwOub0Aj388MkvvUviNeXA6gCWrxF1QfEBVKJJbyLTuUuP+lAhwqNKB6HS8HTfRmh7AefU0ywQbeb7x5oMOW33mdK7fSgNg9H0CG5AoQ9rXiC8kGrqdjlRiqp4f4WajFi6rtj1dF/OYoi2QdHOoLgSEqYxfYyxFtlfptKdikBMfQ1n4tT2pUkLbcrMl0GokxBldHctGCXLkiC4Aev96+SEtDHEuO3qkIkx3qxJ4r1HxnQPTYx5CRNWlB9emUq7LvahqugREvSGL7ndmUgAQAVeskm0Zvy0Un5ahp+mS0NGOPuAmMODMJhMIfoKRYCJRBcvPMJF4oH2xKj50VXvnbtuzXfM5YlJpWwRLqOjp3KqwUqBPH53+RN15/Wb5x2oDqDKVGeodEg8nZU8ZqtGhAi0zTklkIRjVKuRp5aN0kGbhZKoeIEviUBPMZV11Ikzj2ljbPbqsTo51Je1pIlpYSILKd6tHLy2brqE84PbGy8+83vvbQec+RAVdOli4YeJUw3lTlQOllIzVGJ28+OkiJ+9/efo2x246qQ8xW5PJw26WFK//FT78blXHVgZ2EK+yMl4gFSpHqf+ftgNhB7ASnBCw+ikfEK4mT1QqvKIHpJjrmMCs8iLiFzDVm7LSWpo2bXlXUgIqEUVuhcI2rx40PzX/f7TW6YAQ6hHJ1EP/sfO3/v1zKG2tamh/n5IpKSjtwg/jhp/VUMRWzhl5VyegJIehMzHB+OEvvzNV2/+9fXektVwPQZmXc70KmUxmtBOo6FheWYQaTlTtBql7QY+Pdu4w969+bG22hUm8y/E/vA6tuSuvbfxEBmTl+qfwY7z98bPnztQQbWGTPtENndO2V+PsWcgrerZsIk8rbkRR91suM3Zq7Jup9gc7ozLzHEt6cVY3Nwesr4ytk+k1WU9PrJWzLB1v5BmPGfH7VyE3VMi7C4ijAJlkt6GebRFFPxU+ON07895/na9N0mz3HkY7nmAnVyAC6/a5kJar8xvIgmryR8huoSv/dm44GXSjioGEEIjpxfcuBKRnRt3P9t7/ocvfrO9WzU1jS+ISP/Q2Z6dp2J9IRH882aS11Ay5W7T/x9Ylgs0CQkAAA=="
}
}

trait AbstractStringsDsl extends impl.AbstractStringsAbs {self: AbstractStringsDsl =>}
trait AbstractStringsDslSeq extends impl.AbstractStringsSeq {self: AbstractStringsDslSeq =>}
trait AbstractStringsDslExp extends impl.AbstractStringsExp {self: AbstractStringsDslExp =>}
