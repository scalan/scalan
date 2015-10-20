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
    lazy val entityDef: STraitOrClassDef = {
      val module = getModules("AbstractStrings")
      module.entities.find(_.name == "AString").get
    }
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map()
    }
    override def isEntityType = true
    override lazy val tag = {
      weakTypeTag[AString].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Reifiable[_]]) = {
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

  abstract class AStringCompanionAbs extends CompanionBase[AStringCompanionAbs] with AStringCompanion {
    override def toString = "AString"
  }
  def AString: Rep[AStringCompanionAbs]
  implicit def proxyAStringCompanion(p: Rep[AStringCompanion]): AStringCompanion =
    proxyOps[AStringCompanion](p)

  // elem for concrete class
  class SStringElem(val iso: Iso[SStringData, SString])
    extends AStringElem[SString]
    with ConcreteElem[SStringData, SString] {
    override lazy val parent: Option[Elem[_]] = Some(aStringElement)
    override lazy val entityDef = {
      val module = getModules("AbstractStrings")
      module.concreteSClasses.find(_.name == "SString").get
    }
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
  abstract class SStringCompanionAbs extends CompanionBase[SStringCompanionAbs] with SStringCompanion {
    override def toString = "SString"

    def apply(wrappedValue: Rep[String]): Rep[SString] =
      mkSString(wrappedValue)
  }
  object SStringMatcher {
    def unapply(p: Rep[AString]) = unmkSString(p)
  }
  def SString: Rep[SStringCompanionAbs]
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

  // elem for concrete class
  class CStringElem(val iso: Iso[CStringData, CString])
    extends AStringElem[CString]
    with ConcreteElem[CStringData, CString] {
    override lazy val parent: Option[Elem[_]] = Some(aStringElement)
    override lazy val entityDef = {
      val module = getModules("AbstractStrings")
      module.concreteSClasses.find(_.name == "CString").get
    }
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
  abstract class CStringCompanionAbs extends CompanionBase[CStringCompanionAbs] with CStringCompanion {
    override def toString = "CString"

    def apply(wrappedValue: Rep[String]): Rep[CString] =
      mkCString(wrappedValue)
  }
  object CStringMatcher {
    def unapply(p: Rep[AString]) = unmkCString(p)
  }
  def CString: Rep[CStringCompanionAbs]
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

  registerModule(scalan.meta.ScalanCodegen.loadModule(AbstractStrings_Module.dump))
}

// Seq -----------------------------------
trait AbstractStringsSeq extends AbstractStringsDsl with scalan.ScalanSeq {
  self: AbstractStringsDslSeq =>
  lazy val AString: Rep[AStringCompanionAbs] = new AStringCompanionAbs with UserTypeSeq[AStringCompanionAbs] {
    lazy val selfType = element[AStringCompanionAbs]
  }

  case class SeqSString
      (override val wrappedValue: Rep[String])

    extends SString(wrappedValue)
        with UserTypeSeq[SString] {
    lazy val selfType = element[SString]
  }
  lazy val SString = new SStringCompanionAbs with UserTypeSeq[SStringCompanionAbs] {
    lazy val selfType = element[SStringCompanionAbs]
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

    extends CString(wrappedValue)
        with UserTypeSeq[CString] {
    lazy val selfType = element[CString]
  }
  lazy val CString = new CStringCompanionAbs with UserTypeSeq[CStringCompanionAbs] {
    lazy val selfType = element[CStringCompanionAbs]
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
  lazy val AString: Rep[AStringCompanionAbs] = new AStringCompanionAbs with UserTypeDef[AStringCompanionAbs] {
    lazy val selfType = element[AStringCompanionAbs]
  }

  case class ExpSString
      (override val wrappedValue: Rep[String])

    extends SString(wrappedValue) with UserTypeDef[SString] {
    lazy val selfType = element[SString]
  }

  lazy val SString: Rep[SStringCompanionAbs] = new SStringCompanionAbs with UserTypeDef[SStringCompanionAbs] {
    lazy val selfType = element[SStringCompanionAbs]
  }

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

    extends CString(wrappedValue) with UserTypeDef[CString] {
    lazy val selfType = element[CString]
  }

  lazy val CString: Rep[CStringCompanionAbs] = new CStringCompanionAbs with UserTypeDef[CStringCompanionAbs] {
    lazy val selfType = element[CStringCompanionAbs]
  }

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
    object defaultVal {
      def unapply(d: Def[_]): Option[Unit] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem == AStringCompanionElem && method.getName == "defaultVal" =>
          Some(()).asInstanceOf[Option[Unit]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Unit] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

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

object AbstractStrings_Module {
  val packageName = "scalan.primitives"
  val name = "AbstractStrings"
  val dump = "H4sIAAAAAAAAALVVTWwbRRR+3jhx/KMmjQRSciG4JggEdoSEeggSMq6LkNwkyhaETIU0Xo/dKbOzk51JsDn0wBFuiCtCvffGBQmJC0JCHDghQOLMqYBQBfRUxJvZH69dts2FPYzm58177/u+92Zv/wbLKoQd5RFORNOnmjRdO28r3XC7QjM9vRIMTzi9REfvP/65d0W8ohxY68PKdaIuKd6HcjTpTmQ6d+lxD8pEeFTpIFQanuzZCC0v4Jx6mgWixXz/RJMBp60eU3qvB8VBMJwew00o9GDdC4QXUk3dDidKURXvr1KTEUvXZbueHshZDNEyKFoZFFdDwjSmjzHWI/sjKt2pCMTU13AuTu1AmrTQpsR8GYQ6CVFCd9eDYbIsCoIbsNG7QU5JC0OMW64OmRjjzaok3jtkTPfRxJgXMWFF+ejqVNr1Ug8qih4jQa/5ktudiQQAVOAFm0Rzxk8z5adp+Gm4NGSEs/eIOTwMg8kUoq+wBDCR6OK5R7hIPNCuGDY+uOa9dc+t+o65PDGplCzCFXT0RE41WCmQx2+OPlJ3X7110YFKHypMtQdKh8TTWcljtqpEiEDbnFMCSThGtep5atkobbRZKImyF/iSCPQUU1lDnTjzmDbGZq8Wq5NDfUlLmpgWJrKQ4t3OwWvrpkM4P7yz+fxTv3bfdMCZD1FGly4Wfpg41VBqR+VgKTVDOWY3P06K+Ok7vw+/3oVrTspT7PZs0qCLZfXTD9Xvn3nZgdW+LeTLnIz7SJXqcuofhJ1A6D6sBqc0jE5Kp4Sb2X9KVRrSETnhOiYwi3wJkWvYzm05SQ0te7a8CwkB1ahC9wNBG5cPG3+733582xRgCLXoJOrBf9jF+z+fG2lbmxpq74ZESjp8g/CTqPHXNCxhC6esXMgTUNLDkPn4YJzSF7/64vU/vtxfthpuxMCsy5lexSxGE9qp1zWszAwiLWeKVqK03cCn5+t32du3PtRWu8Jk/oU4GNzAltyz9zYfImPyUv3V33X+3PzxUwfKqNaAaZ/Ixu4Z++t/7BlIq3o2bCFP627EUScbbmv2qmzYKTaHO+Myc1xNejEWN7eHrK+M7WNpdVmPj6wVM2w/KKQZL9hxJxdh54wIO4sIo0CZpHdgHm35iLIRM8/0mViIk34w8/nbte4kzXX3YejnYbZzYS68bVsLab00v4lUrCX/hegSvvnn47KXSVOqGEAI9ZyOcON6xKa4ee+T/We/++wX28EVU9n4joj0P53t3HkqNhYSwf9vJnkNRVP0Nv1/AZD40HUPCQAA"
}
}

trait AbstractStringsDsl extends impl.AbstractStringsAbs {self: AbstractStringsDsl =>}
trait AbstractStringsDslSeq extends impl.AbstractStringsSeq {self: AbstractStringsDslSeq =>}
trait AbstractStringsDslExp extends impl.AbstractStringsExp {self: AbstractStringsDslExp =>}
