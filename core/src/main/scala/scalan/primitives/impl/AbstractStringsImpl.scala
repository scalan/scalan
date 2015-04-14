package scalan.primitives
package impl

import scalan._
import scalan.common.Default
import scala.reflect.runtime.universe._
import scala.reflect._
import scalan.common.Default

// Abs -----------------------------------
trait AbstractStringsAbs extends Scalan with AbstractStrings {
  self: AbstractStringsDsl =>
  // single proxy for each type family
  implicit def proxyAString(p: Rep[AString]): AString = {
    proxyOps[AString](p)(classTag[AString])
  }

  class AStringElem[To <: AString]
    extends EntityElem[To] {
    override def isEntityType = true
    override def tag = {
      weakTypeTag[AString].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Reifiable[_]]) = convertAString(x.asRep[AString])
    def convertAString(x : Rep[AString]): Rep[To] = {
      assert(x.selfType1.isInstanceOf[AStringElem[_]])
      x.asRep[To]
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def aStringElement =
    new AStringElem[AString]()

  trait AStringCompanionElem extends CompanionElem[AStringCompanionAbs]
  implicit lazy val AStringCompanionElem: AStringCompanionElem = new AStringCompanionElem {
    lazy val tag = weakTypeTag[AStringCompanionAbs]
    protected def getDefaultRep = AString
  }

  abstract class AStringCompanionAbs extends CompanionBase[AStringCompanionAbs] with AStringCompanion {
    override def toString = "AString"
  }
  def AString: Rep[AStringCompanionAbs]
  implicit def proxyAStringCompanion(p: Rep[AStringCompanion]): AStringCompanion = {
    proxyOps[AStringCompanion](p)
  }

  // elem for concrete class
  class SStringElem(val iso: Iso[SStringData, SString])
    extends AStringElem[SString]
    with ViewElem[SStringData, SString] {
    override def convertAString(x: Rep[AString]) = SString(x.wrappedValueOfBaseType)
    override def getDefaultRep = super[ViewElem].getDefaultRep
    override lazy val tag = super[ViewElem].tag
  }

  // state representation type
  type SStringData = String

  // 3) Iso for concrete class
  class SStringIso
    extends Iso[SStringData, SString] {
    override def from(p: Rep[SString]) =
      unmkSString(p) match {
        case Some((wrappedValueOfBaseType)) => wrappedValueOfBaseType
        case None => !!!
      }
    override def to(p: Rep[String]) = {
      val wrappedValueOfBaseType = p
      SString(wrappedValueOfBaseType)
    }
    lazy val tag = {
      weakTypeTag[SString]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[SString]](SString(""))
    lazy val eTo = new SStringElem(this)
  }
  // 4) constructor and deconstructor
  abstract class SStringCompanionAbs extends CompanionBase[SStringCompanionAbs] with SStringCompanion {
    override def toString = "SString"

    def apply(wrappedValueOfBaseType: Rep[String]): Rep[SString] =
      mkSString(wrappedValueOfBaseType)
    def unapply(p: Rep[SString]) = unmkSString(p)
  }
  def SString: Rep[SStringCompanionAbs]
  implicit def proxySStringCompanion(p: Rep[SStringCompanionAbs]): SStringCompanionAbs = {
    proxyOps[SStringCompanionAbs](p)
  }

  class SStringCompanionElem extends CompanionElem[SStringCompanionAbs] {
    lazy val tag = weakTypeTag[SStringCompanionAbs]
    protected def getDefaultRep = SString
  }
  implicit lazy val SStringCompanionElem: SStringCompanionElem = new SStringCompanionElem

  implicit def proxySString(p: Rep[SString]): SString =
    proxyOps[SString](p)

  implicit class ExtendedSString(p: Rep[SString]) {
    def toData: Rep[SStringData] = isoSString.from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoSString: Iso[SStringData, SString] =
    new SStringIso

  // 6) smart constructor and deconstructor
  def mkSString(wrappedValueOfBaseType: Rep[String]): Rep[SString]
  def unmkSString(p: Rep[SString]): Option[(Rep[String])]

  // elem for concrete class
  class CStringElem(val iso: Iso[CStringData, CString])
    extends AStringElem[CString]
    with ViewElem[CStringData, CString] {
    override def convertAString(x: Rep[AString]) = CString(x.wrappedValueOfBaseType)
    override def getDefaultRep = super[ViewElem].getDefaultRep
    override lazy val tag = super[ViewElem].tag
  }

  // state representation type
  type CStringData = String

  // 3) Iso for concrete class
  class CStringIso
    extends Iso[CStringData, CString] {
    override def from(p: Rep[CString]) =
      unmkCString(p) match {
        case Some((wrappedValueOfBaseType)) => wrappedValueOfBaseType
        case None => !!!
      }
    override def to(p: Rep[String]) = {
      val wrappedValueOfBaseType = p
      CString(wrappedValueOfBaseType)
    }
    lazy val tag = {
      weakTypeTag[CString]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[CString]](CString(""))
    lazy val eTo = new CStringElem(this)
  }
  // 4) constructor and deconstructor
  abstract class CStringCompanionAbs extends CompanionBase[CStringCompanionAbs] with CStringCompanion {
    override def toString = "CString"

    def apply(wrappedValueOfBaseType: Rep[String]): Rep[CString] =
      mkCString(wrappedValueOfBaseType)
    def unapply(p: Rep[CString]) = unmkCString(p)
  }
  def CString: Rep[CStringCompanionAbs]
  implicit def proxyCStringCompanion(p: Rep[CStringCompanionAbs]): CStringCompanionAbs = {
    proxyOps[CStringCompanionAbs](p)
  }

  class CStringCompanionElem extends CompanionElem[CStringCompanionAbs] {
    lazy val tag = weakTypeTag[CStringCompanionAbs]
    protected def getDefaultRep = CString
  }
  implicit lazy val CStringCompanionElem: CStringCompanionElem = new CStringCompanionElem

  implicit def proxyCString(p: Rep[CString]): CString =
    proxyOps[CString](p)

  implicit class ExtendedCString(p: Rep[CString]) {
    def toData: Rep[CStringData] = isoCString.from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoCString: Iso[CStringData, CString] =
    new CStringIso

  // 6) smart constructor and deconstructor
  def mkCString(wrappedValueOfBaseType: Rep[String]): Rep[CString]
  def unmkCString(p: Rep[CString]): Option[(Rep[String])]
}

// Seq -----------------------------------
trait AbstractStringsSeq extends AbstractStringsDsl with ScalanSeq {
  self: AbstractStringsDslSeq =>
  lazy val AString: Rep[AStringCompanionAbs] = new AStringCompanionAbs with UserTypeSeq[AStringCompanionAbs] {
    lazy val selfType = element[AStringCompanionAbs]
  }

  case class SeqSString
      (override val wrappedValueOfBaseType: Rep[String])

    extends SString(wrappedValueOfBaseType)
        with UserTypeSeq[SString] {
    lazy val selfType = element[SString]
  }
  lazy val SString = new SStringCompanionAbs with UserTypeSeq[SStringCompanionAbs] {
    lazy val selfType = element[SStringCompanionAbs]
  }

  def mkSString
      (wrappedValueOfBaseType: Rep[String]): Rep[SString] =
      new SeqSString(wrappedValueOfBaseType)
  def unmkSString(p: Rep[SString]) =
    Some((p.wrappedValueOfBaseType))

  case class SeqCString
      (override val wrappedValueOfBaseType: Rep[String])

    extends CString(wrappedValueOfBaseType)
        with UserTypeSeq[CString] {
    lazy val selfType = element[CString]
  }
  lazy val CString = new CStringCompanionAbs with UserTypeSeq[CStringCompanionAbs] {
    lazy val selfType = element[CStringCompanionAbs]
  }

  def mkCString
      (wrappedValueOfBaseType: Rep[String]): Rep[CString] =
      new SeqCString(wrappedValueOfBaseType)
  def unmkCString(p: Rep[CString]) =
    Some((p.wrappedValueOfBaseType))
}

// Exp -----------------------------------
trait AbstractStringsExp extends AbstractStringsDsl with ScalanExp {
  self: AbstractStringsDslExp =>
  lazy val AString: Rep[AStringCompanionAbs] = new AStringCompanionAbs with UserTypeDef[AStringCompanionAbs] {
    lazy val selfType = element[AStringCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  case class ExpSString
      (override val wrappedValueOfBaseType: Rep[String])

    extends SString(wrappedValueOfBaseType) with UserTypeDef[SString] {
    lazy val selfType = element[SString]
    override def mirror(t: Transformer) = ExpSString(t(wrappedValueOfBaseType))
  }

  lazy val SString: Rep[SStringCompanionAbs] = new SStringCompanionAbs with UserTypeDef[SStringCompanionAbs] {
    lazy val selfType = element[SStringCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object SStringMethods {
  }

  object SStringCompanionMethods {
  }

  def mkSString
    (wrappedValueOfBaseType: Rep[String]): Rep[SString] =
    new ExpSString(wrappedValueOfBaseType)
  def unmkSString(p: Rep[SString]) =
    Some((p.wrappedValueOfBaseType))

  case class ExpCString
      (override val wrappedValueOfBaseType: Rep[String])

    extends CString(wrappedValueOfBaseType) with UserTypeDef[CString] {
    lazy val selfType = element[CString]
    override def mirror(t: Transformer) = ExpCString(t(wrappedValueOfBaseType))
  }

  lazy val CString: Rep[CStringCompanionAbs] = new CStringCompanionAbs with UserTypeDef[CStringCompanionAbs] {
    lazy val selfType = element[CStringCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object CStringMethods {
  }

  object CStringCompanionMethods {
  }

  def mkCString
    (wrappedValueOfBaseType: Rep[String]): Rep[CString] =
    new ExpCString(wrappedValueOfBaseType)
  def unmkCString(p: Rep[CString]) =
    Some((p.wrappedValueOfBaseType))

  object AStringMethods {
    object wrappedValueOfBaseType {
      def unapply(d: Def[_]): Option[Rep[AString]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AStringElem[_]] && method.getName == "wrappedValueOfBaseType" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AStringCompanionElem] && method.getName == "defaultVal" =>
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
        case MethodCall(receiver, method, Seq(msg, _*), _) if receiver.elem.isInstanceOf[AStringCompanionElem] && method.getName == "apply" =>
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
