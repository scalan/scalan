package scalan.primitives
package impl

import scalan._
import scalan.common.Default
import scala.reflect.runtime.universe._
import scalan.common.Default

trait AbstractStringsAbs extends Scalan with AbstractStrings
{ self: AbstractStringsDsl =>
  // single proxy for each type family
  implicit def proxyAString(p: Rep[AString]): AString =
    proxyOps[AString](p)




  abstract class AStringElem[From, To <: AString](iso: Iso[From, To]) extends ViewElem[From, To]()(iso)

  trait AStringCompanionElem extends CompanionElem[AStringCompanionAbs]
  implicit lazy val AStringCompanionElem: AStringCompanionElem = new AStringCompanionElem {
    lazy val tag = typeTag[AStringCompanionAbs]
    lazy val defaultRep = Default.defaultVal(AString)
  }

  abstract class AStringCompanionAbs extends CompanionBase[AStringCompanionAbs] with AStringCompanion {
    override def toString = "AString"
  }
  def AString: Rep[AStringCompanionAbs]
  implicit def proxyAStringCompanion(p: Rep[AStringCompanion]): AStringCompanion = {
    proxyOps[AStringCompanion](p)
  }

  // elem for concrete class
  class SStringElem(iso: Iso[SStringData, SString]) extends AStringElem[SStringData, SString](iso)

  // state representation type
  type SStringData = String

  // 3) Iso for concrete class
  class SStringIso
    extends Iso[SStringData, SString] {
    override def from(p: Rep[SString]) =
      unmkSString(p) match {
        case Some((value)) => value
        case None => !!!
      }
    override def to(p: Rep[String]) = {
      val value = p
      SString(value)
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

    def apply(value: Rep[String]): Rep[SString] =
      mkSString(value)
    def unapply(p: Rep[SString]) = unmkSString(p)
  }
  def SString: Rep[SStringCompanionAbs]
  implicit def proxySStringCompanion(p: Rep[SStringCompanionAbs]): SStringCompanionAbs = {
    proxyOps[SStringCompanionAbs](p)
  }

  class SStringCompanionElem extends CompanionElem[SStringCompanionAbs] {
    lazy val tag = typeTag[SStringCompanionAbs]
    lazy val defaultRep = Default.defaultVal(SString)
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
  def mkSString(value: Rep[String]): Rep[SString]
  def unmkSString(p: Rep[SString]): Option[(Rep[String])]

  // elem for concrete class
  class CStringElem(iso: Iso[CStringData, CString]) extends AStringElem[CStringData, CString](iso)

  // state representation type
  type CStringData = String

  // 3) Iso for concrete class
  class CStringIso
    extends Iso[CStringData, CString] {
    override def from(p: Rep[CString]) =
      unmkCString(p) match {
        case Some((value)) => value
        case None => !!!
      }
    override def to(p: Rep[String]) = {
      val value = p
      CString(value)
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

    def apply(value: Rep[String]): Rep[CString] =
      mkCString(value)
    def unapply(p: Rep[CString]) = unmkCString(p)
  }
  def CString: Rep[CStringCompanionAbs]
  implicit def proxyCStringCompanion(p: Rep[CStringCompanionAbs]): CStringCompanionAbs = {
    proxyOps[CStringCompanionAbs](p)
  }

  class CStringCompanionElem extends CompanionElem[CStringCompanionAbs] {
    lazy val tag = typeTag[CStringCompanionAbs]
    lazy val defaultRep = Default.defaultVal(CString)
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
  def mkCString(value: Rep[String]): Rep[CString]
  def unmkCString(p: Rep[CString]): Option[(Rep[String])]
}

trait AbstractStringsSeq extends AbstractStringsAbs with AbstractStringsDsl with ScalanSeq {
  lazy val AString: Rep[AStringCompanionAbs] = new AStringCompanionAbs with UserTypeSeq[AStringCompanionAbs, AStringCompanionAbs] {
    lazy val selfType = element[AStringCompanionAbs]
  }



  case class SeqSString
      (override val value: Rep[String])
      
    extends SString(value) with UserTypeSeq[AString, SString] {
    lazy val selfType = element[SString].asInstanceOf[Elem[AString]]
  }
  lazy val SString = new SStringCompanionAbs with UserTypeSeq[SStringCompanionAbs, SStringCompanionAbs] {
    lazy val selfType = element[SStringCompanionAbs]
  }

  def mkSString
      (value: Rep[String]) =
      new SeqSString(value)
  def unmkSString(p: Rep[SString]) =
    Some((p.value))



  case class SeqCString
      (override val value: Rep[String])
      
    extends CString(value) with UserTypeSeq[AString, CString] {
    lazy val selfType = element[CString].asInstanceOf[Elem[AString]]
  }
  lazy val CString = new CStringCompanionAbs with UserTypeSeq[CStringCompanionAbs, CStringCompanionAbs] {
    lazy val selfType = element[CStringCompanionAbs]
  }

  def mkCString
      (value: Rep[String]) =
      new SeqCString(value)
  def unmkCString(p: Rep[CString]) =
    Some((p.value))
}

trait AbstractStringsExp extends AbstractStringsAbs with AbstractStringsDsl with ScalanExp {
  lazy val AString: Rep[AStringCompanionAbs] = new AStringCompanionAbs with UserTypeDef[AStringCompanionAbs, AStringCompanionAbs] {
    lazy val selfType = element[AStringCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  case class ExpSString
      (override val value: Rep[String])
      
    extends SString(value) with UserTypeDef[AString, SString] {
    lazy val selfType = element[SString].asInstanceOf[Elem[AString]]
    override def mirror(t: Transformer) = ExpSString(t(value))
  }

  lazy val SString: Rep[SStringCompanionAbs] = new SStringCompanionAbs with UserTypeDef[SStringCompanionAbs, SStringCompanionAbs] {
    lazy val selfType = element[SStringCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object SStringMethods {

  }

  object SStringCompanionMethods {

  }

  def mkSString
    (value: Rep[String]) =
    new ExpSString(value)
  def unmkSString(p: Rep[SString]) =
    Some((p.value))

  case class ExpCString
      (override val value: Rep[String])
      
    extends CString(value) with UserTypeDef[AString, CString] {
    lazy val selfType = element[CString].asInstanceOf[Elem[AString]]
    override def mirror(t: Transformer) = ExpCString(t(value))
  }

  lazy val CString: Rep[CStringCompanionAbs] = new CStringCompanionAbs with UserTypeDef[CStringCompanionAbs, CStringCompanionAbs] {
    lazy val selfType = element[CStringCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object CStringMethods {

  }

  object CStringCompanionMethods {

  }

  def mkCString
    (value: Rep[String]) =
    new ExpCString(value)
  def unmkCString(p: Rep[CString]) =
    Some((p.value))

  object AStringMethods {

  }

  object AStringCompanionMethods {
    object defaultVal {
      def unapply(d: Def[_]): Option[Unit] = d match {
        case MethodCall(receiver, method, _) if receiver.elem.isInstanceOf[AStringCompanionElem] && method.getName == "defaultVal" =>
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
        case MethodCall(receiver, method, Seq(msg, _*)) if receiver.elem.isInstanceOf[AStringCompanionElem] && method.getName == "apply" =>
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
