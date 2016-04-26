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
  val dump = "H4sIAAAAAAAAALVWTYgcRRR+87M7Oz8km5WA2Yvr7Lj+zywGyWHRMMxORJnMDumNkTEoNd21k47V3WVX7TrjIYKHHNSTiDcPAcVLLuJREEEFEQkoIoJnT1EJOZiT4qvqn+kZt7PrwTkU1V1v3nvf9733qq//DnPChzVhEkbcukMlqRt63xSyZrRdacvxWc/aZXST7nz/6Bde6c13ullY7MP8JSI2BetDMdi0RzzeG9LqQJG4JhXS84WE+zs6QsP0GKOmtD23YTvOriQDRhsdW8iNDuQHnjV+Fa5ApgOLpueaPpXUaDEiBBXh+wWqMrLj56J+Hm/xSQy3oVA0Eii2fWJLTB9jLAb25yg3xq7njh0JR8LUtrhKC23KdMQRw7MOZzpMrgMF2+GeL6OoBYxwybOix7xL8AUsdS6TPdLAqMOGIX3bHSpnnJivkCHtookyzyMGQdnO9pjT0HlZSGsq3ogDAKryhE6sPuGsHnNWV5zVDOrbhNmvE3XY873RGIJfJgcw4ujisQNcRB5o27Vqb100X7xjlJ2s+vNIpVLQCc2jo/tSKkTLg9x+c+5dcfuZa6eyUOpDyRbNgZA+MWWyDEK6ysR1Palzjhkk/hAVrKYpqKM00WamTIqm53DioqeQywoKxWzTlspYvauE8qRwX5CcRqaZEc/EeFdS8OpaahHGejdPPP7Ab+0XspCdDlFElwY2gx85lVBoBvWgKVVLMWQ3PU6M+MGbf1hfr8PFbMxT6PZw0qCLOfHzT+UfHz6dhYW+ruQzjAz7SJVoM+ps+S3PlX1Y8PaoH5wU9ghTu32lKlh0h+wyGRKYRJ5D5BJWUtuQU0XLhi7vTERAOajQrufS2ple7U/j2/euqwL0oRKcBH35t33qr1+O7EhdmxIqr/mEc2o9T9huMAyOSshhW8esrKYJyGnPtx0cInv0yS8/O3/r8+6c1nApBKZdTvTKJzGq0NlqVcL8xCDQcqJoKUjb8Bx6rHrbfuna21JrlxlNj4itwWVsyQ39vxN3kTGaXp9cvXr81ocv36M7bGFgS4fw2vp/6K+oHf7H/oG4woPJsTR5Vssy0rdoBNS1kpGXZ/+CPWNMKE4cl6MWDTVPbS3tK2F7PC467fHAElLLyr/1VeuqXtcOA7Z1SLCtWbBBzET+azANPIclcSgqpgiZSXl1X9eV9ihOeP1ANvaD3UyFPTMNl2eSfVq/DHPzoZ7SE5vUZMSnlrozqYN3elDtJ98/feG5ey+c1/1WsbRRcBKPpf2/QM4SvqHvy4fucl+iUa3tcPwews3Jr5764Y3vPv5Iz6MJAxKORjdfABIjHwtB8GjsiBhfNQWfEXYZanzlzgfdR258+queUSXVrzgp3fhTJDmbplVcmkkEPzESZEvIq1ZOVEFPLdv/AM7H6RURCgAA"
}
}

trait AbstractStringsDsl extends impl.AbstractStringsAbs
trait AbstractStringsDslStd extends impl.AbstractStringsStd
trait AbstractStringsDslExp extends impl.AbstractStringsExp
