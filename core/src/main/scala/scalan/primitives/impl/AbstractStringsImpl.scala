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

    def convertAString(x : Rep[AString]): Rep[To] = {
      assert(x.selfType1 match { case _: AStringElem[_] => true; case _ => false })
      x.asRep[To]
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

    override def convertAString(x: Rep[AString]) = SString(x.wrappedValueOfBaseType)
    override def getDefaultRep = super[ConcreteElem].getDefaultRep
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
      p.wrappedValueOfBaseType
    override def to(p: Rep[String]) = {
      val wrappedValueOfBaseType = p
      SString(wrappedValueOfBaseType)
    }
    lazy val defaultRepTo: Rep[SString] = SString("")
    lazy val eTo = new SStringElem(this)
  }
  // 4) constructor and deconstructor
  abstract class SStringCompanionAbs extends CompanionBase[SStringCompanionAbs] with SStringCompanion {
    override def toString = "SString"

    def apply(wrappedValueOfBaseType: Rep[String]): Rep[SString] =
      mkSString(wrappedValueOfBaseType)
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
  def mkSString(wrappedValueOfBaseType: Rep[String]): Rep[SString]
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

    override def convertAString(x: Rep[AString]) = CString(x.wrappedValueOfBaseType)
    override def getDefaultRep = super[ConcreteElem].getDefaultRep
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
      p.wrappedValueOfBaseType
    override def to(p: Rep[String]) = {
      val wrappedValueOfBaseType = p
      CString(wrappedValueOfBaseType)
    }
    lazy val defaultRepTo: Rep[CString] = CString("")
    lazy val eTo = new CStringElem(this)
  }
  // 4) constructor and deconstructor
  abstract class CStringCompanionAbs extends CompanionBase[CStringCompanionAbs] with CStringCompanion {
    override def toString = "CString"

    def apply(wrappedValueOfBaseType: Rep[String]): Rep[CString] =
      mkCString(wrappedValueOfBaseType)
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
  def mkCString(wrappedValueOfBaseType: Rep[String]): Rep[CString]
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
  def unmkSString(p: Rep[AString]) = p match {
    case p: SString @unchecked =>
      Some((p.wrappedValueOfBaseType))
    case _ => None
  }

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
  def unmkCString(p: Rep[AString]) = p match {
    case p: CString @unchecked =>
      Some((p.wrappedValueOfBaseType))
    case _ => None
  }
}

// Exp -----------------------------------
trait AbstractStringsExp extends AbstractStringsDsl with scalan.ScalanExp {
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
  def unmkSString(p: Rep[AString]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: SStringElem @unchecked =>
      Some((p.asRep[SString].wrappedValueOfBaseType))
    case _ =>
      None
  }

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
  def unmkCString(p: Rep[AString]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CStringElem @unchecked =>
      Some((p.asRep[CString].wrappedValueOfBaseType))
    case _ =>
      None
  }

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
  val dump = "H4sIAAAAAAAAALVVzW8bRRR/3jhxbEdNGqmVkgupa4KKWjtCqnoIUuW6boVk4ihbEDIV0ng9dqfMzk5mJsHuoX9AuSGuCPXeW4+VuCAkxIETXxIXLnApIFQBPbXqzOyH1y5bwoE9jGZnZ977fbw3e+83mJcCNqWHKGI1HytUc+28IVXVbTFF1PjNoH9A8WU8ePJT7dtfBhdPOLDchYUbSF6WtAvFcNIa8WTu4v02FBHzsFSBkApOtW2GuhdQij1FAlYnvn+gUI/ieptItd2GfC/oj/fhNuTasOIFzBNYYbdJkZRYRuuL2CAiyXvRvo87fJKD1Q2LeorFNYGI0vB1jpVw/x7m7pgFbOwrOBZB63ADS+8pEJ8HQsUpCjrcjaAfv+YZ0guw2r6JDlFdpxjWXSUIG+qTZY6899EQ7+gtZnteA5aYDq6NuX2fa0NJ4n0t0Bs+p3ZlxAFAO/CaBVGb6FNL9KkZfaouFgRRcguZj7siGI0hfHJzACOuQ5z9lxBxBNxi/eqd6967j92y75jDIwOlYBku6EAvZVSDtULr+OXeR/LR1bsXHCh1oURkoyeVQJ5KWx6pVUaMBcpiTgREYqjdqmS5ZbM09J6Zkih6gc8R05EiKZe0T5R4RJnNZm0pcidD+oLiON6aG/Fcwncjg6+tmyaidPfh2rmXf22944AznaKoQ7q68EUcVEGhEZaDldQMxUjd7DwJ41ce/t7/YguuO4lOUdijWaNDzMsfvit/c+aiA4tdW8hXKBp2tVSyRbHfEc2AqS4sBodYhF8Kh4ia2T9aVejjATqgKhIwzXxOM1ewkdlyHBtZtm1552IBymGF7gQMV6/sVv92v/r4nilAAUvhl7AHn5ILT348NlC2NhWc/EAgznH/bUQPcGdwCUlsXLUglxXM6WZO9DmdZSXHu4L4+uo4xOc/f/DWH5/tzFs3VyOKNvjEuXyarQHhVCoKFiYbQlcn3pZCAm7g4+OVR+S9ux8q62JuNH1XdHo3dXNu23NrLzA0vrP+6m45f659/6kDRe1bjygf8erWETvtf+weSOp7MqxrnVbcUKNmOt365H5ZtVPdJu5Ey9TnctyVkbmZ3WRjpfaeSOrMRvwPVWOGjectNeNpO25mcm0ekWtzlmuYKAV/E6Z5F/cwGRBzdR9Jjwj088inTy+1RgnWrRexn6bZyKQ5c9+tz8B6fXpRS7Ec/yvCQ/o/cDxqAB63p4wICKhk9IYbVaZuj9uPP9l59ev7P9teLpka13cLS/7d6R6elmJ1Boj+J6fAK8ib8rfwnwF7su+lIwkAAA=="
}
}

trait AbstractStringsDsl extends impl.AbstractStringsAbs {self: AbstractStringsDsl =>}
trait AbstractStringsDslSeq extends impl.AbstractStringsSeq {self: AbstractStringsDslSeq =>}
trait AbstractStringsDslExp extends impl.AbstractStringsExp {self: AbstractStringsDslExp =>}
