package scalan.util

import scalan._
import scalan.common.Default
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait ExceptionsAbs extends Exceptions with scalan.Scalan {
  self: ExceptionsDsl =>

  // single proxy for each type family
  implicit def proxySThrowable(p: Rep[SThrowable]): SThrowable = {
    proxyOps[SThrowable](p)(scala.reflect.classTag[SThrowable])
  }

  // TypeWrapper proxy
  //implicit def proxyThrowable(p: Rep[Throwable]): SThrowable =
  //  proxyOps[SThrowable](p.asRep[SThrowable])

  implicit def unwrapValueOfSThrowable(w: Rep[SThrowable]): Rep[Throwable] = w.wrappedValueOfBaseType

  implicit def throwableElement: Elem[Throwable]

  // familyElem
  abstract class SThrowableElem[To <: SThrowable]
    extends WrapperElem[Throwable, To] {
    lazy val parent: Option[Elem[_]] = None
    lazy val entityDef: STraitOrClassDef = {
      val module = getModules("Exceptions")
      module.entities.find(_.name == "SThrowable").get
    }
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map()
    }
    override def isEntityType = true
    override lazy val tag = {
      weakTypeTag[SThrowable].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Reifiable[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[SThrowable] => convertSThrowable(x) }
      tryConvert(element[SThrowable], this, x, conv)
    }

    def convertSThrowable(x : Rep[SThrowable]): Rep[To] = {
      assert(x.selfType1 match { case _: SThrowableElem[_] => true; case _ => false })
      x.asRep[To]
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def sThrowableElement: Elem[SThrowable] =
    elemCache.getOrElseUpdate(
      (classOf[SThrowableElem[SThrowable]], Nil),
      new SThrowableElem[SThrowable] {
        lazy val eTo = element[SThrowableImpl]
      }).asInstanceOf[Elem[SThrowable]]

  implicit case object SThrowableCompanionElem extends CompanionElem[SThrowableCompanionAbs] {
    lazy val tag = weakTypeTag[SThrowableCompanionAbs]
    protected def getDefaultRep = SThrowable
  }

  abstract class SThrowableCompanionAbs extends CompanionBase[SThrowableCompanionAbs] with SThrowableCompanion {
    override def toString = "SThrowable"
  }
  def SThrowable: Rep[SThrowableCompanionAbs]
  implicit def proxySThrowableCompanion(p: Rep[SThrowableCompanion]): SThrowableCompanion =
    proxyOps[SThrowableCompanion](p)

  // default wrapper implementation
  abstract class SThrowableImpl(val wrappedValueOfBaseType: Rep[Throwable]) extends SThrowable {
    def getMessage: Rep[String] =
      methodCallEx[String](self,
        this.getClass.getMethod("getMessage"),
        List())

    def initCause(cause: Rep[SThrowable]): Rep[SThrowable] =
      methodCallEx[SThrowable](self,
        this.getClass.getMethod("initCause", classOf[AnyRef]),
        List(cause))
  }
  trait SThrowableImplCompanion
  // elem for concrete class
  class SThrowableImplElem(val iso: Iso[SThrowableImplData, SThrowableImpl])
    extends SThrowableElem[SThrowableImpl]
    with ConcreteElem[SThrowableImplData, SThrowableImpl] {
    override lazy val parent: Option[Elem[_]] = Some(sThrowableElement)
    override lazy val entityDef = {
      val module = getModules("Exceptions")
      module.concreteSClasses.find(_.name == "SThrowableImpl").get
    }
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map()
    }
    lazy val eTo = this
    override def convertSThrowable(x: Rep[SThrowable]) = SThrowableImpl(x.wrappedValueOfBaseType)
    override def getDefaultRep = super[ConcreteElem].getDefaultRep
    override lazy val tag = {
      weakTypeTag[SThrowableImpl]
    }
  }

  // state representation type
  type SThrowableImplData = Throwable

  // 3) Iso for concrete class
  class SThrowableImplIso
    extends Iso[SThrowableImplData, SThrowableImpl] {
    override def from(p: Rep[SThrowableImpl]) =
      p.wrappedValueOfBaseType
    override def to(p: Rep[Throwable]) = {
      val wrappedValueOfBaseType = p
      SThrowableImpl(wrappedValueOfBaseType)
    }
    lazy val defaultRepTo: Rep[SThrowableImpl] = SThrowableImpl(DefaultOfThrowable.value)
    lazy val eTo = new SThrowableImplElem(this)
  }
  // 4) constructor and deconstructor
  abstract class SThrowableImplCompanionAbs extends CompanionBase[SThrowableImplCompanionAbs] {
    override def toString = "SThrowableImpl"

    def apply(wrappedValueOfBaseType: Rep[Throwable]): Rep[SThrowableImpl] =
      mkSThrowableImpl(wrappedValueOfBaseType)
  }
  object SThrowableImplMatcher {
    def unapply(p: Rep[SThrowable]) = unmkSThrowableImpl(p)
  }
  def SThrowableImpl: Rep[SThrowableImplCompanionAbs]
  implicit def proxySThrowableImplCompanion(p: Rep[SThrowableImplCompanionAbs]): SThrowableImplCompanionAbs = {
    proxyOps[SThrowableImplCompanionAbs](p)
  }

  implicit case object SThrowableImplCompanionElem extends CompanionElem[SThrowableImplCompanionAbs] {
    lazy val tag = weakTypeTag[SThrowableImplCompanionAbs]
    protected def getDefaultRep = SThrowableImpl
  }

  implicit def proxySThrowableImpl(p: Rep[SThrowableImpl]): SThrowableImpl =
    proxyOps[SThrowableImpl](p)

  implicit class ExtendedSThrowableImpl(p: Rep[SThrowableImpl]) {
    def toData: Rep[SThrowableImplData] = isoSThrowableImpl.from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoSThrowableImpl: Iso[SThrowableImplData, SThrowableImpl] =
    cachedIso[SThrowableImplIso]()

  // 6) smart constructor and deconstructor
  def mkSThrowableImpl(wrappedValueOfBaseType: Rep[Throwable]): Rep[SThrowableImpl]
  def unmkSThrowableImpl(p: Rep[SThrowable]): Option[(Rep[Throwable])]

  // elem for concrete class
  class SExceptionElem(val iso: Iso[SExceptionData, SException])
    extends SThrowableElem[SException]
    with ConcreteElem[SExceptionData, SException] {
    override lazy val parent: Option[Elem[_]] = Some(sThrowableElement)
    override lazy val entityDef = {
      val module = getModules("Exceptions")
      module.concreteSClasses.find(_.name == "SException").get
    }
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map()
    }
    lazy val eTo = this
    override def convertSThrowable(x: Rep[SThrowable]) = SException(x.wrappedValueOfBaseType)
    override def getDefaultRep = super[ConcreteElem].getDefaultRep
    override lazy val tag = {
      weakTypeTag[SException]
    }
  }

  // state representation type
  type SExceptionData = Throwable

  // 3) Iso for concrete class
  class SExceptionIso
    extends Iso[SExceptionData, SException] {
    override def from(p: Rep[SException]) =
      p.wrappedValueOfBaseType
    override def to(p: Rep[Throwable]) = {
      val wrappedValueOfBaseType = p
      SException(wrappedValueOfBaseType)
    }
    lazy val defaultRepTo: Rep[SException] = SException(DefaultOfThrowable.value)
    lazy val eTo = new SExceptionElem(this)
  }
  // 4) constructor and deconstructor
  abstract class SExceptionCompanionAbs extends CompanionBase[SExceptionCompanionAbs] with SExceptionCompanion {
    override def toString = "SException"

    def apply(wrappedValueOfBaseType: Rep[Throwable]): Rep[SException] =
      mkSException(wrappedValueOfBaseType)
  }
  object SExceptionMatcher {
    def unapply(p: Rep[SThrowable]) = unmkSException(p)
  }
  def SException: Rep[SExceptionCompanionAbs]
  implicit def proxySExceptionCompanion(p: Rep[SExceptionCompanionAbs]): SExceptionCompanionAbs = {
    proxyOps[SExceptionCompanionAbs](p)
  }

  implicit case object SExceptionCompanionElem extends CompanionElem[SExceptionCompanionAbs] {
    lazy val tag = weakTypeTag[SExceptionCompanionAbs]
    protected def getDefaultRep = SException
  }

  implicit def proxySException(p: Rep[SException]): SException =
    proxyOps[SException](p)

  implicit class ExtendedSException(p: Rep[SException]) {
    def toData: Rep[SExceptionData] = isoSException.from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoSException: Iso[SExceptionData, SException] =
    cachedIso[SExceptionIso]()

  // 6) smart constructor and deconstructor
  def mkSException(wrappedValueOfBaseType: Rep[Throwable]): Rep[SException]
  def unmkSException(p: Rep[SThrowable]): Option[(Rep[Throwable])]

  registerModule(scalan.meta.ScalanCodegen.loadModule(Exceptions_Module.dump))
}

// Seq -----------------------------------
trait ExceptionsSeq extends ExceptionsDsl with scalan.ScalanSeq {
  self: ExceptionsDslSeq =>
  lazy val SThrowable: Rep[SThrowableCompanionAbs] = new SThrowableCompanionAbs with UserTypeSeq[SThrowableCompanionAbs] {
    lazy val selfType = element[SThrowableCompanionAbs]

    override def apply(msg: Rep[String]): Rep[SThrowable] =
      SThrowableImpl(new Throwable(msg))
  }

    // override proxy if we deal with TypeWrapper
  //override def proxyThrowable(p: Rep[Throwable]): SThrowable =
  //  proxyOpsEx[Throwable,SThrowable, SeqSThrowableImpl](p, bt => SeqSThrowableImpl(bt))

  implicit lazy val throwableElement: Elem[Throwable] =
    new SeqBaseElemEx[Throwable, SThrowable](element[SThrowable])(weakTypeTag[Throwable], DefaultOfThrowable)

  case class SeqSThrowableImpl
      (override val wrappedValueOfBaseType: Rep[Throwable])

    extends SThrowableImpl(wrappedValueOfBaseType)
       with SeqSThrowable with UserTypeSeq[SThrowableImpl] {
    lazy val selfType = element[SThrowableImpl]
    override def getMessage: Rep[String] =
      wrappedValueOfBaseType.getMessage

    override def initCause(cause: Rep[SThrowable]): Rep[SThrowable] =
      SThrowableImpl(wrappedValueOfBaseType.initCause(cause))
  }
  lazy val SThrowableImpl = new SThrowableImplCompanionAbs with UserTypeSeq[SThrowableImplCompanionAbs] {
    lazy val selfType = element[SThrowableImplCompanionAbs]
  }

  def mkSThrowableImpl
      (wrappedValueOfBaseType: Rep[Throwable]): Rep[SThrowableImpl] =
      new SeqSThrowableImpl(wrappedValueOfBaseType)
  def unmkSThrowableImpl(p: Rep[SThrowable]) = p match {
    case p: SThrowableImpl @unchecked =>
      Some((p.wrappedValueOfBaseType))
    case _ => None
  }

  case class SeqSException
      (override val wrappedValueOfBaseType: Rep[Throwable])

    extends SException(wrappedValueOfBaseType)
       with SeqSThrowable with UserTypeSeq[SException] {
    lazy val selfType = element[SException]
    override def getMessage: Rep[String] =
      wrappedValueOfBaseType.getMessage

    override def initCause(cause: Rep[SThrowable]): Rep[SThrowable] =
      SThrowableImpl(wrappedValueOfBaseType.initCause(cause))
  }
  lazy val SException = new SExceptionCompanionAbs with UserTypeSeq[SExceptionCompanionAbs] {
    lazy val selfType = element[SExceptionCompanionAbs]
  }

  def mkSException
      (wrappedValueOfBaseType: Rep[Throwable]): Rep[SException] =
      new SeqSException(wrappedValueOfBaseType)
  def unmkSException(p: Rep[SThrowable]) = p match {
    case p: SException @unchecked =>
      Some((p.wrappedValueOfBaseType))
    case _ => None
  }

  implicit def wrapThrowableToSThrowable(v: Throwable): SThrowable = SThrowableImpl(v)
}

// Exp -----------------------------------
trait ExceptionsExp extends ExceptionsDsl with scalan.ScalanExp {
  self: ExceptionsDslExp =>
  lazy val SThrowable: Rep[SThrowableCompanionAbs] = new SThrowableCompanionAbs with UserTypeDef[SThrowableCompanionAbs] {
    lazy val selfType = element[SThrowableCompanionAbs]
    override def mirror(t: Transformer) = this

    def apply(msg: Rep[String]): Rep[SThrowable] =
      newObjEx(classOf[SThrowable], List(msg))
  }

  implicit lazy val throwableElement: Elem[Throwable] =
    new ExpBaseElemEx[Throwable, SThrowable](element[SThrowable])(weakTypeTag[Throwable], DefaultOfThrowable)

  case class ExpSThrowableImpl
      (override val wrappedValueOfBaseType: Rep[Throwable])

    extends SThrowableImpl(wrappedValueOfBaseType) with UserTypeDef[SThrowableImpl] {
    lazy val selfType = element[SThrowableImpl]
    override def mirror(t: Transformer) = ExpSThrowableImpl(t(wrappedValueOfBaseType))
  }

  lazy val SThrowableImpl: Rep[SThrowableImplCompanionAbs] = new SThrowableImplCompanionAbs with UserTypeDef[SThrowableImplCompanionAbs] {
    lazy val selfType = element[SThrowableImplCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object SThrowableImplMethods {
  }

  def mkSThrowableImpl
    (wrappedValueOfBaseType: Rep[Throwable]): Rep[SThrowableImpl] =
    new ExpSThrowableImpl(wrappedValueOfBaseType)
  def unmkSThrowableImpl(p: Rep[SThrowable]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: SThrowableImplElem @unchecked =>
      Some((p.asRep[SThrowableImpl].wrappedValueOfBaseType))
    case _ =>
      None
  }

  case class ExpSException
      (override val wrappedValueOfBaseType: Rep[Throwable])

    extends SException(wrappedValueOfBaseType) with UserTypeDef[SException] {
    lazy val selfType = element[SException]
    override def mirror(t: Transformer) = ExpSException(t(wrappedValueOfBaseType))
  }

  lazy val SException: Rep[SExceptionCompanionAbs] = new SExceptionCompanionAbs with UserTypeDef[SExceptionCompanionAbs] {
    lazy val selfType = element[SExceptionCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object SExceptionMethods {
    object getMessage {
      def unapply(d: Def[_]): Option[Rep[SException]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SExceptionElem] && method.getName == "getMessage" =>
          Some(receiver).asInstanceOf[Option[Rep[SException]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[SException]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object initCause {
      def unapply(d: Def[_]): Option[(Rep[SException], Rep[SThrowable])] = d match {
        case MethodCall(receiver, method, Seq(cause, _*), _) if receiver.elem.isInstanceOf[SExceptionElem] && method.getName == "initCause" =>
          Some((receiver, cause)).asInstanceOf[Option[(Rep[SException], Rep[SThrowable])]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SException], Rep[SThrowable])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object SExceptionCompanionMethods {
  }

  def mkSException
    (wrappedValueOfBaseType: Rep[Throwable]): Rep[SException] =
    new ExpSException(wrappedValueOfBaseType)
  def unmkSException(p: Rep[SThrowable]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: SExceptionElem @unchecked =>
      Some((p.asRep[SException].wrappedValueOfBaseType))
    case _ =>
      None
  }

  object SThrowableMethods {
    object getMessage {
      def unapply(d: Def[_]): Option[Rep[SThrowable]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SThrowableElem[_]] && method.getName == "getMessage" =>
          Some(receiver).asInstanceOf[Option[Rep[SThrowable]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[SThrowable]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object initCause {
      def unapply(d: Def[_]): Option[(Rep[SThrowable], Rep[SThrowable])] = d match {
        case MethodCall(receiver, method, Seq(cause, _*), _) if receiver.elem.isInstanceOf[SThrowableElem[_]] && method.getName == "initCause" =>
          Some((receiver, cause)).asInstanceOf[Option[(Rep[SThrowable], Rep[SThrowable])]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SThrowable], Rep[SThrowable])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object wrappedValueOfBaseType {
      def unapply(d: Def[_]): Option[Rep[SThrowable]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SThrowableElem[_]] && method.getName == "wrappedValueOfBaseType" =>
          Some(receiver).asInstanceOf[Option[Rep[SThrowable]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[SThrowable]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object SThrowableCompanionMethods {
    object apply {
      def unapply(d: Def[_]): Option[Rep[String]] = d match {
        case MethodCall(receiver, method, Seq(msg, _*), _) if receiver.elem == SThrowableCompanionElem && method.getName == "apply" =>
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

object Exceptions_Module {
  val packageName = "scalan.util"
  val name = "Exceptions"
  val dump = "H4sIAAAAAAAAALVVz28bRRR+3qRx/IOmLSpScmmIDAUEdpRLkXJAIXERkhtH3ahUpkIar8fOltndycw4XXPogRtwqyr1VEHvufE3ICEOnBAgceZU4FABlZCo+mb2p6NsGw74MNoZv33ve9/3vdnD3+GUFPCqdAgjftOjijRt87whVcNu+8pVkyvBYMzoFh2Ken1+cu+fzyxY6MHcHpFbkvWgEj20Q54+23S/AxXiO1SqQEgFL3dMhZYTMEYd5QZ+y/W8sSJ9RlsdV6r1Dsz2g8FkH25DqQNnnMB3BFXU3mRESirj83mqEbnpvmL2ky7Pavgt3UUr18WuIK5C+FjjTBR/lXJ74gf+xFNwOobW5RoWxpRdjwdCJSXKmG4vGCTbWZ/gAZzr3CQHpIUlRi1bCdcf4Zs1TpyPyYhuY4gOn0XAkrLh7oSb/UwHqpLuI0Hve5yZk5ADACqwZkA0M36aKT9NzU/DpsIlzP2E6D93RBBOIPqVZgBCjinefE6KJANt+4PG5zecDx/bNc/SL4caStl0OIeJLhS4wUiBPH579Y589N6DSxZUe1B15UZfKkEclZc8ZqtGfD9QBnNKIBEjVGulSC1TZQNjjlii4gQeJz5miqmso07MdVylg/VZPVangPqy4jQJLYW8lPa7XNCv8c0mYWzn4eJbr/zWvm6BNV2igiltNL5Ikiqo2rt7IrilWTes6qUSE1xcKm364sM/Bt+swg0rpSrOfDJ1MMUp+fOPtR9ef8eC+Z7x8mVGRj1kS7YZ9bpiM/BVD+aDAyqif8oHhOmnY9UqD+iQjJmKOcw3P4PNK1gunDpONTPrxuGlhIBaZNLtwKeNyzuNv+3v7h5qDwqoR/9EY/jEvfTvL6eHythTwUu3BOGcDq4RNqbd4btEUi2sAbmgYAbnOeYnPqkUiRBLoZclE3zO7PEayHRLhnMpG4taaTr/s1Q2WXPh59P+TdL/3I1elo9vAwmtRrTZgUfPrjxyP3rwhTLeKYXTl1S3fxNvhXXz3uIzbJRcln/1Vq0/F3/60oIKuqXvKo/wxuoJR/x/HFtImciWJbTIiziHDjXm2cxXzImYSF3NQrOIiNUc72swLXlNY/rAKBf1PqeXC893RM5yEY7Wsfnr7TAFvnpC/Y8wkCI4hgGu17NH+9Xr29OHmqCUH7yoa7FRxsplMXwBKwXusWPt0EC3H9/ffuP7r381l2ZVuwBn3k8/q5nk4ZHZfSGrjl/KHFgFs9obKYjXCkHs6xGmHpYzvvwUrh1urX3lmcFYoGHkvyu573po6Ck/BeGZsjgPCQAA"
}
}

trait ExceptionsDsl extends impl.ExceptionsAbs {self: ExceptionsDsl =>}
trait ExceptionsDslExp extends impl.ExceptionsExp {self: ExceptionsDslExp =>}
