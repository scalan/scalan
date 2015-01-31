package scalan.util
package impl

import scalan._
import scalan.common.Default
import scala.reflect.runtime.universe._
import scalan.common.Default

// Abs -----------------------------------
trait ExceptionsAbs extends Scalan with Exceptions
{ self: ExceptionsDsl =>
  // single proxy for each type family
  implicit def proxySThrowable(p: Rep[SThrowable]): SThrowable =
    proxyOps[SThrowable](p)
  // BaseTypeEx proxy
  implicit def proxyThrowable(p: Rep[Throwable]): SThrowable =
    proxyOps[SThrowable](p.asRep[SThrowable])

  implicit def defaultSThrowableElem: Elem[SThrowable] = element[SThrowableImpl].asElem[SThrowable]
  implicit def ThrowableElement: Elem[Throwable]

  abstract class SThrowableElem[From, To <: SThrowable](iso: Iso[From, To]) extends ViewElem[From, To]()(iso)

  trait SThrowableCompanionElem extends CompanionElem[SThrowableCompanionAbs]
  implicit lazy val SThrowableCompanionElem: SThrowableCompanionElem = new SThrowableCompanionElem {
    lazy val tag = typeTag[SThrowableCompanionAbs]
    protected def getDefaultRep = SThrowable
  }

  abstract class SThrowableCompanionAbs extends CompanionBase[SThrowableCompanionAbs] with SThrowableCompanion {
    override def toString = "SThrowable"
    
    def apply(msg: Rep[String]): Rep[Throwable] =
      newObjEx(classOf[Throwable], List(msg.asRep[Any]))

  }
  def SThrowable: Rep[SThrowableCompanionAbs]
  implicit def proxySThrowableCompanion(p: Rep[SThrowableCompanion]): SThrowableCompanion = {
    proxyOps[SThrowableCompanion](p)
  }

  //default wrapper implementation
    abstract class SThrowableImpl(val wrappedValueOfBaseType: Rep[Throwable]) extends SThrowable {
    
    def getMessage: Rep[String] =
      methodCallEx[String](self,
        this.getClass.getMethod("getMessage"),
        List())

  }
  trait SThrowableImplCompanion
  // elem for concrete class
  class SThrowableImplElem(iso: Iso[SThrowableImplData, SThrowableImpl]) extends SThrowableElem[SThrowableImplData, SThrowableImpl](iso)

  // state representation type
  type SThrowableImplData = Throwable

  // 3) Iso for concrete class
  class SThrowableImplIso
    extends Iso[SThrowableImplData, SThrowableImpl] {
    override def from(p: Rep[SThrowableImpl]) =
      unmkSThrowableImpl(p) match {
        case Some((wrappedValueOfBaseType)) => wrappedValueOfBaseType
        case None => !!!
      }
    override def to(p: Rep[Throwable]) = {
      val wrappedValueOfBaseType = p
      SThrowableImpl(wrappedValueOfBaseType)
    }
    lazy val tag = {
      weakTypeTag[SThrowableImpl]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[SThrowableImpl]](SThrowableImpl(Default.defaultOf[Throwable]))
    lazy val eTo = new SThrowableImplElem(this)
  }
  // 4) constructor and deconstructor
  abstract class SThrowableImplCompanionAbs extends CompanionBase[SThrowableImplCompanionAbs] with SThrowableImplCompanion {
    override def toString = "SThrowableImpl"

    def apply(wrappedValueOfBaseType: Rep[Throwable]): Rep[SThrowableImpl] =
      mkSThrowableImpl(wrappedValueOfBaseType)
    def unapply(p: Rep[SThrowableImpl]) = unmkSThrowableImpl(p)
  }
  def SThrowableImpl: Rep[SThrowableImplCompanionAbs]
  implicit def proxySThrowableImplCompanion(p: Rep[SThrowableImplCompanionAbs]): SThrowableImplCompanionAbs = {
    proxyOps[SThrowableImplCompanionAbs](p)
  }

  class SThrowableImplCompanionElem extends CompanionElem[SThrowableImplCompanionAbs] {
    lazy val tag = typeTag[SThrowableImplCompanionAbs]
    protected def getDefaultRep = SThrowableImpl
  }
  implicit lazy val SThrowableImplCompanionElem: SThrowableImplCompanionElem = new SThrowableImplCompanionElem

  implicit def proxySThrowableImpl(p: Rep[SThrowableImpl]): SThrowableImpl =
    proxyOps[SThrowableImpl](p)

  implicit class ExtendedSThrowableImpl(p: Rep[SThrowableImpl]) {
    def toData: Rep[SThrowableImplData] = isoSThrowableImpl.from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoSThrowableImpl: Iso[SThrowableImplData, SThrowableImpl] =
    new SThrowableImplIso

  // 6) smart constructor and deconstructor
  def mkSThrowableImpl(wrappedValueOfBaseType: Rep[Throwable]): Rep[SThrowableImpl]
  def unmkSThrowableImpl(p: Rep[SThrowableImpl]): Option[(Rep[Throwable])]

  //default wrapper implementation
  
  // elem for concrete class
  class SExceptionElem(iso: Iso[SExceptionData, SException]) extends SThrowableElem[SExceptionData, SException](iso)

  // state representation type
  type SExceptionData = Throwable

  // 3) Iso for concrete class
  class SExceptionIso
    extends Iso[SExceptionData, SException] {
    override def from(p: Rep[SException]) =
      unmkSException(p) match {
        case Some((wrappedValueOfBaseType)) => wrappedValueOfBaseType
        case None => !!!
      }
    override def to(p: Rep[Throwable]) = {
      val wrappedValueOfBaseType = p
      SException(wrappedValueOfBaseType)
    }
    lazy val tag = {
      weakTypeTag[SException]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[SException]](SException(Default.defaultOf[Throwable]))
    lazy val eTo = new SExceptionElem(this)
  }
  // 4) constructor and deconstructor
  abstract class SExceptionCompanionAbs extends CompanionBase[SExceptionCompanionAbs] with SExceptionCompanion {
    override def toString = "SException"

    def apply(wrappedValueOfBaseType: Rep[Throwable]): Rep[SException] =
      mkSException(wrappedValueOfBaseType)
    def unapply(p: Rep[SException]) = unmkSException(p)
  }
  def SException: Rep[SExceptionCompanionAbs]
  implicit def proxySExceptionCompanion(p: Rep[SExceptionCompanionAbs]): SExceptionCompanionAbs = {
    proxyOps[SExceptionCompanionAbs](p)
  }

  class SExceptionCompanionElem extends CompanionElem[SExceptionCompanionAbs] {
    lazy val tag = typeTag[SExceptionCompanionAbs]
    protected def getDefaultRep = SException
  }
  implicit lazy val SExceptionCompanionElem: SExceptionCompanionElem = new SExceptionCompanionElem

  implicit def proxySException(p: Rep[SException]): SException =
    proxyOps[SException](p)

  implicit class ExtendedSException(p: Rep[SException]) {
    def toData: Rep[SExceptionData] = isoSException.from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoSException: Iso[SExceptionData, SException] =
    new SExceptionIso

  // 6) smart constructor and deconstructor
  def mkSException(wrappedValueOfBaseType: Rep[Throwable]): Rep[SException]
  def unmkSException(p: Rep[SException]): Option[(Rep[Throwable])]
}

// Seq -----------------------------------
trait ExceptionsSeq extends ExceptionsAbs with ExceptionsDsl with ScalanSeq
{ self: ExceptionsDslSeq =>
  lazy val SThrowable: Rep[SThrowableCompanionAbs] = new SThrowableCompanionAbs with UserTypeSeq[SThrowableCompanionAbs, SThrowableCompanionAbs] {
    lazy val selfType = element[SThrowableCompanionAbs]
    
    override def apply(msg: Rep[String]): Rep[Throwable] =
      new Throwable(msg)

  }

    // override proxy if we deal with BaseTypeEx
  override def proxyThrowable(p: Rep[Throwable]): SThrowable =
    proxyOpsEx[Throwable,SThrowable, SeqSThrowableImpl](p, bt => SeqSThrowableImpl(bt))

    implicit lazy val ThrowableElement: Elem[Throwable] = new SeqBaseElemEx[Throwable, SThrowable](element[SThrowable])

  case class SeqSThrowableImpl
      (override val wrappedValueOfBaseType: Rep[Throwable])
      
    extends SThrowableImpl(wrappedValueOfBaseType)
       with SeqSThrowable with UserTypeSeq[SThrowable, SThrowableImpl] {
    lazy val selfType = element[SThrowableImpl].asInstanceOf[Elem[SThrowable]]
    
    override def getMessage: Rep[String] =
      wrappedValueOfBaseType.getMessage

  }
  lazy val SThrowableImpl = new SThrowableImplCompanionAbs with UserTypeSeq[SThrowableImplCompanionAbs, SThrowableImplCompanionAbs] {
    lazy val selfType = element[SThrowableImplCompanionAbs]
  }

  def mkSThrowableImpl
      (wrappedValueOfBaseType: Rep[Throwable]) =
      new SeqSThrowableImpl(wrappedValueOfBaseType)
  def unmkSThrowableImpl(p: Rep[SThrowableImpl]) =
    Some((p.wrappedValueOfBaseType))

  case class SeqSException
      (override val wrappedValueOfBaseType: Rep[Throwable])
      
    extends SException(wrappedValueOfBaseType)
       with SeqSThrowable with UserTypeSeq[SThrowable, SException] {
    lazy val selfType = element[SException].asInstanceOf[Elem[SThrowable]]
    
    override def getMessage: Rep[String] =
      wrappedValueOfBaseType.getMessage

  }
  lazy val SException = new SExceptionCompanionAbs with UserTypeSeq[SExceptionCompanionAbs, SExceptionCompanionAbs] {
    lazy val selfType = element[SExceptionCompanionAbs]
  }

  def mkSException
      (wrappedValueOfBaseType: Rep[Throwable]) =
      new SeqSException(wrappedValueOfBaseType)
  def unmkSException(p: Rep[SException]) =
    Some((p.wrappedValueOfBaseType))
}

// Exp -----------------------------------
trait ExceptionsExp extends ExceptionsAbs with ExceptionsDsl with ScalanExp
{ self: ExceptionsDslExp =>
  lazy val SThrowable: Rep[SThrowableCompanionAbs] = new SThrowableCompanionAbs with UserTypeDef[SThrowableCompanionAbs, SThrowableCompanionAbs] {
    lazy val selfType = element[SThrowableCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  implicit lazy val ThrowableElement: Elem[Throwable] = new ExpBaseElemEx[Throwable, SThrowable](element[SThrowable])

  case class ExpSThrowableImpl
      (override val wrappedValueOfBaseType: Rep[Throwable])
      
    extends SThrowableImpl(wrappedValueOfBaseType) with UserTypeDef[SThrowable, SThrowableImpl] {
    lazy val selfType = element[SThrowableImpl].asInstanceOf[Elem[SThrowable]]
    override def mirror(t: Transformer) = ExpSThrowableImpl(t(wrappedValueOfBaseType))
  }

  lazy val SThrowableImpl: Rep[SThrowableImplCompanionAbs] = new SThrowableImplCompanionAbs with UserTypeDef[SThrowableImplCompanionAbs, SThrowableImplCompanionAbs] {
    lazy val selfType = element[SThrowableImplCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object SThrowableImplMethods {

  }



  def mkSThrowableImpl
    (wrappedValueOfBaseType: Rep[Throwable]) =
    new ExpSThrowableImpl(wrappedValueOfBaseType)
  def unmkSThrowableImpl(p: Rep[SThrowableImpl]) =
    Some((p.wrappedValueOfBaseType))

  case class ExpSException
      (override val wrappedValueOfBaseType: Rep[Throwable])
      
    extends SException(wrappedValueOfBaseType) with UserTypeDef[SThrowable, SException] {
    lazy val selfType = element[SException].asInstanceOf[Elem[SThrowable]]
    override def mirror(t: Transformer) = ExpSException(t(wrappedValueOfBaseType))
  }

  lazy val SException: Rep[SExceptionCompanionAbs] = new SExceptionCompanionAbs with UserTypeDef[SExceptionCompanionAbs, SExceptionCompanionAbs] {
    lazy val selfType = element[SExceptionCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object SExceptionMethods {
    object getMessage {
      def unapply(d: Def[_]): Option[Rep[SException]] = d match {
        case MethodCall(receiver, method, _) if receiver.elem.isInstanceOf[SExceptionElem] && method.getName == "getMessage" =>
          Some(receiver).asInstanceOf[Option[Rep[SException]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[SException]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object SExceptionCompanionMethods {

  }

  def mkSException
    (wrappedValueOfBaseType: Rep[Throwable]) =
    new ExpSException(wrappedValueOfBaseType)
  def unmkSException(p: Rep[SException]) =
    Some((p.wrappedValueOfBaseType))

  object SThrowableMethods {
    object getMessage {
      def unapply(d: Def[_]): Option[Rep[SThrowable]] = d match {
        case MethodCall(receiver, method, _) if receiver.elem.isInstanceOf[SThrowableElem[_, _]] && method.getName == "getMessage" =>
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
        case MethodCall(receiver, method, Seq(msg, _*)) if receiver.elem.isInstanceOf[SThrowableCompanionElem] && method.getName == "apply" =>
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
