package scalan.util
package impl

import scalan._
import scalan.common.Default
import scala.reflect.runtime.universe._
import scalan.common.Default

trait ExceptionsAbs extends Scalan with Exceptions
{ self: ExceptionsDsl =>
  // single proxy for each type family
  implicit def proxyThrowable(p: Rep[Throwable]): SThrowable =
    proxyOps[SThrowable](p.asRep[SThrowable])
  implicit lazy val ThrowableElement: Elem[Throwable] = new BaseElemEx[Throwable, SThrowable](element[SThrowable])
  implicit lazy val DefaultOfThrowable: Default[Throwable] = SThrowable.defaultVal

  abstract class SThrowableElem[From, To <: SThrowable](iso: Iso[From, To]) extends ViewElem[From, To]()(iso)

  trait SThrowableCompanionElem extends CompanionElem[SThrowableCompanionAbs]
  implicit lazy val SThrowableCompanionElem: SThrowableCompanionElem = new SThrowableCompanionElem {
    lazy val tag = typeTag[SThrowableCompanionAbs]
    lazy val defaultRep = Default.defaultVal(SThrowable)
  }

  abstract class SThrowableCompanionAbs extends CompanionBase[SThrowableCompanionAbs] with SThrowableCompanion {
    override def toString = "SThrowable"
  }
  def SThrowable: Rep[SThrowableCompanionAbs]
  implicit def proxySThrowableCompanion(p: Rep[SThrowableCompanion]): SThrowableCompanion = {
    proxyOps[SThrowableCompanion](p)
  }

  // elem for concrete class
  class SExceptionElem(iso: Iso[SExceptionData, SException]) extends SThrowableElem[SExceptionData, SException](iso)

  // state representation type
  type SExceptionData = Throwable

  // 3) Iso for concrete class
  class SExceptionIso
    extends Iso[SExceptionData, SException] {
    override def from(p: Rep[SException]) =
      unmkSException(p) match {
        case Some((value)) => value
        case None => !!!
      }
    override def to(p: Rep[Throwable]) = {
      val value = p
      SException(value)
    }
    lazy val tag = {
      weakTypeTag[SException]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[SException]](SException(element[Throwable].defaultRepValue))
    lazy val eTo = new SExceptionElem(this)
  }
  // 4) constructor and deconstructor
  abstract class SExceptionCompanionAbs extends CompanionBase[SExceptionCompanionAbs] with SExceptionCompanion {
    override def toString = "SException"

    def apply(value: Rep[Throwable]): Rep[SException] =
      mkSException(value)
    def unapply(p: Rep[SException]) = unmkSException(p)
  }
  def SException: Rep[SExceptionCompanionAbs]
  implicit def proxySExceptionCompanion(p: Rep[SExceptionCompanionAbs]): SExceptionCompanionAbs = {
    proxyOps[SExceptionCompanionAbs](p)
  }

  class SExceptionCompanionElem extends CompanionElem[SExceptionCompanionAbs] {
    lazy val tag = typeTag[SExceptionCompanionAbs]
    lazy val defaultRep = Default.defaultVal(SException)
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
  def mkSException(value: Rep[Throwable]): Rep[SException]
  def unmkSException(p: Rep[SException]): Option[(Rep[Throwable])]
}

trait ExceptionsSeq extends ExceptionsAbs with ExceptionsDsl with ScalanSeq {
  lazy val SThrowable: Rep[SThrowableCompanionAbs] = new SThrowableCompanionAbs with UserTypeSeq[SThrowableCompanionAbs, SThrowableCompanionAbs] {
    lazy val selfType = element[SThrowableCompanionAbs]
  }

  // override proxy if we deal with BaseTypeEx
  override def proxyThrowable(p: Rep[Throwable]): SThrowable =
    proxyOpsEx[Throwable,SThrowable](p)

  case class SeqSException
      (override val value: Rep[Throwable])
      
    extends SException(value) with UserTypeSeq[SThrowable, SException] {
    lazy val selfType = element[SException].asInstanceOf[Elem[SThrowable]]
  }
  lazy val SException = new SExceptionCompanionAbs with UserTypeSeq[SExceptionCompanionAbs, SExceptionCompanionAbs] {
    lazy val selfType = element[SExceptionCompanionAbs]
  }

  def mkSException
      (value: Rep[Throwable]) =
      new SeqSException(value)
  def unmkSException(p: Rep[SException]) =
    Some((p.value))
}

trait ExceptionsExp extends ExceptionsAbs with ExceptionsDsl with ScalanExp {
  lazy val SThrowable: Rep[SThrowableCompanionAbs] = new SThrowableCompanionAbs with UserTypeDef[SThrowableCompanionAbs, SThrowableCompanionAbs] {
    lazy val selfType = element[SThrowableCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  case class ExpSException
      (override val value: Rep[Throwable])
      
    extends SException(value) with UserTypeDef[SThrowable, SException] {
    lazy val selfType = element[SException].asInstanceOf[Elem[SThrowable]]
    override def mirror(t: Transformer) = ExpSException(t(value))
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
    (value: Rep[Throwable]) =
    new ExpSException(value)
  def unmkSException(p: Rep[SException]) =
    Some((p.value))

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
    object defaultVal {
      def unapply(d: Def[_]): Option[Unit] = d match {
        case MethodCall(receiver, method, _) if receiver.elem.isInstanceOf[SThrowableCompanionElem] && method.getName == "defaultVal" =>
          Some(()).asInstanceOf[Option[Unit]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Unit] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}
