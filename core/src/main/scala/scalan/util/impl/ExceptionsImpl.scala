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

  implicit def unwrapValueOfSThrowable(w: Rep[SThrowable]): Rep[Throwable] = w.wrappedValue

  implicit lazy val throwableElement: Elem[Throwable] =
    element[SThrowable].asInstanceOf[WrapperElem[_, _]].baseElem.asInstanceOf[Elem[Throwable]]

  // familyElem
  class SThrowableElem[To <: SThrowable]
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

    def convertSThrowable(x: Rep[SThrowable]): Rep[To] = {
      x.selfType1 match {
        case _: SThrowableElem[_] => x.asRep[To]
        case e => !!!(s"Expected $x to have SThrowableElem[_], but got $e")
      }
    }
    lazy val baseElem = {
      new BaseTypeElem[Throwable, SThrowable](this.asInstanceOf[Element[SThrowable]])(weakTypeTag[Throwable], DefaultOfThrowable)
    }
    lazy val eTo: Elem[_] = new SThrowableImplElem(isoSThrowableImpl)
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def sThrowableElement: Elem[SThrowable] =
    elemCache.getOrElseUpdate(
      (classOf[SThrowableElem[SThrowable]], Nil),
      new SThrowableElem[SThrowable]).asInstanceOf[Elem[SThrowable]]

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
  abstract class SThrowableImpl(val wrappedValue: Rep[Throwable]) extends SThrowable {
    def getMessage: Rep[String] =
      methodCallEx[String](self,
        this.getClass.getMethod("getMessage"),
        List())

    def initCause(cause: Rep[SThrowable]): Rep[SThrowable] =
      methodCallEx[SThrowable](self,
        this.getClass.getMethod("initCause", classOf[AnyRef]),
        List(cause.asInstanceOf[AnyRef]))
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
    override lazy val eTo: Elem[_] = this
    override def convertSThrowable(x: Rep[SThrowable]) = SThrowableImpl(x.wrappedValue)
    override def getDefaultRep = SThrowableImpl(DefaultOfThrowable.value)
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
      p.wrappedValue
    override def to(p: Rep[Throwable]) = {
      val wrappedValue = p
      SThrowableImpl(wrappedValue)
    }
    lazy val eTo = new SThrowableImplElem(this)
  }
  // 4) constructor and deconstructor
  abstract class SThrowableImplCompanionAbs extends CompanionBase[SThrowableImplCompanionAbs] {
    override def toString = "SThrowableImpl"

    def apply(wrappedValue: Rep[Throwable]): Rep[SThrowableImpl] =
      mkSThrowableImpl(wrappedValue)
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
  def mkSThrowableImpl(wrappedValue: Rep[Throwable]): Rep[SThrowableImpl]
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
    override lazy val eTo: Elem[_] = this
    override def convertSThrowable(x: Rep[SThrowable]) = SException(x.wrappedValue)
    override def getDefaultRep = SException(DefaultOfThrowable.value)
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
      p.wrappedValue
    override def to(p: Rep[Throwable]) = {
      val wrappedValue = p
      SException(wrappedValue)
    }
    lazy val eTo = new SExceptionElem(this)
  }
  // 4) constructor and deconstructor
  abstract class SExceptionCompanionAbs extends CompanionBase[SExceptionCompanionAbs] with SExceptionCompanion {
    override def toString = "SException"

    def apply(wrappedValue: Rep[Throwable]): Rep[SException] =
      mkSException(wrappedValue)
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
  def mkSException(wrappedValue: Rep[Throwable]): Rep[SException]
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
  //  proxyOpsEx[Throwable, SThrowable, SeqSThrowableImpl](p, bt => SeqSThrowableImpl(bt))

  case class SeqSThrowableImpl
      (override val wrappedValue: Rep[Throwable])

    extends SThrowableImpl(wrappedValue)
       with SeqSThrowable with UserTypeSeq[SThrowableImpl] {
    lazy val selfType = element[SThrowableImpl]
    override def getMessage: Rep[String] =
      wrappedValue.getMessage

    override def initCause(cause: Rep[SThrowable]): Rep[SThrowable] =
      SThrowableImpl(wrappedValue.initCause(cause))
  }
  lazy val SThrowableImpl = new SThrowableImplCompanionAbs with UserTypeSeq[SThrowableImplCompanionAbs] {
    lazy val selfType = element[SThrowableImplCompanionAbs]
  }

  def mkSThrowableImpl
      (wrappedValue: Rep[Throwable]): Rep[SThrowableImpl] =
      new SeqSThrowableImpl(wrappedValue)
  def unmkSThrowableImpl(p: Rep[SThrowable]) = p match {
    case p: SThrowableImpl @unchecked =>
      Some((p.wrappedValue))
    case _ => None
  }

  case class SeqSException
      (override val wrappedValue: Rep[Throwable])

    extends SException(wrappedValue)
       with SeqSThrowable with UserTypeSeq[SException] {
    lazy val selfType = element[SException]
    override def getMessage: Rep[String] =
      wrappedValue.getMessage

    override def initCause(cause: Rep[SThrowable]): Rep[SThrowable] =
      SThrowableImpl(wrappedValue.initCause(cause))
  }
  lazy val SException = new SExceptionCompanionAbs with UserTypeSeq[SExceptionCompanionAbs] {
    lazy val selfType = element[SExceptionCompanionAbs]
  }

  def mkSException
      (wrappedValue: Rep[Throwable]): Rep[SException] =
      new SeqSException(wrappedValue)
  def unmkSException(p: Rep[SThrowable]) = p match {
    case p: SException @unchecked =>
      Some((p.wrappedValue))
    case _ => None
  }

  implicit def wrapThrowableToSThrowable(v: Throwable): SThrowable = SThrowableImpl(v)
}

// Exp -----------------------------------
trait ExceptionsExp extends ExceptionsDsl with scalan.ScalanExp {
  self: ExceptionsDslExp =>
  lazy val SThrowable: Rep[SThrowableCompanionAbs] = new SThrowableCompanionAbs with UserTypeDef[SThrowableCompanionAbs] {
    lazy val selfType = element[SThrowableCompanionAbs]

    def apply(msg: Rep[String]): Rep[SThrowable] =
      newObjEx(classOf[SThrowable], List(msg.asRep[Any]))
  }

  case class ExpSThrowableImpl
      (override val wrappedValue: Rep[Throwable])

    extends SThrowableImpl(wrappedValue) with UserTypeDef[SThrowableImpl] {
    lazy val selfType = element[SThrowableImpl]
  }

  lazy val SThrowableImpl: Rep[SThrowableImplCompanionAbs] = new SThrowableImplCompanionAbs with UserTypeDef[SThrowableImplCompanionAbs] {
    lazy val selfType = element[SThrowableImplCompanionAbs]
  }

  object SThrowableImplMethods {
  }

  def mkSThrowableImpl
    (wrappedValue: Rep[Throwable]): Rep[SThrowableImpl] =
    new ExpSThrowableImpl(wrappedValue)
  def unmkSThrowableImpl(p: Rep[SThrowable]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: SThrowableImplElem @unchecked =>
      Some((p.asRep[SThrowableImpl].wrappedValue))
    case _ =>
      None
  }

  case class ExpSException
      (override val wrappedValue: Rep[Throwable])

    extends SException(wrappedValue) with UserTypeDef[SException] {
    lazy val selfType = element[SException]
  }

  lazy val SException: Rep[SExceptionCompanionAbs] = new SExceptionCompanionAbs with UserTypeDef[SExceptionCompanionAbs] {
    lazy val selfType = element[SExceptionCompanionAbs]
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
    (wrappedValue: Rep[Throwable]): Rep[SException] =
    new ExpSException(wrappedValue)
  def unmkSException(p: Rep[SThrowable]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: SExceptionElem @unchecked =>
      Some((p.asRep[SException].wrappedValue))
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

    object wrappedValue {
      def unapply(d: Def[_]): Option[Rep[SThrowable]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SThrowableElem[_]] && method.getName == "wrappedValue" =>
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
  val dump = "H4sIAAAAAAAAALVVvW8cRRR/t3Z8vg/iJAgku4mxjk/BneUmSC6QsS8I6fyhrBWiI0Ka2xufN8zOjmfmnD2KFCmhQ7QI0ruj4R9AQhRUCJCoqQIUEZAKxJvZz7O8iRuuGO3MvX3v936/35s9+R0uKAkvKY8wwtsB1aTt2ucNpVtul2tfT7bD4ZjRLXpw//mvvW3+tnJgoQ9zh0RtKdaHWvzQjUT27NKjHtQI96jSoVQaXujZCh0vZIx62g95xw+CsSYDRjs9X+n1HswOwuHkCO5BpQeXvJB7kmrqbjKiFFXJ+Tw1iPxsX7P7ya7Ia/CO6aJT6GJfEl8jfKxxKY6/QYU74SGfBBouJtB2hYGFMVU/EKHUaYkqpjsMh+l2lhM8gCu9O+SYdLDEqONq6fMRvtkQxPuQjOgOhpjwWQSsKDvYnwi7n+lBXdEjJOjdQDB7EgkAQAXWLIh2zk8746dt+Gm5VPqE+R8R8+eeDKMJxL/KDEAkMMXrT0mRZqBdPmx9fNt7/7HbCBzzcmSgVG2Hc5joaokbrBTI47c3PlWP3nlwzYF6H+q+2hgoLYmni5InbDUI56G2mDMCiRyhWitlatkqGxhzyhI1LwwE4ZgpobKJOjHf87UJNmfNRJ0S6qta0DS0EolK1u9ySb/WN5uEsb2Hi2+8+Fv3lgPOdIkapnTR+DJNqqHu7h/K8K5h3bJqllpCcHmprOmXH/4x/GYVbjsZVUnm86mDKS6on39s/PDqWw7M962XrzMy6iNbqstosCs3Q677MB8eUxn/Uz0mzDydqVZ1SA/ImOmEw2LzM9i8huXSqRPUMLNuHV5JCWjEJt0JOW1d32v97X732YnxoIRm/E88hv/61/755eKBtvbU0LwriRB0eJOwcTz7CxpmcIoTVpKTWhn1iQBmWbLBV+wehz9XKx3JpXwYGpXp/E/S1mYthD+XdW2TnrMHsyyfDR7Jq8cUuWFAL6888j948Im2PqlE0xfS7uAO3gDr9r3FJ1gmvRj/6q86fy7+9IUDNXTGwNcBEa3Vc47z/ziikDGRL0toh2dx5jxqjbJZrFiQLhW4nofmETGrBd7XYFrohsH0ntUr7n3OLFef7oOC0WIcnTPzN7tRBnz1nPqfYiBDcAYDwqyXT/dr1jenDw1BGT94KTcSo4y1zxL4ElZK3OMm2qGB7j3+fOe177/61V6QdeMCnG+efUJzyaNTE/tMXh2/igWwGmaNNzIQr5SCODKDSwMsZ315H26ebK19GdjBWKBR7L/twjc8svRU/wM4dVzE+wgAAA=="
}
}

trait ExceptionsDsl extends impl.ExceptionsAbs {self: ExceptionsDsl =>}
trait ExceptionsDslExp extends impl.ExceptionsExp {self: ExceptionsDslExp =>}
