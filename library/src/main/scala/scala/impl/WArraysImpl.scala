package scala

import scalan._
import impl._
import scala.Array
import scala.wrappers.WrappersModule
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait WArraysDefs extends scalan.Scalan with WArrays {
  self: WrappersModule =>

  // entityProxy: single proxy for each type family
  implicit def proxyWArray[T](p: Rep[WArray[T]]): WArray[T] = {
    proxyOps[WArray[T]](p)(scala.reflect.classTag[WArray[T]])
  }

  //proxyBT: TypeWrapper proxy
  //implicit def proxyArray[T:Elem](p: Rep[Array[T]]): WArray[T] =
  //  proxyOps[WArray[T]](p.asRep[WArray[T]])

  implicit def unwrapValueOfWArray[T](w: Rep[WArray[T]]): Rep[Array[T]] = w.wrappedValue

  implicit def arrayElement[T:Elem]: Elem[Array[T]] =
    element[WArray[T]].asInstanceOf[WrapperElem[_, _]].baseElem.asInstanceOf[Elem[Array[T]]]

  // familyElem
  class WArrayElem[T, To <: WArray[T]](implicit _eeT: Elem[T])
    extends WrapperElem[Array[T], To] {
    def eeT = _eeT
    lazy val parent: Option[Elem[_]] = None
    lazy val typeArgs = TypeArgs("T" -> (eeT -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagT = eeT.tag
      weakTypeTag[WArray[T]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[WArray[T]] => convertWArray(x) }
      tryConvert(element[WArray[T]], this, x, conv)
    }

    def convertWArray(x: Rep[WArray[T]]): Rep[To] = {
      x.elem match {
        case _: WArrayElem[_, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have WArrayElem[_, _], but got $e", x)
      }
    }
    lazy val baseElem = {
            implicit val wT = element[T].tag
      new BaseTypeElem[Array[T], WArray[T]](this.asInstanceOf[Elem[WArray[T]]])
    }
    lazy val eTo: Elem[_] = new WArrayImplElem[T](isoWArrayImpl(eeT))(eeT)
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def wArrayElement[T](implicit eeT: Elem[T]): Elem[WArray[T]] =
    elemCache.getOrElseUpdate(
      (classOf[WArrayElem[T, WArray[T]]], Seq(eeT)),
      new WArrayElem[T, WArray[T]]).asInstanceOf[Elem[WArray[T]]]

  implicit case object WArrayCompanionElem extends CompanionElem[WArrayCompanionCtor] {
    lazy val tag = weakTypeTag[WArrayCompanionCtor]
    protected def getDefaultRep = WArray
  }

  abstract class WArrayCompanionCtor extends CompanionDef[WArrayCompanionCtor] with WArrayCompanion {
    def selfType = WArrayCompanionElem
    override def toString = "WArray"
  }
  implicit def proxyWArrayCompanionCtor(p: Rep[WArrayCompanionCtor]): WArrayCompanionCtor =
    proxyOps[WArrayCompanionCtor](p)

  // default wrapper implementation
  abstract class WArrayImpl[T](val wrappedValue: Rep[Array[T]])(implicit val eeT: Elem[T]) extends WArray[T] with Def[WArrayImpl[T]] {
    lazy val selfType = element[WArrayImpl[T]]

    def apply(i: Rep[Int]): Rep[T] =
      methodCallEx[T](self,
        this.getClass.getMethod("apply", classOf[AnyRef]),
        List(i.asInstanceOf[AnyRef]))

    def length: Rep[Int] =
      methodCallEx[Int](self,
        this.getClass.getMethod("length"),
        List())
  }
  case class WArrayImplCtor[T](override val wrappedValue: Rep[Array[T]])(implicit override val eeT: Elem[T]) extends WArrayImpl[T](wrappedValue) {
  }
  trait WArrayImplCompanion
  // elem for concrete class
  class WArrayImplElem[T](val iso: Iso[WArrayImplData[T], WArrayImpl[T]])(implicit override val eeT: Elem[T])
    extends WArrayElem[T, WArrayImpl[T]]
    with ConcreteElem[WArrayImplData[T], WArrayImpl[T]] {
    override lazy val parent: Option[Elem[_]] = Some(wArrayElement(element[T]))
    override lazy val typeArgs = TypeArgs("T" -> (eeT -> scalan.util.Invariant))
    override lazy val eTo: Elem[_] = this
    override def convertWArray(x: Rep[WArray[T]]) = WArrayImpl(x.wrappedValue)
    override def getDefaultRep = WArrayImpl(DefaultOfArray[T])
    override lazy val tag = {
      implicit val tagT = eeT.tag
      weakTypeTag[WArrayImpl[T]]
    }
  }

  // state representation type
  type WArrayImplData[T] = Array[T]

  // 3) Iso for concrete class
  class WArrayImplIso[T](implicit eeT: Elem[T])
    extends EntityIso[WArrayImplData[T], WArrayImpl[T]] with Def[WArrayImplIso[T]] {
    override def from(p: Rep[WArrayImpl[T]]) =
      p.wrappedValue
    override def to(p: Rep[Array[T]]) = {
      val wrappedValue = p
      WArrayImpl(wrappedValue)
    }
    lazy val eFrom = element[Array[T]]
    lazy val eTo = new WArrayImplElem[T](self)
    lazy val selfType = new WArrayImplIsoElem[T](eeT)
    def productArity = 1
    def productElement(n: Int) = eeT
  }
  case class WArrayImplIsoElem[T](eeT: Elem[T]) extends Elem[WArrayImplIso[T]] {
    def getDefaultRep = reifyObject(new WArrayImplIso[T]()(eeT))
    lazy val tag = {
      implicit val tagT = eeT.tag
      weakTypeTag[WArrayImplIso[T]]
    }
    lazy val typeArgs = TypeArgs("T" -> (eeT -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class WArrayImplCompanionCtor extends CompanionDef[WArrayImplCompanionCtor] {
    def selfType = WArrayImplCompanionElem
    override def toString = "WArrayImplCompanion"

    @scalan.OverloadId("fromFields")
    def apply[T](wrappedValue: Rep[Array[T]])(implicit eeT: Elem[T]): Rep[WArrayImpl[T]] =
      mkWArrayImpl(wrappedValue)

    def unapply[T](p: Rep[WArray[T]]) = unmkWArrayImpl(p)
  }
  lazy val WArrayImplRep: Rep[WArrayImplCompanionCtor] = new WArrayImplCompanionCtor
  lazy val WArrayImpl: WArrayImplCompanionCtor = proxyWArrayImplCompanion(WArrayImplRep)
  implicit def proxyWArrayImplCompanion(p: Rep[WArrayImplCompanionCtor]): WArrayImplCompanionCtor = {
    proxyOps[WArrayImplCompanionCtor](p)
  }

  implicit case object WArrayImplCompanionElem extends CompanionElem[WArrayImplCompanionCtor] {
    lazy val tag = weakTypeTag[WArrayImplCompanionCtor]
    protected def getDefaultRep = WArrayImplRep
  }

  implicit def proxyWArrayImpl[T](p: Rep[WArrayImpl[T]]): WArrayImpl[T] =
    proxyOps[WArrayImpl[T]](p)

  implicit class ExtendedWArrayImpl[T](p: Rep[WArrayImpl[T]])(implicit eeT: Elem[T]) {
    def toData: Rep[WArrayImplData[T]] = isoWArrayImpl(eeT).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoWArrayImpl[T](implicit eeT: Elem[T]): Iso[WArrayImplData[T], WArrayImpl[T]] =
    reifyObject(new WArrayImplIso[T]()(eeT))

  registerModule(WArraysModule)

  lazy val WArray: Rep[WArrayCompanionCtor] = new WArrayCompanionCtor {
  }

  object WArrayImplMethods {
  }

  def mkWArrayImpl[T]
    (wrappedValue: Rep[Array[T]])(implicit eeT: Elem[T]): Rep[WArrayImpl[T]] = {
    new WArrayImplCtor[T](wrappedValue)
  }
  def unmkWArrayImpl[T](p: Rep[WArray[T]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: WArrayImplElem[T] @unchecked =>
      Some((p.asRep[WArrayImpl[T]].wrappedValue))
    case _ =>
      None
  }

  object WArrayMethods {
    object wrappedValue {
      def unapply(d: Def[_]): Option[Rep[WArray[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WArrayElem[_, _]] && method.getName == "wrappedValue" =>
          Some(receiver).asInstanceOf[Option[Rep[WArray[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[WArray[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[WArray[T]], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[WArrayElem[_, _]] && method.getName == "apply" =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[WArray[T]], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[WArray[T]], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object length {
      def unapply(d: Def[_]): Option[Rep[WArray[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WArrayElem[_, _]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[WArray[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[WArray[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object WArrayCompanionMethods {
  }
}

object WArraysModule extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAALVWz28bRRR+3sTxrxCSoETqoSVEWyFosQsSFJQDComDCm4SdaO2MhVovDt2p8zuDrvP6ZpDb/QAF4Q4IXGoKLcICXEB7kiIQ/8BzhxQA0I9tBISiJnZX44b0/ZQH0a7s2/f++b7vvfWe39AMQxgKbQJJ17dpUjqlr5eDdG0zvpOn9N12t3a/+jY71+dvGvAXBumLpNwPeRtqMQXzUhk1xY6LZjbYJ7T9JDhwHR1CoR6K67RUDUah9Uwh95aaUGFeDYN0Q9ChGfilxu2zzm1kfleg7luH0mH00aLhSjjJzu+M/gAroHRglnb9+yAIrXWOAlDGib7ZarSs+y+ou8HWyKvcT/AnYAwlPhkjdk4/hwV1sDzvYGLMJNA2xIKloyp0UhIIs64gusyky0oMVf4AaZVS7LCZd9Jbyc9IjdgvnWF7JKGrNprWBgwr6eSCWK/T3p0U4ao8KI8Q0h5d2cgaJK8FqJzoF4kAEBIVV/SyOo5afWMtLoizbRowAhnHxL1cDvwowHEv8IEQKRSnHxAijQDbXqO+fEl+517Vs011MuRwlLWiEoy0dNjHKb1keT+fO6z8M6bN04bUG1DlYWrnRADYuOwDxK+asTzfNSYMwpJ0JMSLo+TUFdZlTEjPqnYviuIJzMlZE5LpTizGapgtfdEos8Y8ksoaBpqRKKQnXdcR2kzrRHOt28feeH4fvOikVkgKVGRKS3ZUkGaFGHqwmoQkEGSXK2zCIUdzbBaqlG+lv+neEbDs7f/dH46BZcMKCTkJbUeTi+ZYv7VL348Tre/MaDc1v7e4KSnlVPsrNPQbkPZ36VBvF/aJVxdHapeyaFd0ueYcDpMxoQkA2FpbGsKqpha0ZYvpMevxabd9D1qbmybd61fPt9TngxgOn4S9+q/7PQ/v850UdsVYfpqQISgznnC+zQlufgIxKvlmA5aHHrhSKGQINPPESYo3UlTTTY5dYezq2XhvvQI1dgBqsnzMkqno+N8pn25//fNH6xPv37NgKm3oNiVAoQtKHb8vuekhpeTEmmEb6R7I26UBicBcbMBuktkw8uGRFhMRekj443zyX4shfwtgQaanWUhVWcxQaxeq5/x4oRonvh+7yq79dyGVmP48A+0ZDqfv71+feGvm+89pUdIucPQJcI89QgDJO33xzgg4KCVairygvZdjK6klqXcC/Oj5jAP9V51aNouZhGK7WrseMt36dzyHfbujU9QN30hOvjF2epckQN+Rec5qvO8PIJ1uhmtpWS8OAprfswsGgGkLSF77cnYzmvD5MZWEfmpX5fwl8dobiUsS9mv3fty8/lb3/2mB2lV6SX73ss+trk4sa3KsuuURuq6nkN9JYOQW1aGlpLJKyeBRpIAU+vbB+mZSWWM/8+M8HFWZ/4P0bBRYG4JAAA="
}
}

trait WArraysModule extends impl.WArraysDefs {self: WrappersModule =>}
