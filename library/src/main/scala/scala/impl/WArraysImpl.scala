package scala

import scalan._
import impl._
import scala.Array
import scala.reflect.runtime.universe._
import scala.reflect._
import scala.wrappers.WrappersDsl

package impl {
// Abs -----------------------------------
trait WArraysDefs extends scalan.Scalan with WArrays {
  self: WrappersDsl =>

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

  registerModule(WArrays_Module)

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

object WArrays_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAALVVz2/cRBR+62Szv0KbBCVSDy0hcoVKy25BgoJyQCHZoMI2ieqorZaq1aw9u50ytgf7berl0Bs9wAUhTkgcKsotQkJcgDsS4tB/gDMH1IBQD62EBGJm/GuzzVJ6wIeRPX5+75vv+97z7m9QDANYDG3CiVd3KZK6pe9XQjStc77T53SNdjf3Pjj26xenHhgw24apayRcC3kbKvFNMxLZvYVOC2bXmec0PWQ4MF2dAqHeims0VI3GQTXMoa+WW1Ahnk1D9IMQ4dn444btc05tZL7XYK7bR9LhtNFiIcr4yY7vDN6Dm2C0YMb2PTugSK1VTsKQhsl+mar0LHuu6OfBpshrPApwOyAMJT5ZYyaOP0+FNfB8b+AiHEqgbQoFS8bUaCQkEWddwXWZyRaUmCv8ANOqJVnhmu+kj5MekRsw17pOdkhDVu01LAyY11PJBLHfJT26IUNUeFGeIaS8uz0QNEleC9HZVy8SACCkqi9pZPWctHpGWl2RZlo0YISz94l6uRX40QDiqzABEKkUpx6TIs1Am55jfnjZfuehVXMN9XGksJQ1opJM9MwYh2l9JLk/nv8kvP/m7TMGVNtQZeFKJ8SA2Djsg4SvGvE8HzXmjEIS9KSES+Mk1FVWZMyITyq27wriyUwJmdNSKc5shipY7T2V6DOG/BIKmoYakShk5x3XUdpMq4TzrXtHXji+17xkZBZISlRkSku2VJAmRZi6uBIEZJAkV+sMQmFbM6yWapSv5X8pntHw3L3fnR9Ow2UDCgl5Sa3/ppdMMffqZ98fp1tfGVBua3+vc9LTyil21mhot6Hs79Ag3i/tEK7uDlSv5NAu6XNMOB0mY0KSgbA4tjUFVUwta8sX0uPXYtNu+B4117fMB9ZPn+4qTwYwHb+Je/Vvduavnw91UdsVYfpGQISgzgXC+zQlufgExKvlmA5aGPrgSKGQINPvESYo3U5TTTY5dYezq2X+kfQI1dgBqsnzMkqno+N8pn259+ed76yPv3zNgKm3oNiVAoQtKHb8vuekhpeTEmmEb6R7I26UBicBcbMBukNkw8uGRFhIRekj440LyX4shbwWQQPNzjKfqrOQIFaf1c96cUI0T367e4PdPbGu1Rg+/GMtmc7nr2/dmv/jztWn9Qgpdxi6RJinn2CApP3+Pw4I2G+lmoq8qH0XoyupZTH3wtyoOcwDvVcdmrYLWYRiuxo73vJdOrt0n125/RHqpi9E+/84m53rcsAv6zxHdZ6XR7BON6PVlIwXR2HNjZlFI4C0JWSvHY7tvDpMbmwVkZ/6dQl/aYzmVsKylP3mw883nr/7zS96kFaVXrLvvexnm4sT26osu05ppO7rOdRXMgi5ZWVoKZm8chJoJAkwtb49ImUqo/wXj5BxTqf9B1i5xcZrCQAA"
}
}

trait WArraysDsl extends impl.WArraysDefs {self: WrappersDsl =>}
