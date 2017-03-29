package scalan.primitives

import scala.annotation.unchecked.uncheckedVariance
import scalan._
import scala.reflect.runtime.universe._
import scalan.common.OverloadHack.{Overloaded2, Overloaded1}
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait StructItemsAbs extends StructItems {
  self: StructsDsl with Scalan =>

  // single proxy for each type family
  implicit def proxyStructItem[Val, Schema <: Struct](p: Rep[StructItem[Val, Schema]]): StructItem[Val, Schema] = {
    proxyOps[StructItem[Val, Schema]](p)(scala.reflect.classTag[StructItem[Val, Schema]])
  }

  // familyElem
  class StructItemElem[Val, Schema <: Struct, To <: StructItem[Val, Schema]](implicit _eVal: Elem[Val @uncheckedVariance], _eSchema: Elem[Schema])
    extends EntityElem[To] {
    def eVal = _eVal
    def eSchema = _eSchema
    lazy val parent: Option[Elem[_]] = None
    lazy val typeArgs = TypeArgs("Val" -> (eVal -> scalan.util.Covariant), "Schema" -> (eSchema -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagAnnotatedVal = eVal.tag
      implicit val tagSchema = eSchema.tag
      weakTypeTag[StructItem[Val, Schema]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[StructItem[Val, Schema]] => convertStructItem(x) }
      tryConvert(element[StructItem[Val, Schema]], this, x, conv)
    }

    def convertStructItem(x: Rep[StructItem[Val, Schema]]): Rep[To] = {
      x.elem match {
        case _: StructItemElem[_, _, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have StructItemElem[_, _, _], but got $e", x)
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def structItemElement[Val, Schema <: Struct](implicit eVal: Elem[Val @uncheckedVariance], eSchema: Elem[Schema]): Elem[StructItem[Val, Schema]] =
    cachedElem[StructItemElem[Val, Schema, StructItem[Val, Schema]]](eVal, eSchema)

  implicit case object StructItemCompanionElem extends CompanionElem[StructItemCompanionAbs] {
    lazy val tag = weakTypeTag[StructItemCompanionAbs]
    protected def getDefaultRep = StructItem
  }

  abstract class StructItemCompanionAbs extends CompanionDef[StructItemCompanionAbs] {
    def selfType = StructItemCompanionElem
    override def toString = "StructItem"
  }
  def StructItem: Rep[StructItemCompanionAbs]
  implicit def proxyStructItemCompanionAbs(p: Rep[StructItemCompanionAbs]): StructItemCompanionAbs =
    proxyOps[StructItemCompanionAbs](p)

  abstract class AbsStructItemBase[Val, Schema <: Struct]
      (key: Rep[StructKey[Schema]], value: Rep[Val])(implicit eVal: Elem[Val], eSchema: Elem[Schema])
    extends StructItemBase[Val, Schema](key, value) with Def[StructItemBase[Val, Schema]] {
    lazy val selfType = element[StructItemBase[Val, Schema]]
  }
  // elem for concrete class
  class StructItemBaseElem[Val, Schema <: Struct](val iso: Iso[StructItemBaseData[Val, Schema], StructItemBase[Val, Schema]])(implicit override val eVal: Elem[Val], override val eSchema: Elem[Schema])
    extends StructItemElem[Val, Schema, StructItemBase[Val, Schema]]
    with ConcreteElem[StructItemBaseData[Val, Schema], StructItemBase[Val, Schema]] {
    override lazy val parent: Option[Elem[_]] = Some(structItemElement(element[Val], element[Schema]))
    override lazy val typeArgs = TypeArgs("Val" -> (eVal -> scalan.util.Invariant), "Schema" -> (eSchema -> scalan.util.Invariant))

    override def convertStructItem(x: Rep[StructItem[Val, Schema]]) = StructItemBase(x.key, x.value)
    override def getDefaultRep = StructItemBase(element[StructKey[Schema]].defaultRepValue, element[Val].defaultRepValue)
    override lazy val tag = {
      implicit val tagVal = eVal.tag
      implicit val tagSchema = eSchema.tag
      weakTypeTag[StructItemBase[Val, Schema]]
    }
  }

  // state representation type
  type StructItemBaseData[Val, Schema <: Struct] = (StructKey[Schema], Val)

  // 3) Iso for concrete class
  class StructItemBaseIso[Val, Schema <: Struct](implicit eVal: Elem[Val], eSchema: Elem[Schema])
    extends EntityIso[StructItemBaseData[Val, Schema], StructItemBase[Val, Schema]] with Def[StructItemBaseIso[Val, Schema]] {
    override def from(p: Rep[StructItemBase[Val, Schema]]) =
      (p.key, p.value)
    override def to(p: Rep[(StructKey[Schema], Val)]) = {
      val Pair(key, value) = p
      StructItemBase(key, value)
    }
    lazy val eFrom = pairElement(element[StructKey[Schema]], element[Val])
    lazy val eTo = new StructItemBaseElem[Val, Schema](self)
    lazy val selfType = new StructItemBaseIsoElem[Val, Schema](eVal, eSchema)
    def productArity = 2
    def productElement(n: Int) = n match {
      case 0 => eVal
      case 1 => eSchema
    }
  }
  case class StructItemBaseIsoElem[Val, Schema <: Struct](eVal: Elem[Val], eSchema: Elem[Schema]) extends Elem[StructItemBaseIso[Val, Schema]] {
    def getDefaultRep = reifyObject(new StructItemBaseIso[Val, Schema]()(eVal, eSchema))
    lazy val tag = {
      implicit val tagVal = eVal.tag
      implicit val tagSchema = eSchema.tag
      weakTypeTag[StructItemBaseIso[Val, Schema]]
    }
    lazy val typeArgs = TypeArgs("Val" -> (eVal -> scalan.util.Invariant), "Schema" -> (eSchema -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class StructItemBaseCompanionAbs extends CompanionDef[StructItemBaseCompanionAbs] {
    def selfType = StructItemBaseCompanionElem
    override def toString = "StructItemBase"
    @scalan.OverloadId("fromData")
    def apply[Val, Schema <: Struct](p: Rep[StructItemBaseData[Val, Schema]])(implicit eSchema: Elem[Schema]): Rep[StructItemBase[Val, Schema]] = {
      implicit val eVal = p._2.elem
      isoStructItemBase[Val, Schema].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[Val, Schema <: Struct](key: Rep[StructKey[Schema]], value: Rep[Val])(implicit eSchema: Elem[Schema]): Rep[StructItemBase[Val, Schema]] =
      mkStructItemBase(key, value)

    def unapply[Val, Schema <: Struct](p: Rep[StructItem[Val, Schema]]) = unmkStructItemBase(p)
  }
  lazy val StructItemBaseRep: Rep[StructItemBaseCompanionAbs] = new StructItemBaseCompanionAbs
  lazy val StructItemBase: StructItemBaseCompanionAbs = proxyStructItemBaseCompanion(StructItemBaseRep)
  implicit def proxyStructItemBaseCompanion(p: Rep[StructItemBaseCompanionAbs]): StructItemBaseCompanionAbs = {
    proxyOps[StructItemBaseCompanionAbs](p)
  }

  implicit case object StructItemBaseCompanionElem extends CompanionElem[StructItemBaseCompanionAbs] {
    lazy val tag = weakTypeTag[StructItemBaseCompanionAbs]
    protected def getDefaultRep = StructItemBase
  }

  implicit def proxyStructItemBase[Val, Schema <: Struct](p: Rep[StructItemBase[Val, Schema]]): StructItemBase[Val, Schema] =
    proxyOps[StructItemBase[Val, Schema]](p)

  implicit class ExtendedStructItemBase[Val, Schema <: Struct](p: Rep[StructItemBase[Val, Schema]])(implicit eVal: Elem[Val], eSchema: Elem[Schema]) {
    def toData: Rep[StructItemBaseData[Val, Schema]] = isoStructItemBase(eVal, eSchema).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoStructItemBase[Val, Schema <: Struct](implicit eVal: Elem[Val], eSchema: Elem[Schema]): Iso[StructItemBaseData[Val, Schema], StructItemBase[Val, Schema]] =
    reifyObject(new StructItemBaseIso[Val, Schema]()(eVal, eSchema))

  // 6) smart constructor and deconstructor
  def mkStructItemBase[Val, Schema <: Struct](key: Rep[StructKey[Schema]], value: Rep[Val])(implicit eSchema: Elem[Schema]): Rep[StructItemBase[Val, Schema]]
  def unmkStructItemBase[Val, Schema <: Struct](p: Rep[StructItem[Val, Schema]]): Option[(Rep[StructKey[Schema]], Rep[Val])]

  registerModule(StructItems_Module)
}

// Exp -----------------------------------
trait StructItemsExp extends StructItemsDsl {
  self: StructsDsl with ScalanExp =>

  lazy val StructItem: Rep[StructItemCompanionAbs] = new StructItemCompanionAbs {
  }

  case class ExpStructItemBase[Val, Schema <: Struct]
      (override val key: Rep[StructKey[Schema]], override val value: Rep[Val])(implicit eVal: Elem[Val], eSchema: Elem[Schema])
    extends AbsStructItemBase[Val, Schema](key, value)

  object StructItemBaseMethods {
  }

  def mkStructItemBase[Val, Schema <: Struct]
    (key: Rep[StructKey[Schema]], value: Rep[Val])(implicit eSchema: Elem[Schema]): Rep[StructItemBase[Val, Schema]] = {
    implicit val eVal = value.elem
    new ExpStructItemBase[Val, Schema](key, value)
  }
  def unmkStructItemBase[Val, Schema <: Struct](p: Rep[StructItem[Val, Schema]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: StructItemBaseElem[Val, Schema] @unchecked =>
      Some((p.asRep[StructItemBase[Val, Schema]].key, p.asRep[StructItemBase[Val, Schema]].value))
    case _ =>
      None
  }

  object StructItemMethods {
    object key {
      def unapply(d: Def[_]): Option[Rep[StructItem[Val, Schema]] forSome {type Val; type Schema <: Struct}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[StructItemElem[_, _, _]] && method.getName == "key" =>
          Some(receiver).asInstanceOf[Option[Rep[StructItem[Val, Schema]] forSome {type Val; type Schema <: Struct}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[StructItem[Val, Schema]] forSome {type Val; type Schema <: Struct}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object value {
      def unapply(d: Def[_]): Option[Rep[StructItem[Val, Schema]] forSome {type Val; type Schema <: Struct}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[StructItemElem[_, _, _]] && method.getName == "value" =>
          Some(receiver).asInstanceOf[Option[Rep[StructItem[Val, Schema]] forSome {type Val; type Schema <: Struct}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[StructItem[Val, Schema]] forSome {type Val; type Schema <: Struct}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}

object StructItems_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAALVWS2wbRRgeO3b8ClVSRBVUSkO6Fa9gV/RQpKhCaeJAgptE2dKCqUDj3bEzzb7YHbtrDkUc6AHEBSEOSBwqQFwiJNRL1RMHkBBCPXDl3EPVtEI9NCcQ/8w+/Vi3VGIPo9mZf/7H93//P7NzB2UdG804CtawUdYJw2VZzBccJsmnTbWtkSXSXN/9+Olb38zdT6OpOhrfws6So9VRwZtUXSucy0ytoallaqhVg1HWlXShgqFyzbNR4TYqw2xIsVPzNVTAhkIcZtoOQ894hyuKqWlEYdQ0KlTX2ww3NFKpUYeBfKZhqt330SWUrqFJxTQUmzAiL2rYcYjjr+cJV0/D/4L4765bkY1BB8/YmDLwD2xMevKbxJK7hml0dYb2+a6tW9wtkCkR1wIgVnRLE2YyNZSjumXaLLCaAwtbphr8ZgwMC2h/7QLu4ApYbVVkZlOjxZVZWNnGLbIGIlw8CzE4RGue6VrEV15ymNpjz7UQQhZk9WXhWTkCrRyCVuagSTKxKdboB5hvbtim20XelxpDyOUq5h6gItBAqoYqfXJeeWdPLulpftjlvuSFRzlQdDiBYSI/AO6vm5879167ciKNinVUpM5Cw2E2VlicBz5eJWwYJhM+hxBiuwUpnE1KobCyADJ9PCkopm5hAzT5YE5ApjSqUMaF+dpjfn4SwM8xiwSiaddKhfEmVZQg0yLWtI3bT750dLf6VjqkgG+iACplKCk7UMpQEfjQVtgKI7pvgI+TDI2dxVqEMyyMy8oW0bFY40PRjcb8CLdCgJ69fVf95Rg6n0YpH1bfi4fLJKjY/8pX14+SjR/SKF8XzF/WcEvklOO2RByljvJmh9jeeq6DNT4bmtecSpq4rTEf7ThMYwATQzOJRWsRjuG8KIZUEH7Jo/OaaRBpeUO6L//2xQ5nq40mvB2viv+hJ/7+c1+TCSIDytukGyIOtd+bg4KXnDd8mdyDUiH2D4Ze8WGGoSzA0CbJVsJMD2RVKBBi07EjB1N9BjIENATaMlUtkUpxL/uV5IgX1Ag9SWFDp4xYfAo7JPKbs+pQUr2I+np7Vy1P3z10MY3GV1G2CXRxaijbMNuGGhQudHxGXHYqWOurKihUbGM9vAg6GBoXNBaGDgQUajOqVc766x5x4JtBwlERT7zgbHTAd5mfK68YnkYmvXht5yK98fyyII+wtxhogkNFj2eyqZOp2Xv03SufMlFqKbf3BlhvXICGOy/AOxxhKxAMsc0Pwv36ANVG1Wxwtf14+fITf3373uOi++YblOnYko79h94btMr/sbeiPr6C270rg13SRkdG0MqLhKhHrn5052Rj7jPRibMiwKjHiOlTvAtMtQ2AWtkmasCRR+rCxdjFOB1lUpCst9XHCySiUeohCbloBoRkWP3J3LtWH0ZI/rf6qPTi42bk5Sq4UE7Ae4koGraJyl8pRIdXlEeh41++em51+tybogQmVCHk7YStffib7zS25sUD5bkRDxQQkqq6xbp8cvznk398+Pv33wkU4iGWYrSBLPsBWDbV4aXYIU4Y22xCbLJPWyDkpb2v1164cfWmoFKRFwDcNEb48IvY7lpDuctf0P1M4nZiTIKmy+skljmxa/8LZunexMoLAAA="
}
}

