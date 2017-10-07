package scalan.primitives

import scala.annotation.unchecked.uncheckedVariance
import scalan._
import scala.reflect.runtime.universe._
import scalan.common.OverloadHack.{Overloaded2, Overloaded1}
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait StructItemsDefs extends StructItems {
  self: Structs with Scalan =>

  // entityProxy: single proxy for each type family
  implicit def proxyStructItem[Val, Schema <: Struct](p: Rep[StructItem[Val, Schema]]): StructItem[Val, Schema] = {
    proxyOps[StructItem[Val, Schema]](p)(scala.reflect.classTag[StructItem[Val, Schema]])
  }

  // familyElem
  class StructItemElem[Val, Schema <: Struct, To <: StructItem[Val, Schema]](implicit _eVal: Elem[Val @uncheckedVariance], _eSchema: Elem[Schema])
    extends EntityElem[To] {
    def eVal = _eVal
    def eSchema = _eSchema
    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Val" -> (eVal -> scalan.util.Covariant), "Schema" -> (eSchema -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagAnnotatedVal = eVal.tag
      implicit val tagSchema = eSchema.tag
      weakTypeTag[StructItem[Val, Schema]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
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

  implicit case object StructItemCompanionElem extends CompanionElem[StructItemCompanionCtor] {
    lazy val tag = weakTypeTag[StructItemCompanionCtor]
    protected def getDefaultRep = StructItem
  }

  abstract class StructItemCompanionCtor extends CompanionDef[StructItemCompanionCtor] {
    def selfType = StructItemCompanionElem
    override def toString = "StructItem"
  }
  implicit def proxyStructItemCompanionCtor(p: Rep[StructItemCompanionCtor]): StructItemCompanionCtor =
    proxyOps[StructItemCompanionCtor](p)

  case class StructItemBaseCtor[Val, Schema <: Struct]
      (override val key: Rep[StructKey[Schema]], override val value: Rep[Val])
    extends StructItemBase[Val, Schema](key, value) with Def[StructItemBase[Val, Schema]] {
    implicit val eVal = value.elem;
implicit val eSchema = key.eSchema
    lazy val selfType = element[StructItemBase[Val, Schema]]
  }
  // elem for concrete class
  class StructItemBaseElem[Val, Schema <: Struct](val iso: Iso[StructItemBaseData[Val, Schema], StructItemBase[Val, Schema]])(implicit override val eVal: Elem[Val], override val eSchema: Elem[Schema])
    extends StructItemElem[Val, Schema, StructItemBase[Val, Schema]]
    with ConcreteElem[StructItemBaseData[Val, Schema], StructItemBase[Val, Schema]] {
    override lazy val parent: Option[Elem[_]] = Some(structItemElement(element[Val], element[Schema]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Val" -> (eVal -> scalan.util.Invariant), "Schema" -> (eSchema -> scalan.util.Invariant))

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
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Val" -> (eVal -> scalan.util.Invariant), "Schema" -> (eSchema -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class StructItemBaseCompanionCtor extends CompanionDef[StructItemBaseCompanionCtor] {
    def selfType = StructItemBaseCompanionElem
    override def toString = "StructItemBaseCompanion"
    @scalan.OverloadId("fromData")
    def apply[Val, Schema <: Struct](p: Rep[StructItemBaseData[Val, Schema]]): Rep[StructItemBase[Val, Schema]] = {
      implicit val eVal = p._2.elem;
implicit val eSchema = p._1.eSchema
      isoStructItemBase[Val, Schema].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[Val, Schema <: Struct](key: Rep[StructKey[Schema]], value: Rep[Val]): Rep[StructItemBase[Val, Schema]] =
      mkStructItemBase(key, value)

    def unapply[Val, Schema <: Struct](p: Rep[StructItem[Val, Schema]]) = unmkStructItemBase(p)
  }
  lazy val StructItemBaseRep: Rep[StructItemBaseCompanionCtor] = new StructItemBaseCompanionCtor
  lazy val StructItemBase: StructItemBaseCompanionCtor = proxyStructItemBaseCompanion(StructItemBaseRep)
  implicit def proxyStructItemBaseCompanion(p: Rep[StructItemBaseCompanionCtor]): StructItemBaseCompanionCtor = {
    proxyOps[StructItemBaseCompanionCtor](p)
  }

  implicit case object StructItemBaseCompanionElem extends CompanionElem[StructItemBaseCompanionCtor] {
    lazy val tag = weakTypeTag[StructItemBaseCompanionCtor]
    protected def getDefaultRep = StructItemBaseRep
  }

  implicit def proxyStructItemBase[Val, Schema <: Struct](p: Rep[StructItemBase[Val, Schema]]): StructItemBase[Val, Schema] =
    proxyOps[StructItemBase[Val, Schema]](p)

  implicit class ExtendedStructItemBase[Val, Schema <: Struct](p: Rep[StructItemBase[Val, Schema]]) {
    def toData: Rep[StructItemBaseData[Val, Schema]] = {
      implicit val eVal = p.value.elem;
implicit val eSchema = p.key.eSchema
      isoStructItemBase(eVal, eSchema).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoStructItemBase[Val, Schema <: Struct](implicit eVal: Elem[Val], eSchema: Elem[Schema]): Iso[StructItemBaseData[Val, Schema], StructItemBase[Val, Schema]] =
    reifyObject(new StructItemBaseIso[Val, Schema]()(eVal, eSchema))

  registerModule(StructItemsModule)

  lazy val StructItem: Rep[StructItemCompanionCtor] = new StructItemCompanionCtor {
  }

  object StructItemBaseMethods {
  }

  def mkStructItemBase[Val, Schema <: Struct]
    (key: Rep[StructKey[Schema]], value: Rep[Val]): Rep[StructItemBase[Val, Schema]] = {
    new StructItemBaseCtor[Val, Schema](key, value)
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

object StructItemsModule extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAALVWTWwbRRQeb5w6dtKfBFIEApGmW1FKZZdeihSBlKZJ1WKcqBsCmAo03h270+zuDLvjsEZVJS6VaOFA1RMSh0qcUIQEuVQ9ICSEhDj0jriBOKEi1ANRD0W8mf3xrh0nLRI+jNbjed977/u+eev1u2jY99CUb2Ibu2WHCFw21POsL3TjNWa1bXKKNH9lGxPXr93/UkP76mg39VeoJ9rYph8Qq44m2eq8Q8WiR1thwLKHqaii3fOuoKKjO2pToCPVME1FpqlslUaPImaqaGK5w4nRcZlLnQShsjNCOgxgHn/Dw5wTr6eUF3cGygYCVBG7JvEF83yBDoTxFZPZNjEFZW6FOk5b4IZNKlXqCzi/z2Su6RFBjDkb+z7x30OXUb6KRoiEpMn3ovreWeRd3P66FKVQlsQNz58jXPXZcQTaE5WzyGUpcKZAHc48EacoANwFZsVf8y6GDTRRvYjXcAVStCqG8Kjbgsi9LCujDNlVRaMcm6u4RWoQKbcK0IdP7KakWx0JeA5xzsFMx1Ut5S415YSasqRGN4hHpXew/HHJY0EHhZ/cEEKBhDi6A0SMQOZdS7963nx70xh1NBkcqB6LgPHsAE8rMYDJH89d9++dvnlCQ6U6KlF/tuELD5siLXTE1yh2XSZUuQmF2GuBXtOD9FJZZuEMUJpvMKsTi20yh2MXkCJix0Apm5pUyMNyb2+kz5Ysg5SCk/hoHkhP+h10h2XsLOd25/tL31767emfxzU0JE0YcC8FOwSw27SjrDCHbRva0UScHLKWQqUM5pDx6Xv0nZvXhIZyVZQLsv5abFwEJWcCD42FEaFV/6EnHvyypym0SPiBTcT5j7/ZbJSXP/5IQ1qWpyI0YMxDU3FxApXA1G1TnBHEiViS65MCDa1gWz6W4o1dhnmBOFjtyWUskOtTPd9L2xSY6P3cH39aPxxD5xULyiUxWQ9lTICYeOmz24fI0lcaGqmri7xg45ayqNTrFPHNOhpha8QL9wtr2JZPW9q0YJEmbtvxLU4TFio+NVBxTiSbMwGXFzNufzQUr8Zcoi8s6X8bP91Yl9rJ358AYldJJyEZBlSW9mKox6vRmeL27I+FV1lP0svlsEDD0G+bDM6SiJuG6gKoYwdSIXoul02QJ4AQo+Xn7YHuSVfZC1IgYVNhkFxeebimYZh3bXsS+6RbtTTPM4MuiJoKIw8+vW/cuPqNhgpn0XATXOFvaYvhBmu7VjyB4D0lSCBOxnv5rFFg4mAPO8nraw3D8IUJKdD+2DxtQe3KSrQfWgY+U6jLSPI0G1tpf9SJjC2fcUNUob9wa/19euf5hXAiyMNnM2hqa7LLouIqQVfL6eSp1men7S5g/I79+sqVyb++ePcx9WYYaVDhYK4fe4T3QjzG/8e5j1JumshyMgQtZL3WPwg9dHAbI4VdEevgxod3X24c/UQN22HVbHd4qMdDcs6Ot13wsrlKrNgCjzhow/WtvnGrrmpW28PZuZ6+HF2v5Pqcly6lx3pzLLaewNZ3bPNW/T9ar7+7Wm+Lr2fvAoSM9kgDo2M8qo57FP5V0zXiR4k9ND1ANCPyCih/efPz2pE7G78rzUrSdTCr3eSfYNdiQc/cLISV+L16ySQpZWAkSmf+Cx4HuLM5DAAA"
}
}

