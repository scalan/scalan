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
      (override val key: Rep[StructKey[Schema]], override val value: Rep[Val])(implicit eVal: Elem[Val], eSchema: Elem[Schema])
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
  class StructItemBaseCompanionCtor extends CompanionDef[StructItemBaseCompanionCtor] {
    def selfType = StructItemBaseCompanionElem
    override def toString = "StructItemBaseCompanion"
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

  implicit class ExtendedStructItemBase[Val, Schema <: Struct](p: Rep[StructItemBase[Val, Schema]])(implicit eVal: Elem[Val], eSchema: Elem[Schema]) {
    def toData: Rep[StructItemBaseData[Val, Schema]] = isoStructItemBase(eVal, eSchema).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoStructItemBase[Val, Schema <: Struct](implicit eVal: Elem[Val], eSchema: Elem[Schema]): Iso[StructItemBaseData[Val, Schema], StructItemBase[Val, Schema]] =
    reifyObject(new StructItemBaseIso[Val, Schema]()(eVal, eSchema))

  registerModule(StructItems_Module)

  lazy val StructItem: Rep[StructItemCompanionCtor] = new StructItemCompanionCtor {
  }

  object StructItemBaseMethods {
  }

  def mkStructItemBase[Val, Schema <: Struct]
    (key: Rep[StructKey[Schema]], value: Rep[Val])(implicit eSchema: Elem[Schema]): Rep[StructItemBase[Val, Schema]] = {
    implicit val eVal = value.elem
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

object StructItems_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAALVWT2hcRRif3exm/8WSVCyRqo3pFv/F3WIPVUKRNNlo4jYJebWVtSiz781uXjPvj29m41sPFQ/2oAgi4kHwUGzxEoTSS/XkQUFEevDq2YM0FemhAUHxm3l/dzdvWwu+wzBv5pvvz+/7fd/M9i2UZQ6aYiqm2KwYhOOKIudzjJeVU5bWoWSBtFZ33n/s9y9n7qTRRAONbmC2wGgDFbxJzbXDucK1OppY1E2tZnKdd8uGVMFRpe7ZqAob1b1slGOnZuuogE2VMG45jKPHvcNV1aKUqFy3zKpuGB2Om5RU6zrjIJ9pWlr3LXQBpetoXLVM1SGcKPMUM0aYv54nQr0e/hfkf3fVjmwMOnjawToH/8DGuCe/Tmyla1pm1+Bon+/aqi3cApkScW0AYsmwqTSTqaOcbtiWwwOrObCwYWnBb8bEsID218/jLVwFq+2qwh3dbAtlNlY3cZusgIgQz0IMjNDW6a5NfOUlxrUee66NELIhq89JzyoRaJUQtIoArawQR8dUfweLzTXHcrvI+1IjCLlCxcxdVAQaSM3Uyh+cU1/fVUpGWhx2hS956VEOFB1KYJjMD4D74/on7PZLl46nUbGBijqbazLuYJXHeeDjVcKmaXHpcwghdtqQwumkFEorcyDTx5OCahk2NkGTD+YYZIrqqs6FsFh7wM9PAvg5bpNANO3aqTDepIqSZJrHlK7dfPjZIzu119IhBXwTBVCpQEk5gVKOisCHjsqXODF8A2Ic52jkDKYRzrAwqqgbxMByTQxFNxrzQ9wKAXri5h/aD0fRuTRK+bD6XtxbJkHF/uc///YIWfs6jfINyfxFitsypwK3BcLUBspbW8Tx1nNbmIrZnnnNaaSFO5T7aMdhGgGYOJpKLFqbCAxnZTGkgvBLHp1XLJOUF9fKd5SfPt0WbHXQmLfjVfE/+vG/f93X4pLIgPIm6YaIQ+335qDgJecVXyZ3t1TI/YOhV2KY4igLMHRIspUw0wNZlQqk2GTsyMFUn4EMAQ2BtkyNJlIp7mW/khzxghqiJyls6JQRi09iRiK/BaseTaoXWV87f13+Rvn4ygtpNLqMsi2gC6ujbNPqmFpQuNDxOXH5yWCtr6qgULGDjfAi2MLQuKCxcHQgoFCH67R6xl/3iAPfFJKOynjiBeegA77L4lxlyfQ08vIz17ff1m88tSjJI+3NB5rgUNHjmWIZZGL6tv7GpQ+5LLWU23sDrDbPQ8OdleAdirCVCIbY5gfhfnmAasNqNrjarl68+NCfl998UHbffFPnBrbLR/9D7w1a5f/YW1EfX8Ht3pXBLumgw0No5UVCtMPX3rt1ojnzkezEWRlg1GPk9BHRBSY6JkCtbhIt4Mh9deFi7GKcjDIpSdbb6uMFEtEodY+EnLcCQnKsfWftXm/sRUjxt3y/9BLjeuTlMrhQScB7gagUO0QTrxRiwCvKo9Cxz148uzx59lVZAmOaFPJ2wta+95vvFLZn5QPlySEPFBAq1wybd8Xk2Pcnfnn356+uSBTiIZZitIEs+wHYjm7AS3GLsDC26YTYFJ+2QMgLu1+sPH3j2m+SSkVRAHDTmOHDL2K729fec54TrJ9GwkiMRtBxRZHE0iZ3nX8BhSvwmccLAAA="
}
}

