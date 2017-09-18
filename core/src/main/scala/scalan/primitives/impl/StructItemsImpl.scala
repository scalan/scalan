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

  registerModule(StructItemsModule)

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

object StructItemsModule extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAALVWXWgcVRS+O5vN7mZDaGIN8Q9jMlJ/ym6xSAuhlPxK4jYJndrAtih3Z26SaebnOnM3nZFS8aUPii8iPog+FJS+BEX6UopI8QdKkT74Jj5KQZCK9MGgYPHcO7+7yazNQ+dhuHPn3O+c853vnnu37qKC66BRV8UGtqomYbiqiPGky2TlhK21DDJDVj/5Wz2/XDpaltBQA/WuY3fGNRqoHAxmPRqPFabV0eCcbmmzFtOZL5sCgqFqPfBR4z5qu/mQU6sm6miEf644mFLidGC9/GBY7YsBsowtlbjMdlyGngkwaqptGERlum3VdNNsMdw0SK2uuwzse5q25r+JLqJ8He1TbUt1CCPKtIFdl7jhfIlweD3+Lotvf4kmPnbGecrBOoMwwce+wP4koYpv2ZZvMjQQhrZEeVhgUyEeBW7nTWoIN4U6KuomtR0WeS2Ch3Vbiz57LAwTaKh+Dm/iGnhdqynM0a01DkaxuoHXyCKYcPNeyMElxuopn5IQvOIyrc2fRxFClIJSXhKhVRPWqjFrVc6arBBHx4b+FuY/lx3b81Hw5PIIeRzi4P9ARAhk1tLkd8+qZ7aViinxxR4PpixCKgHQ0xmqFQUCdm+e/MC998rlIxLqa6A+3Z1suszBKksLISSsgi3LZiLmmEPsrEENx7JqKLxMgk2HUMqqbVJsAVLIZj+UytBVnXFjPjcQFiiD/SKjJDLNezQX55u1S/naSUoN/7sL31z49cmfB6VAmB51UrB5gO2SjpDkNDYMSEdikXPw2heUS7FNMjh2T3/98ntMQrk6ynntAltqnoNyTngO6g9WBPK9rx/595eBVSaF1c9MIvL/dfHGt7/dOd4jIamdpzIkoECvcaLgGOoDVbdUNs+IGbLE3yMM5U9jIxELTPQq6joxsZjjr4rQ0mMd3+UuAcb1PvD7H9oPh9BZwYJQSUTWAwkTIIaOfnz9WbL8hYRKDbGT5wy8JiTK6zVDXLWBSvYmcYL54iY2+GhXmRY1sopbBgurnCYsqPhoZsUp4WxOiM2di9KvBMVbtC0izy3Lfym3PtziteP/h4HYDeLHJEPTaqe9HNTj1dCm1J39iif+j8fu+esAQwXIt0WyvcTFTUMlAMJsNLVkPNfhoIcAQoTWM2tkqicdZSdIkQRJdcHJShtafCLcKeySJG4un6eytojoC1/+M3Vl/PEn7kuouIAKq6ALd1dhFJp2y9KiHgSnFyMem4rm8u1SgZ6DHWzGh9omhh4MPZKh4Ug+LaYbtdPhfCAaeEZRwokYpfeeg4bDVPji6rwVwDL5xWtb5/Xbz88FTYEbL7TBian9CY2CrNhReSezSztU1W0fRsfvV5cuPfrnZ288Ig6IUlNnJqbyoT0cD1E3f4jtH6UkNdjOSx5SaBfczn7ooPEuagqyItr41XfuHmsefF/03IJINukhYijzdjvYsoB2dYNokQ722G+D95kdXVfs2KS+Qk3t7T29QxK95PYiv2k7kh/D2g17+1rj4ciPv1eSePdDHNWMGswQ1cAO0fhNi5hwEwwkdvij4ysLIyuviROmXxNGwZ+4ne9+bz2B6YQ4ZZ/rcscCI3nWpMzng8PfH/vp7R+vfC6oSKdYSUkJKh8mQB3dhNvuJnHj3MYyclNCWYNIL25/uvjC7at3hLz6+AaB08WKL6/JbvA6On0xCMLtlBZ3khIRNF++iVJla/GX/x/tKWn03gwAAA=="
}
}

