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
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("Val" -> Left(eVal), "Schema" -> Left(eSchema))
    }
    override def isEntityType = true
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
      x.selfType1 match {
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
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("Val" -> Left(eVal), "Schema" -> Left(eSchema))
    }

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
    def productElement(n: Int) = (eVal, eSchema).productElement(n)
  }
  case class StructItemBaseIsoElem[Val, Schema <: Struct](eVal: Elem[Val], eSchema: Elem[Schema]) extends Elem[StructItemBaseIso[Val, Schema]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new StructItemBaseIso[Val, Schema]()(eVal, eSchema))
    lazy val tag = {
      implicit val tagVal = eVal.tag
      implicit val tagSchema = eSchema.tag
      weakTypeTag[StructItemBaseIso[Val, Schema]]
    }
  }
  // 4) constructor and deconstructor
  class StructItemBaseCompanionAbs extends CompanionDef[StructItemBaseCompanionAbs] {
    def selfType = StructItemBaseCompanionElem
    override def toString = "StructItemBase"
    @scalan.OverloadId("fromData")
    def apply[Val, Schema <: Struct](p: Rep[StructItemBaseData[Val, Schema]])(implicit eVal: Elem[Val], eSchema: Elem[Schema]): Rep[StructItemBase[Val, Schema]] =
      isoStructItemBase(eVal, eSchema).to(p)
    @scalan.OverloadId("fromFields")
    def apply[Val, Schema <: Struct](key: Rep[StructKey[Schema]], value: Rep[Val])(implicit eVal: Elem[Val], eSchema: Elem[Schema]): Rep[StructItemBase[Val, Schema]] =
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
  def mkStructItemBase[Val, Schema <: Struct](key: Rep[StructKey[Schema]], value: Rep[Val])(implicit eVal: Elem[Val], eSchema: Elem[Schema]): Rep[StructItemBase[Val, Schema]]
  def unmkStructItemBase[Val, Schema <: Struct](p: Rep[StructItem[Val, Schema]]): Option[(Rep[StructKey[Schema]], Rep[Val])]

  registerModule(StructItems_Module)
}

// Std -----------------------------------
trait StructItemsStd extends StructItemsDsl {
  self: StructsDsl with ScalanStd =>
  lazy val StructItem: Rep[StructItemCompanionAbs] = new StructItemCompanionAbs {
  }

  case class StdStructItemBase[Val, Schema <: Struct]
      (override val key: Rep[StructKey[Schema]], override val value: Rep[Val])(implicit eVal: Elem[Val], eSchema: Elem[Schema])
    extends AbsStructItemBase[Val, Schema](key, value) {
  }

  def mkStructItemBase[Val, Schema <: Struct]
    (key: Rep[StructKey[Schema]], value: Rep[Val])(implicit eVal: Elem[Val], eSchema: Elem[Schema]): Rep[StructItemBase[Val, Schema]] =
    new StdStructItemBase[Val, Schema](key, value)
  def unmkStructItemBase[Val, Schema <: Struct](p: Rep[StructItem[Val, Schema]]) = p match {
    case p: StructItemBase[Val, Schema] @unchecked =>
      Some((p.key, p.value))
    case _ => None
  }
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
    (key: Rep[StructKey[Schema]], value: Rep[Val])(implicit eVal: Elem[Val], eSchema: Elem[Schema]): Rep[StructItemBase[Val, Schema]] =
    new ExpStructItemBase[Val, Schema](key, value)
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
  val dump = "H4sIAAAAAAAAALVWS4gcRRiuee28ljx8BRM0m834iMSZoIcchhB2N7OycbK7bIcgY1BqumtnO1tdXXbVLD0eIl72oHgRrx6CighBkNw9KYhIDiIi6NVTXJE9JCfFv6pfM5Pt2RhwDkVV9T//4/u//6//5g4qCA89K0xMMas7ROK6ofdzQtaMFpO2HFxyrT4lF8h69W7zxG/bC0tZdLCDpjawuCBoB5WDTcvn8d6QVhuVMTOJkK4nJDrR1hYapkspMaXtsobtOH2Ju5Q02raQzTbKd11r8Ba6jjJtdMh0mekRSYwFioUgIrwvEeWRHZ/L+jxY4YkN1lBRNIaiuOxhW4L7YONQIL9GuDFgLhs4Eh0IXVvhyi2QKdoOdz0ZmSiCug3Xio55huECPdK+hrdwA0z0Gob0bNaDf1Y5NjdxjyyDiBLPg8OC0PXLA67PORAR0gKAlhxOtcaczxFCkIKXtBf1BKB6DFBdAVQziGdjar+N1cdVz/UHKPhlcgj5HFSc3kdFpIG0mFV776r5+j2j6mTVn33lSlE7NAWKjqfQQecCgPxu7UOx+8qNs1lU6aCKLea6QnrYlMM5D+GqYsZcqX2OEcReD9I1m5YubWUOZMY4UTZdh2MGmkIspyFR1DZtqYTV3XSYnhTsi5KTSDTj80wc70xKvJo4C5jS1TtPvvjMH63Xsig7aqIMKg1gvhcplagCdOibckkSJzSg1oMS5a5gmuAMF1OGuUEcrO/UUvaTtTjBrRig5+78aX17Bl3NxrCGXjxYJkFFQfzyc/WnU+ezqNTRxF+kuNcBZEWLEmfFW3CZ7KCSu0W84EtxC1O12zOzRYus4z6VId7DQOUAKIlmUkuUE4ViU1dDJgKgGhB62WWktrhau2t8/9FNxVcPTQdfgpr9xz77968H1qWmMuC8SQYx5lDpo1koB+l5NZSZ2i8Z+vvR2Cu1HJeoADD0SbqVONf35VUr0GJHhv5yNDNmIE9AQ6Qtr5Ix0cDeSookCGqCnrSwoS8mPJ7HgiR+K149lVYxusKeWGs/RnfOf51FhYuosA50EW1U6Lp9ZkWlC/1dEl/OR3eZUbpAqWIPOxGfg0Y3g7QT2tfxcpqPRMC5SkAOw3XI4dld+40b70tdIRl/tG+vdK9Bn2zqiJ9OANFhx4AU96HGpCqLHp6vtrcf/+vTNx/V/bLUtaWDee3Mf+iWUXP7H7shGuMXuD16c39f89DJCTQIIiHWyVvv7pzrnv5A986CDjDpCXp7TFXt4T4DlM1NYl3B8EwpYB6mb5aHnrIjSRI1cUab8zChEwZlHoxk6rT4EJRR63JiIJSuDkELSISgcs92YNbZIiI056HZFLyNMLWQtOv3Pl5+4fat3zXcFUUS6J4snmYSRvh8z/yq8W0cbWVnKBRoJIpLsVen0rySlppziAP2A07vXvyifOzL7ie6HEtqFCImFfG7sPd4eAnzpp5vnp8w34BQreVwGFZh8/I3535854fPP9MPwr/Ze4R94woAAA=="
}
}

