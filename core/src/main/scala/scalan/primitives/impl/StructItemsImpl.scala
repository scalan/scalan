package scalan.primitives

import scalan._
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait StructItemsAbs extends StructItems {
  self: StructsDsl with Scalan =>

  // single proxy for each type family
  implicit def proxyStructItem[Val](p: Rep[StructItem[Val]]): StructItem[Val] = {
    proxyOps[StructItem[Val]](p)(scala.reflect.classTag[StructItem[Val]])
  }

  // familyElem
  class StructItemElem[Val, To <: StructItem[Val]](implicit _eVal: Elem[Val])
    extends EntityElem[To] {
    def eVal = _eVal
    lazy val parent: Option[Elem[_]] = None
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("Val" -> Left(eVal))
    }
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagVal = eVal.tag
      weakTypeTag[StructItem[Val]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[StructItem[Val]] => convertStructItem(x) }
      tryConvert(element[StructItem[Val]], this, x, conv)
    }

    def convertStructItem(x: Rep[StructItem[Val]]): Rep[To] = {
      x.selfType1 match {
        case _: StructItemElem[_, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have StructItemElem[_, _], but got $e", x)
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def structItemElement[Val](implicit eVal: Elem[Val]): Elem[StructItem[Val]] =
    cachedElem[StructItemElem[Val, StructItem[Val]]](eVal)

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

  abstract class AbsStructItemBase[Val]
      (key: Rep[StructKey], value: Rep[Val])(implicit eVal: Elem[Val])
    extends StructItemBase[Val](key, value) with Def[StructItemBase[Val]] {
    lazy val selfType = element[StructItemBase[Val]]
  }
  // elem for concrete class
  class StructItemBaseElem[Val](val iso: Iso[StructItemBaseData[Val], StructItemBase[Val]])(implicit override val eVal: Elem[Val])
    extends StructItemElem[Val, StructItemBase[Val]]
    with ConcreteElem[StructItemBaseData[Val], StructItemBase[Val]] {
    override lazy val parent: Option[Elem[_]] = Some(structItemElement(element[Val]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("Val" -> Left(eVal))
    }

    override def convertStructItem(x: Rep[StructItem[Val]]) = StructItemBase(x.key, x.value)
    override def getDefaultRep = StructItemBase(element[StructKey].defaultRepValue, element[Val].defaultRepValue)
    override lazy val tag = {
      implicit val tagVal = eVal.tag
      weakTypeTag[StructItemBase[Val]]
    }
  }

  // state representation type
  type StructItemBaseData[Val] = (StructKey, Val)

  // 3) Iso for concrete class
  class StructItemBaseIso[Val](implicit eVal: Elem[Val])
    extends EntityIso[StructItemBaseData[Val], StructItemBase[Val]] with Def[StructItemBaseIso[Val]] {
    override def from(p: Rep[StructItemBase[Val]]) =
      (p.key, p.value)
    override def to(p: Rep[(StructKey, Val)]) = {
      val Pair(key, value) = p
      StructItemBase(key, value)
    }
    lazy val eFrom = pairElement(element[StructKey], element[Val])
    lazy val eTo = new StructItemBaseElem[Val](self)
    lazy val selfType = new StructItemBaseIsoElem[Val](eVal)
    def productArity = 1
    def productElement(n: Int) = eVal
  }
  case class StructItemBaseIsoElem[Val](eVal: Elem[Val]) extends Elem[StructItemBaseIso[Val]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new StructItemBaseIso[Val]()(eVal))
    lazy val tag = {
      implicit val tagVal = eVal.tag
      weakTypeTag[StructItemBaseIso[Val]]
    }
  }
  // 4) constructor and deconstructor
  class StructItemBaseCompanionAbs extends CompanionDef[StructItemBaseCompanionAbs] {
    def selfType = StructItemBaseCompanionElem
    override def toString = "StructItemBase"
    def apply[Val](p: Rep[StructItemBaseData[Val]])(implicit eVal: Elem[Val]): Rep[StructItemBase[Val]] =
      isoStructItemBase(eVal).to(p)
    def apply[Val](key: Rep[StructKey], value: Rep[Val])(implicit eVal: Elem[Val]): Rep[StructItemBase[Val]] =
      mkStructItemBase(key, value)
  }
  object StructItemBaseMatcher {
    def unapply[Val](p: Rep[StructItem[Val]]) = unmkStructItemBase(p)
  }
  lazy val StructItemBase: Rep[StructItemBaseCompanionAbs] = new StructItemBaseCompanionAbs
  implicit def proxyStructItemBaseCompanion(p: Rep[StructItemBaseCompanionAbs]): StructItemBaseCompanionAbs = {
    proxyOps[StructItemBaseCompanionAbs](p)
  }

  implicit case object StructItemBaseCompanionElem extends CompanionElem[StructItemBaseCompanionAbs] {
    lazy val tag = weakTypeTag[StructItemBaseCompanionAbs]
    protected def getDefaultRep = StructItemBase
  }

  implicit def proxyStructItemBase[Val](p: Rep[StructItemBase[Val]]): StructItemBase[Val] =
    proxyOps[StructItemBase[Val]](p)

  implicit class ExtendedStructItemBase[Val](p: Rep[StructItemBase[Val]])(implicit eVal: Elem[Val]) {
    def toData: Rep[StructItemBaseData[Val]] = isoStructItemBase(eVal).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoStructItemBase[Val](implicit eVal: Elem[Val]): Iso[StructItemBaseData[Val], StructItemBase[Val]] =
    reifyObject(new StructItemBaseIso[Val]()(eVal))

  // 6) smart constructor and deconstructor
  def mkStructItemBase[Val](key: Rep[StructKey], value: Rep[Val])(implicit eVal: Elem[Val]): Rep[StructItemBase[Val]]
  def unmkStructItemBase[Val](p: Rep[StructItem[Val]]): Option[(Rep[StructKey], Rep[Val])]

  registerModule(StructItems_Module)
}

// Seq -----------------------------------
trait StructItemsSeq extends StructItemsDsl {
  self: StructsDsl with ScalanSeq =>
  lazy val StructItem: Rep[StructItemCompanionAbs] = new StructItemCompanionAbs {
  }

  case class SeqStructItemBase[Val]
      (override val key: Rep[StructKey], override val value: Rep[Val])(implicit eVal: Elem[Val])
    extends AbsStructItemBase[Val](key, value) {
  }

  def mkStructItemBase[Val]
    (key: Rep[StructKey], value: Rep[Val])(implicit eVal: Elem[Val]): Rep[StructItemBase[Val]] =
    new SeqStructItemBase[Val](key, value)
  def unmkStructItemBase[Val](p: Rep[StructItem[Val]]) = p match {
    case p: StructItemBase[Val] @unchecked =>
      Some((p.key, p.value))
    case _ => None
  }
}

// Exp -----------------------------------
trait StructItemsExp extends StructItemsDsl {
  self: StructsDsl with ScalanExp =>
  lazy val StructItem: Rep[StructItemCompanionAbs] = new StructItemCompanionAbs {
  }

  case class ExpStructItemBase[Val]
      (override val key: Rep[StructKey], override val value: Rep[Val])(implicit eVal: Elem[Val])
    extends AbsStructItemBase[Val](key, value)

  object StructItemBaseMethods {
  }

  def mkStructItemBase[Val]
    (key: Rep[StructKey], value: Rep[Val])(implicit eVal: Elem[Val]): Rep[StructItemBase[Val]] =
    new ExpStructItemBase[Val](key, value)
  def unmkStructItemBase[Val](p: Rep[StructItem[Val]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: StructItemBaseElem[Val] @unchecked =>
      Some((p.asRep[StructItemBase[Val]].key, p.asRep[StructItemBase[Val]].value))
    case _ =>
      None
  }

  object StructItemMethods {
    object key {
      def unapply(d: Def[_]): Option[Rep[StructItem[Val]] forSome {type Val}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[StructItemElem[_, _]] && method.getName == "key" =>
          Some(receiver).asInstanceOf[Option[Rep[StructItem[Val]] forSome {type Val}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[StructItem[Val]] forSome {type Val}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object value {
      def unapply(d: Def[_]): Option[Rep[StructItem[Val]] forSome {type Val}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[StructItemElem[_, _]] && method.getName == "value" =>
          Some(receiver).asInstanceOf[Option[Rep[StructItem[Val]] forSome {type Val}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[StructItem[Val]] forSome {type Val}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}

object StructItems_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAALVWPYwbRRR+9tnns33KD3DoQEQcJwMCgR0oSHFFdLk4KLDcnW4jhEwEGq/HziazM3M749MuRcoU0CFaJCLRIKVBVDSIBglRUCGERE0VglAKUoF4M/vj9SlLLgUuRjszb957833fe+Nbd6CuQnhOeYQR3g2oJl3Xfm8q3XH7XPs6fkuMpoyep+NV8e1nr3zx5NdVOD6AxStEnVdsAM3kox/J/Nul+w40Cfeo0iJUGp5xbISeJxijnvYF7/lBMNVkyGjP8ZXecKA2FKN4H65DxYETnuBeSDV1txhRiqp0fYmajPx83rTzeEfOYvCeuUWvcItLIfE1po8xTiT2e1S6MRc8DjQcS1PbkSYttGn4gRShzkI00N0VMcqmNU5wAR5xrpID0sMQk56rQ59P8GRbEu8amdBtNDHmNUxYUTa+FEs7X3Cgpeg+AnQxkMyuRBIAkIFXbRLdGT7dHJ+uwafj0tAnzP+AmM3dUEQxJL/KAkAk0cVLD3CReaB9Pup8eNl7957bDqrmcGRSadgbLqKjp0vUYKlAHL/f+1jdff3mmSq0BtDy1eZQ6ZB4ukh5ilabcC60zTkHkIQTZGu9jC0bZRNtDkmi6YlAEo6eUiiXkSfme742xmZtOWWnBPqGljQzrUSykt93reS+VjdbhLHd20+8/Ozv/XeqUJ0P0USXLgo/zJxqaKEapp6+qGmQBjDjcQ0LbxNLecMMzWg2Nv4jhRyM52//MfruNFyu5hCmEY/GGrqoq19+bv/0wtkqLA2sxi8wMhkgiqrPaLATbgmuB7AkDmiY7DQOCDNf92WxMaJjMmU6xbYIygKComGttBolNYhtWOVXMgDaiXi3BaedC7udv9wfPrlltBnCcrKTlOc//pm/fz021la2iOk1Guf4YlHPI95MqHiTxgXc7eZqHtoMpzTU8a5TWu6qnDzrwJqtFI6sVg4FqFH0kHmrGcSPEACb00xN54iis2CG8VNlurU6f3zPeYzdOftNFepvQH2MRCoH6kMx5aOsgLDJahrpc9laZZ5ILBgSkiBTWtJu1sAmYXMtSfuBkswa8pc3bqz8+fn7j9pGsjT0dUBk5/RDtJGs6v/HNgGHeMK051cevuCbhX67klskSpnvIEW+zfjakTkw48bMKjVtF3LVcDKlR4Z+gI/qAVXIWSspN1cE9OT6Xf+9mx9p23Mq0fyjtzO8iq/Mhg30FJ5bL+HaTdFF3K7f+3T7xR+/+s220ZbhCaud5w/tjJRI3hdi889ihhyuLyZxCpfH2jJ02ov/C8tsZQjcCAAA"
}
}

trait StructItemsDsl extends impl.StructItemsAbs {self: StructsDsl with Scalan =>}
trait StructItemsDslSeq extends impl.StructItemsSeq {self: StructsDsl with ScalanSeq =>}
trait StructItemsDslExp extends impl.StructItemsExp {self: StructsDsl with ScalanExp =>}
