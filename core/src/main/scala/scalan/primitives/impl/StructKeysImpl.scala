package scalan.primitives

import scalan._
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait StructKeysDefs extends StructKeys {
  self: Structs with Scalan =>

  // entityProxy: single proxy for each type family
  implicit def proxyStructKey[Schema <: Struct](p: Rep[StructKey[Schema]]): StructKey[Schema] = {
    proxyOps[StructKey[Schema]](p)(scala.reflect.classTag[StructKey[Schema]])
  }

  // familyElem
  class StructKeyElem[Schema <: Struct, To <: StructKey[Schema]](implicit _eSchema: Elem[Schema])
    extends EntityElem[To] {
    def eSchema = _eSchema
    lazy val parent: Option[Elem[_]] = None
    lazy val typeArgs = TypeArgs("Schema" -> (eSchema -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagSchema = eSchema.tag
      weakTypeTag[StructKey[Schema]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[StructKey[Schema]] => convertStructKey(x) }
      tryConvert(element[StructKey[Schema]], this, x, conv)
    }

    def convertStructKey(x: Rep[StructKey[Schema]]): Rep[To] = {
      x.elem match {
        case _: StructKeyElem[_, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have StructKeyElem[_, _], but got $e", x)
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def structKeyElement[Schema <: Struct](implicit eSchema: Elem[Schema]): Elem[StructKey[Schema]] =
    cachedElem[StructKeyElem[Schema, StructKey[Schema]]](eSchema)

  implicit case object StructKeyCompanionElem extends CompanionElem[StructKeyCompanionCtor] {
    lazy val tag = weakTypeTag[StructKeyCompanionCtor]
    protected def getDefaultRep = StructKey
  }

  abstract class StructKeyCompanionCtor extends CompanionDef[StructKeyCompanionCtor] {
    def selfType = StructKeyCompanionElem
    override def toString = "StructKey"
  }
  implicit def proxyStructKeyCompanionCtor(p: Rep[StructKeyCompanionCtor]): StructKeyCompanionCtor =
    proxyOps[StructKeyCompanionCtor](p)

  case class IndexStructKeyCtor[Schema <: Struct]
      (override val index: Rep[Int])(implicit eSchema: Elem[Schema])
    extends IndexStructKey[Schema](index) with Def[IndexStructKey[Schema]] {
    lazy val selfType = element[IndexStructKey[Schema]]
  }
  // elem for concrete class
  class IndexStructKeyElem[Schema <: Struct](val iso: Iso[IndexStructKeyData[Schema], IndexStructKey[Schema]])(implicit override val eSchema: Elem[Schema])
    extends StructKeyElem[Schema, IndexStructKey[Schema]]
    with ConcreteElem[IndexStructKeyData[Schema], IndexStructKey[Schema]] {
    override lazy val parent: Option[Elem[_]] = Some(structKeyElement(element[Schema]))
    override lazy val typeArgs = TypeArgs("Schema" -> (eSchema -> scalan.util.Invariant))

    override def convertStructKey(x: Rep[StructKey[Schema]]) = IndexStructKey(x.index)
    override def getDefaultRep = IndexStructKey(0)
    override lazy val tag = {
      implicit val tagSchema = eSchema.tag
      weakTypeTag[IndexStructKey[Schema]]
    }
  }

  // state representation type
  type IndexStructKeyData[Schema <: Struct] = Int

  // 3) Iso for concrete class
  class IndexStructKeyIso[Schema <: Struct](implicit eSchema: Elem[Schema])
    extends EntityIso[IndexStructKeyData[Schema], IndexStructKey[Schema]] with Def[IndexStructKeyIso[Schema]] {
    override def from(p: Rep[IndexStructKey[Schema]]) =
      p.index
    override def to(p: Rep[Int]) = {
      val index = p
      IndexStructKey(index)
    }
    lazy val eFrom = element[Int]
    lazy val eTo = new IndexStructKeyElem[Schema](self)
    lazy val selfType = new IndexStructKeyIsoElem[Schema](eSchema)
    def productArity = 1
    def productElement(n: Int) = eSchema
  }
  case class IndexStructKeyIsoElem[Schema <: Struct](eSchema: Elem[Schema]) extends Elem[IndexStructKeyIso[Schema]] {
    def getDefaultRep = reifyObject(new IndexStructKeyIso[Schema]()(eSchema))
    lazy val tag = {
      implicit val tagSchema = eSchema.tag
      weakTypeTag[IndexStructKeyIso[Schema]]
    }
    lazy val typeArgs = TypeArgs("Schema" -> (eSchema -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class IndexStructKeyCompanionCtor extends CompanionDef[IndexStructKeyCompanionCtor] {
    def selfType = IndexStructKeyCompanionElem
    override def toString = "IndexStructKeyCompanion"

    @scalan.OverloadId("fromFields")
    def apply[Schema <: Struct](index: Rep[Int])(implicit eSchema: Elem[Schema]): Rep[IndexStructKey[Schema]] =
      mkIndexStructKey(index)

    def unapply[Schema <: Struct](p: Rep[StructKey[Schema]]) = unmkIndexStructKey(p)
  }
  lazy val IndexStructKeyRep: Rep[IndexStructKeyCompanionCtor] = new IndexStructKeyCompanionCtor
  lazy val IndexStructKey: IndexStructKeyCompanionCtor = proxyIndexStructKeyCompanion(IndexStructKeyRep)
  implicit def proxyIndexStructKeyCompanion(p: Rep[IndexStructKeyCompanionCtor]): IndexStructKeyCompanionCtor = {
    proxyOps[IndexStructKeyCompanionCtor](p)
  }

  implicit case object IndexStructKeyCompanionElem extends CompanionElem[IndexStructKeyCompanionCtor] {
    lazy val tag = weakTypeTag[IndexStructKeyCompanionCtor]
    protected def getDefaultRep = IndexStructKeyRep
  }

  implicit def proxyIndexStructKey[Schema <: Struct](p: Rep[IndexStructKey[Schema]]): IndexStructKey[Schema] =
    proxyOps[IndexStructKey[Schema]](p)

  implicit class ExtendedIndexStructKey[Schema <: Struct](p: Rep[IndexStructKey[Schema]])(implicit eSchema: Elem[Schema]) {
    def toData: Rep[IndexStructKeyData[Schema]] = isoIndexStructKey(eSchema).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoIndexStructKey[Schema <: Struct](implicit eSchema: Elem[Schema]): Iso[IndexStructKeyData[Schema], IndexStructKey[Schema]] =
    reifyObject(new IndexStructKeyIso[Schema]()(eSchema))

  case class NameStructKeyCtor[Schema <: Struct]
      (override val name: Rep[String])(implicit eSchema: Elem[Schema])
    extends NameStructKey[Schema](name) with Def[NameStructKey[Schema]] {
    lazy val selfType = element[NameStructKey[Schema]]
  }
  // elem for concrete class
  class NameStructKeyElem[Schema <: Struct](val iso: Iso[NameStructKeyData[Schema], NameStructKey[Schema]])(implicit override val eSchema: Elem[Schema])
    extends StructKeyElem[Schema, NameStructKey[Schema]]
    with ConcreteElem[NameStructKeyData[Schema], NameStructKey[Schema]] {
    override lazy val parent: Option[Elem[_]] = Some(structKeyElement(element[Schema]))
    override lazy val typeArgs = TypeArgs("Schema" -> (eSchema -> scalan.util.Invariant))

    override def convertStructKey(x: Rep[StructKey[Schema]]) = NameStructKey(x.name)
    override def getDefaultRep = NameStructKey("")
    override lazy val tag = {
      implicit val tagSchema = eSchema.tag
      weakTypeTag[NameStructKey[Schema]]
    }
  }

  // state representation type
  type NameStructKeyData[Schema <: Struct] = String

  // 3) Iso for concrete class
  class NameStructKeyIso[Schema <: Struct](implicit eSchema: Elem[Schema])
    extends EntityIso[NameStructKeyData[Schema], NameStructKey[Schema]] with Def[NameStructKeyIso[Schema]] {
    override def from(p: Rep[NameStructKey[Schema]]) =
      p.name
    override def to(p: Rep[String]) = {
      val name = p
      NameStructKey(name)
    }
    lazy val eFrom = element[String]
    lazy val eTo = new NameStructKeyElem[Schema](self)
    lazy val selfType = new NameStructKeyIsoElem[Schema](eSchema)
    def productArity = 1
    def productElement(n: Int) = eSchema
  }
  case class NameStructKeyIsoElem[Schema <: Struct](eSchema: Elem[Schema]) extends Elem[NameStructKeyIso[Schema]] {
    def getDefaultRep = reifyObject(new NameStructKeyIso[Schema]()(eSchema))
    lazy val tag = {
      implicit val tagSchema = eSchema.tag
      weakTypeTag[NameStructKeyIso[Schema]]
    }
    lazy val typeArgs = TypeArgs("Schema" -> (eSchema -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class NameStructKeyCompanionCtor extends CompanionDef[NameStructKeyCompanionCtor] {
    def selfType = NameStructKeyCompanionElem
    override def toString = "NameStructKeyCompanion"

    @scalan.OverloadId("fromFields")
    def apply[Schema <: Struct](name: Rep[String])(implicit eSchema: Elem[Schema]): Rep[NameStructKey[Schema]] =
      mkNameStructKey(name)

    def unapply[Schema <: Struct](p: Rep[StructKey[Schema]]) = unmkNameStructKey(p)
  }
  lazy val NameStructKeyRep: Rep[NameStructKeyCompanionCtor] = new NameStructKeyCompanionCtor
  lazy val NameStructKey: NameStructKeyCompanionCtor = proxyNameStructKeyCompanion(NameStructKeyRep)
  implicit def proxyNameStructKeyCompanion(p: Rep[NameStructKeyCompanionCtor]): NameStructKeyCompanionCtor = {
    proxyOps[NameStructKeyCompanionCtor](p)
  }

  implicit case object NameStructKeyCompanionElem extends CompanionElem[NameStructKeyCompanionCtor] {
    lazy val tag = weakTypeTag[NameStructKeyCompanionCtor]
    protected def getDefaultRep = NameStructKeyRep
  }

  implicit def proxyNameStructKey[Schema <: Struct](p: Rep[NameStructKey[Schema]]): NameStructKey[Schema] =
    proxyOps[NameStructKey[Schema]](p)

  implicit class ExtendedNameStructKey[Schema <: Struct](p: Rep[NameStructKey[Schema]])(implicit eSchema: Elem[Schema]) {
    def toData: Rep[NameStructKeyData[Schema]] = isoNameStructKey(eSchema).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoNameStructKey[Schema <: Struct](implicit eSchema: Elem[Schema]): Iso[NameStructKeyData[Schema], NameStructKey[Schema]] =
    reifyObject(new NameStructKeyIso[Schema]()(eSchema))

  registerModule(StructKeys_Module)

  lazy val StructKey: Rep[StructKeyCompanionCtor] = new StructKeyCompanionCtor {
  }

  object IndexStructKeyMethods {
    object name {
      def unapply(d: Def[_]): Option[Rep[IndexStructKey[Schema]] forSome {type Schema <: Struct}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[IndexStructKeyElem[_]] && method.getName == "name" =>
          Some(receiver).asInstanceOf[Option[Rep[IndexStructKey[Schema]] forSome {type Schema <: Struct}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[IndexStructKey[Schema]] forSome {type Schema <: Struct}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `toString`: Overrides Object method
  }

  def mkIndexStructKey[Schema <: Struct]
    (index: Rep[Int])(implicit eSchema: Elem[Schema]): Rep[IndexStructKey[Schema]] = {
    new IndexStructKeyCtor[Schema](index)
  }
  def unmkIndexStructKey[Schema <: Struct](p: Rep[StructKey[Schema]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: IndexStructKeyElem[Schema] @unchecked =>
      Some((p.asRep[IndexStructKey[Schema]].index))
    case _ =>
      None
  }

  object NameStructKeyMethods {
    object index {
      def unapply(d: Def[_]): Option[Rep[NameStructKey[Schema]] forSome {type Schema <: Struct}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[NameStructKeyElem[_]] && method.getName == "index" =>
          Some(receiver).asInstanceOf[Option[Rep[NameStructKey[Schema]] forSome {type Schema <: Struct}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[NameStructKey[Schema]] forSome {type Schema <: Struct}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `toString`: Overrides Object method
  }

  def mkNameStructKey[Schema <: Struct]
    (name: Rep[String])(implicit eSchema: Elem[Schema]): Rep[NameStructKey[Schema]] = {
    new NameStructKeyCtor[Schema](name)
  }
  def unmkNameStructKey[Schema <: Struct](p: Rep[StructKey[Schema]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: NameStructKeyElem[Schema] @unchecked =>
      Some((p.asRep[NameStructKey[Schema]].name))
    case _ =>
      None
  }

  object StructKeyMethods {
    object index {
      def unapply(d: Def[_]): Option[Rep[StructKey[Schema]] forSome {type Schema <: Struct}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[StructKeyElem[_, _]] && method.getName == "index" =>
          Some(receiver).asInstanceOf[Option[Rep[StructKey[Schema]] forSome {type Schema <: Struct}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[StructKey[Schema]] forSome {type Schema <: Struct}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object name {
      def unapply(d: Def[_]): Option[Rep[StructKey[Schema]] forSome {type Schema <: Struct}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[StructKeyElem[_, _]] && method.getName == "name" =>
          Some(receiver).asInstanceOf[Option[Rep[StructKey[Schema]] forSome {type Schema <: Struct}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[StructKey[Schema]] forSome {type Schema <: Struct}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}

object StructKeys_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAAL1WS2wbRRj+7cTxK62SICJ6aAmuK17FjlpFBQWEQuOgFDexun0gUwHj3bGzZV/s/k7XHMqJHuCCEOKAxKGiFZcICXEpIHEBJIRQD1w5c0ANqOqhlZBA/DP7sJ3YbQCVPYxmZ2f/x/d9//yz8RukPBdmPJUZzCqZHFlJkfMFD4vKcVtrG3yRN1c339r368cHbyVhsg5ja8xb9Iw6ZINJxXfiuYJaFSaXdEurWKhjp2hKEwilauCjLHyUB/ko9vw1X4Uss1Tuoe16CA8FP5dV2zC4irptlXXTbCNrGLxc1T2k/aMNW+u8DhcgWYUJ1bZUlyNXjhrM87gXrme4MK/H71n53ll1uj62B3jSZTpSfORjIth/gjtKx7KtjomwOwxt1RFh0Z489x0CYtl0DOlmtApp3XRsFyOvafKwZmvR66jFaAGmqufYOiuT11ZZQVe3WsKYw9TXWIuv0BaxPUU5eNxonuw4PDSe91Dr8+c7AOAQq4dkZKUuaKUYtJIArahwV2eG/gYTH2uu7XcgeBIjAL4wcfAuJiILvGJpxbfPqi/dVvJmUvzsi1gyMqI0GXpwiMIkPwTu9yfe824+f+lIEnJ1yOneQsNDl6nYq4MQrzyzLBtlzDGEzG0RhYVhFEovC7Rni06yqm06zCJLIZjjxJShqzqKzWJtV8jPEPDT6PBoa9J3EnG+wypKiukoM4za9T1PHNisvJiMJRC6yJJJhUrKjYwiZEkPbRVf4J3QvhgnEMYUdY2bTCIthpzfHTN3CCKG4+Hrv2vfzcLZJCRCEEOfO+ONTEw9+eGXB3jt0yRk6lLnSwZrSQYFSovcU+uQsde5G6yn15khZgNZTGu8ydoGhtj2gjJCoCDMDC1RhwvE5qX0E1H6+UC8K7bFi0u14i3lh/c3hDZdGA++BDX7l37kz593N1HKFiFFpxD3I4hHqNZjMPYPI9XhNVc36WRZ53PffHXqxtcrKcnrVJjRaWa0eVDUYULd5ITPxCx5WrZwG4li2Cf9T/dQvycRZSm/I6R5oIQo6tGKwc2daYVOsGWRcCyxrkuR8t7hKRP7m39c/kJ598pTSRg7BqkmEetVIdWw25YWFRSdxMh9fC5a26J2KiDmMjM+oNcZHShU8AjTEdlt1I3y6XA9oJieGaDocgGPim3yycJN/eVL76CUcsLvP09XG+fo+JqXKe/tIiJTjhHJ9INE5qfD5EUEpWUriA2Lj1/dOK9fe3RJCiZAUdocT/RD/u/KdhvZsIVsCd8AfYpxP0KyUAhyC8X2v+tpl2hWA+UkxkMxfeJtbodkiOHpXjd3PaCirv3ZxYv337j8yn2ysWQaOprMKc7+g7YSdYF72DagH9oRCvu/K6lPT11cJR99PeUeMSTGV7u254ir0hCuFrlqMJdr4g7DTbpjBSwc/uDZM8ceOHNKlvS4JjcFX+JWMPhGeJw58/L68sgdri+0qVgxHeyIyeFvn/npzR8/uRKXdJRiros8wmQYvxMd9l6cWmFIakpIPFF64fZHK49d+/wX2RlyQkLUmKz4VtjbEfpZTgcxeF0+A+qFkx7qqUyFzHpYk8P5vwGgJNZr5AsAAA=="
}
}

