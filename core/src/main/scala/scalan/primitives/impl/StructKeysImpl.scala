package scalan.primitives

import scalan._
import scalan.common._
import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait StructKeysAbs extends StructKeys {
  self: StructsDsl with Scalan =>

  // single proxy for each type family
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

  implicit case object StructKeyCompanionElem extends CompanionElem[StructKeyCompanionAbs] {
    lazy val tag = weakTypeTag[StructKeyCompanionAbs]
    protected def getDefaultRep = StructKey
  }

  abstract class StructKeyCompanionAbs extends CompanionDef[StructKeyCompanionAbs] {
    def selfType = StructKeyCompanionElem
    override def toString = "StructKey"
  }
  def StructKey: Rep[StructKeyCompanionAbs]
  implicit def proxyStructKeyCompanionAbs(p: Rep[StructKeyCompanionAbs]): StructKeyCompanionAbs =
    proxyOps[StructKeyCompanionAbs](p)

  abstract class AbsIndexStructKey[Schema <: Struct]
      (index: Rep[Int])(implicit eSchema: Elem[Schema])
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
  class IndexStructKeyCompanionAbs extends CompanionDef[IndexStructKeyCompanionAbs] {
    def selfType = IndexStructKeyCompanionElem
    override def toString = "IndexStructKey"

    @scalan.OverloadId("fromFields")
    def apply[Schema <: Struct](index: Rep[Int])(implicit eSchema: Elem[Schema]): Rep[IndexStructKey[Schema]] =
      mkIndexStructKey(index)

    def unapply[Schema <: Struct](p: Rep[StructKey[Schema]]) = unmkIndexStructKey(p)
  }
  lazy val IndexStructKeyRep: Rep[IndexStructKeyCompanionAbs] = new IndexStructKeyCompanionAbs
  lazy val IndexStructKey: IndexStructKeyCompanionAbs = proxyIndexStructKeyCompanion(IndexStructKeyRep)
  implicit def proxyIndexStructKeyCompanion(p: Rep[IndexStructKeyCompanionAbs]): IndexStructKeyCompanionAbs = {
    proxyOps[IndexStructKeyCompanionAbs](p)
  }

  implicit case object IndexStructKeyCompanionElem extends CompanionElem[IndexStructKeyCompanionAbs] {
    lazy val tag = weakTypeTag[IndexStructKeyCompanionAbs]
    protected def getDefaultRep = IndexStructKey
  }

  implicit def proxyIndexStructKey[Schema <: Struct](p: Rep[IndexStructKey[Schema]]): IndexStructKey[Schema] =
    proxyOps[IndexStructKey[Schema]](p)

  implicit class ExtendedIndexStructKey[Schema <: Struct](p: Rep[IndexStructKey[Schema]])(implicit eSchema: Elem[Schema]) {
    def toData: Rep[IndexStructKeyData[Schema]] = isoIndexStructKey(eSchema).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoIndexStructKey[Schema <: Struct](implicit eSchema: Elem[Schema]): Iso[IndexStructKeyData[Schema], IndexStructKey[Schema]] =
    reifyObject(new IndexStructKeyIso[Schema]()(eSchema))

  // 6) smart constructor and deconstructor
  def mkIndexStructKey[Schema <: Struct](index: Rep[Int])(implicit eSchema: Elem[Schema]): Rep[IndexStructKey[Schema]]
  def unmkIndexStructKey[Schema <: Struct](p: Rep[StructKey[Schema]]): Option[(Rep[Int])]

  abstract class AbsNameStructKey[Schema <: Struct]
      (name: Rep[String])(implicit eSchema: Elem[Schema])
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
  class NameStructKeyCompanionAbs extends CompanionDef[NameStructKeyCompanionAbs] {
    def selfType = NameStructKeyCompanionElem
    override def toString = "NameStructKey"

    @scalan.OverloadId("fromFields")
    def apply[Schema <: Struct](name: Rep[String])(implicit eSchema: Elem[Schema]): Rep[NameStructKey[Schema]] =
      mkNameStructKey(name)

    def unapply[Schema <: Struct](p: Rep[StructKey[Schema]]) = unmkNameStructKey(p)
  }
  lazy val NameStructKeyRep: Rep[NameStructKeyCompanionAbs] = new NameStructKeyCompanionAbs
  lazy val NameStructKey: NameStructKeyCompanionAbs = proxyNameStructKeyCompanion(NameStructKeyRep)
  implicit def proxyNameStructKeyCompanion(p: Rep[NameStructKeyCompanionAbs]): NameStructKeyCompanionAbs = {
    proxyOps[NameStructKeyCompanionAbs](p)
  }

  implicit case object NameStructKeyCompanionElem extends CompanionElem[NameStructKeyCompanionAbs] {
    lazy val tag = weakTypeTag[NameStructKeyCompanionAbs]
    protected def getDefaultRep = NameStructKey
  }

  implicit def proxyNameStructKey[Schema <: Struct](p: Rep[NameStructKey[Schema]]): NameStructKey[Schema] =
    proxyOps[NameStructKey[Schema]](p)

  implicit class ExtendedNameStructKey[Schema <: Struct](p: Rep[NameStructKey[Schema]])(implicit eSchema: Elem[Schema]) {
    def toData: Rep[NameStructKeyData[Schema]] = isoNameStructKey(eSchema).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoNameStructKey[Schema <: Struct](implicit eSchema: Elem[Schema]): Iso[NameStructKeyData[Schema], NameStructKey[Schema]] =
    reifyObject(new NameStructKeyIso[Schema]()(eSchema))

  // 6) smart constructor and deconstructor
  def mkNameStructKey[Schema <: Struct](name: Rep[String])(implicit eSchema: Elem[Schema]): Rep[NameStructKey[Schema]]
  def unmkNameStructKey[Schema <: Struct](p: Rep[StructKey[Schema]]): Option[(Rep[String])]

  registerModule(StructKeys_Module)
}

// Exp -----------------------------------
trait StructKeysExp extends StructKeysDsl {
  self: StructsDsl with ScalanExp =>

  lazy val StructKey: Rep[StructKeyCompanionAbs] = new StructKeyCompanionAbs {
  }

  case class ExpIndexStructKey[Schema <: Struct]
      (override val index: Rep[Int])(implicit eSchema: Elem[Schema])
    extends AbsIndexStructKey[Schema](index)

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
    (index: Rep[Int])(implicit eSchema: Elem[Schema]): Rep[IndexStructKey[Schema]] =
    new ExpIndexStructKey[Schema](index)
  def unmkIndexStructKey[Schema <: Struct](p: Rep[StructKey[Schema]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: IndexStructKeyElem[Schema] @unchecked =>
      Some((p.asRep[IndexStructKey[Schema]].index))
    case _ =>
      None
  }

  case class ExpNameStructKey[Schema <: Struct]
      (override val name: Rep[String])(implicit eSchema: Elem[Schema])
    extends AbsNameStructKey[Schema](name)

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
    (name: Rep[String])(implicit eSchema: Elem[Schema]): Rep[NameStructKey[Schema]] =
    new ExpNameStructKey[Schema](name)
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
  val dump = "H4sIAAAAAAAAAL1WTWwbRRSetZM4tkNJU7UKEhHBuKL82YEWFclCEBoHpbiJ1e0PmIpovDt2tuzODjtjd82h3HqAG0IckDhUAnGJkBAXBBIXqFQhlAMSJ84cUEtV9UBPIN7M/nnbuA2g4sNod3b8fr7ve+/N5u9onHtonhvYxrTiEIErunpe5KKsH3PNnk2WSOe3n9df3MO3DmTQdAtNbGC+xO0WygcPdZ/Fz7owGyiPqUG4cD0u0MMNZbtquLZNDGG5tGo5Tk/gtk2qDYuLWgONtV1z8BY6j7QGmjZcanhEEP2IjTknPNyfJFRYworf8+p9sMYSH7Qq468OxX/Cw5aA8MHHdHD+OGH6gLp04Ai0Kwxtjcmw4EyR+AxyWHGYrdxkGyhnOcz1ROQ1Bx42XDN6HaMYNtBM4yzu4yp47VZ14Vm0K40xbLyJu2QVjsjjY5ADJ3bnxICR0HiRCzPlz2cIIeDjGRVYJcGsEmNWkZiVdeJZ2LbexvJj03P9AQp+WhYhn4GJJ+9iIrJA6tQsv3vGeP2mXnQy8s++DCWnApoAQw+N0IaiB7C9fPx9fuPli4czqNBCBYsvtrnwsCGGZRDCVcSUukLFHCOIvS4wWBrFoPKyCGdukUnecB2GKVgKsZwComzLsIQ8LPemQnpGYJ8TjERHNZ9pcb6jakFp6Qi27eaVB57af7X+agZl0i7yYFKHYvAiowLlQQ49Q7xCBqF9ud4v0IRubBAHK6TlkveTNXeHIGI4Hr1yzby0gM5kYhBDnzvjDUzMPPfR1/tJ8/MMmmwpmS/buKsYlCgtEW600KTbJ16wn+tjWz5ty2LOJB3cs0WI7TAoWQBFoPmRFcqIRKymlK9F6RcD8a66lJSXm+U/9B8+2JTa9NBU8CUo2b+sw3/+sqsjlGwFGreoSfwI4iyUegzGI6NIZaTpWQ40lj559rtvTl7/dnVc8ToTZnQK2z0S1HSYUJKc9KktgKcVKm4jUS5zyv/eIepntShL9V2gHAmUEEU9VreJszOtQANbkQnHEktcypTnRqcM7L921azMXps7l0ETR9F4B4jlDTTednvUjAoKGrEgvngp2tPSxEIBYQ87cX/uY2goUPAC7YvI7gnLrp4K9wOK4TcvO1wh4FF3HbK7dMN64+J7QklZ89PtdK19FtpXTaX8YIKISjlGJJcGCczvC5OXEVRWaBCbKD/x1eY5a+uxZSWYAEVls6ilIf93ZXsb2egWshV82+hTriWBMqVSkFsotv9dT/fJWbWtnOT6dEyffDu0QzLkUht2c9cGFQ3tLy5c2Hv9k/U9arBMti3hYFZe+AdjJZoC93BsoDS0WQj7vysppacEV8VHaqbcI4bkup7YPgRcVUZwtUQMG3vElFcY4sAVK2Dh4IcvnD46e/qkKukpUx0KvsSjYPsL4THMaur6cuAO1xc4VK47TAzkw8Hvn//pnR8/+zQu6SjFQoK8QLvD+FnU7HmcWmlEanpIPFB6/ubHq49vffmrmgwFKSEYTDS+FA5PhDTLYQzyYpxQGrAv/QyxD5UqlTZEnCeX/t8f8pyIoQsAAA=="
}
}

