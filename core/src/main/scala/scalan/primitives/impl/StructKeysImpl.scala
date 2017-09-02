package scalan.primitives

import scalan._
import scalan.common._
import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait StructKeysDefs extends StructKeys {
  self: Structs with ScalanExp =>

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
    override def toString = "IndexStructKey"

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
    override def toString = "NameStructKey"

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
  val dump = "H4sIAAAAAAAAAL1WS2wbRRj+7cTxI2mVBBG1h5SQbsWr2FGrqEgBodA4KMVNrG4fYCpgvDtOtuyL3d/OmkM50QPcEOKAxKECxCVCQlwQSFwACSHUA1fOHFBDVfXQnkD8M/uwndhtAJU9jGZnZ//H933//LP1B2R8D2Z8jZnMLlocWVGV80UfFfW0ozdNvsQba9tvH/r946O30zBRg5EN5i/5Zg3y4aQcuMlcRb0CE8uGrZdtNLCtWNIEQrES+igJH6V+PpSuvxYqkGe2xn10PB/h4fDnkuaYJtfQcOySYVlNZHWTlyqGj7R/uO7o7TfgMqQrMK45tuZx5OpJk/k+96P1HBfmjeQ9L9/ba27Hx+4Az3rMQIqPfIyH+89wV23bjt22EPZHoa25IizaM8oDl4BYsVxTuhmuQNawXMfD2GuWPGw4evw6bDNagMnKJdZiJfK6XlLRM+x1Ycxl2utsna/SFrE9Qzn43Gycbbs8Mj7qo97jL3ABwCVWj8nIih3QigloRQGaonLPYKbxJhMfq54TtCF8UkMAgTBx9B4mYgu8bOvKOxe1l++oo1Za/ByIWHIyoiwZemiAwiQ/BO6PZ97zbz1/9UQaCjUoGP5i3UePaditgwivUWbbDsqYEwiZt04Uzg6iUHpZpD07dJLXHMtlNlmKwBwjpkxDM1BsFmv7In4GgJ9Fl8db04GbSvIdVFFSTCeZaVavH3zyyHb5xXQigchFnkyqVFJebBQhT3poavgCb0f2xTiOMKJqG9xiEmkxFILOmLtLEAkcj1y/of8wBxfTkIpAjHzujTcyMfnUh18f4dXP05CrSZ0vm2xdMihQWuK+VoOc0+JeuJ5tMVPM+rKY1XmDNU2MsO0GZYhAQZgZWKIuF4gtSOmn4vRHQ/GuOjZXlqvKbfWn97eENj0YC7+ENfuXceLPX/c3UMoWIUOnEA9iiIeo1hMwDg8i1eVVz7DoZGnx+e++OXfz29WM5HUyyug8M5s8LOoooU5ywmdqjjyt2LiLRDEckv6nuqg/mIqzlN8RsjxUQhz1cNnk1t60QifYikg4kVjHpUh5enDKxP5L23rxwI3pzTSMnIJMg4j1K5CpO01bjwuKTmLkAT4Xr+1QOxUQ85iVHNAtRgcKFTzCVEx2Ew2zdD5aDymmZwYoukLIo+pYfGL2lvHK1XdRSjkV9J6na/VLdHwtyJSnO4jIlBNEcr0gkfmpKHkRQXHFDmND5YmvtjaNa48tS8GEKEqbY6leyP9d2e4iG3aQLeHro08xHkZIz86GuUVi+9/1tE80q75yEuOxhD7xNr9HMsTwdLebex5Qcdf+4sqVB29+8uoDsrHk6gZazFXm/kFbibvAfWwb0AvtEIX935XUo6cOrpKPnp5ynxgS42sd2/PEVXEAV0tcM5nHdXGH4RbdsUIWjn/w7IVTBy6ckyU9pstN4ZekFfS/EZ5m7oK8vjx6l+sLbVLKlottMTn+/TO/vPXzZ58mJR2nWOggjzARxe/Gh72fpDY7IDU1Ip4ovXzno9XHr335m+wMBSEhakx2civs7gi9LGfDGPwOn1IS0km5K1xRfMNCaV3EyWHzb/b07W3nCwAA"
}
}

