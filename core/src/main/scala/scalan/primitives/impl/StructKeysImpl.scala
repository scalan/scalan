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
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("Schema" -> Left(eSchema))
    }
    override def isEntityType = true
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
      x.selfType1 match {
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
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("Schema" -> Left(eSchema))
    }

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
    def isEntityType = true
    def getDefaultRep = reifyObject(new IndexStructKeyIso[Schema]()(eSchema))
    lazy val tag = {
      implicit val tagSchema = eSchema.tag
      weakTypeTag[IndexStructKeyIso[Schema]]
    }
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
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("Schema" -> Left(eSchema))
    }

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
    def isEntityType = true
    def getDefaultRep = reifyObject(new NameStructKeyIso[Schema]()(eSchema))
    lazy val tag = {
      implicit val tagSchema = eSchema.tag
      weakTypeTag[NameStructKeyIso[Schema]]
    }
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

// Std -----------------------------------
trait StructKeysStd extends StructKeysDsl {
  self: StructsDsl with ScalanStd =>
  lazy val StructKey: Rep[StructKeyCompanionAbs] = new StructKeyCompanionAbs {
  }

  case class StdIndexStructKey[Schema <: Struct]
      (override val index: Rep[Int])(implicit eSchema: Elem[Schema])
    extends AbsIndexStructKey[Schema](index) {
  }

  def mkIndexStructKey[Schema <: Struct]
    (index: Rep[Int])(implicit eSchema: Elem[Schema]): Rep[IndexStructKey[Schema]] =
    new StdIndexStructKey[Schema](index)
  def unmkIndexStructKey[Schema <: Struct](p: Rep[StructKey[Schema]]) = p match {
    case p: IndexStructKey[Schema] @unchecked =>
      Some((p.index))
    case _ => None
  }

  case class StdNameStructKey[Schema <: Struct]
      (override val name: Rep[String])(implicit eSchema: Elem[Schema])
    extends AbsNameStructKey[Schema](name) {
  }

  def mkNameStructKey[Schema <: Struct]
    (name: Rep[String])(implicit eSchema: Elem[Schema]): Rep[NameStructKey[Schema]] =
    new StdNameStructKey[Schema](name)
  def unmkNameStructKey[Schema <: Struct](p: Rep[StructKey[Schema]]) = p match {
    case p: NameStructKey[Schema] @unchecked =>
      Some((p.name))
    case _ => None
  }
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
  val dump = "H4sIAAAAAAAAAL1WS2wbRRj+147jV+gLikBQkRrzKAI7UKEeLFSF1EUpbhJlC0ImAo13x86W2dllZxytOZRbDnBDXDlUgBBShYQ4IvUCSBVCHBBCSHDlVIqqHNoTiH9mH7ZbO7S8fBjNzP7+H9/3zT9z4QrkRAAPC4swwmsulaRm6vmikFWzyaUjB6c9u8/oCdotX2sc/nl7aTkDe9swu0nECcHaUIwmzdBP56a0W1Ak3KJCeoGQcLilI9QtjzFqScfjdcd1+5J0GK23HCEbLZjpePbgdTgHRgv2WR63AiqpucSIEFTE+wWqMnLSdVGvB6v+MAavqyrqI1WcCYgjMX2MsS+yX6e+OeAeH7gS9sSprfoqLbTJO67vBTIJkUd3m56dLGc4wQ040DpLtkgdQ/Tqpgwc3sN/ln1ivUZ6dAVNlPkMJiwo654Z+HqdRRMhbQRo2fWZ9pgNfQBACp7SWdSGANVSgGoKoKpJA4cw5w2iPq4FXjiA6GdkAUIfXTz+Fy4SD7TJ7epbG9bL182ym1F/DlUqeZ3QLDp6YIocNBcI5KX1d8TOc+ePZaDUhpIjFjtCBsSSo5zHcJUJ557UOacIkqCHdFWm0aWjLKLNDZooWp7rE46eYiznkCjmWI5UxmpvLqZnCvZ56dPE1Ah9I613fkq9WjhLhLG1y/c+8dCvzZcykBkPUUSXJio/SJxKKKIc+pZ8ng5i/2rcK2HWtDapSzTSaiiGwzG/SxIpHI9c/s3+agE2MimIccxb4w1d5MSPP5S/P3I8A4W2lvlJRnptxFE0GXVXgyWPyzYUvC0aRF/yW4Sp2UQe8zbtkj6TMbqjsGQRFgnzUw+kTxVmDa19IwGgHMl3xeO0enKtes38+t0LSp0BzEVfohP6h3Ps95/2dKUWroScw20aJiBn8WSncDw4jVafrgWOi31kiz79xecvXL24ktPMHogrepGwPo1OdVzQsDgV01jASMtc3kSjGg7p+AdHyL/HSKrU3yXkaaSFJOsZBf+tqQX71bIqOBXZMKQq+dD0kpH/u9dbd7Erxy9mIHcKcl0kVrQg1/H63E6OFPZdSUP5bLJnjBOLR4gExE2UFzWgedW/ShFHpufS/ZUd55Xzb0stVCMcb5arnbPYnBq6nPuH1epy0mrzkwDQxmVjHKe/d9puYghuYEjXPEFUaqxIyFQqUdKxQv53EdyhrpiJGlDjkykvanX0tlAewXq3lpLcqZ9ubx+8+sGrd+qroNBxpEv86sJtXARJ3/4PGz2Mo5rFtP+5iMakNIRUUzF2C/z75KhxQ7sdtS4Ni5CwP6bNT/qciKMFUJnCqBljiOicu/7eymPffvaLboolxQb2ZJ6+iEab4ThgcQ7qCThEJwJSxRmpBPWuSEuzOjItK2mrtxJ1MX4knp1THxfv+6Tzvu4uBfWcohYT6W0z+Yl5mvgN/UZ6dJc3EhpVm66PD16cHP3yme/e/OajD/U18yeK7Gk2JwsAAA=="
}
}

