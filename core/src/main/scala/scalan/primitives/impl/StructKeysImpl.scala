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
    lazy val typeArgs = TypeArgs("Schema" -> eSchema)
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
    override lazy val typeArgs = TypeArgs("Schema" -> eSchema)

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
    lazy val typeArgs = TypeArgs("Schema" -> eSchema)
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
    override lazy val typeArgs = TypeArgs("Schema" -> eSchema)

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
    lazy val typeArgs = TypeArgs("Schema" -> eSchema)
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
  val dump = "H4sIAAAAAAAAAL1WT4hbRRj/kmw2m2St29VKBYtrTFX8k6wuUmGRsnZT2Zpml32tlbgok/cm2VfnvTe+mSwvHip46EE9iXgQPBQUL0UQbwqloIKICHr17KlWSg/2pPjNvH9Jm6yt/3IY3ps3+b7v92e+mfOXIS98eECYhBG35lBJaoZ+XhGyajRcacvBcc/qM7pKuz88ctErvfF2KwtzbZjeJmJVsDYUw4dGwJNnQ1pNKBLXpEJ6vpBwX1NnqJseY9SUtufWbcfpS9JhtN60hVxuwlTHswavwhnINGHO9FzTp5IaRxgRgopofoaqiuzkvajfB+s8zeHWFYr6EIoTPrEllo855sL1m5QbA9dzB46EPVFp61yVhWvKNOCIYc3hTKfJNaFgO9zzZZy1gBm2PSt+nXIJTsB88zTZIXXM2qsb0rfdngrGifkK6dEWLlHLpxCDoKx7YsBpFLwspDWSL+AAgKo8oQurpZzVEs5qirOqQX2bMPs1oj5u+F4wgPCXyQEEHEM8+hch4gi04VrVN7fMF68ZZSer/hyoUgq6oGkMdO8Eh2h5kNtvNt8RV589dygLpTaUbLHSEdInphy2QURXmbiuJ3XNCYPE76GClUkK6iwruOY6mxRNz+HExUgRl7MoFLNNW6rFam42kmcC9wXJabw0E/BMgndhAl7tpSOEsY1Ldz928JfGC1nIjqYoYkgDN4MfB5VQRDv0TfkcHUTx1Xi7hGnD3KYO0UyroRikY2GXIhI6Hrz0q/X1ImxlExKjnDenG4aYf+r9zw/SjU+yMNPWNj/KSE8rqFhapcJsw4y3Q/1wvrBDmHoaq2LBol3SZzLidpiUHJIiYWHiDuVUMbasnZ+J4ZdD87Y8l1aPblR/M75997zypg+z4Zdwy/5hH/r9pz1dqW0rIW+7Fg1iinO41RMy7p8kKqcbvu1gY9mhT375xckrF1p5ret8hOh5wvo03NMRoBScyplZxExrrrxBRDUc0Pn3DUm/PxOj1N8lFGjohLjqqQajzs15BRvYmgKcWCxNqSAfmAwZ1b9rs3knu3z4QhbyxyDfRWFFE/Idr+9a8YbCRixpIJ+J5zKjwuIGIj5xYt+F7WdBda9SqJHhOXRv5ar90rm3pLZpJhhtleud09ialjWce1K0Gk6CtjCOAL24nBnl6e/ttRsUgusU0pjHmEqNFQnZSiUsOnLI/26C29QBM9YDanw80UW9Ld0Sy0Nc79ZQ4kP207Nn91358OU79EEw07GlQ3h18RaOgbhr/4dtHkZZzWHZ/9xEI1ZKKdVSjJwB/744atxKwy6hTLUJMq1SkxGfWuq2QR28DYUCLL13+NSx/adO6h06a+lF4Zeka4+/ux0nfFnfNB7a5aaBi6oNh+NNEh+Wvnr6x9e/+/gj3a6H0ZVS0iXsjerncV8WCbTKBGhGpDmqeebaB62Hv//sZ93ES8o9eIa4yf1tuHmPChzVoO6wqZqh8CrPEPO4P5XJhjTz1CD+BOkRpWVSCwAA"
}
}

