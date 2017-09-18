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

  registerModule(StructKeysModule)

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

object StructKeysModule extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAAL1WX2gcRRif28vl7nJpaGIN8X+bXlGr3AVLaSVISZtEUq9J6LYNxKLM7U4u287ujrtz6Z6E6lMf9E3EB9GHgtKXoIgvolLEKohIH3wTH6UgiFL6YFGw+M3M/rlLbtNEqPsw7M7OfN/3+36/+b5Z+x3lfA/t9g1MsVOxCccVXb5P+Lysn3DNJiWTZOm9v4wL84XDRQ0NLaLeZexP+nQRFdXLVMDid52bNTQ4bTnmlMMt3irb0gRHlZryURU+qt18lNt2jdfQiPhc8DBjxFtn6+DWbHVuBpNF7BjE567nc7RH2agaLqXE4JbrVC3bbnJcp6Ras3wO63vqrtl6BV1E2RraabiO4RFO9GMU+z7xw/kCEeat+Lsov1tzLPGxMc5THrY4hAk+dqr1JwnTW47rtGyOBsLQ5pgIC9aUSMAgtzM2o9JNrobyls1cj0de8+Bh2TWjzx4HwwQaqp3DK7gKXhtVnXuW0xDGGDbO4waZhSVieS9g8AldOtViJDRe8rnZ4S9gCCHGQCnPyNAqSdYqcdYqImtlnXgWptarWPyc99yghdSTySIUCBNP38VEZIFMOWb5jbPGi7f1kq2JzYEIpihDKoChx1JUKwmC7H538i3/1vOXD2mobxH1Wf5E3eceNni7EMKElbDjuFzGHOcQew3gcDSNQ+llAtasE0rRcG2GHbAUZrMfqKKWYXGxWMwNhASlZD/PGYmWZgOWifGmnVKxd4Ix2rq2enX1l4d/GtSUMAPmtZnNgtlN4EhJHsOUAhyNR87Ba5+iS3dtMjh6y3rp8ptcQ5kaygSdApurnwM6xwMP9asdSr53rEP//DywxLWQ/VQQkf8v8199/euNIz0a0jrzVAQAOtQaLwqOoyKoumnwF0grTJIYRzjq1Y1lYmMpFzGUpHIeWPdd3CScmN3Hf/vD/HYMnZWYpSai1GxJhmBi6PC7n+8j8x9pqLAoz+00xQ0pSMHOJPGNRVRwV4in5vMrmIq3rqLMm2QJNykPOW1Pj+J3dyq/jIjcjcujnInglxRVs65DytPz5T/1799eE0yJ/8Mc5aCKkiDKahaKVIx6bxqPjMx7lg0lcYUcvPbF6ZtXZ3OSyqEw9DOYNomqRmHkCQrBaWYMPM04POEpZEsM+6T/R9vY3pOJ4Mj/HOWJIj+KumeKEnsr8iiBoAdmBOBYVYlLAfmRdMhA88d/H72y98GH7mgofxzlloBBvyuFubrbdMyoNkBX4STgR6O5bCepUAuwh+242axgqI1QuzgajohucotWz4Tzil54dqOkVu5KcEtgMe5iZyoA4nAIURitzDjKHS8/9dnaBev6k9PyEKtcSZs7Mm2JHex0tr2zOb3hbG4gGq0jWuapizbFuJ8jbXRUIQ6F9r9raYfosF2lJMZn/ztPYpho93XXUhTdNz65dOn+mx+8fJ/siIW6xW3MymPb6IdR+7qH/Q6lSyoLEDqzvz2RqfF8d6kl2ZZUdXSXe0meGBuJg11AYyWFxkliUOwRU1zMiA0XR0XQgXeOLBwfWTgtW1S/KRepP3E/6H7NPYHZuGzKT2xyJYNF5Smb8ZZ4OfDNcz++9sOVD+NCEEHsS4jgQJuKn0WNwI+hjaZA00NNAMMXb78/u//6pzdk1+gT6oLu5MRX3fZu0Ul6XsXgJ6QoJQgnbZzDMRYKbGNtVQyv/ws3OYdjDA0AAA=="
}
}

