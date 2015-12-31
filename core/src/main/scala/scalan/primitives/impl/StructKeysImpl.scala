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
  implicit def proxyStructKey(p: Rep[StructKey]): StructKey = {
    proxyOps[StructKey](p)(scala.reflect.classTag[StructKey])
  }

  // familyElem
  class StructKeyElem[To <: StructKey]
    extends EntityElem[To] {
    lazy val parent: Option[Elem[_]] = None
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map()
    }
    override def isEntityType = true
    override lazy val tag = {
      weakTypeTag[StructKey].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[StructKey] => convertStructKey(x) }
      tryConvert(element[StructKey], this, x, conv)
    }

    def convertStructKey(x: Rep[StructKey]): Rep[To] = {
      x.selfType1 match {
        case _: StructKeyElem[_] => x.asRep[To]
        case e => !!!(s"Expected $x to have StructKeyElem[_], but got $e", x)
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def structKeyElement: Elem[StructKey] =
    cachedElem[StructKeyElem[StructKey]]()

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

  abstract class AbsIndexStructKey
      (keys: Rep[KeySet], index: Rep[Int])
    extends IndexStructKey(keys, index) with Def[IndexStructKey] {
    lazy val selfType = element[IndexStructKey]
  }
  // elem for concrete class
  class IndexStructKeyElem(val iso: Iso[IndexStructKeyData, IndexStructKey])
    extends StructKeyElem[IndexStructKey]
    with ConcreteElem[IndexStructKeyData, IndexStructKey] {
    override lazy val parent: Option[Elem[_]] = Some(structKeyElement)
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map()
    }

    override def convertStructKey(x: Rep[StructKey]) = IndexStructKey(x.keys, x.index)
    override def getDefaultRep = IndexStructKey(element[KeySet].defaultRepValue, 0)
    override lazy val tag = {
      weakTypeTag[IndexStructKey]
    }
  }

  // state representation type
  type IndexStructKeyData = (KeySet, Int)

  // 3) Iso for concrete class
  class IndexStructKeyIso
    extends EntityIso[IndexStructKeyData, IndexStructKey] with Def[IndexStructKeyIso] {
    override def from(p: Rep[IndexStructKey]) =
      (p.keys, p.index)
    override def to(p: Rep[(KeySet, Int)]) = {
      val Pair(keys, index) = p
      IndexStructKey(keys, index)
    }
    lazy val eFrom = pairElement(element[KeySet], element[Int])
    lazy val eTo = new IndexStructKeyElem(self)
    lazy val selfType = new IndexStructKeyIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class IndexStructKeyIsoElem() extends Elem[IndexStructKeyIso] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new IndexStructKeyIso())
    lazy val tag = {
      weakTypeTag[IndexStructKeyIso]
    }
  }
  // 4) constructor and deconstructor
  class IndexStructKeyCompanionAbs extends CompanionDef[IndexStructKeyCompanionAbs] {
    def selfType = IndexStructKeyCompanionElem
    override def toString = "IndexStructKey"
    def apply(p: Rep[IndexStructKeyData]): Rep[IndexStructKey] =
      isoIndexStructKey.to(p)
    def apply(keys: Rep[KeySet], index: Rep[Int]): Rep[IndexStructKey] =
      mkIndexStructKey(keys, index)
  }
  object IndexStructKeyMatcher {
    def unapply(p: Rep[StructKey]) = unmkIndexStructKey(p)
  }
  lazy val IndexStructKey: Rep[IndexStructKeyCompanionAbs] = new IndexStructKeyCompanionAbs
  implicit def proxyIndexStructKeyCompanion(p: Rep[IndexStructKeyCompanionAbs]): IndexStructKeyCompanionAbs = {
    proxyOps[IndexStructKeyCompanionAbs](p)
  }

  implicit case object IndexStructKeyCompanionElem extends CompanionElem[IndexStructKeyCompanionAbs] {
    lazy val tag = weakTypeTag[IndexStructKeyCompanionAbs]
    protected def getDefaultRep = IndexStructKey
  }

  implicit def proxyIndexStructKey(p: Rep[IndexStructKey]): IndexStructKey =
    proxyOps[IndexStructKey](p)

  implicit class ExtendedIndexStructKey(p: Rep[IndexStructKey]) {
    def toData: Rep[IndexStructKeyData] = isoIndexStructKey.from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoIndexStructKey: Iso[IndexStructKeyData, IndexStructKey] =
    reifyObject(new IndexStructKeyIso())

  // 6) smart constructor and deconstructor
  def mkIndexStructKey(keys: Rep[KeySet], index: Rep[Int]): Rep[IndexStructKey]
  def unmkIndexStructKey(p: Rep[StructKey]): Option[(Rep[KeySet], Rep[Int])]

  registerModule(StructKeys_Module)
}

// Seq -----------------------------------
trait StructKeysSeq extends StructKeysDsl {
  self: StructsDsl with ScalanSeq =>
  lazy val StructKey: Rep[StructKeyCompanionAbs] = new StructKeyCompanionAbs {
  }

  case class SeqIndexStructKey
      (override val keys: Rep[KeySet], override val index: Rep[Int])
    extends AbsIndexStructKey(keys, index) {
  }

  def mkIndexStructKey
    (keys: Rep[KeySet], index: Rep[Int]): Rep[IndexStructKey] =
    new SeqIndexStructKey(keys, index)
  def unmkIndexStructKey(p: Rep[StructKey]) = p match {
    case p: IndexStructKey @unchecked =>
      Some((p.keys, p.index))
    case _ => None
  }
}

// Exp -----------------------------------
trait StructKeysExp extends StructKeysDsl {
  self: StructsDsl with ScalanExp =>
  lazy val StructKey: Rep[StructKeyCompanionAbs] = new StructKeyCompanionAbs {
  }

  case class ExpIndexStructKey
      (override val keys: Rep[KeySet], override val index: Rep[Int])
    extends AbsIndexStructKey(keys, index)

  object IndexStructKeyMethods {
  }

  def mkIndexStructKey
    (keys: Rep[KeySet], index: Rep[Int]): Rep[IndexStructKey] =
    new ExpIndexStructKey(keys, index)
  def unmkIndexStructKey(p: Rep[StructKey]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: IndexStructKeyElem @unchecked =>
      Some((p.asRep[IndexStructKey].keys, p.asRep[IndexStructKey].index))
    case _ =>
      None
  }

  object StructKeyMethods {
    object keys {
      def unapply(d: Def[_]): Option[Rep[StructKey]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[StructKeyElem[_]] && method.getName == "keys" =>
          Some(receiver).asInstanceOf[Option[Rep[StructKey]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[StructKey]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object index {
      def unapply(d: Def[_]): Option[Rep[StructKey]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[StructKeyElem[_]] && method.getName == "index" =>
          Some(receiver).asInstanceOf[Option[Rep[StructKey]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[StructKey]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}

object StructKeys_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAALVVTYhbVRQ+SWYmk2Toz5SCnY11jIpFk6kgXcyiDNO0DMaZoa9KicVy83Inve19995592ZIuuiyi3Yn3RZacCN0I65EEEEEceFKRHDtqiqli3aleO59970kY58/i2YR7s+553znO98578FvMKtjeFWHhBPRiKghjcCt17SpBy1hmBm9K3sDTs/QnRfkV/dOfrL0eREOdmDuCtFnNO9AJVm0hipbB3S3DRUiQqqNjLWBl9ouQjOUnNPQMCmaLIoGhnQ5bbaZNqttmOnK3mgXbkChDYdCKcKYGhqsc6I11f58nlpELNtX3H60pcYxRNNm0ZzI4kJMmEH4GONQYn+eqmAkpBhFBg54aFvKwkKbMouUjE0aoozursheup0RBA9gsX2V7JEmhug3AxMz0ceXNUXCa6RPN9HEms8gYE35zoWRcvtSG6qa7iJBG5Hi7mSoAAAr8JYD0Rjz08j4aVh+6gGNGeHsOrGX27EcjiD5FUoAQ4Uu3vgXF6kH2hK9+q1L4QdPg1pUtI+HFkrZZTiHjl7MUYMrBfL47fmP9ONz908VodqBKtNrXW1iEprJknu2akQIaRzmjEAS97Fay3nVclHW0GafJCqhjBQR6MlTuYB14ixkxhrbswVfnRzqy0bR1LQwVIUs3+M5+TrdrBPOtx8ee/OVX1sXi1CcDlFBlwEKP06dGqigGgaheYc63GX7V/H85kfKcn7t4e+9b1bgUjFjyjv+b8VBF7P6px9rP7x+ugjzHSfls5z0O0iWbnEabcXrUpgOzMs9Gic35T3C7eqZxSr36A4ZcOMpnMy9hLkbOJ7bdIpaYladwAspAbVEo5tS0PrZ7fqT4Ls7D6wEY1hIbpIu/JOd+uPnAzvGqdPAzDU60g7SQQMlbF7Phj+ZQ7IDaib4djdHs8D2b8nALBM9OnyGnxhezpOAotsxi3Dk7NG3v/7ivUdfbs46FSx6Yt4nfECTAeB5GXNkoRdWMNKGMGMdeDU4TC7+otvjINqw8Kbks7Q/p39SQDrmPr158+ijjy8fce0532UmIqq+8j+aM+2l59h8MF3BEsKePslto8rEsFrM7hI6/9Z+S/seN6YPkfRqZo8z6bBnVqUl10h3NRFmICN6ePkx+/D+beO6szCc/gpsda/i2F11cY7hu+WcMgWeGEz5xtO7mye+/+wXp6iqpRj7QpipT41X0jQ7HrX91I7ZsJ2QxJnIGpvHVsLl/RckyoAo7QcAAA=="
}
}

