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

    def apply[Schema <: Struct](index: Rep[Int])(implicit eSchema: Elem[Schema]): Rep[IndexStructKey[Schema]] =
      mkIndexStructKey(index)
  }
  object IndexStructKeyMatcher {
    def unapply[Schema <: Struct](p: Rep[StructKey[Schema]]) = unmkIndexStructKey(p)
  }
  lazy val IndexStructKey: Rep[IndexStructKeyCompanionAbs] = new IndexStructKeyCompanionAbs
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

  registerModule(StructKeys_Module)
}

// Seq -----------------------------------
trait StructKeysSeq extends StructKeysDsl {
  self: StructsDsl with ScalanSeq =>
  lazy val StructKey: Rep[StructKeyCompanionAbs] = new StructKeyCompanionAbs {
  }

  case class SeqIndexStructKey[Schema <: Struct]
      (override val index: Rep[Int])(implicit eSchema: Elem[Schema])
    extends AbsIndexStructKey[Schema](index) {
  }

  def mkIndexStructKey[Schema <: Struct]
    (index: Rep[Int])(implicit eSchema: Elem[Schema]): Rep[IndexStructKey[Schema]] =
    new SeqIndexStructKey[Schema](index)
  def unmkIndexStructKey[Schema <: Struct](p: Rep[StructKey[Schema]]) = p match {
    case p: IndexStructKey[Schema] @unchecked =>
      Some((p.index))
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
  }
}

object StructKeys_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAALVWTYgcRRR+PbOzvTOz5E8jUVxc11FRdGajSA57COtmIqvt7rIdRcag1PTUzFasru7tqll6POSYg97Eq2DAi5CLeBIhCCKIB08igmdPMRJyMCfFV9V/MzFtouAeiq7q6vfe9/Pe7JXrUJMRPCE9wolo+1SRtmue16VquV2hmJq8GgzGnJ6hwxPBVx+f/PShLypwuAfze0SekbwH9eShG4f5s0v3HagT4VGpgkgqeNQxGTpewDn1FAtEh/n+WJE+px2HSbXmwFw/GEz24SJYDhzxAuFFVFF3gxMpqUzPF6iuiOX7utlPtsMih+hoFJ0pFOciwhSWjzmOJPd3aehORCAmvoJDaWnboS4L79jMD4NIZSlsDLcXDLLtnCB4AMecC+SAdDDFqOOqiIkRftkMifcOGdEtvKKvz2HBkvLhuUlo9lUHGpLuI0GbfsjNSRwCACrwnCmiXfDTzvlpa35aLo0Y4exdol/uREE8geTPqgLEIYZ45i4hsgi0Kwat9857b95ym35FfxzrUmyDcB4DPVLiBiMF8vjt7gfy5kuXT1Wg0YMGk+t9qSLiqWnJU7aaRIhAmZpzAkk0QrVWytQyWdbxzm2WqHuBHxKBkVIqF1Enzjym9GV9tpiqU0K9rUKaXbXi0MrxLpfgNb7ZIJzvXHvw2cd/7b5RgcpsijqGdNH4URZUQR3dMPbUK3SSxtfrYQXzrrdHfWKY1ks9Llb7H4rI6Xjy2m+Db1bhfCUnMc15b7phiJr86cfmD0+drsBCz7j8LCejHvIou5z629FGIFQPFoIDGiVv7APC9dMddbQHdEjGXKXsTtNSRVoULJf2Y0g1Z2vG+1ZGQDOx71YgaOvsTut397sPr2h3RrCYvEka9E926o+fDw2VMa6CGhMDGmckV7GxczoeK5M1pDsR83GMHNAXvv7ytRtXt2pG2WMpotcJH9OkqVNABTid01rFTJtC/U1GvSyZ/MenxD9hZSjNewU2TbyQVT2n6b83t+C42tSAc5MVKTXkpXLIqP8Du879/PrpqxWovQy1IQorHaj1g7EYZC2FY1fRWL2YnVmzwmILkYj4mfOSAbSs51cj0cgNfHp05SZ76/L7yhjVimdn5Xb/Ag6nNQPn4QKtgZOjtcsJuKvZs2H/2aVLx2988vZ9Zkgt9JnySdha/RcjKpso/+MIglnRq1j27El9Ruc7zA3jtYI344bw9u+WpoKezDXTu+f/gwJ6XTNhp283irGn4GiqTZi1mUyzRbBSIpubEoUUXLz10dbT33/+i+nJhqYcR4LIf4+ne3GWrbQG/Q9IwU7STDrPFBLsOa2MQfEXbNVjagMJAAA="
}
}

