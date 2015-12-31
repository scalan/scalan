package scalan.primitives

import scala.annotation.unchecked.uncheckedVariance
import scalan._
import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait StructItemsAbs extends StructItems {
  self: StructsDsl with Scalan =>

  // single proxy for each type family
  implicit def proxyStructItem[Val, Schema](p: Rep[StructItem[Val, Schema]]): StructItem[Val, Schema] = {
    proxyOps[StructItem[Val, Schema]](p)(scala.reflect.classTag[StructItem[Val, Schema]])
  }

  // familyElem
  class StructItemElem[Val, Schema, To <: StructItem[Val, Schema]](implicit _eVal: Elem[Val @uncheckedVariance], _eSchema: Elem[Schema])
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

  implicit def structItemElement[Val, Schema](implicit eVal: Elem[Val @uncheckedVariance], eSchema: Elem[Schema]): Elem[StructItem[Val, Schema]] =
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

  abstract class AbsStructItemBase[Val, Schema]
      (key: Rep[StructKey], value: Rep[Val])(implicit eVal: Elem[Val], eSchema: Elem[Schema])
    extends StructItemBase[Val, Schema](key, value) with Def[StructItemBase[Val, Schema]] {
    lazy val selfType = element[StructItemBase[Val, Schema]]
  }
  // elem for concrete class
  class StructItemBaseElem[Val, Schema](val iso: Iso[StructItemBaseData[Val, Schema], StructItemBase[Val, Schema]])(implicit override val eVal: Elem[Val], override val eSchema: Elem[Schema])
    extends StructItemElem[Val, Schema, StructItemBase[Val, Schema]]
    with ConcreteElem[StructItemBaseData[Val, Schema], StructItemBase[Val, Schema]] {
    override lazy val parent: Option[Elem[_]] = Some(structItemElement(element[Val], element[Schema]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("Val" -> Left(eVal), "Schema" -> Left(eSchema))
    }

    override def convertStructItem(x: Rep[StructItem[Val, Schema]]) = StructItemBase(x.key, x.value)
    override def getDefaultRep = StructItemBase(element[StructKey].defaultRepValue, element[Val].defaultRepValue)
    override lazy val tag = {
      implicit val tagVal = eVal.tag
      implicit val tagSchema = eSchema.tag
      weakTypeTag[StructItemBase[Val, Schema]]
    }
  }

  // state representation type
  type StructItemBaseData[Val, Schema] = (StructKey, Val)

  // 3) Iso for concrete class
  class StructItemBaseIso[Val, Schema](implicit eVal: Elem[Val], eSchema: Elem[Schema])
    extends EntityIso[StructItemBaseData[Val, Schema], StructItemBase[Val, Schema]] with Def[StructItemBaseIso[Val, Schema]] {
    override def from(p: Rep[StructItemBase[Val, Schema]]) =
      (p.key, p.value)
    override def to(p: Rep[(StructKey, Val)]) = {
      val Pair(key, value) = p
      StructItemBase(key, value)
    }
    lazy val eFrom = pairElement(element[StructKey], element[Val])
    lazy val eTo = new StructItemBaseElem[Val, Schema](self)
    lazy val selfType = new StructItemBaseIsoElem[Val, Schema](eVal, eSchema)
    def productArity = 2
    def productElement(n: Int) = (eVal, eSchema).productElement(n)
  }
  case class StructItemBaseIsoElem[Val, Schema](eVal: Elem[Val], eSchema: Elem[Schema]) extends Elem[StructItemBaseIso[Val, Schema]] {
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
    def apply[Val, Schema](p: Rep[StructItemBaseData[Val, Schema]])(implicit eVal: Elem[Val], eSchema: Elem[Schema]): Rep[StructItemBase[Val, Schema]] =
      isoStructItemBase(eVal, eSchema).to(p)
    def apply[Val, Schema](key: Rep[StructKey], value: Rep[Val])(implicit eVal: Elem[Val], eSchema: Elem[Schema]): Rep[StructItemBase[Val, Schema]] =
      mkStructItemBase(key, value)
  }
  object StructItemBaseMatcher {
    def unapply[Val, Schema](p: Rep[StructItem[Val, Schema]]) = unmkStructItemBase(p)
  }
  lazy val StructItemBase: Rep[StructItemBaseCompanionAbs] = new StructItemBaseCompanionAbs
  implicit def proxyStructItemBaseCompanion(p: Rep[StructItemBaseCompanionAbs]): StructItemBaseCompanionAbs = {
    proxyOps[StructItemBaseCompanionAbs](p)
  }

  implicit case object StructItemBaseCompanionElem extends CompanionElem[StructItemBaseCompanionAbs] {
    lazy val tag = weakTypeTag[StructItemBaseCompanionAbs]
    protected def getDefaultRep = StructItemBase
  }

  implicit def proxyStructItemBase[Val, Schema](p: Rep[StructItemBase[Val, Schema]]): StructItemBase[Val, Schema] =
    proxyOps[StructItemBase[Val, Schema]](p)

  implicit class ExtendedStructItemBase[Val, Schema](p: Rep[StructItemBase[Val, Schema]])(implicit eVal: Elem[Val], eSchema: Elem[Schema]) {
    def toData: Rep[StructItemBaseData[Val, Schema]] = isoStructItemBase(eVal, eSchema).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoStructItemBase[Val, Schema](implicit eVal: Elem[Val], eSchema: Elem[Schema]): Iso[StructItemBaseData[Val, Schema], StructItemBase[Val, Schema]] =
    reifyObject(new StructItemBaseIso[Val, Schema]()(eVal, eSchema))

  // 6) smart constructor and deconstructor
  def mkStructItemBase[Val, Schema](key: Rep[StructKey], value: Rep[Val])(implicit eVal: Elem[Val], eSchema: Elem[Schema]): Rep[StructItemBase[Val, Schema]]
  def unmkStructItemBase[Val, Schema](p: Rep[StructItem[Val, Schema]]): Option[(Rep[StructKey], Rep[Val])]

  registerModule(StructItems_Module)
}

// Seq -----------------------------------
trait StructItemsSeq extends StructItemsDsl {
  self: StructsDsl with ScalanSeq =>
  lazy val StructItem: Rep[StructItemCompanionAbs] = new StructItemCompanionAbs {
  }

  case class SeqStructItemBase[Val, Schema]
      (override val key: Rep[StructKey], override val value: Rep[Val])(implicit eVal: Elem[Val], eSchema: Elem[Schema])
    extends AbsStructItemBase[Val, Schema](key, value) {
  }

  def mkStructItemBase[Val, Schema]
    (key: Rep[StructKey], value: Rep[Val])(implicit eVal: Elem[Val], eSchema: Elem[Schema]): Rep[StructItemBase[Val, Schema]] =
    new SeqStructItemBase[Val, Schema](key, value)
  def unmkStructItemBase[Val, Schema](p: Rep[StructItem[Val, Schema]]) = p match {
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

  case class ExpStructItemBase[Val, Schema]
      (override val key: Rep[StructKey], override val value: Rep[Val])(implicit eVal: Elem[Val], eSchema: Elem[Schema])
    extends AbsStructItemBase[Val, Schema](key, value)

  object StructItemBaseMethods {
  }

  def mkStructItemBase[Val, Schema]
    (key: Rep[StructKey], value: Rep[Val])(implicit eVal: Elem[Val], eSchema: Elem[Schema]): Rep[StructItemBase[Val, Schema]] =
    new ExpStructItemBase[Val, Schema](key, value)
  def unmkStructItemBase[Val, Schema](p: Rep[StructItem[Val, Schema]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: StructItemBaseElem[Val, Schema] @unchecked =>
      Some((p.asRep[StructItemBase[Val, Schema]].key, p.asRep[StructItemBase[Val, Schema]].value))
    case _ =>
      None
  }

  object StructItemMethods {
    object key {
      def unapply(d: Def[_]): Option[Rep[StructItem[Val, Schema]] forSome {type Val; type Schema}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[StructItemElem[_, _, _]] && method.getName == "key" =>
          Some(receiver).asInstanceOf[Option[Rep[StructItem[Val, Schema]] forSome {type Val; type Schema}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[StructItem[Val, Schema]] forSome {type Val; type Schema}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object value {
      def unapply(d: Def[_]): Option[Rep[StructItem[Val, Schema]] forSome {type Val; type Schema}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[StructItemElem[_, _, _]] && method.getName == "value" =>
          Some(receiver).asInstanceOf[Option[Rep[StructItem[Val, Schema]] forSome {type Val; type Schema}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[StructItem[Val, Schema]] forSome {type Val; type Schema}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}

object StructItems_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAALVWTWwbRRR+duw4tqP+8VMIgobUtAIVu3DpIUJRmjqoYJIoW1XIVFTj9djZZnZ2szOOdjlU4tIDiAviikQlLki9oJ64IC5IiAMnhJA4cypBqIf2VMSb2V+72RQQ7GE0O/v2/Xzf997urT0oCw9OCZMwwps2laRp6P2ykA2jzaUlg7ec/ojRC3TwlPPNZ698MfdVEQ53YXqLiAuCdaEabtq+m+wNutOBKuEmFdLxhITnOzpCy3QYo6a0HN6ybHskSY/RVscScrEDpZ7TD3bgOhQ6cMR0uOlRSY0VRoSgIjqfoSojK7mv6vtg3U1j8JaqopWp4pJHLInpY4wjof0mdY2AOzywJRyKUlt3VVpoU7Fs1/FkHKKC7racfnxb4gQP4FjnGtklLQwxbBnSs/gQ36y7xNwmQ7qGJsq8hAkLygaXAlffT3WgJugOAnTRdpk+8V0AQAZe1Uk0U3yaCT5NhU/DoJ5FmPUeUQ83PMcPILwKUwC+iy7OPMJF7IG2eb/xwRXznftG3S6ql32VSkVXOI2OTuSoQVOBOH63+bG4+/rNc0WodaFmieWekB4xZZbyCK064dyROucEQOINka2FPLZ0lGW0mZBE1XRsl3D0FEE5izwxy7SkMlZnsxE7OdBXpEtj04LvFpJ653Pq1bpZIYxt3Hn65Rd+a79dhOJ4iCq6NFD4XuxUQg3VMDLlRUntKIBaD0uYukxYijMeTBvmFrWJPlNL1U/XygFpJQCdvvN7/9uzcKWYwBpl8feYRBdl8fNP9R9fXCrCTFfrfpWRYReRFW1G7XVvxeGyCzPOLvXCJ5VdwtRuX2YrfTogIyYjvLNATSFQEuZzO9SlCsVF3Q2FGIB6KOg1h9PG6kbjnvH9J7eUXj2YDZ+ELfunde7BL4cGUksZcd6mQYI5Nvo4C9WQnjdpkMFdP5xLQqvlhIQy1jqi+a4SQh8iTzvQZsczr8wVJgKUKHqIvZUU4gcG2N9JhYYyOsBPntBw9qViPU8ETfNW4nk2ry10Gz252Xmc7S19XYTyG1AeoCZEB8o9Z8T7cX/iDJfUl+fjs8K4JrAfiUfsWLThNJsHnYTOdbJnlvYzySvukT0QfxW+vHHjiT8+v/qYnmYzPUvaxG2c/QezLB49/+OsgglhYNrjJw9PHQ9OHsBfWAntn7z9/t5rvTMf6clW1gWmHau3z6ieOjriiLK5TfuXCX5EFDD/ZqpVMx+a44lFqOTx0ZlVYkp94T9Th1pXU8PIup5BEYuO8HM9y8Z/jl0qENRaOHkMx6ZHF+5a7978UOrxW/DH/wnWe9fwI7yoAz2H7y3kkGFEvCOj1+9/uvbSD7d/1VzUlIJw8PHkPySVi+/uS7768ZqkQsXJFI/jQQlNF/4XTIN3QfsJAAA="
}
}

trait StructItemsDslSeq extends impl.StructItemsSeq {self: StructsDsl with ScalanSeq =>}
trait StructItemsDslExp extends impl.StructItemsExp {self: StructsDsl with ScalanExp =>}
