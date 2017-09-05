package scalan.dynamic

import java.lang.reflect.Method
import scalan._
import scalan.common.Lazy
import scalan.staged.Expressions
import scalan.universe.api.TypesApi
import scalan.universe.api.UniverseUtils._
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait SpecializationsDefs extends scalan.Scalan with Specializations {
  self: Scalan =>

  // entityProxy: single proxy for each type family
  implicit def proxyIsoFunc[T, R, M](p: Rep[IsoFunc[T, R, M]]): IsoFunc[T, R, M] = {
    proxyOps[IsoFunc[T, R, M]](p)(scala.reflect.classTag[IsoFunc[T, R, M]])
  }

  // familyElem
  class IsoFuncElem[T, R, M, To <: IsoFunc[T, R, M]](implicit _eT: Elem[T], _eR: Elem[R], _eM: Elem[M])
    extends EntityElem[To] {
    def eT = _eT
    def eR = _eR
    def eM = _eM
    lazy val parent: Option[Elem[_]] = None
    lazy val typeArgs = TypeArgs("T" -> (eT -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant), "M" -> (eM -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagT = eT.tag
      implicit val tagR = eR.tag
      implicit val tagM = eM.tag
      weakTypeTag[IsoFunc[T, R, M]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[IsoFunc[T, R, M]] => convertIsoFunc(x) }
      tryConvert(element[IsoFunc[T, R, M]], this, x, conv)
    }

    def convertIsoFunc(x: Rep[IsoFunc[T, R, M]]): Rep[To] = {
      x.elem match {
        case _: IsoFuncElem[_, _, _, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have IsoFuncElem[_, _, _, _], but got $e", x)
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def isoFuncElement[T, R, M](implicit eT: Elem[T], eR: Elem[R], eM: Elem[M]): Elem[IsoFunc[T, R, M]] =
    cachedElem[IsoFuncElem[T, R, M, IsoFunc[T, R, M]]](eT, eR, eM)

  implicit case object IsoFuncCompanionElem extends CompanionElem[IsoFuncCompanionCtor] {
    lazy val tag = weakTypeTag[IsoFuncCompanionCtor]
    protected def getDefaultRep = IsoFunc
  }

  abstract class IsoFuncCompanionCtor extends CompanionDef[IsoFuncCompanionCtor] {
    def selfType = IsoFuncCompanionElem
    override def toString = "IsoFunc"
  }
  implicit def proxyIsoFuncCompanionCtor(p: Rep[IsoFuncCompanionCtor]): IsoFuncCompanionCtor =
    proxyOps[IsoFuncCompanionCtor](p)

  case class IsoFuncBaseCtor[T, R, M]
      (override val func: Rep[T => R], override val metric: Rep[T => M])(implicit eT: Elem[T], eR: Elem[R], eM: Elem[M])
    extends IsoFuncBase[T, R, M](func, metric) with Def[IsoFuncBase[T, R, M]] {
    lazy val selfType = element[IsoFuncBase[T, R, M]]
  }
  // elem for concrete class
  class IsoFuncBaseElem[T, R, M](val iso: Iso[IsoFuncBaseData[T, R, M], IsoFuncBase[T, R, M]])(implicit override val eT: Elem[T], override val eR: Elem[R], override val eM: Elem[M])
    extends IsoFuncElem[T, R, M, IsoFuncBase[T, R, M]]
    with ConcreteElem[IsoFuncBaseData[T, R, M], IsoFuncBase[T, R, M]] {
    override lazy val parent: Option[Elem[_]] = Some(isoFuncElement(element[T], element[R], element[M]))
    override lazy val typeArgs = TypeArgs("T" -> (eT -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant), "M" -> (eM -> scalan.util.Invariant))

    override def convertIsoFunc(x: Rep[IsoFunc[T, R, M]]) = IsoFuncBase(x.func, x.metric)
    override def getDefaultRep = IsoFuncBase(constFun[T, R](element[R].defaultRepValue), constFun[T, M](element[M].defaultRepValue))
    override lazy val tag = {
      implicit val tagT = eT.tag
      implicit val tagR = eR.tag
      implicit val tagM = eM.tag
      weakTypeTag[IsoFuncBase[T, R, M]]
    }
  }

  // state representation type
  type IsoFuncBaseData[T, R, M] = (T => R, T => M)

  // 3) Iso for concrete class
  class IsoFuncBaseIso[T, R, M](implicit eT: Elem[T], eR: Elem[R], eM: Elem[M])
    extends EntityIso[IsoFuncBaseData[T, R, M], IsoFuncBase[T, R, M]] with Def[IsoFuncBaseIso[T, R, M]] {
    override def from(p: Rep[IsoFuncBase[T, R, M]]) =
      (p.func, p.metric)
    override def to(p: Rep[(T => R, T => M)]) = {
      val Pair(func, metric) = p
      IsoFuncBase(func, metric)
    }
    lazy val eFrom = pairElement(element[T => R], element[T => M])
    lazy val eTo = new IsoFuncBaseElem[T, R, M](self)
    lazy val selfType = new IsoFuncBaseIsoElem[T, R, M](eT, eR, eM)
    def productArity = 3
    def productElement(n: Int) = n match {
      case 0 => eT
      case 1 => eR
      case 2 => eM
    }
  }
  case class IsoFuncBaseIsoElem[T, R, M](eT: Elem[T], eR: Elem[R], eM: Elem[M]) extends Elem[IsoFuncBaseIso[T, R, M]] {
    def getDefaultRep = reifyObject(new IsoFuncBaseIso[T, R, M]()(eT, eR, eM))
    lazy val tag = {
      implicit val tagT = eT.tag
      implicit val tagR = eR.tag
      implicit val tagM = eM.tag
      weakTypeTag[IsoFuncBaseIso[T, R, M]]
    }
    lazy val typeArgs = TypeArgs("T" -> (eT -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant), "M" -> (eM -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class IsoFuncBaseCompanionCtor extends CompanionDef[IsoFuncBaseCompanionCtor] {
    def selfType = IsoFuncBaseCompanionElem
    override def toString = "IsoFuncBaseCompanion"
    @scalan.OverloadId("fromData")
    def apply[T, R, M](p: Rep[IsoFuncBaseData[T, R, M]]): Rep[IsoFuncBase[T, R, M]] = {
      implicit val eT = p._1.elem.eDom;
implicit val eR = p._1.elem.eRange;
implicit val eM = p._2.elem.eRange
      isoIsoFuncBase[T, R, M].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[T, R, M](func: Rep[T => R], metric: Rep[T => M]): Rep[IsoFuncBase[T, R, M]] =
      mkIsoFuncBase(func, metric)

    def unapply[T, R, M](p: Rep[IsoFunc[T, R, M]]) = unmkIsoFuncBase(p)
  }
  lazy val IsoFuncBaseRep: Rep[IsoFuncBaseCompanionCtor] = new IsoFuncBaseCompanionCtor
  lazy val IsoFuncBase: IsoFuncBaseCompanionCtor = proxyIsoFuncBaseCompanion(IsoFuncBaseRep)
  implicit def proxyIsoFuncBaseCompanion(p: Rep[IsoFuncBaseCompanionCtor]): IsoFuncBaseCompanionCtor = {
    proxyOps[IsoFuncBaseCompanionCtor](p)
  }

  implicit case object IsoFuncBaseCompanionElem extends CompanionElem[IsoFuncBaseCompanionCtor] {
    lazy val tag = weakTypeTag[IsoFuncBaseCompanionCtor]
    protected def getDefaultRep = IsoFuncBaseRep
  }

  implicit def proxyIsoFuncBase[T, R, M](p: Rep[IsoFuncBase[T, R, M]]): IsoFuncBase[T, R, M] =
    proxyOps[IsoFuncBase[T, R, M]](p)

  implicit class ExtendedIsoFuncBase[T, R, M](p: Rep[IsoFuncBase[T, R, M]])(implicit eT: Elem[T], eR: Elem[R], eM: Elem[M]) {
    def toData: Rep[IsoFuncBaseData[T, R, M]] = isoIsoFuncBase(eT, eR, eM).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoIsoFuncBase[T, R, M](implicit eT: Elem[T], eR: Elem[R], eM: Elem[M]): Iso[IsoFuncBaseData[T, R, M], IsoFuncBase[T, R, M]] =
    reifyObject(new IsoFuncBaseIso[T, R, M]()(eT, eR, eM))

  registerModule(Specializations_Module)

  lazy val IsoFunc: Rep[IsoFuncCompanionCtor] = new IsoFuncCompanionCtor {
  }

  object IsoFuncBaseMethods {
    object apply {
      def unapply(d: Def[_]): Option[(Rep[IsoFuncBase[T, R, M]], Rep[T]) forSome {type T; type R; type M}] = d match {
        case MethodCall(receiver, method, Seq(x, _*), _) if receiver.elem.isInstanceOf[IsoFuncBaseElem[_, _, _]] && method.getName == "apply" =>
          Some((receiver, x)).asInstanceOf[Option[(Rep[IsoFuncBase[T, R, M]], Rep[T]) forSome {type T; type R; type M}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[IsoFuncBase[T, R, M]], Rep[T]) forSome {type T; type R; type M}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `toString`: Overrides Object method

    // WARNING: Cannot generate matcher for method `equals`: Overrides Object method
  }

  def mkIsoFuncBase[T, R, M]
    (func: Rep[T => R], metric: Rep[T => M]): Rep[IsoFuncBase[T, R, M]] = {
    implicit val eT = func.elem.eDom;
implicit val eR = func.elem.eRange;
implicit val eM = metric.elem.eRange
    new IsoFuncBaseCtor[T, R, M](func, metric)
  }
  def unmkIsoFuncBase[T, R, M](p: Rep[IsoFunc[T, R, M]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: IsoFuncBaseElem[T, R, M] @unchecked =>
      Some((p.asRep[IsoFuncBase[T, R, M]].func, p.asRep[IsoFuncBase[T, R, M]].metric))
    case _ =>
      None
  }

  object IsoFuncMethods {
    object func {
      def unapply(d: Def[_]): Option[Rep[IsoFunc[T, R, M]] forSome {type T; type R; type M}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[IsoFuncElem[_, _, _, _]] && method.getName == "func" =>
          Some(receiver).asInstanceOf[Option[Rep[IsoFunc[T, R, M]] forSome {type T; type R; type M}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[IsoFunc[T, R, M]] forSome {type T; type R; type M}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object metric {
      def unapply(d: Def[_]): Option[Rep[IsoFunc[T, R, M]] forSome {type T; type R; type M}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[IsoFuncElem[_, _, _, _]] && method.getName == "metric" =>
          Some(receiver).asInstanceOf[Option[Rep[IsoFunc[T, R, M]] forSome {type T; type R; type M}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[IsoFunc[T, R, M]] forSome {type T; type R; type M}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[IsoFunc[T, R, M]], Rep[T]) forSome {type T; type R; type M}] = d match {
        case MethodCall(receiver, method, Seq(x, _*), _) if receiver.elem.isInstanceOf[IsoFuncElem[_, _, _, _]] && method.getName == "apply" =>
          Some((receiver, x)).asInstanceOf[Option[(Rep[IsoFunc[T, R, M]], Rep[T]) forSome {type T; type R; type M}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[IsoFunc[T, R, M]], Rep[T]) forSome {type T; type R; type M}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}

object Specializations_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAALVWS2hcVRj+Z/KYyUxMkxaLFhLTMKU+6kyxiypBJG0mkjJ5kJtYicFy5t4z01Pvy3vOxDtd1JVd6EZEBAUXxRY3QRA32rWCiHQh7ly5cCGNIl20ICj+59zXTDJ3qqizONxz5j/f//i+85+z8wsMcQ+muU5MYpctKkhZU99zXJS0JcdomXSeNlZ235j6+aMTd7MwsQnDFwmf5+YmjAQfVd+NvzVh1GBigdlG1RZMtEuWghBQrgU+KtJHpZePUseu2RqMEFunXDgeF3A02FzRHdOkumCOXWGW1RKkbtJKjXGB9oN1x2i/ClcgW4Nx3bF1jwqqnTUJ55SH63kq4Vk8H1Hz9oqb+Ngf4LpHmMD40Md4YL9GXa1tO3bbEjAWhrbiyrDQpkh9FwuxaLmmcjNYgxyzXMcTkdccerjoGNF00Ca4AAdrl8g2qaDXZkUTHrObEswl+iukSZfRRJoPYQ6cmo31tktD8CIXRpc/3wUAF1l9SkVWTopWjotWlkUradRjxGSXifxz1XP8NgS/zACALyFO3AciQqBV2yi9uaW/dE8rWlm52Zex5FVEOQR6JEVhih8s7tdr7/A7z187nYXCJhQYn6tz4RFddOogrFeR2LYjVMxxCYnXRApn0ihUXubQZo9ORnTHcomNSGExR5Epk+lMSGO59kDIT0rxc8KlkWnWdzNxvmknSonpLDHN1dsPP3lst/piNpZA6GIEITU8Ul4EKiC3yJ2Flq2H6HIcF5BZT0osp2vd0yU1lUPBT8Z8n9jiKh2//avx1UnYykImrG0Yyt+jEyEOPv3BzWN09ZMs5DeV/BdM0lTEyuLNU65vQt7Zpl6wntsmpvzqSW7OoA3SMkVY8s5aDWCtBEynnlyXykLOqhORidIvBppedmxaWlgt3dW+eXdHStaD0eCf4Cj/yU7/8cNYQyg1CxhsRARgcQewA8S1mEqj2qWStO8X3z90YPLCj4roYcOxCFNqm6rBkIeHXeUyFdY2ldaAQbU4GScjhxkBw+jZY72Ck+PxNOAUgShQZXSkA2Uys8dplq5HSINVk1o9tdkZ9X6AtX4A+9PeD7DUD2B/egKK4UE6QzhN8pQkTqaTiHLe/f36F9rbN57JwvA5GGqgUjmyV3dathE1DrxxBPXFmWhtz6nGRkE8YsUX0TbBxomNTcDhSL0twczKC+F6oFn8TYMKVCWTcOjB4TBguau8aAd4ovTE5zuvsVuPLSjVKm+rPXGS8m70sUpquNFZyPu2gejK/PTq1Qd/u37hkOrq+ToTFnFLJ/9BT49a8P/Ys6FbNwMYdvfKf9h9Cx234pHYIhB0R5/vlGY6OckR60fhvyJajltBNB4Ugu6oORadmLnDXr72llAXRMbvfrys1C/hW2FWbT6K+8opIpmnukk8asiXC7XwZRXQf+q9586fe+j8hsIeNZRR8E/c6Xu/A5eIO6seLY/2ebSgUalquaItP059+ex3r3/78Q11WBI6BBzQXKonTyP0PBYmYbRRWWGv5ehrJiU5LdQcqunKvQ+XH7/12U/qAihI9eLNY8evwUSqvtstseEAr0NJ2OmkmEPvctyWw+W/ACdQ1RzHCwAA"
}
}

