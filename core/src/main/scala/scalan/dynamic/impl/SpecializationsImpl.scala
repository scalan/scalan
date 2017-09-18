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

  registerModule(SpecializationsModule)

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

object SpecializationsModule extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAALVWW2hcRRiePZtkd7MhbdIa4qUa46a1KrvFIi0EKUmTSMrmQk40EINl9pzZ7dRzGc+ZTc9KrfjSB30TERR9KCh9CUrxRUREvICI9EF8EZ9ECoIopQ8WBYv/zLludk9SUfdhODP7z/dfvm/+ma1fUa/roDFXwwa2yibhuKzK7ymXl9QFW28aZIbU3/pDO7ecP15Q0PA66juD3RnXWEcF/2PWY9G3yvUqGpqjlj5rccpbJVNCcFSu+j4qwkelm49SYtdkFY2K6ZqDGSPONqzHbg+rfTNAFrClEZfbjsvR/T5GRbMNg2ic2laFmmaT45pBKlXqcrDvqdl66zl0AWWraK9mW5pDOFFPGth1iRus54mAp9G8IOetJRb76Ixz1cGUQ5jgY69vv0KY2rJsq2VyNBiEtsREWGBTJB6D2s6bzJBueqsoR01mOzz0mgMPZ2w9nPZYGBbQcPUs3sQV8NqoqNyhVkOAMaw9ixtkEUyEeR/k4BKjvtpiJAAvulxv8+cxhBBjoJRHZWjluGrlqGplUbWSShyKDfo8Fn8uO7bXQv4vk0XIExCP7AIRIpBZSy+9vKE9fVMtmorY7IlgCjKkPADdl6JaSRBU96uVV90bT1w6pqD+ddRP3amayx2s8aQQgoIVsWXZXMYc1RA7DeBwPI1D6WUKbLYJpaDZJsMWIAXVHACqDKpRLozF2mBAUEr1c5yR0DTrsUyUb9opFXunGDNan5//5PxP93w/pPjC9JiTgM0C7A7pSEmexIYB6Sg8dA5e+326VNskQ+M36DOXXuEKylRRxmsX2FLtLNA56TlowN/hy/cWPfbXD4N1rgTspyYR+v849+lnP1870aMgpb1OBUhAhV7jhMFxlJt37bmmpQUlEuMoR5nVWCdiutI+XZBTMRSloO7cNi/sEGVE+qFfftO/PII2ZCmkVMKK3ZY6AWL4+JsfTZDl9xSUX5fHec7ADalTQdoMcbV1lLc3ieOv5zaxIb66ajWnkzpuGjygOlk1n/axVNoZESWdlCc8E6Zf9BlctC1Smlsu/a5+/dqWIFD8P8JRTz2sOdQzC60rSvreNHYZETx9N//Gvj0HTv8oue3TbRNTeUoOVlGvAyKSQR8MipjKpE+VXJyIohbDYY76wLNDuwUnxnIa8HZNBEqQoNJoPIEykdnmVCGrIVLPrEHMrnJMRt0JsLITQGfanQALOwF0psdRMTg709glcZ6CxAPpJIJu3/9z+vIDd919S0G5U6i3DpJ0u2qyt2Y3LT3sgXB7cuLx6XAt265S6HnYwWZ0qW5iuAOgR3M0Eiq3yalReSpY9/UKvzEUl0F+xdw6aCRIRGwtz1s+KC89/OHWOXr18JzfkoTxWjpYXPuN3UzjKm8kS71rRwhfA1cuXrzj+jun98n7Kl+j3MSsdOQf3Fbh5fI/3kYooawhOe6Pzhqk0K67/6Q9+2Ojo0nLUxmt+kwkroSkpHfhLT6fu1L879UgxkRw+0EX5RRdzBDNwA7RxTuMmPBO9Bk/+vqJtVOja0/Kq2dAl0b+P1Gf7/6qXcBsUt7BD+7wAgOj0qzJeEt8HP3i8W9f/Obyu/KkxLXmaI/KiBa/88DzYJCE3gIxBQ1YJDeekpwayAxEc+Hm24sPXf3gmrwV+oVg4d6xordtrE6PtSupz8dLSAPan9BvQpgviOGlvwGYCrWB6AwAAA=="
}
}

