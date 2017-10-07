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
trait SpecializationsDefs extends Specializations {
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
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("T" -> (eT -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant), "M" -> (eM -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagT = eT.tag
      implicit val tagR = eR.tag
      implicit val tagM = eM.tag
      weakTypeTag[IsoFunc[T, R, M]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
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
      (override val func: Rep[T => R], override val metric: Rep[T => M])
    extends IsoFuncBase[T, R, M](func, metric) with Def[IsoFuncBase[T, R, M]] {
    implicit val eT = func.elem.eDom;
implicit val eR = func.elem.eRange;
implicit val eM = metric.elem.eRange
    lazy val selfType = element[IsoFuncBase[T, R, M]]
  }
  // elem for concrete class
  class IsoFuncBaseElem[T, R, M](val iso: Iso[IsoFuncBaseData[T, R, M], IsoFuncBase[T, R, M]])(implicit override val eT: Elem[T], override val eR: Elem[R], override val eM: Elem[M])
    extends IsoFuncElem[T, R, M, IsoFuncBase[T, R, M]]
    with ConcreteElem[IsoFuncBaseData[T, R, M], IsoFuncBase[T, R, M]] {
    override lazy val parent: Option[Elem[_]] = Some(isoFuncElement(element[T], element[R], element[M]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("T" -> (eT -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant), "M" -> (eM -> scalan.util.Invariant))

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
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("T" -> (eT -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant), "M" -> (eM -> scalan.util.Invariant))
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

  implicit class ExtendedIsoFuncBase[T, R, M](p: Rep[IsoFuncBase[T, R, M]]) {
    def toData: Rep[IsoFuncBaseData[T, R, M]] = {
      implicit val eT = p.func.elem.eDom;
implicit val eR = p.func.elem.eRange;
implicit val eM = p.metric.elem.eRange
      isoIsoFuncBase(eT, eR, eM).from(p)
    }
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
  val dump = "H4sIAAAAAAAAALVWTWwbRRQeb5w4cdKkdUkRiEIIphEB2YVLkXJAaZNUQc6PslGQTEU0Xo/NtLs7w+44rFHVYw/tiaoXkDhU4oQiJOgFIYQQCAlx6IkL6qmgSkgIhHqg6qGIN7M/3rW9MUjgw8gzfu+bed/3zRvv/4aGXQfNuAY2sV2yiMAlXX1fdEVRX2P1lkmWSOMndrNw7eqDjzR0uIoOUXeHOqKFTfoOqVfRNLuwbFGx4dCmn7DtYCoq6NCyLahoFy21KNB8xd+mLLcp99umGGQsVFBhu82J3raZTa0IoTwYIZ4GMI+85mDOidN1lBcHAyUTAWoM2wZxBXNcgZ7288sGM01iCMrsMrWslsA1k5Qr1BUQf9hgtuEQQfQzJnZd4r6FLqFsBY0SCUmj+Ziatzd4B7f3XIpSOJbE9eO3CFd1ti2BJoPjbHB5FIjJUYszR4Rb5ADuTVYPp1kbwwIqVM7jPVyGLZplXTjUbkLmFEvKKFNGKmicY+MCbpJ1yJRLOajDJWZD0q1CPJ5BnHMw00vqLKUONaWImpKkpqgTh0rvYPnjpsO8NvI/mSGEPAnxwgCIEIEs2/XilXPG6/f1cUuTyZ6qcQwwnkrxtBIDmPxu65p77+yNUxrKV1Geuos1VzjYEHGhA77GsW0zoY4bUYidJug1m6aX2mURYoDSbI3V26HYBrM4tgEpIHYClDKpQYUMlmtTgT59WQYpBSdhaBZIj+pNu8Myd5Fzs/3NxS8v/vzEj0c0NCRN6HEnBjsEsAeUo6xwBpsmlKOJcHPYNe8rpTOLHJm9R9+4cVVoKFNBGS/pr43aeVBywXPQhJ/hW/Uveurh7cmG0ALhU4sI9/8i99XXv9x9JashLcnTGBSgL0NR4eEEyq26bKVlGwFFcnxMoMy2/JKPplvJ6ZqaymHCk+PjXfP8AaeMRJ/79ff6tyfROUWFskrI2D9yJ0AUXn7/82fJ5scaGq2q27xi4qbyqRRtibhGFY2yPeL467k9bMpvfb2aq5MGbpnhVY6z5ss+kyo7J5LSBY/L2xmWP+4ruM5sUlzZLP6pf399Xwoof39UoGwj5Bz4HII2FRX9ZJq6nEidflh97+jU8d07StuROrMwVbdkroKGHTCROvRcQGKqkr5UavFEdGo5zAs0Ajs7tN/h5FhOA+72ROAEBaqCnomhnMhkkptqZDtEyi6bxOprx/ipewG2/Gg5LHWl9xbdm76Wnt5bmkDjwb05jV3SqVEKeDxdQPDs6MN3H+jXr3yqodyraLgBdnT7+nG4xlp2Pex/8EoK4onT4Vo26VDod9jBVvR47mFo/dCfBToWurYlqFneCdZ9r8JnBnVIiL6dDT18LKhE5pZWbR9VFJ//bP9teuu5Fb8fyeCdA9A65FUHxuqd2DjbAxtC+PB/cvny9B8f7h5Vz9VojQoL8+LJf/FYhW/L//gYoZi5Cmqcjq4alJC03n/Snf3R6OnR6lJGq74UsRch7uqDhEtcz4M1TlzFgaEx53QXs5v0DURP6ZwYnX9MbnCvJwPH1NugUNDUpsFJsylO0gPtQIlL9z9Yn791867qtHnpAujldvR3sSO5x5PyjPh4Mb6hqUlT/A0l6WblSQwAAA=="
}
}

