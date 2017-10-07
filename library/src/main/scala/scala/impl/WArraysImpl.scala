package scala

import scalan._
import impl._
import scala.wrappers.WrappersModule
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait WArraysDefs extends scalan.Scalan with WArrays {
  self: WrappersModule =>

  // entityProxy: single proxy for each type family
  implicit def proxyWArray[T](p: Rep[WArray[T]]): WArray[T] = {
    proxyOps[WArray[T]](p)(scala.reflect.classTag[WArray[T]])
  }

  implicit def castWArrayElement[T](elem: Elem[WArray[T]]): WArrayElem[T, WArray[T]] =
    elem.asInstanceOf[WArrayElem[T, WArray[T]]]

  implicit lazy val containerWArray: Functor[WArray] = new Functor[WArray] {
    def tag[A](implicit evA: WeakTypeTag[A]) = weakTypeTag[WArray[A]]
    def lift[A](implicit evA: Elem[A]) = element[WArray[A]]
    def unlift[A](implicit eFT: Elem[WArray[A]]) =
      castWArrayElement(eFT).eT
    def getElem[A](fa: Rep[WArray[A]]) = fa.elem
    def unapply[T](e: Elem[_]) = e match {
      case e: WArrayElem[_,_] => Some(e.asElem[WArray[T]])
      case _ => None
    }
    def map[A,B](xs: Rep[WArray[A]])(f: Rep[A] => Rep[B]) = { implicit val eA = unlift(xs.elem); xs.map(fun(f))}
  }

  case class WArrayIso[A, B](innerIso: Iso[A, B]) extends Iso1UR[A, B, WArray] {
    lazy val selfType = new ConcreteIsoElem[WArray[A], WArray[B], WArrayIso[A, B]](eFrom, eTo).
      asInstanceOf[Elem[IsoUR[WArray[A], WArray[B]]]]
    def cC = container[WArray]
    def from(x: Rep[WArray[B]]) = x.map(innerIso.fromFun)
    def to(x: Rep[WArray[A]]) = x.map(innerIso.toFun)
  }

  def wArrayIso[A, B](innerIso: Iso[A, B]) =
    reifyObject(WArrayIso[A, B](innerIso)).asInstanceOf[Iso1[A, B, WArray]]

  // familyElem
  class WArrayElem[T, To <: WArray[T]](implicit _eT: Elem[T])
    extends EntityElem1[T, To, WArray](_eT, container[WArray]) {
    def eT = _eT
    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("T" -> (eT -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[WArray[T]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[WArray[T]] => convertWArray(x) }
      tryConvert(element[WArray[T]], this, x, conv)
    }

    def convertWArray(x: Rep[WArray[T]]): Rep[To] = {
      x.elem match {
        case _: WArrayElem[_, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have WArrayElem[_, _], but got $e", x)
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def wArrayElement[T](implicit eT: Elem[T]): Elem[WArray[T]] =
    cachedElem[WArrayElem[T, WArray[T]]](eT)

  implicit case object WArrayCompanionElem extends CompanionElem[WArrayCompanionCtor] {
    lazy val tag = weakTypeTag[WArrayCompanionCtor]
    protected def getDefaultRep = WArray
  }

  abstract class WArrayCompanionCtor extends CompanionDef[WArrayCompanionCtor] with WArrayCompanion {
    def selfType = WArrayCompanionElem
    override def toString = "WArray"
  }
  implicit def proxyWArrayCompanionCtor(p: Rep[WArrayCompanionCtor]): WArrayCompanionCtor =
    proxyOps[WArrayCompanionCtor](p)

  registerModule(WArraysModule)

  lazy val WArray: Rep[WArrayCompanionCtor] = new WArrayCompanionCtor {
    def fill[T](n: Rep[Int])(elem: Rep[Thunk[T]]): Rep[WArray[T]] =
      methodCallEx[WArray[T]](self,
        this.getClass.getMethod("fill", classOf[AnyRef], classOf[AnyRef]),
        List(n.asInstanceOf[AnyRef], elem.asInstanceOf[AnyRef]))
  }

  case class ViewWArray[A, B](source: Rep[WArray[A]], override val innerIso: Iso[A, B])
    extends View1[A, B, WArray](wArrayIso(innerIso)) {
    override def toString = s"ViewWArray[${innerIso.eTo.name}]($source)"
    override def equals(other: Any) = other match {
      case v: ViewWArray[_, _] => source == v.source && innerIso.eTo == v.innerIso.eTo
      case _ => false
    }
  }

  object WArrayMethods {
    object apply {
      def unapply(d: Def[_]): Option[(Rep[WArray[T]], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[WArrayElem[_, _]] && method.getName == "apply" =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[WArray[T]], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[WArray[T]], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object zip {
      def unapply(d: Def[_]): Option[(Rep[WArray[T]], Rep[WArray[B]]) forSome {type T; type B}] = d match {
        case MethodCall(receiver, method, Seq(ys, _*), _) if receiver.elem.isInstanceOf[WArrayElem[_, _]] && method.getName == "zip" =>
          Some((receiver, ys)).asInstanceOf[Option[(Rep[WArray[T]], Rep[WArray[B]]) forSome {type T; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[WArray[T]], Rep[WArray[B]]) forSome {type T; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object map {
      def unapply(d: Def[_]): Option[(Rep[WArray[T]], Rep[T => B]) forSome {type T; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[WArrayElem[_, _]] && method.getName == "map" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[WArray[T]], Rep[T => B]) forSome {type T; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[WArray[T]], Rep[T => B]) forSome {type T; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object length {
      def unapply(d: Def[_]): Option[Rep[WArray[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WArrayElem[_, _]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[WArray[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[WArray[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object WArrayCompanionMethods {
    object fill {
      def unapply(d: Def[_]): Option[(Rep[Int], Rep[Thunk[T]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(n, elem, _*), _) if receiver.elem == WArrayCompanionElem && method.getName == "fill" =>
          Some((n, elem)).asInstanceOf[Option[(Rep[Int], Rep[Thunk[T]]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Int], Rep[Thunk[T]]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object UserTypeWArray {
    def unapply(s: Exp[_]): Option[Iso[_, _]] = {
      s.elem match {
        case e: WArrayElem[a,to] => e.eItem match {
          case UnpackableElem(iso) => Some(iso)
          case _ => None
        }
        case _ => None
      }
    }
  }

  override def unapplyViews[T](s: Exp[T]): Option[Unpacked[T]] = (s match {
    case Def(view: ViewWArray[_, _]) =>
      Some((view.source, view.iso))
    case UserTypeWArray(iso: Iso[a, b]) =>
      val newIso = wArrayIso(iso)
      val repr = reifyObject(UnpackView(s.asRep[WArray[b]], newIso))
      Some((repr, newIso))
    case _ =>
      super.unapplyViews(s)
  }).asInstanceOf[Option[Unpacked[T]]]

  override def rewriteDef[T](d: Def[T]) = d match {
    case view1@ViewWArray(Def(view2@ViewWArray(arr, innerIso2)), innerIso1) =>
      val compIso = composeIso(innerIso1, innerIso2)
      implicit val eAB = compIso.eTo
      ViewWArray(arr, compIso)

    case WArrayMethods.map(xs, f) => (xs, f) match {
      case (_, Def(IdentityLambda())) =>
        xs
      case (xs: RepWArray[a] @unchecked, LambdaResultHasViews(f, iso: Iso[b, c])) =>
        val f1 = f.asRep[a => c]
        implicit val eB = iso.eFrom
        val s = xs.map(f1 >> iso.fromFun)
        val res = ViewWArray(s, iso)
        res
      case (HasViews(source, Def(contIso: WArrayIso[a, b])), f: RFunc[_, c]@unchecked) =>
        val f1 = f.asRep[b => c]
        val iso = contIso.innerIso
        implicit val eC = f1.elem.eRange
        source.asRep[WArray[a]].map(iso.toFun >> f1)
      case _ =>
        super.rewriteDef(d)
    }
    case _ => super.rewriteDef(d)
  }
}

object WArraysModule extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAAIVWTWwbRRR+3jh1EuevgRSBoLRhJaAUb1sp4ien4DoSYOKKjVoUKtB4PTbT7s4ss+OyRlWPlWi5gHLlgMQJ5QK9cOKCuHDoHXEDwQW1QhyoeijizexP1iZu9rDyzs773nvf971Z796ByUjCscgjPuG1gCpSc83v9UjZ7lui0/fpWdr9Vdxa+vzm/a8tWNyGORadZ1L1ic8+pp1tWBaXGwFTLcl6ScCWJEw1Ya7BFVMDOzCLCk40kzSOTuPsl8ZOI9aasLQ1CKk74IKzIEdwDkYohiHMoxckCUMqR0o5fTDQcCBCTRPu0UgJGSk4nsQ7nvB96ikmuMOCoK9I26dOk0UK9y96gnuSKurWfRJFNPoQrkG5CVNUQ7L8edo8D1rhHu7/6zKUYlkaN9n/Ng1Nn4NAwXxaTivUpeCeCgtCIVWWooJwH4hO9ljmBBdgqXmJXCEOpug5rpKM9zByQQzLqEMONaEaEu8y6dFNjNRLFewjon5X0222xCGUwjBEM50xtdT2qKnl1NQ0NbZLJdPeIfrlOSniASRXaQIg1hAnD4DIEGiDd+wbF71377nVwNLBsS5mxjQ6jUBPjzF2Ruc3168v//XV+49YMLMNU22mAhLap4pap5RVCedCmYpzFonsoWQr4yQzqq/jHmS13BadQaa3J4KQcERKuZ1FsXzmMaU367WFVKJ9iUY1VUizrWXkPe923Bjr2PUw9Ac/XP3+6m9P/nzYggntwziUBdgJhH1IO4ayOvF9bMdSWXLMOpOI5YqAHl75m7335U1lQakJpXjYYq32JRRzLZYwm0Qkbv2XvfTgl/muslLtxzaR5T/zTrdd2/r0EwusYZ6msQG3gU1lxSmYQI1TevT9cQWHLqxLSQbDi6UtYxt9m4337k8UVnOSX3xYfS2ZqJ6bhdknX/nj7M6bptiFPROZbWndxkh5yXN1wRVhnObaJHZ+RkF1o889NGX+Ii1tPGm5B5/9827nx1Nw0ShTTJjjJFmWzX0eCvwsFV4gV7ONuJ4Z+HSRRn07OsKjM/KcJHmukFRCNXHDpuDU3jhn/+P+tLOrzaDfP4aELCSK1YtTs2qCTfFH0/IkrIwhwU3HB71w7d4Xmydu3/rdyDGjBxGzcjV0MBpVUmIUlPXw5eI/NU58M5FTDz677+7c+NaCyhsw2fVJL9r35Jhsiz7vZNOPnwlFY/VatlYedjVOO5EkyL8eVwiefXg6KTiSjWpfMd85n66v4YlgrmOQ07ya/zqesX4k7UTH1l7nCaqyX/hu9yN2+/kNM42j87A4jIXsVArTtKrwv4QGLfjl5eE5m08/qlHygRkxx6v/AZfb8gCOCAAA"
}
}

trait WArraysModule extends scala.impl.WArraysDefs {self: WrappersModule =>}
