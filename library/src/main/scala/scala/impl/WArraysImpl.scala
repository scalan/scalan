package scala

import scalan._
import impl._
import scala.Array
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

  implicit def unwrapValueOfWArray[T](w: Rep[WArray[T]]): Rep[Array[T]] = w.wrappedValue

  implicit def arrayElement[T:Elem]: Elem[Array[T]] =
    element[WArray[T]].asInstanceOf[WrapperElem1[_, _, CBase, CW] forSome { type CBase[_]; type CW[_] }].baseElem.asInstanceOf[Elem[Array[T]]]

  implicit def castWArrayElement[T](elem: Elem[WArray[T]]): WArrayElem[T, WArray[T]] =
    elem.asInstanceOf[WArrayElem[T, WArray[T]]]

  implicit lazy val containerArray: Cont[Array] = new Cont[Array] {
    def tag[A](implicit evA: WeakTypeTag[A]) = weakTypeTag[Array[A]]
    def lift[A](implicit evA: Elem[A]) = element[Array[A]]
    def unlift[A](implicit eFT: Elem[Array[A]]) =
      castWArrayElement(eFT.asInstanceOf[Elem[WArray[A]]]).eT
    def getElem[A](fa: Rep[Array[A]]) = fa.elem
    def unapply[T](e: Elem[_]) = e match {
      case e: BaseTypeElem1[_,_,_] if e.wrapperElem.isInstanceOf[WArrayElem[_,_]] => Some(e.asElem[Array[T]])
      case _ => None
    }
  }

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
    extends WrapperElem1[T, To, Array, WArray](_eT, container[Array], container[WArray]) {
    def eT = _eT
    lazy val parent: Option[Elem[_]] = None
    lazy val typeArgs = TypeArgs("T" -> (eT -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[WArray[T]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[WArray[T]] => convertWArray(x) }
      tryConvert(element[WArray[T]], this, x, conv)
    }

    def convertWArray(x: Rep[WArray[T]]): Rep[To] = {
      x.elem match {
        case _: WArrayElem[_, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have WArrayElem[_, _], but got $e", x)
      }
    }
    lazy val baseElem =
      new BaseTypeElem1[T, Array, WArray[T]](this.asInstanceOf[Elem[WArray[T]]])(
        element[T], container[Array])
    lazy val eTo: Elem[_] = new WArrayImplElem[T](isoWArrayImpl(eT))(eT)
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def wArrayElement[T](implicit eT: Elem[T]): Elem[WArray[T]] =
    elemCache.getOrElseUpdate(
      (classOf[WArrayElem[T, WArray[T]]], Seq(eT)),
      new WArrayElem[T, WArray[T]]).asInstanceOf[Elem[WArray[T]]]

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

  // default wrapper implementation
  abstract class WArrayImpl[T](val wrappedValue: Rep[Array[T]])(implicit val eT: Elem[T]) extends WArray[T] with Def[WArrayImpl[T]] {
    lazy val selfType = element[WArrayImpl[T]]

    def apply(i: Rep[Int]): Rep[T] =
      methodCallEx[T](self,
        this.getClass.getMethod("apply", classOf[AnyRef]),
        List(i.asInstanceOf[AnyRef]))

    def zip[B](ys: Rep[WArray[B]])(emB: Elem[B]): Rep[WArray[(T, B)]] =
      methodCallEx[WArray[(T, B)]](self,
        this.getClass.getMethod("zip", classOf[AnyRef], classOf[AnyRef]),
        List(ys.asInstanceOf[AnyRef], emB.asInstanceOf[AnyRef]))

    def map[B](f: Rep[T => B])(emB: Elem[B]): Rep[WArray[B]] =
      methodCallEx[WArray[B]](self,
        this.getClass.getMethod("map", classOf[AnyRef], classOf[AnyRef]),
        List(f.asInstanceOf[AnyRef], emB.asInstanceOf[AnyRef]))

    def length: Rep[Int] =
      methodCallEx[Int](self,
        this.getClass.getMethod("length"),
        List())
  }
  case class WArrayImplCtor[T](override val wrappedValue: Rep[Array[T]])(implicit override val eT: Elem[T]) extends WArrayImpl[T](wrappedValue) {
  }
  trait WArrayImplCompanion
  // elem for concrete class
  class WArrayImplElem[T](val iso: Iso[WArrayImplData[T], WArrayImpl[T]])(implicit override val eT: Elem[T])
    extends WArrayElem[T, WArrayImpl[T]]
    with ConcreteElem1[T, WArrayImplData[T], WArrayImpl[T], WArray] {
    override lazy val parent: Option[Elem[_]] = Some(wArrayElement(element[T]))
    override lazy val typeArgs = TypeArgs("T" -> (eT -> scalan.util.Invariant))
    override lazy val eTo: Elem[_] = this
    override def convertWArray(x: Rep[WArray[T]]) = WArrayImpl(x.wrappedValue)
    override def getDefaultRep = WArrayImpl(DefaultOfArray[T])
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[WArrayImpl[T]]
    }
  }

  // state representation type
  type WArrayImplData[T] = Array[T]

  // 3) Iso for concrete class
  class WArrayImplIso[T](implicit eT: Elem[T])
    extends EntityIso[WArrayImplData[T], WArrayImpl[T]] with Def[WArrayImplIso[T]] {
    override def from(p: Rep[WArrayImpl[T]]) =
      p.wrappedValue
    override def to(p: Rep[Array[T]]) = {
      val wrappedValue = p
      WArrayImpl(wrappedValue)
    }
    lazy val eFrom = element[Array[T]]
    lazy val eTo = new WArrayImplElem[T](self)
    lazy val selfType = new WArrayImplIsoElem[T](eT)
    def productArity = 1
    def productElement(n: Int) = eT
  }
  case class WArrayImplIsoElem[T](eT: Elem[T]) extends Elem[WArrayImplIso[T]] {
    def getDefaultRep = reifyObject(new WArrayImplIso[T]()(eT))
    lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[WArrayImplIso[T]]
    }
    lazy val typeArgs = TypeArgs("T" -> (eT -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class WArrayImplCompanionCtor extends CompanionDef[WArrayImplCompanionCtor] {
    def selfType = WArrayImplCompanionElem
    override def toString = "WArrayImplCompanion"

    @scalan.OverloadId("fromFields")
    def apply[T](wrappedValue: Rep[Array[T]]): Rep[WArrayImpl[T]] =
      mkWArrayImpl(wrappedValue)

    def unapply[T](p: Rep[WArray[T]]) = unmkWArrayImpl(p)
  }
  lazy val WArrayImplRep: Rep[WArrayImplCompanionCtor] = new WArrayImplCompanionCtor
  lazy val WArrayImpl: WArrayImplCompanionCtor = proxyWArrayImplCompanion(WArrayImplRep)
  implicit def proxyWArrayImplCompanion(p: Rep[WArrayImplCompanionCtor]): WArrayImplCompanionCtor = {
    proxyOps[WArrayImplCompanionCtor](p)
  }

  implicit case object WArrayImplCompanionElem extends CompanionElem[WArrayImplCompanionCtor] {
    lazy val tag = weakTypeTag[WArrayImplCompanionCtor]
    protected def getDefaultRep = WArrayImplRep
  }

  implicit def proxyWArrayImpl[T](p: Rep[WArrayImpl[T]]): WArrayImpl[T] =
    proxyOps[WArrayImpl[T]](p)

  implicit class ExtendedWArrayImpl[T](p: Rep[WArrayImpl[T]])(implicit eT: Elem[T]) {
    def toData: Rep[WArrayImplData[T]] = isoWArrayImpl(eT).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoWArrayImpl[T](implicit eT: Elem[T]): Iso[WArrayImplData[T], WArrayImpl[T]] =
    reifyObject(new WArrayImplIso[T]()(eT))

  registerModule(WArraysModule)

  lazy val WArray: Rep[WArrayCompanionCtor] = new WArrayCompanionCtor {
    def fill[T](n: Rep[Int])(elem: Rep[Thunk[T]])(emT: Elem[T]): Rep[WArray[T]] =
      methodCallEx[WArray[T]](self,
        this.getClass.getMethod("fill", classOf[AnyRef], classOf[AnyRef], classOf[AnyRef]),
        List(n.asInstanceOf[AnyRef], elem.asInstanceOf[AnyRef], emT.asInstanceOf[AnyRef]))
  }

  case class ViewWArray[A, B](source: Rep[WArray[A]], override val innerIso: Iso[A, B])
    extends View1[A, B, WArray](wArrayIso(innerIso)) {
    override def toString = s"ViewWArray[${innerIso.eTo.name}]($source)"
    override def equals(other: Any) = other match {
      case v: ViewWArray[_, _] => source == v.source && innerIso.eTo == v.innerIso.eTo
      case _ => false
    }
  }

  object WArrayImplMethods {
  }

  def mkWArrayImpl[T]
    (wrappedValue: Rep[Array[T]]): Rep[WArrayImpl[T]] = {
    implicit val eT = wrappedValue.elem.typeArgs("T")._1.asElem[T]
    new WArrayImplCtor[T](wrappedValue)
  }
  def unmkWArrayImpl[T](p: Rep[WArray[T]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: WArrayImplElem[T] @unchecked =>
      Some((p.asRep[WArrayImpl[T]].wrappedValue))
    case _ =>
      None
  }

  object WArrayMethods {
    object wrappedValue {
      def unapply(d: Def[_]): Option[Rep[WArray[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WArrayElem[_, _]] && method.getName == "wrappedValue" =>
          Some(receiver).asInstanceOf[Option[Rep[WArray[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[WArray[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

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
      def unapply(d: Def[_]): Option[(Rep[WArray[T]], Rep[WArray[B]], Elem[B]) forSome {type T; type B}] = d match {
        case MethodCall(receiver, method, Seq(ys, emB, _*), _) if receiver.elem.isInstanceOf[WArrayElem[_, _]] && method.getName == "zip" =>
          Some((receiver, ys, emB)).asInstanceOf[Option[(Rep[WArray[T]], Rep[WArray[B]], Elem[B]) forSome {type T; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[WArray[T]], Rep[WArray[B]], Elem[B]) forSome {type T; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object map {
      def unapply(d: Def[_]): Option[(Rep[WArray[T]], Rep[T => B], Elem[B]) forSome {type T; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, emB, _*), _) if receiver.elem.isInstanceOf[WArrayElem[_, _]] && method.getName == "map" =>
          Some((receiver, f, emB)).asInstanceOf[Option[(Rep[WArray[T]], Rep[T => B], Elem[B]) forSome {type T; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[WArray[T]], Rep[T => B], Elem[B]) forSome {type T; type B}] = exp match {
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
      def unapply(d: Def[_]): Option[(Rep[Int], Rep[Thunk[T]], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(n, elem, emT, _*), _) if receiver.elem == WArrayCompanionElem && method.getName == "fill" =>
          Some((n, elem, emT)).asInstanceOf[Option[(Rep[Int], Rep[Thunk[T]], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Int], Rep[Thunk[T]], Elem[T]) forSome {type T}] = exp match {
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
  val dump = "H4sIAAAAAAAAALVWXWgcVRS+O9l0k9lNYqOp2GgT4/pX624rSJUosk02tromoRNSWINyd/bueuv8XGfuprMlFHzpg75Jn4Q+FARfgiJ9UESkKIL40HfxRZCKIIoUsSBYPPfO3NnZTTZbH9yHYebOud895/u+c2a3f0PDvodmfRNb2CnYhOOCIe9LPs8br7j1lkUWSWPr8p/6j2+f/1RDE1U0Rv116vEWtuh5Uq+gsbLDKW/nbRnN0eFKCFcUcMXd4PLRjvkKmlxrM2K0HdehdoxQHIyQ3AYw95zxMGPE60nl2GCg7o0ANYodk/jc9XyOHgz3F03XsojJqesUqW23OK5ZpFihPof4u0zXMT3CibFgYd8n/lvoAkpX0AgRkDR+HpXP7RXWwd2Z15qHKYe0BG4Yf5owWWfb5mg8SmeFiVQgJkNt5npcHZEBuDfcunrMhjRITLG0r4LSDoYYNFk5izdxEU5tFg3uUacJYFmGzTdxkyxDiAjPQA0+sRqCark9YIgxBn55SqZR6LBSiFkpCFbyBvGosAcWL1c9N2ij8JcaQigQEEcGQCgEUnbq+Xc2zFdvGVlbE5sDWd4oYMz0sa3UAUj89vR7/s0XrxzXkF5FOvVLNZ972ORJjRVV2HFcLtNVS2nsNUGquX5SyVNKEAPUpWtuva10Nl2bYQeQIs5zIJJFTcpFsFibiHTYlWRQkTOiQtMBS8X19mtTsbfEmNX+euvLrZ/u/36/hoaE/wLmJWCHAHaPcqRLFrBlQTkaV4fDqXqolOHaZP/cTfralXe5hlIVlAq6fbRSOwtKzgceyoU7Qpfepsf/+WG8wbVI+L5FqPO/yFz76pcbL6Q1pHXzNAoFGGUoSiXH0b4zJc/D7Yghcb2Po9SauNHFJReI68GeZ32PNGJVH/319/o3R9GGrFV6QVFyR/YDiMln3v/8YbL6kYZGqrJTlyzclEYUqiwS36yiEXeTeOF6ZhNb4m5XM2bqpIFbVlcbR7SEus721ZURwdk8NC+0nyo/G0q07Dokv7Sa/8v47tK2UEi8v5ej3Dk5Fevr2GoRxevwDq7F5VAPuRHF4vKIjJpJ7JhLpaIc5HuONLKm4NNli9iD0TnSQ9FPQVd1ThGCPNDPWrKfPv77xIcPHZy+raHMS2i4AUz7u1I9XHNbTl31Lgx3TgJ+Qq2lu8mHXsUetuOZv4lhbMFs4eiAEqTFqVVcj9ZDGeA3izo0xHeHlDwHokrE3sIpJ0Tl+Sc+2z5Hrz++JHspScpAT6rvyicXL0798cHrd8uROFKj3MYsf/Q/DEQ1v/7HgYcSJpiU1ynlkXGxK/piQzVhoLg81rHM9J05tJRYjel7ci/6Vryws2NiaP7Isz8vXnpZTqqJDmEyLKo1OTk4GlsAO2HqkHgw6/LlSY6ySy3HBAHiF4nUZuIFPcHHc3sQlSsHC0qVY72ETPcQYuyYjz2HSpdC+hNh5y0k9Q7dyzrMTwGPc314NCLhQbsLty4vH75+9YYkTxcWglnkxH9lOn4Jna7DgBC2EfdPd1J9vruB1FjJRN8FGFoykwQ51e6PxXjkJj/8R9jDxMa/4a16jCELAAA="
}
}

trait WArraysModule extends scala.impl.WArraysDefs {self: WrappersModule =>}
