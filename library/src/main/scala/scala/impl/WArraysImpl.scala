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

  //proxyBT: TypeWrapper proxy
  //implicit def proxyArray[T:Elem](p: Rep[Array[T]]): WArray[T] =
  //  proxyOps[WArray[T]](p.asRep[WArray[T]])

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

    def length: Rep[Int] =
      methodCallEx[Int](self,
        this.getClass.getMethod("length"),
        List())

    def map[B](f: Rep[T => B]): Rep[WArray[B]] =
      methodCallEx[WArray[B]](self,
        this.getClass.getMethod("map", classOf[AnyRef]),
        List(f.asInstanceOf[AnyRef]))
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
    def apply[T](wrappedValue: Rep[Array[T]])(implicit eT: Elem[T]): Rep[WArrayImpl[T]] =
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
    (wrappedValue: Rep[Array[T]])(implicit eT: Elem[T]): Rep[WArrayImpl[T]] = {
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
  }

  object WArrayCompanionMethods {
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
  val dump = "H4sIAAAAAAAAALVWzW8bRRR/3sbxV5omQYnUQ0uItkJAYhfUqlSRkII/UMHEVjZqkalA492xO2V3dtkdp2sOvdEDXBDqCYlDRblFSIgLcEdCHPoPcAYJNSDUQyshgZiP/XDcuGkP9WE0O/vmvd/7vd97690/IRv4sByYyEa07GCGyobcbwRMN952rYGNa7jX2vv45B9frd7XYL4D01dQUAvsDhTUph56yd5gVhPmG4RadcoIG+qOdMGg3FQxKiJG5aAY+sit9SYUEDVxwFw/YPCculwxXdvGJiMurRDHGTDUtXGlSQLG7ae6rjX8EK6D1oQ506Wmjxk2qjYKAhxE53ks3JPkuSCfhy0vjfEwwG0fEcbx8Rhzyn4Le8aQunToMJiNoLU8AYvblHDocSIuOJ4tw0w1IUccz/VZHDXHI1xxrfhxiiJ+AAvNq2gHVXjUfsVgPqF94cxD5geojze5iTDP8hwCbPe2hx6OnJcCZu2LF3oA4PGqviKRlVPSyglpZUGabmCfIJt8hMTLtu+GQ1C/zBGAULhYPcRF7AHXqaV/ctl894FRcjRxORRY8hJRjjt6doLCZH04uT9vfR7ce+PWOQ2KHSiSYKMbMB+ZbFQHEV8lRKnLJOaEQuT3eQlXJpVQRtngNmM6KZiu4yHKPUVkzvBK2cQkTBiLs6NRfSaQn2Mejk210Msk+U7qKCmmKrLt9t3ja6f26u9oiQSiEAXu0uAt5cdOGUxf2vB9NIyci3WOQWZbMiyWYpiu+UcET2h4/u5f1k+n4bIGmYi8KNbj1Yu7WHj1ix9P4fY3GuQ7Ut8NG/Vl5QQ7NRyYHci7O9hX57kdZIvdgdXLWbiHBjaLOB0l4wgng8HyxNb0sGBqXUo+E6dfUqLddCnWG239vvHLzV2hSR9m1BvVq/+Rc//+OttjUq4MZq75yPOwdRHZAxyTnH2IeLEsHsy8WE5Kq6WRG8czmQiafM9Aw9ux+6m6jZ3DvTMoKgWIJk+jiDqdmKQzqcu9f27/YHz29XkNpt+EbI8XIGhCtusOqBULnk9KhkP2enw2pkYucOQjJxmgO4g3PG9IBktxUQaM2JWL0bkqBf8tgwSa5LIYV2cpQiyulS9Q5ZDpL32/e43ceaEhqzGa/KGSjOfztzduLP59+/1n5AjJdwlzkKeffoIBEvf7UxwQsL+HS8LyktSdQpcTy3KqhYXHk94IS2uPYqnlq/5N8if66vnfazffklPoWMqLNItSGp0PDI5WuWAQodiPs1RD/jWeTWNATc5z8mIE2lJyIDRQVH1ouA6eX7lH3rv1KZOjKBPu/w62ulf5Z2dd5nhC+jkzxuBMPazGJXp5nKyFCRNyDJAUKk/tmGqy6mjJlYC9tBY1Dn9lAsdGVHsuxusPvtx88c53v0lii0JFfBrR5C9AKhkl9jyfBUI5Yr+WQj2bQEgbiZvmou8Bn08SSQRMrO399MzG4lL/ssb42JKe/wcIHe6nBAoAAA=="
}
}

trait WArraysModule extends impl.WArraysDefs {self: WrappersModule =>}
