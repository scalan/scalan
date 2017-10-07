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
  abstract class WArrayImpl[T](val wrappedValue: Rep[Array[T]]) extends WArray[T] with Def[WArrayImpl[T]] {
    implicit val eT = wrappedValue.eT
    lazy val selfType = element[WArrayImpl[T]]

    def apply(i: Rep[Int]): Rep[T] =
      methodCallEx[T](self,
        this.getClass.getMethod("apply", classOf[AnyRef]),
        List(i.asInstanceOf[AnyRef]))

    def zip[B](ys: Rep[WArray[B]]): Rep[WArray[(T, B)]] =
      methodCallEx[WArray[(T, B)]](self,
        this.getClass.getMethod("zip", classOf[AnyRef]),
        List(ys.asInstanceOf[AnyRef]))

    def map[B](f: Rep[T => B]): Rep[WArray[B]] =
      methodCallEx[WArray[B]](self,
        this.getClass.getMethod("map", classOf[AnyRef]),
        List(f.asInstanceOf[AnyRef]))

    def length: Rep[Int] =
      methodCallEx[Int](self,
        this.getClass.getMethod("length"),
        List())
  }
  case class WArrayImplCtor[T](override val wrappedValue: Rep[Array[T]]) extends WArrayImpl[T](wrappedValue) {
  }
  trait WArrayImplCompanion
  // elem for concrete class
  class WArrayImplElem[T](val iso: Iso[WArrayImplData[T], WArrayImpl[T]])(implicit override val eT: Elem[T])
    extends WArrayElem[T, WArrayImpl[T]]
    with ConcreteElem1[T, WArrayImplData[T], WArrayImpl[T], WArray] {
    override lazy val parent: Option[Elem[_]] = Some(wArrayElement(element[T]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("T" -> (eT -> scalan.util.Invariant))
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
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("T" -> (eT -> scalan.util.Invariant))
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

  implicit class ExtendedWArrayImpl[T](p: Rep[WArrayImpl[T]]) {
    def toData: Rep[WArrayImplData[T]] = {
      implicit val eT = p.wrappedValue.eT
      isoWArrayImpl(eT).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoWArrayImpl[T](implicit eT: Elem[T]): Iso[WArrayImplData[T], WArrayImpl[T]] =
    reifyObject(new WArrayImplIso[T]()(eT))

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

  object WArrayImplMethods {
  }

  def mkWArrayImpl[T]
    (wrappedValue: Rep[Array[T]]): Rep[WArrayImpl[T]] = {
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
  val dump = "H4sIAAAAAAAAALVWS2wbRRgeb5w6cV5tIEVQaEJkXqXYLRIqKCBkHAcKJo66USqZCDRej820+xhmx+kaVT320J6ockLiUIkTipCglwohhEBIiEPviBuoCAmBUA9UPRTxz+zOeu3ESTngw2h3Zv7X933/v976Aw37HM35Fraxm3eIwHlTPRd9kTPf9BptmyyS5s/etekrl+98YqD9NTRB/TXKRRvb9H3SqKEZ72zZoaLKaSs0WOWYigqaKLuCik7OUZsCHamEYQoyTGGnMLnIYqGCplc7jJgd13OpE3so7O0haQZu7j/NMWOE96VyfG9HvYbgahS7FvGFx32BHg3tC5Zn28QS1HML1HHaAtdtUqhQX8D9/ZbnWpwIYpZs7PvEfw9dQOkKGiHSJY3fR9V7p8q6frfnpSCFtKTf8P4pwlSdHUegySidKpOpwJ0MdZjHhQ6RAXfveg39mnYxbKDpyhm8gQsQolUwBaduCyynvF4apcm+Chpj2DqLW2QZLOVWBurwid2UcKsrAUMpxhiI6VmVS74LTT6GJi+hyZmEU6kdLA9XuBd0UPhLDSEUSBdH93ChPZCy28hdWrfeum2OOYY0DlSNo+BjdoCmFRmA5Penrvi3Xr16wkDZGspSv1j3BceWSBId4TWGXdcTKt0YQsxbwNf8IL5UlCLcAUjTda/R0WRbnsOwC54iYMeBKZtaVMjLcm8q4mdHlIFKwYi+mg5YKq53UA9L2yJjdufb81+d/+XhHw8YaEiKMGA84XYI3O5SjpJCCds2lGMIHRyiZkOmTM8hB+Zv0bevXhYGSlVQKujVV7V+BphcCDgaDy1Cqf5DT9z9abIpjIj4gUXo+F9mvv7mt5svpw1k9OI0CgWYZShKJyfQvtNFznEnQkiuDwqUWpUPWbmMB3J9qO89u0saMatP/P5n47tjaF3VqrSgIbkn+YGL6ec//OIxsvKpgUZqql2XbNxSQpSsLBLfqqERb4PwcD+zgW35tKMYMw3SxG1b92oSlpDXuYG8MiIxW4DuhfbT5Y+FFC17LsktreT+Nn/Y3JIMyfMHBBo/p0ZjYw3bbaJxHd6GtVwO94EbQSyXx9Wt2YTFfCoV5aDOBTLIqnafLtvE2du7QNmQ9JPQVd0okpBHBklL9dPI3Q/umJuXPjdQ5nU03ASk/R2hHq57bbehexcmvCCBeEXvpXvBh17FHDvx4N/AMLZgtgh0UBPSFtQurEX7IQ3wm0NdGOKnw5qeg1El0jZ/0g29itzT17fO0RtPLaleSoKypyb1x+Wzixdn/vr4nfvUSBypU+Fgljv2Hwainl//48BDCRFMq3VGa2RSWkWfbagmvCiXJ7uSOXRvCi0mdmP4ntkNvioPOzsGhuaOvvDr4uYbalJNdQFT16Jak5NDoIkSyAlTl8SDOasOXxNobKntWkBAfJBIbTbeyCbweHEXoMbLQUmzcrwfkEN9gJjb5mNfUKVSSH8q7LxSku9QvayL/AzgOD8ARzMiHri7cPuj5SM3rt1U4GWlhGAWufH/ma5eQqVnYUBI2cjn57qpvtTbQHArk/gkwIAZVpkkwKn1fiwmIzX54R+iPiTW/wX9dLOSPgsAAA=="
}
}

trait WArraysModule extends scala.impl.WArraysDefs {self: WrappersModule =>}
