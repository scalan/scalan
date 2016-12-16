package scalan.collections

import scala.collection.immutable.HashSet
import scalan._
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait HashSetsAbs extends scalan.ScalanDsl with HashSets {
  self: HashSetsDsl =>

  // single proxy for each type family
  implicit def proxySHashSet[A](p: Rep[SHashSet[A]]): SHashSet[A] = {
    proxyOps[SHashSet[A]](p)(scala.reflect.classTag[SHashSet[A]])
  }

  // TypeWrapper proxy
  //implicit def proxyHashSet[A:Elem](p: Rep[HashSet[A]]): SHashSet[A] =
  //  proxyOps[SHashSet[A]](p.asRep[SHashSet[A]])

  implicit def unwrapValueOfSHashSet[A](w: Rep[SHashSet[A]]): Rep[HashSet[A]] = w.wrappedValue

  implicit def hashSetElement[A:Elem]: Elem[HashSet[A]] =
    element[SHashSet[A]].asInstanceOf[WrapperElem1[_, _, CBase, CW] forSome { type CBase[_]; type CW[_] }].baseElem.asInstanceOf[Elem[HashSet[A]]]

  implicit def castSHashSetElement[A](elem: Elem[SHashSet[A]]): SHashSetElem[A, SHashSet[A]] =
    elem.asInstanceOf[SHashSetElem[A, SHashSet[A]]]

  implicit lazy val containerHashSet: Cont[HashSet] = new Cont[HashSet] {
    def tag[A](implicit evA: WeakTypeTag[A]) = weakTypeTag[HashSet[A]]
    def lift[A](implicit evA: Elem[A]) = element[HashSet[A]]
    def unlift[A](implicit eFT: Elem[HashSet[A]]) =
      castSHashSetElement(eFT.asInstanceOf[Elem[SHashSet[A]]]).eA
    def getElem[A](fa: Rep[HashSet[A]]) = !!!("Operation is not supported by HashSet container " + fa)
    def unapply[T](e: Elem[_]) = e match {
      case e: BaseTypeElem1[_,_,_] if e.wrapperElem.isInstanceOf[SHashSetElem[_,_]] => Some(e.asElem[HashSet[T]])
      case _ => None
    }
  }

  implicit lazy val containerSHashSet: Cont[SHashSet] = new Cont[SHashSet] {
    def tag[A](implicit evA: WeakTypeTag[A]) = weakTypeTag[SHashSet[A]]
    def lift[A](implicit evA: Elem[A]) = element[SHashSet[A]]
    def unlift[A](implicit eFT: Elem[SHashSet[A]]) =
      castSHashSetElement(eFT).eA
    def getElem[A](fa: Rep[SHashSet[A]]) = fa.selfType1
    def unapply[T](e: Elem[_]) = e match {
      case e: SHashSetElem[_,_] => Some(e.asElem[SHashSet[T]])
      case _ => None
    }
  }

  case class SHashSetIso[A, B](innerIso: Iso[A, B]) extends Iso1UR[A, B, SHashSet] {
    lazy val selfType = new ConcreteIsoElem[SHashSet[A], SHashSet[B], SHashSetIso[A, B]](eFrom, eTo).
      asInstanceOf[Elem[IsoUR[SHashSet[A], SHashSet[B]]]]
    def cC = container[SHashSet]
    def from(x: Rep[SHashSet[B]]) = x.map(innerIso.fromFun)
    def to(x: Rep[SHashSet[A]]) = x.map(innerIso.toFun)
  }

  def sHashSetIso[A, B](innerIso: Iso[A, B]) =
    reifyObject(SHashSetIso[A, B](innerIso)).asInstanceOf[Iso1[A, B, SHashSet]]

  // familyElem
  class SHashSetElem[A, To <: SHashSet[A]](implicit _eA: Elem[A])
    extends WrapperElem1[A, To, HashSet, SHashSet](_eA, container[HashSet], container[SHashSet]) {
    def eA = _eA
    lazy val parent: Option[Elem[_]] = None
    lazy val typeArgs = TypeArgs("A" -> (eA -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[SHashSet[A]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[SHashSet[A]] => convertSHashSet(x) }
      tryConvert(element[SHashSet[A]], this, x, conv)
    }

    def convertSHashSet(x: Rep[SHashSet[A]]): Rep[To] = {
      x.selfType1 match {
        case _: SHashSetElem[_, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have SHashSetElem[_, _], but got $e", x)
      }
    }
    lazy val baseElem =
      new BaseTypeElem1[A, HashSet, SHashSet[A]](this.asInstanceOf[Elem[SHashSet[A]]])(
        element[A], container[HashSet])
    lazy val eTo: Elem[_] = new SHashSetImplElem[A](isoSHashSetImpl(eA))(eA)
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def sHashSetElement[A](implicit eA: Elem[A]): Elem[SHashSet[A]] =
    elemCache.getOrElseUpdate(
      (classOf[SHashSetElem[A, SHashSet[A]]], Seq(eA)),
      new SHashSetElem[A, SHashSet[A]]).asInstanceOf[Elem[SHashSet[A]]]

  implicit case object SHashSetCompanionElem extends CompanionElem[SHashSetCompanionAbs] {
    lazy val tag = weakTypeTag[SHashSetCompanionAbs]
    protected def getDefaultRep = SHashSet
  }

  abstract class SHashSetCompanionAbs extends CompanionDef[SHashSetCompanionAbs] with SHashSetCompanion {
    def selfType = SHashSetCompanionElem
    override def toString = "SHashSet"
  }
  def SHashSet: Rep[SHashSetCompanionAbs]
  implicit def proxySHashSetCompanionAbs(p: Rep[SHashSetCompanionAbs]): SHashSetCompanionAbs =
    proxyOps[SHashSetCompanionAbs](p)

  // default wrapper implementation
  abstract class SHashSetImpl[A](val wrappedValue: Rep[HashSet[A]])(implicit val eA: Elem[A]) extends SHashSet[A] with Def[SHashSetImpl[A]] {
    lazy val selfType = element[SHashSetImpl[A]]

    def $plus(elem: Rep[A]): Rep[SHashSet[A]] =
      methodCallEx[SHashSet[A]](self,
        this.getClass.getMethod("$plus", classOf[AnyRef]),
        List(elem.asInstanceOf[AnyRef]))

    def map[B:Elem](f: Rep[A => B]): Rep[SHashSet[B]] =
      methodCallEx[SHashSet[B]](self,
        this.getClass.getMethod("map", classOf[AnyRef], classOf[Elem[B]]),
        List(f.asInstanceOf[AnyRef], element[B]))

    def fold(z: Rep[A])(f: Rep[((A, A)) => A]): Rep[A] =
      methodCallEx[A](self,
        this.getClass.getMethod("fold", classOf[AnyRef], classOf[AnyRef]),
        List(z.asInstanceOf[AnyRef], f.asInstanceOf[AnyRef]))
  }
  trait SHashSetImplCompanion
  // elem for concrete class
  class SHashSetImplElem[A](val iso: Iso[SHashSetImplData[A], SHashSetImpl[A]])(implicit override val eA: Elem[A])
    extends SHashSetElem[A, SHashSetImpl[A]]
    with ConcreteElem1[A, SHashSetImplData[A], SHashSetImpl[A], SHashSet] {
    override lazy val parent: Option[Elem[_]] = Some(sHashSetElement(element[A]))
    override lazy val typeArgs = TypeArgs("A" -> (eA -> scalan.util.Invariant))
    override lazy val eTo: Elem[_] = this
    override def convertSHashSet(x: Rep[SHashSet[A]]) = // Converter is not generated by meta
!!!("Cannot convert from SHashSet to SHashSetImpl: missing fields List(wrappedValue)")
    override def getDefaultRep = SHashSetImpl(DefaultOfHashSet[A])
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[SHashSetImpl[A]]
    }
  }

  // state representation type
  type SHashSetImplData[A] = HashSet[A]

  // 3) Iso for concrete class
  class SHashSetImplIso[A](implicit eA: Elem[A])
    extends EntityIso[SHashSetImplData[A], SHashSetImpl[A]] with Def[SHashSetImplIso[A]] {
    override def from(p: Rep[SHashSetImpl[A]]) =
      p.wrappedValue
    override def to(p: Rep[HashSet[A]]) = {
      val wrappedValue = p
      SHashSetImpl(wrappedValue)
    }
    lazy val eFrom = element[HashSet[A]]
    lazy val eTo = new SHashSetImplElem[A](self)
    lazy val selfType = new SHashSetImplIsoElem[A](eA)
    def productArity = 1
    def productElement(n: Int) = eA
  }
  case class SHashSetImplIsoElem[A](eA: Elem[A]) extends Elem[SHashSetImplIso[A]] {
    def getDefaultRep = reifyObject(new SHashSetImplIso[A]()(eA))
    lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[SHashSetImplIso[A]]
    }
    lazy val typeArgs = TypeArgs("A" -> (eA -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class SHashSetImplCompanionAbs extends CompanionDef[SHashSetImplCompanionAbs] {
    def selfType = SHashSetImplCompanionElem
    override def toString = "SHashSetImpl"

    @scalan.OverloadId("fromFields")
    def apply[A](wrappedValue: Rep[HashSet[A]])(implicit eA: Elem[A]): Rep[SHashSetImpl[A]] =
      mkSHashSetImpl(wrappedValue)

    def unapply[A](p: Rep[SHashSet[A]]) = unmkSHashSetImpl(p)
  }
  lazy val SHashSetImplRep: Rep[SHashSetImplCompanionAbs] = new SHashSetImplCompanionAbs
  lazy val SHashSetImpl: SHashSetImplCompanionAbs = proxySHashSetImplCompanion(SHashSetImplRep)
  implicit def proxySHashSetImplCompanion(p: Rep[SHashSetImplCompanionAbs]): SHashSetImplCompanionAbs = {
    proxyOps[SHashSetImplCompanionAbs](p)
  }

  implicit case object SHashSetImplCompanionElem extends CompanionElem[SHashSetImplCompanionAbs] {
    lazy val tag = weakTypeTag[SHashSetImplCompanionAbs]
    protected def getDefaultRep = SHashSetImpl
  }

  implicit def proxySHashSetImpl[A](p: Rep[SHashSetImpl[A]]): SHashSetImpl[A] =
    proxyOps[SHashSetImpl[A]](p)

  implicit class ExtendedSHashSetImpl[A](p: Rep[SHashSetImpl[A]])(implicit eA: Elem[A]) {
    def toData: Rep[SHashSetImplData[A]] = isoSHashSetImpl(eA).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoSHashSetImpl[A](implicit eA: Elem[A]): Iso[SHashSetImplData[A], SHashSetImpl[A]] =
    reifyObject(new SHashSetImplIso[A]()(eA))

  // 6) smart constructor and deconstructor
  def mkSHashSetImpl[A](wrappedValue: Rep[HashSet[A]])(implicit eA: Elem[A]): Rep[SHashSetImpl[A]]
  def unmkSHashSetImpl[A](p: Rep[SHashSet[A]]): Option[(Rep[HashSet[A]])]

  registerModule(HashSets_Module)
}

// Std -----------------------------------
trait HashSetsStd extends scalan.ScalanDslStd with HashSetsDsl {
  self: HashSetsDslStd =>

  lazy val SHashSet: Rep[SHashSetCompanionAbs] = new SHashSetCompanionAbs {
    override def empty[A:Elem]: Rep[SHashSet[A]] =
      SHashSetImpl(HashSet.empty[A])
  }

  // override proxy if we deal with TypeWrapper
  //override def proxyHashSet[A:Elem](p: Rep[HashSet[A]]): SHashSet[A] =
  //  proxyOpsEx[HashSet[A], SHashSet[A], StdSHashSetImpl[A]](p, bt => StdSHashSetImpl(bt))

  case class StdSHashSetImpl[A]
      (override val wrappedValue: Rep[HashSet[A]])(implicit eA: Elem[A])
    extends SHashSetImpl[A](wrappedValue) with SHashSetImplDecls[A] {
    override def $plus(elem: Rep[A]): Rep[SHashSet[A]] =
      SHashSetImpl(wrappedValue.$plus(elem))

    override def fold(z: Rep[A])(f: Rep[((A, A)) => A]): Rep[A] =
      wrappedValue.fold(z)(scala.Function.untupled(f))
  }

  def mkSHashSetImpl[A]
    (wrappedValue: Rep[HashSet[A]])(implicit eA: Elem[A]): Rep[SHashSetImpl[A]] =
    new StdSHashSetImpl[A](wrappedValue)
  def unmkSHashSetImpl[A](p: Rep[SHashSet[A]]) = p match {
    case p: SHashSetImpl[A] @unchecked =>
      Some((p.wrappedValue))
    case _ => None
  }

  implicit def wrapHashSetToSHashSet[A:Elem](v: HashSet[A]): SHashSet[A] = SHashSetImpl(v)
}

// Exp -----------------------------------
trait HashSetsExp extends scalan.ScalanDslExp with HashSetsDsl {
  self: HashSetsDslExp =>

  lazy val SHashSet: Rep[SHashSetCompanionAbs] = new SHashSetCompanionAbs {
    def empty[A:Elem]: Rep[SHashSet[A]] =
      methodCallEx[SHashSet[A]](self,
        this.getClass.getMethod("empty", classOf[Elem[A]]),
        List(element[A]))
  }

  case class ViewSHashSet[A, B](source: Rep[SHashSet[A]], override val innerIso: Iso[A, B])
    extends View1[A, B, SHashSet](sHashSetIso(innerIso)) {
    override def toString = s"ViewSHashSet[${innerIso.eTo.name}]($source)"
    override def equals(other: Any) = other match {
      case v: ViewSHashSet[_, _] => source == v.source && innerIso.eTo == v.innerIso.eTo
      case _ => false
    }
  }

  case class ExpSHashSetImpl[A]
      (override val wrappedValue: Rep[HashSet[A]])(implicit eA: Elem[A])
    extends SHashSetImpl[A](wrappedValue)

  object SHashSetImplMethods {
  }

  def mkSHashSetImpl[A]
    (wrappedValue: Rep[HashSet[A]])(implicit eA: Elem[A]): Rep[SHashSetImpl[A]] =
    new ExpSHashSetImpl[A](wrappedValue)
  def unmkSHashSetImpl[A](p: Rep[SHashSet[A]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: SHashSetImplElem[A] @unchecked =>
      Some((p.asRep[SHashSetImpl[A]].wrappedValue))
    case _ =>
      None
  }

  object SHashSetMethods {
    object + {
      def unapply(d: Def[_]): Option[(Rep[SHashSet[A]], Rep[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(elem, _*), _) if receiver.elem.isInstanceOf[SHashSetElem[_, _]] && method.getName == "$plus" =>
          Some((receiver, elem)).asInstanceOf[Option[(Rep[SHashSet[A]], Rep[A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SHashSet[A]], Rep[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object map {
      def unapply(d: Def[_]): Option[(Rep[SHashSet[A]], Rep[A => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[SHashSetElem[_, _]] && method.getName == "map" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[SHashSet[A]], Rep[A => B]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SHashSet[A]], Rep[A => B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fold {
      def unapply(d: Def[_]): Option[(Rep[SHashSet[A]], Rep[A], Rep[((A, A)) => A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(z, f, _*), _) if receiver.elem.isInstanceOf[SHashSetElem[_, _]] && method.getName == "fold" =>
          Some((receiver, z, f)).asInstanceOf[Option[(Rep[SHashSet[A]], Rep[A], Rep[((A, A)) => A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SHashSet[A]], Rep[A], Rep[((A, A)) => A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object SHashSetCompanionMethods {
    object empty {
      def unapply(d: Def[_]): Option[Unit forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem == SHashSetCompanionElem && method.getName == "empty" =>
          Some(()).asInstanceOf[Option[Unit forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Unit forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object UserTypeSHashSet {
    def unapply(s: Exp[_]): Option[Iso[_, _]] = {
      s.elem match {
        case e: SHashSetElem[a,to] => e.eItem match {
          case UnpackableElem(iso) => Some(iso)
          case _ => None
        }
        case _ => None
      }
    }
  }

  override def unapplyViews[T](s: Exp[T]): Option[Unpacked[T]] = (s match {
    case Def(view: ViewSHashSet[_, _]) =>
      Some((view.source, view.iso))
    case UserTypeSHashSet(iso: Iso[a, b]) =>
      val newIso = sHashSetIso(iso)
      val repr = reifyObject(UnpackView(s.asRep[SHashSet[b]], newIso))
      Some((repr, newIso))
    case _ =>
      super.unapplyViews(s)
  }).asInstanceOf[Option[Unpacked[T]]]

  override def rewriteDef[T](d: Def[T]) = d match {
    case view1@ViewSHashSet(Def(view2@ViewSHashSet(arr, innerIso2)), innerIso1) =>
      val compIso = composeIso(innerIso1, innerIso2)
      implicit val eAB = compIso.eTo
      ViewSHashSet(arr, compIso)

    case SHashSetMethods.map(xs, f) => (xs, f) match {
      case (_, Def(IdentityLambda())) =>
        xs
      case (xs: RHS[a] @unchecked, LambdaResultHasViews(f, iso: Iso[b, c])) =>
        val f1 = f.asRep[a => c]
        implicit val eB = iso.eFrom
        val s = xs.map(f1 >> iso.fromFun)
        val res = ViewSHashSet(s, iso)
        res
      case (HasViews(source, Def(contIso: SHashSetIso[a, b])), f: RFunc[_, c]@unchecked) =>
        val f1 = f.asRep[b => c]
        val iso = contIso.innerIso
        implicit val eC = f1.elem.eRange
        source.asRep[SHashSet[a]].map(iso.toFun >> f1)
      case _ =>
        super.rewriteDef(d)
    }
    case _ => super.rewriteDef(d)
  }
}

object HashSets_Module extends scalan.ModuleInfo {
  val dump = "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"
}
}

trait HashSetsDsl extends impl.HashSetsAbs
trait HashSetsDslExp extends impl.HashSetsExp
