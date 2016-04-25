package scalan.collections

import scala.collection.immutable.HashSet
import scalan._
import scalan.common.Default
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
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eA))
    }
    override def isEntityType = true
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
        element[A], container[HashSet], DefaultOfHashSet[A])
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
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eA))
    }
    override lazy val eTo: Elem[_] = this
    override def convertSHashSet(x: Rep[SHashSet[A]]) = // Converter is not generated by meta
!!!("Cannot convert from SHashSet to SHashSetImpl: missing fields List(wrappedValue)")
    override def getDefaultRep = SHashSetImpl(DefaultOfHashSet[A].value)
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
    def isEntityType = true
    def getDefaultRep = reifyObject(new SHashSetImplIso[A]()(eA))
    lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[SHashSetImplIso[A]]
    }
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
    case SHashSetMethods.map(xs, Def(IdentityLambda())) => xs

    case view1@ViewSHashSet(Def(view2@ViewSHashSet(arr, innerIso2)), innerIso1) =>
      val compIso = composeIso(innerIso1, innerIso2)
      implicit val eAB = compIso.eTo
      ViewSHashSet(arr, compIso)

    // Rule: W(a).m(args) ==> iso.to(a.m(unwrap(args)))
    case mc @ MethodCall(Def(wrapper: ExpSHashSetImpl[_]), m, args, neverInvoke) if !isValueAccessor(m) =>
      val resultElem = mc.selfType
      val wrapperIso = getIsoByElem(resultElem)
      wrapperIso match {
        case iso: Iso[base,ext] =>
          val eRes = iso.eFrom
          val newCall = unwrapMethodCall(mc, wrapper.wrappedValue, eRes)
          iso.to(newCall)
      }

    case SHashSetMethods.map(xs, f) => (xs, f) match {
      case (xs: RHS[a] @unchecked, LambdaResultHasViews(f, iso: Iso[b, c])) =>
        val f1 = f.asRep[a => c]
        implicit val eB = iso.eFrom
        val s = xs.map(f1 >> iso.fromFun)
        val res = ViewSHashSet(s, iso)
        res
      case (HasViews(source, Def(contIso: SHashSetIso[a, b])), f: Rep[Function1[_, c] @unchecked]) =>
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
