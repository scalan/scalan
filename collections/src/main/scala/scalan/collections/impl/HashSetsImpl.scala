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
