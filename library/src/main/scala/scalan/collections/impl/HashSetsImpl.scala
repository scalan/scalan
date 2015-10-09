package scalan.collections

import scala.collection.immutable.HashSet
import scalan._
import scalan.common.Default
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait HashSetsAbs extends HashSets with scalan.Scalan {
  self: ScalanCommunityDsl =>

  // single proxy for each type family
  implicit def proxySHashSet[A](p: Rep[SHashSet[A]]): SHashSet[A] = {
    proxyOps[SHashSet[A]](p)(scala.reflect.classTag[SHashSet[A]])
  }

  // TypeWrapper proxy
  //implicit def proxyHashSet[A:Elem](p: Rep[HashSet[A]]): SHashSet[A] =
  //  proxyOps[SHashSet[A]](p.asRep[SHashSet[A]])

  implicit def unwrapValueOfSHashSet[A](w: Rep[SHashSet[A]]): Rep[HashSet[A]] = w.wrappedValueOfBaseType

  implicit def hashSetElement[A:Elem]: Elem[HashSet[A]]

  implicit def castSHashSetElement[A](elem: Elem[SHashSet[A]]): SHashSetElem[A, SHashSet[A]] = elem.asInstanceOf[SHashSetElem[A, SHashSet[A]]]

  implicit lazy val containerHashSet: Cont[HashSet] = new Container[HashSet] {
    def tag[A](implicit evA: WeakTypeTag[A]) = weakTypeTag[HashSet[A]]
    def lift[A](implicit evA: Elem[A]) = element[HashSet[A]]
  }

  implicit lazy val containerSHashSet: Cont[SHashSet] = new Container[SHashSet] {
    def tag[A](implicit evA: WeakTypeTag[A]) = weakTypeTag[SHashSet[A]]
    def lift[A](implicit evA: Elem[A]) = element[SHashSet[A]]
  }
  case class SHashSetIso[A,B](iso: Iso[A,B]) extends Iso1[A, B, SHashSet](iso) {
    def from(x: Rep[SHashSet[B]]) = x.map(iso.fromFun)
    def to(x: Rep[SHashSet[A]]) = x.map(iso.toFun)
    lazy val defaultRepTo = SHashSet.empty[B]
  }

  // familyElem
  abstract class SHashSetElem[A, To <: SHashSet[A]](implicit val eA: Elem[A])
    extends WrapperElem1[A, To, HashSet, SHashSet]()(eA, container[HashSet], container[SHashSet]) {
    lazy val parent: Option[Elem[_]] = None
    lazy val entityDef: STraitOrClassDef = {
      val module = getModules("HashSets")
      module.entities.find(_.name == "SHashSet").get
    }
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eA))
    }
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[SHashSet[A]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Reifiable[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[SHashSet[A]] => convertSHashSet(x) }
      tryConvert(element[SHashSet[A]], this, x, conv)
    }

    def convertSHashSet(x : Rep[SHashSet[A]]): Rep[To] = {
      assert(x.selfType1 match { case _: SHashSetElem[_, _] => true; case _ => false })
      x.asRep[To]
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def sHashSetElement[A](implicit eA: Elem[A]): Elem[SHashSet[A]] =
    elemCache.getOrElseUpdate(
      (classOf[SHashSetElem[A, SHashSet[A]]], Seq(eA)),
      new SHashSetElem[A, SHashSet[A]] {
        lazy val eTo = element[SHashSetImpl[A]]
      }).asInstanceOf[Elem[SHashSet[A]]]

  implicit case object SHashSetCompanionElem extends CompanionElem[SHashSetCompanionAbs] {
    lazy val tag = weakTypeTag[SHashSetCompanionAbs]
    protected def getDefaultRep = SHashSet
  }

  abstract class SHashSetCompanionAbs extends CompanionBase[SHashSetCompanionAbs] with SHashSetCompanion {
    override def toString = "SHashSet"
  }
  def SHashSet: Rep[SHashSetCompanionAbs]
  implicit def proxySHashSetCompanion(p: Rep[SHashSetCompanion]): SHashSetCompanion =
    proxyOps[SHashSetCompanion](p)

  // default wrapper implementation
  abstract class SHashSetImpl[A](val wrappedValueOfBaseType: Rep[HashSet[A]])(implicit val eA: Elem[A]) extends SHashSet[A] {
    def $plus(elem: Rep[A]): Rep[SHashSet[A]] =
      methodCallEx[SHashSet[A]](self,
        this.getClass.getMethod("$plus", classOf[AnyRef]),
        List(elem))

    def map[B:Elem](f: Rep[A => B]): Rep[SHashSet[B]] =
      methodCallEx[SHashSet[B]](self,
        this.getClass.getMethod("map", classOf[AnyRef], classOf[Elem[B]]),
        List(f, element[B]))

    def fold(z: Rep[A])(f: Rep[((A, A)) => A]): Rep[A] =
      methodCallEx[A](self,
        this.getClass.getMethod("fold", classOf[AnyRef], classOf[AnyRef]),
        List(z, f))
  }
  trait SHashSetImplCompanion
  // elem for concrete class
  class SHashSetImplElem[A](val iso: Iso[SHashSetImplData[A], SHashSetImpl[A]])(implicit eA: Elem[A])
    extends SHashSetElem[A, SHashSetImpl[A]]
    with ConcreteElem1[A, SHashSetImplData[A], SHashSetImpl[A], SHashSet] {
    override lazy val parent: Option[Elem[_]] = Some(sHashSetElement(element[A]))
    override lazy val entityDef = {
      val module = getModules("HashSets")
      module.concreteSClasses.find(_.name == "SHashSetImpl").get
    }
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eA))
    }
    lazy val eTo = this
    override def convertSHashSet(x: Rep[SHashSet[A]]) = SHashSetImpl(x.wrappedValueOfBaseType)
    override def getDefaultRep = super[ConcreteElem1].getDefaultRep
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[SHashSetImpl[A]]
    }
  }

  // state representation type
  type SHashSetImplData[A] = HashSet[A]

  // 3) Iso for concrete class
  class SHashSetImplIso[A](implicit eA: Elem[A])
    extends Iso[SHashSetImplData[A], SHashSetImpl[A]] {
    override def from(p: Rep[SHashSetImpl[A]]) =
      p.wrappedValueOfBaseType
    override def to(p: Rep[HashSet[A]]) = {
      val wrappedValueOfBaseType = p
      SHashSetImpl(wrappedValueOfBaseType)
    }
    lazy val defaultRepTo: Rep[SHashSetImpl[A]] = SHashSetImpl(DefaultOfHashSet[A].value)
    lazy val eTo = new SHashSetImplElem[A](this)
  }
  // 4) constructor and deconstructor
  abstract class SHashSetImplCompanionAbs extends CompanionBase[SHashSetImplCompanionAbs] {
    override def toString = "SHashSetImpl"

    def apply[A](wrappedValueOfBaseType: Rep[HashSet[A]])(implicit eA: Elem[A]): Rep[SHashSetImpl[A]] =
      mkSHashSetImpl(wrappedValueOfBaseType)
  }
  object SHashSetImplMatcher {
    def unapply[A](p: Rep[SHashSet[A]]) = unmkSHashSetImpl(p)
  }
  def SHashSetImpl: Rep[SHashSetImplCompanionAbs]
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
    cachedIso[SHashSetImplIso[A]](eA)

  // 6) smart constructor and deconstructor
  def mkSHashSetImpl[A](wrappedValueOfBaseType: Rep[HashSet[A]])(implicit eA: Elem[A]): Rep[SHashSetImpl[A]]
  def unmkSHashSetImpl[A](p: Rep[SHashSet[A]]): Option[(Rep[HashSet[A]])]

  registerModule(scalan.meta.ScalanCodegen.loadModule(HashSets_Module.dump))
}

// Seq -----------------------------------
trait HashSetsSeq extends HashSetsDsl with scalan.ScalanSeq {
  self: ScalanCommunityDslSeq =>
  lazy val SHashSet: Rep[SHashSetCompanionAbs] = new SHashSetCompanionAbs with UserTypeSeq[SHashSetCompanionAbs] {
    lazy val selfType = element[SHashSetCompanionAbs]
    override def empty[A:Elem]: Rep[SHashSet[A]] =
      SHashSetImpl(HashSet.empty[A])
  }

    // override proxy if we deal with TypeWrapper
  //override def proxyHashSet[A:Elem](p: Rep[HashSet[A]]): SHashSet[A] =
  //  proxyOpsEx[HashSet[A],SHashSet[A], SeqSHashSetImpl[A]](p, bt => SeqSHashSetImpl(bt))

  implicit def hashSetElement[A:Elem]: Elem[HashSet[A]] =
    new SeqBaseElemEx1[A, SHashSet[A], HashSet](element[SHashSet[A]])(
      element[A], container[HashSet], DefaultOfHashSet[A])

  case class SeqSHashSetImpl[A]
      (override val wrappedValueOfBaseType: Rep[HashSet[A]])
      (implicit eA: Elem[A])
    extends SHashSetImpl[A](wrappedValueOfBaseType)
       with SeqSHashSet[A] with UserTypeSeq[SHashSetImpl[A]] {
    lazy val selfType = element[SHashSetImpl[A]]
    override def $plus(elem: Rep[A]): Rep[SHashSet[A]] =
      SHashSetImpl(wrappedValueOfBaseType.$plus(elem))

    override def fold(z: Rep[A])(f: Rep[((A, A)) => A]): Rep[A] =
      wrappedValueOfBaseType.fold(z)(scala.Function.untupled(f))
  }
  lazy val SHashSetImpl = new SHashSetImplCompanionAbs with UserTypeSeq[SHashSetImplCompanionAbs] {
    lazy val selfType = element[SHashSetImplCompanionAbs]
  }

  def mkSHashSetImpl[A]
      (wrappedValueOfBaseType: Rep[HashSet[A]])(implicit eA: Elem[A]): Rep[SHashSetImpl[A]] =
      new SeqSHashSetImpl[A](wrappedValueOfBaseType)
  def unmkSHashSetImpl[A](p: Rep[SHashSet[A]]) = p match {
    case p: SHashSetImpl[A] @unchecked =>
      Some((p.wrappedValueOfBaseType))
    case _ => None
  }

  implicit def wrapHashSetToSHashSet[A:Elem](v: HashSet[A]): SHashSet[A] = SHashSetImpl(v)
}

// Exp -----------------------------------
trait HashSetsExp extends HashSetsDsl with scalan.ScalanExp {
  self: ScalanCommunityDslExp =>
  lazy val SHashSet: Rep[SHashSetCompanionAbs] = new SHashSetCompanionAbs with UserTypeDef[SHashSetCompanionAbs] {
    lazy val selfType = element[SHashSetCompanionAbs]
    override def mirror(t: Transformer) = this

    def empty[A:Elem]: Rep[SHashSet[A]] =
      methodCallEx[SHashSet[A]](self,
        this.getClass.getMethod("empty", classOf[Elem[A]]),
        List(element[A]))
  }

  case class ViewSHashSet[A, B](source: Rep[SHashSet[A]])(iso: Iso1[A, B, SHashSet])
    extends View1[A, B, SHashSet](iso) {
    def copy(source: Rep[SHashSet[A]]) = ViewSHashSet(source)(iso)
    override def toString = s"ViewSHashSet[${innerIso.eTo.name}]($source)"
    override def equals(other: Any) = other match {
      case v: ViewSHashSet[_, _] => source == v.source && innerIso.eTo == v.innerIso.eTo
      case _ => false
    }
  }

  implicit def hashSetElement[A:Elem]: Elem[HashSet[A]] =
    new ExpBaseElemEx1[A, SHashSet[A], HashSet](element[SHashSet[A]])(
      element[A], container[HashSet], DefaultOfHashSet[A])

  case class ExpSHashSetImpl[A]
      (override val wrappedValueOfBaseType: Rep[HashSet[A]])
      (implicit eA: Elem[A])
    extends SHashSetImpl[A](wrappedValueOfBaseType) with UserTypeDef[SHashSetImpl[A]] {
    lazy val selfType = element[SHashSetImpl[A]]
    override def mirror(t: Transformer) = ExpSHashSetImpl[A](t(wrappedValueOfBaseType))
  }

  lazy val SHashSetImpl: Rep[SHashSetImplCompanionAbs] = new SHashSetImplCompanionAbs with UserTypeDef[SHashSetImplCompanionAbs] {
    lazy val selfType = element[SHashSetImplCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object SHashSetImplMethods {
  }

  def mkSHashSetImpl[A]
    (wrappedValueOfBaseType: Rep[HashSet[A]])(implicit eA: Elem[A]): Rep[SHashSetImpl[A]] =
    new ExpSHashSetImpl[A](wrappedValueOfBaseType)
  def unmkSHashSetImpl[A](p: Rep[SHashSet[A]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: SHashSetImplElem[A] @unchecked =>
      Some((p.asRep[SHashSetImpl[A]].wrappedValueOfBaseType))
    case _ =>
      None
  }

  object SHashSetMethods {
    object wrappedValueOfBaseType {
      def unapply(d: Def[_]): Option[Rep[SHashSet[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SHashSetElem[_, _]] && method.getName == "wrappedValueOfBaseType" =>
          Some(receiver).asInstanceOf[Option[Rep[SHashSet[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[SHashSet[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

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
      val newIso = SHashSetIso(iso)
      val repr = reifyObject(UnpackView(s.asRep[SHashSet[b]])(newIso))
      Some((repr, newIso))
    case _ =>
      super.unapplyViews(s)
  }).asInstanceOf[Option[Unpacked[T]]]

  override def rewriteDef[T](d: Def[T]) = d match {
    case SHashSetMethods.map(xs, Def(IdentityLambda())) => xs

    case view1@ViewSHashSet(Def(view2@ViewSHashSet(arr))) =>
      val compIso = composeIso(view1.innerIso, view2.innerIso)
      implicit val eAB = compIso.eTo
      ViewSHashSet(arr)(SHashSetIso(compIso))

    // Rule: W(a).m(args) ==> iso.to(a.m(unwrap(args)))
    case mc @ MethodCall(Def(wrapper: ExpSHashSetImpl[_]), m, args, neverInvoke) if !isValueAccessor(m) =>
      val resultElem = mc.selfType
      val wrapperIso = getIsoByElem(resultElem)
      wrapperIso match {
        case iso: Iso[base,ext] =>
          val eRes = iso.eFrom
          val newCall = unwrapMethodCall(mc, wrapper.wrappedValueOfBaseType, eRes)
          iso.to(newCall)
      }

    case SHashSetMethods.map(xs, f) => (xs, f) match {
      case (xs: RHS[a] @unchecked, LambdaResultHasViews(f, iso: Iso[b, c])) =>
        val f1 = f.asRep[a => c]
        implicit val eA = xs.elem.eItem
        implicit val eB = iso.eFrom
        val s = xs.map(fun { x =>
          val tmp = f1(x)
          iso.from(tmp)
        })
        val res = ViewSHashSet(s)(SHashSetIso(iso))
        res
      case (HasViews(source, contIso: SHashSetIso[a, b]), f: Rep[Function1[_, c] @unchecked]) =>
        val f1 = f.asRep[b => c]
        val iso = contIso.iso
        implicit val eA = iso.eFrom
        implicit val eB = iso.eTo
        implicit val eC = f1.elem.eRange
        source.asRep[SHashSet[a]].map(fun { x => f1(iso.to(x)) })
      case _ =>
        super.rewriteDef(d)
    }

    case _ => super.rewriteDef(d)
  }
}

object HashSets_Module {
  val packageName = "scalan.collections"
  val name = "HashSets"
  val dump = "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"
}
}

trait HashSetsDsl extends impl.HashSetsAbs {self: ScalanCommunityDsl =>}
trait HashSetsDslExp extends impl.HashSetsExp {self: ScalanCommunityDslExp =>}
