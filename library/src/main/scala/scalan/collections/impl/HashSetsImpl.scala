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

  implicit def unwrapValueOfSHashSet[A](w: Rep[SHashSet[A]]): Rep[HashSet[A]] = w.wrappedValue

  implicit def hashSetElement[A:Elem]: Elem[HashSet[A]] =
    element[SHashSet[A]].asInstanceOf[WrapperElem1[_, _, CBase, CW] forSome { type CBase[_]; type CW[_] }].baseElem.asInstanceOf[Elem[HashSet[A]]]

  implicit def castSHashSetElement[A](elem: Elem[SHashSet[A]]): SHashSetElem[A, SHashSet[A]] =
    elem.asInstanceOf[SHashSetElem[A, SHashSet[A]]]

  implicit lazy val containerHashSet: Container[HashSet] = new Container[HashSet] {
    def tag[A](implicit evA: WeakTypeTag[A]) = weakTypeTag[HashSet[A]]
    def lift[A](implicit evA: Elem[A]) = element[HashSet[A]]
  }

  implicit lazy val containerSHashSet: Container[SHashSet] = new Container[SHashSet] {
    def tag[A](implicit evA: WeakTypeTag[A]) = weakTypeTag[SHashSet[A]]
    def lift[A](implicit evA: Elem[A]) = element[SHashSet[A]]
  }

  case class SHashSetIso[A, B](iso: Iso[A, B]) extends Iso1[A, B, SHashSet](iso) {
    def from(x: Rep[SHashSet[B]]) = x.map(iso.fromFun)
    def to(x: Rep[SHashSet[A]]) = x.map(iso.toFun)
  }

  // familyElem
  class SHashSetElem[A, To <: SHashSet[A]](implicit _eA: Elem[A])
    extends WrapperElem1[A, To, HashSet, SHashSet](_eA, container[HashSet], container[SHashSet]) {
    def eA = _eA
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
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[SHashSet[A]] => convertSHashSet(x) }
      tryConvert(element[SHashSet[A]], this, x, conv)
    }

    def convertSHashSet(x: Rep[SHashSet[A]]): Rep[To] = {
      x.selfType1 match {
        case _: SHashSetElem[_, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have SHashSetElem[_, _], but got $e")
      }
    }
    lazy val baseElem =
      new BaseTypeElem1[A, HashSet, SHashSet[A]](this.asInstanceOf[Element[SHashSet[A]]])(
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
  implicit def proxySHashSetCompanion(p: Rep[SHashSetCompanion]): SHashSetCompanion =
    proxyOps[SHashSetCompanion](p)

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
    extends Iso[SHashSetImplData[A], SHashSetImpl[A]] {
    override def from(p: Rep[SHashSetImpl[A]]) =
      p.wrappedValue
    override def to(p: Rep[HashSet[A]]) = {
      val wrappedValue = p
      SHashSetImpl(wrappedValue)
    }
    lazy val eTo = new SHashSetImplElem[A](this)
  }
  // 4) constructor and deconstructor
  class SHashSetImplCompanionAbs extends CompanionDef[SHashSetImplCompanionAbs] {
    def selfType = SHashSetImplCompanionElem
    override def toString = "SHashSetImpl"

    def apply[A](wrappedValue: Rep[HashSet[A]])(implicit eA: Elem[A]): Rep[SHashSetImpl[A]] =
      mkSHashSetImpl(wrappedValue)
  }
  object SHashSetImplMatcher {
    def unapply[A](p: Rep[SHashSet[A]]) = unmkSHashSetImpl(p)
  }
  lazy val SHashSetImpl: Rep[SHashSetImplCompanionAbs] = new SHashSetImplCompanionAbs
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
  def mkSHashSetImpl[A](wrappedValue: Rep[HashSet[A]])(implicit eA: Elem[A]): Rep[SHashSetImpl[A]]
  def unmkSHashSetImpl[A](p: Rep[SHashSet[A]]): Option[(Rep[HashSet[A]])]

  registerModule(scalan.meta.ScalanCodegen.loadModule(HashSets_Module.dump))
}

// Seq -----------------------------------
trait HashSetsSeq extends HashSetsDsl with scalan.ScalanSeq {
  self: ScalanCommunityDslSeq =>
  lazy val SHashSet: Rep[SHashSetCompanionAbs] = new SHashSetCompanionAbs {
    override def empty[A:Elem]: Rep[SHashSet[A]] =
      SHashSetImpl(HashSet.empty[A])
  }

  // override proxy if we deal with TypeWrapper
  //override def proxyHashSet[A:Elem](p: Rep[HashSet[A]]): SHashSet[A] =
  //  proxyOpsEx[HashSet[A], SHashSet[A], SeqSHashSetImpl[A]](p, bt => SeqSHashSetImpl(bt))

  case class SeqSHashSetImpl[A]
      (override val wrappedValue: Rep[HashSet[A]])(implicit eA: Elem[A])
    extends SHashSetImpl[A](wrappedValue) with SeqSHashSet[A] {
    override def $plus(elem: Rep[A]): Rep[SHashSet[A]] =
      SHashSetImpl(wrappedValue.$plus(elem))

    override def fold(z: Rep[A])(f: Rep[((A, A)) => A]): Rep[A] =
      wrappedValue.fold(z)(scala.Function.untupled(f))
  }

  def mkSHashSetImpl[A]
    (wrappedValue: Rep[HashSet[A]])(implicit eA: Elem[A]): Rep[SHashSetImpl[A]] =
    new SeqSHashSetImpl[A](wrappedValue)
  def unmkSHashSetImpl[A](p: Rep[SHashSet[A]]) = p match {
    case p: SHashSetImpl[A] @unchecked =>
      Some((p.wrappedValue))
    case _ => None
  }

  implicit def wrapHashSetToSHashSet[A:Elem](v: HashSet[A]): SHashSet[A] = SHashSetImpl(v)
}

// Exp -----------------------------------
trait HashSetsExp extends HashSetsDsl with scalan.ScalanExp {
  self: ScalanCommunityDslExp =>
  lazy val SHashSet: Rep[SHashSetCompanionAbs] = new SHashSetCompanionAbs {
    def empty[A:Elem]: Rep[SHashSet[A]] =
      methodCallEx[SHashSet[A]](self,
        this.getClass.getMethod("empty", classOf[Elem[A]]),
        List(element[A]))
  }

  case class ViewSHashSet[A, B](source: Rep[SHashSet[A]])(iso: Iso1[A, B, SHashSet])
    extends View1[A, B, SHashSet](iso) {
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
          val newCall = unwrapMethodCall(mc, wrapper.wrappedValue, eRes)
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
  val dump = "H4sIAAAAAAAAALVWT2wUVRh/O9vtdru1hVpRDFVoVmsVuqVRieGA/bPFyvZPOoC6EvTtzNsyMP925i3OmoiEqAe4KTGRxCAHPfXmQePBg4mJkcREQ8REOXjQg4AxRCVGUb/3Zt7sztJpi4l7mJ038+b78/v9vu97S1dRynXQ/a6CdWwOG4TiYZnfj7k0JxdMqtH6jKXWdDJJKic2fKjMmOOuhHpKqP0QdiddvYQy/k3Bs8N7mVSLKINNhbjUclyKthS5h7xi6TpRqGaZec0wahSXdZIvai7dWURtZUutV9ExlCiidYplKg6hRJ7QsesSN3jeQVhEWrjO8HV9zm74MPMsi3xTFnsdrFEIH3ys8/cvEFuum5ZZNyjqDkKbs1lYsCetGbblUOEiDeYOWapYtpkYHqDe4mF8FOfBxWJepo5mLsKXWRsrR/AimYUtbHsbBOwSvbK3bvN1sog6XVIFgKYNW+dPPBshBAyM8iCGG/gMh/gMM3xyMnE0rGsvYvZy3rG8OvJ/iSRCng0mtq5iQlggBVPNnTygPHtdzhoS+9hjoaR5hu1g6N4YNXAqAMfPFl53r+0+t0NCnSXUqbljZZc6WKHNlAdoZbFpWpTHHAKInUVgayCOLe5lDPa0SCKjWIaNTbAUQNkFPOmaolG2mT3rCtiJgT5NbSK2Jjw7Eea7OSZfrpsJrOvzlzduu+9K4WkJSVEXGTApg/AdYZSiDvkJ7B6SCQ3Ms2sPRYkxjjG7ZLzGNb2C+xCIwcs/q5+OoANSCF/gbW2MgYmU+83X2QtDuyTUUeL6ntLxYgkQdAs6MeacCcukJdRhHSWO/yZ9FOvsblkG0yqp4JpOA1ybAUkCIBRtjq1EmzC0dnLVJwQAWV+4s5ZJclPzud/lz08vMV06qMt/45fm39qOG992VyiXLEVdLzjYtom6H+s1ImBOQmVHgU/fCh0BKezSz7fe0fTZXQkRM39PkUTGhL02huOqLiBooQ7RAPpDDvvjVMhVu2Gh2Kdf3fWxhFJPolQFqHGLKFW2aqYqygFaJiUeHRfPElFqQP7YwYbQjt88NiMeRBhp300xr6ox0V1/K41Iv268eFZCGZBSWaMGtnMja+wJ/2OdoygtWbbzKS4dP6J2dhkQr2+tfJvg2bYSPFBhvBLDxLXc1sd+nDy9h3eUngYgfFuQV3OlU3Qbq1GsmcQRqTZFw6qo068V2TLI+oFr2sFzpyhvGAkvOq3myodhPOzk323i9h9pgair4E0IDrZHX4XwLN/NGtUBEa8XeyeaCfW1ZrPr+nC9vWHj0agg0+BTMAJ5BCA3ZpyP1iTkPxBDgByoAyR67Prbsw9+8f4PHPVOpjNoOWY46Rui8lqaSK9vDxIxaiacH2CCNwEA5c8kGEbyQGwkVVb2xACfnO0TaP/S5Og7Bueph3i+zGeazhtNPT5uTPjbIbtLz1SrH4w81M3Hcktrhyk9HVQRX8xBp3c0lSxbjVkQnhzA21qgLf2+k40M3cLqtGhDkeoronZYLvjt2T/rNJgN8doUk9m4bilH9m37qVLte/iGP3s1QF+EBKVCKeqPmzXBoBHlEecE8HHp3TN9X5b3LJ70C0Zh3xT4mepOv6icGhwbDTI8bnlE3Qcx0Pde2t1/5fyZYBa151hguah0/dZ+IBx0gsgtKxLJsBv6ZPDNg7WPTsUP/JXFADaS3/1z/g3HTUoovZaJ/1/mPLvdGOUUhmKisswsdtA98dNtqmYqF6bfur2n/7nvOcvtqmVAp+P2Ycg50LWEs5Z+HC7HV2rPK2lszLb1+iuVobNfnfnrZYmlmWJ4CwiSlRqP4/kikmgLC41rPfIk3tm0CpU/2Pvun79kLs5JrS2H/ZmR4OMPBdDUoECzlzRCHv/jVb82oH84Qawp2trKjrccmIAemM/LT/vjy5xT+K7XGrL2kx2NnCL8F8FhSLTGCDUBHTcLZJXpG0fvvz3Q0DG/DgAA"
}
}

trait HashSetsDsl extends impl.HashSetsAbs {self: ScalanCommunityDsl =>}
trait HashSetsDslExp extends impl.HashSetsExp {self: ScalanCommunityDslExp =>}
