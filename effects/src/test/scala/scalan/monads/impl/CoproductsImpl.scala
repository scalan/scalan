package scalan.monads

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait CoproductsAbs extends scalan.ScalanDsl with Coproducts {
  self: MonadsDsl =>

  // single proxy for each type family
  implicit def proxyCoproduct[F[_], G[_], A](p: Rep[Coproduct[F, G, A]]): Coproduct[F, G, A] = {
    proxyOps[Coproduct[F, G, A]](p)(scala.reflect.classTag[Coproduct[F, G, A]])
  }

  // familyElem
  class CoproductElem[F[_], G[_], A, To <: Coproduct[F, G, A]](implicit _cF: Cont[F], _cG: Cont[G], _eA: Elem[A])
    extends EntityElem[To] {
    def cF = _cF
    def cG = _cG
    def eA = _eA
    lazy val parent: Option[Elem[_]] = None
    lazy val typeArgs = TypeArgs("F" -> cF, "G" -> cG, "A" -> eA)
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[Coproduct[F, G, A]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[Coproduct[F, G, A]] => convertCoproduct(x) }
      tryConvert(element[Coproduct[F, G, A]], this, x, conv)
    }

    def convertCoproduct(x: Rep[Coproduct[F, G, A]]): Rep[To] = {
      x.selfType1.asInstanceOf[Elem[_]] match {
        case _: CoproductElem[_, _, _, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have CoproductElem[_, _, _, _], but got $e", x)
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def coproductElement[F[_], G[_], A](implicit cF: Cont[F], cG: Cont[G], eA: Elem[A]): Elem[Coproduct[F, G, A]] =
    cachedElem[CoproductElem[F, G, A, Coproduct[F, G, A]]](cF, cG, eA)

  implicit case object CoproductCompanionElem extends CompanionElem[CoproductCompanionAbs] {
    lazy val tag = weakTypeTag[CoproductCompanionAbs]
    protected def getDefaultRep = Coproduct
  }

  abstract class CoproductCompanionAbs extends CompanionDef[CoproductCompanionAbs] with CoproductCompanion {
    def selfType = CoproductCompanionElem
    override def toString = "Coproduct"
  }
  def Coproduct: Rep[CoproductCompanionAbs]
  implicit def proxyCoproductCompanionAbs(p: Rep[CoproductCompanionAbs]): CoproductCompanionAbs =
    proxyOps[CoproductCompanionAbs](p)

  abstract class AbsCoproductImpl[F[_], G[_], A]
      (run: Rep[Either[F[A], G[A]]])(implicit cF: Cont[F], cG: Cont[G], eA: Elem[A])
    extends CoproductImpl[F, G, A](run) with Def[CoproductImpl[F, G, A]] {
    lazy val selfType = element[CoproductImpl[F, G, A]]
  }
  // elem for concrete class
  class CoproductImplElem[F[_], G[_], A](val iso: Iso[CoproductImplData[F, G, A], CoproductImpl[F, G, A]])(implicit override val cF: Cont[F], override val cG: Cont[G], override val eA: Elem[A])
    extends CoproductElem[F, G, A, CoproductImpl[F, G, A]]
    with ConcreteElem[CoproductImplData[F, G, A], CoproductImpl[F, G, A]] {
    override lazy val parent: Option[Elem[_]] = Some(coproductElement(container[F], container[G], element[A]))
    override lazy val typeArgs = TypeArgs("F" -> cF, "G" -> cG, "A" -> eA)

    override def convertCoproduct(x: Rep[Coproduct[F, G, A]]) = CoproductImpl(x.run)
    override def getDefaultRep = CoproductImpl(element[Either[F[A], G[A]]].defaultRepValue)
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[CoproductImpl[F, G, A]]
    }
  }

  // state representation type
  type CoproductImplData[F[_], G[_], A] = Either[F[A], G[A]]

  // 3) Iso for concrete class
  class CoproductImplIso[F[_], G[_], A](implicit cF: Cont[F], cG: Cont[G], eA: Elem[A])
    extends EntityIso[CoproductImplData[F, G, A], CoproductImpl[F, G, A]] with Def[CoproductImplIso[F, G, A]] {
    override def from(p: Rep[CoproductImpl[F, G, A]]) =
      p.run
    override def to(p: Rep[Either[F[A], G[A]]]) = {
      val run = p
      CoproductImpl(run)
    }
    lazy val eFrom = element[Either[F[A], G[A]]]
    lazy val eTo = new CoproductImplElem[F, G, A](self)
    lazy val selfType = new CoproductImplIsoElem[F, G, A](cF, cG, eA)
    def productArity = 3
    def productElement(n: Int) = n match {
      case 0 => cF
      case 1 => cG
      case 2 => eA
    }
  }
  case class CoproductImplIsoElem[F[_], G[_], A](cF: Cont[F], cG: Cont[G], eA: Elem[A]) extends Elem[CoproductImplIso[F, G, A]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new CoproductImplIso[F, G, A]()(cF, cG, eA))
    lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[CoproductImplIso[F, G, A]]
    }
    lazy val typeArgs = TypeArgs("F" -> cF, "G" -> cG, "A" -> eA)
  }
  // 4) constructor and deconstructor
  class CoproductImplCompanionAbs extends CompanionDef[CoproductImplCompanionAbs] with CoproductImplCompanion {
    def selfType = CoproductImplCompanionElem
    override def toString = "CoproductImpl"

    @scalan.OverloadId("fromFields")
    def apply[F[_], G[_], A](run: Rep[Either[F[A], G[A]]])(implicit cF: Cont[F], cG: Cont[G], eA: Elem[A]): Rep[CoproductImpl[F, G, A]] =
      mkCoproductImpl(run)

    def unapply[F[_], G[_], A](p: Rep[Coproduct[F, G, A]]) = unmkCoproductImpl(p)
  }
  lazy val CoproductImplRep: Rep[CoproductImplCompanionAbs] = new CoproductImplCompanionAbs
  lazy val CoproductImpl: CoproductImplCompanionAbs = proxyCoproductImplCompanion(CoproductImplRep)
  implicit def proxyCoproductImplCompanion(p: Rep[CoproductImplCompanionAbs]): CoproductImplCompanionAbs = {
    proxyOps[CoproductImplCompanionAbs](p)
  }

  implicit case object CoproductImplCompanionElem extends CompanionElem[CoproductImplCompanionAbs] {
    lazy val tag = weakTypeTag[CoproductImplCompanionAbs]
    protected def getDefaultRep = CoproductImpl
  }

  implicit def proxyCoproductImpl[F[_], G[_], A](p: Rep[CoproductImpl[F, G, A]]): CoproductImpl[F, G, A] =
    proxyOps[CoproductImpl[F, G, A]](p)

  implicit class ExtendedCoproductImpl[F[_], G[_], A](p: Rep[CoproductImpl[F, G, A]])(implicit cF: Cont[F], cG: Cont[G], eA: Elem[A]) {
    def toData: Rep[CoproductImplData[F, G, A]] = isoCoproductImpl(cF, cG, eA).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoCoproductImpl[F[_], G[_], A](implicit cF: Cont[F], cG: Cont[G], eA: Elem[A]): Iso[CoproductImplData[F, G, A], CoproductImpl[F, G, A]] =
    reifyObject(new CoproductImplIso[F, G, A]()(cF, cG, eA))

  // 6) smart constructor and deconstructor
  def mkCoproductImpl[F[_], G[_], A](run: Rep[Either[F[A], G[A]]])(implicit cF: Cont[F], cG: Cont[G], eA: Elem[A]): Rep[CoproductImpl[F, G, A]]
  def unmkCoproductImpl[F[_], G[_], A](p: Rep[Coproduct[F, G, A]]): Option[(Rep[Either[F[A], G[A]]])]

  registerModule(Coproducts_Module)
}

// Std -----------------------------------
trait CoproductsStd extends scalan.ScalanDslStd with CoproductsDsl {
  self: MonadsDslStd =>

  lazy val Coproduct: Rep[CoproductCompanionAbs] = new CoproductCompanionAbs {
  }

  case class StdCoproductImpl[F[_], G[_], A]
      (override val run: Rep[Either[F[A], G[A]]])(implicit cF: Cont[F], cG: Cont[G], eA: Elem[A])
    extends AbsCoproductImpl[F, G, A](run) {
  }

  def mkCoproductImpl[F[_], G[_], A]
    (run: Rep[Either[F[A], G[A]]])(implicit cF: Cont[F], cG: Cont[G], eA: Elem[A]): Rep[CoproductImpl[F, G, A]] =
    new StdCoproductImpl[F, G, A](run)
  def unmkCoproductImpl[F[_], G[_], A](p: Rep[Coproduct[F, G, A]]) = p match {
    case p: CoproductImpl[F, G, A] @unchecked =>
      Some((p.run))
    case _ => None
  }
}

// Exp -----------------------------------
trait CoproductsExp extends scalan.ScalanDslExp with CoproductsDsl {
  self: MonadsDslExp =>

  lazy val Coproduct: Rep[CoproductCompanionAbs] = new CoproductCompanionAbs {
  }

  case class ExpCoproductImpl[F[_], G[_], A]
      (override val run: Rep[Either[F[A], G[A]]])(implicit cF: Cont[F], cG: Cont[G], eA: Elem[A])
    extends AbsCoproductImpl[F, G, A](run)

  object CoproductImplMethods {
  }

  object CoproductImplCompanionMethods {
  }

  def mkCoproductImpl[F[_], G[_], A]
    (run: Rep[Either[F[A], G[A]]])(implicit cF: Cont[F], cG: Cont[G], eA: Elem[A]): Rep[CoproductImpl[F, G, A]] =
    new ExpCoproductImpl[F, G, A](run)
  def unmkCoproductImpl[F[_], G[_], A](p: Rep[Coproduct[F, G, A]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CoproductImplElem[F, G, A] @unchecked =>
      Some((p.asRep[CoproductImpl[F, G, A]].run))
    case _ =>
      None
  }

  object CoproductMethods {
    object run {
      def unapply(d: Def[_]): Option[Rep[Coproduct[F, G, A]] forSome {type F[_]; type G[_]; type A}] = d match {
        case MethodCall(receiver, method, _, _) if (receiver.elem.asInstanceOf[Elem[_]] match { case _: CoproductElem[_, _, _, _] => true; case _ => false }) && method.getName == "run" =>
          Some(receiver).asInstanceOf[Option[Rep[Coproduct[F, G, A]] forSome {type F[_]; type G[_]; type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Coproduct[F, G, A]] forSome {type F[_]; type G[_]; type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object CoproductCompanionMethods {
  }
}

object Coproducts_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAALVWTYgcRRR+M7O7szOzxnVFY0DJ7jJREZ0JLhhhERlnZ0fD7A/pSMIkbKzprpnt2F3ddtcsPR6iBwmoNxEFwUNE8RIU8eY5goh48OrZU1RCDgkIiq+q/8f5ScD0oehX/fr9fN97r+rqHzDrOvC4qxKDsIpJOako8r3m8rLSYFzngy1L6xt0g3b3Xv7sr3Pmu4ezsNiGuX3ibrhGGwr+S8Ozo3eFay0oEKZSl1uOy2GlJT1UVcswqMp1i1V10+xz0jFotaW7fL0FMx1LG7wBlyDTgkXVYqpDOVXqBnFd6gb781REpEdyQcqDHTv2waoii2oii9MO0TmGjz4Wff1T1FYGzGIDk8OhILQdW4SFOiXq2ZjDK6ZtSDe5FuR107YcHnrNo4d9SwvFGUZwA5ZaF8kBqaLXXlXhjs56wphN1NdJj26jilCfwRxcanRPD2waGC+5XEv582wAQFaelYFVYswqEWYVgVlZoY5ODP1NIj7uOpY3AP/J5AA8G008PcVEaIE2mFZ+77x67rZSMrPiZ0+EkpcBzaGho2MqRNKD2P5w6gP3ZvPKiSwU21DU3VrH5Q5RebIMArhKhDGLy5gjBInTQwZXxzEovdRQZ6hMCqpl2oShpQDLBSTK0FWdC2WxtxDQMwb7PLdpqJrx7EyU7/KYfGUt1Ylh7F4/8syx3xtns5BNuyigSQWbwQmNcijULdvBJlJ5YF+s93PIbMYgC7GZFmtSFEvBi9f8hOginJ64/qf2/XE4n43QDYK5M0LRxNLzn3x3jO5+lYX5tqz/TYP0JLUCvg3qqm2Ytw6o4+/nD4gh3kbSm9dol/QNHoCeRCuHaHFYHtu6NhVQrsuWyITpl/yq3rYYLW/ulm8pP354VRStAwv+F7+X/9FP/P3roS6X9cwh5/RZiG0OJ0Cai7mGzvepM0zQkJykJKZtgtLoVSRR9ENVLJM+sHpT37vyPpdsZbz0KNnpXMTWXZf/LU8gLpxy31y+/NCNzy88KDtxvqNzk9jl43fRh2Hb3MM+gwghv9qPxLJYVpCth6OeEYOxnvS/kvgxAf2jmbBEpBKHrLoZcjJTt9jo5ktwOcJAc5KB5nQDtBYZaBjUnFooHO5L5S3tRD372DjqJbjssLL18ddH97IwexJmu9iMbgtmO1afaSFreKpy6vGXwr1MmjVkiTjEjFiSzzLEmA91hDJS48IwLKPVmpMNieXsnVn6L4wJ089BGvQctkh653+dzoXEuZmoaimvBQFNL/6lKKQRhZ86UJIFMh6hKaTdBdb3kjWxvhYntIYVXxlT8RtUNYhDNdEj1MRLnT/G1j568czJR868KgfpgiaV/C/RGTP6CrpF7HV5YXpywoUJlcoN08YLMb6sXXvhl7d++vILebjEDHIoxuWE3RzGbzGiuVFaq2PSUoKpiTV66fan20/9/O1v8n5RFPMXTzsWXUHjtvWGDrLClvSFN8oEwDh/xEROlOHbYnnnX9bdR8sADAAA"
}
}

