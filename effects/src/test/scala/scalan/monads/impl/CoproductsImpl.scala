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
    lazy val typeArgs = scala.collection.immutable.ListMap[String, TypeDesc]("F" -> cF, "G" -> cG, "A" -> eA)
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
    override lazy val typeArgs = scala.collection.immutable.ListMap[String, TypeDesc]("F" -> cF, "G" -> cG, "A" -> eA)

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
    def productElement(n: Int) = (cF, cG, eA).productElement(n)
  }
  case class CoproductImplIsoElem[F[_], G[_], A](cF: Cont[F], cG: Cont[G], eA: Elem[A]) extends Elem[CoproductImplIso[F, G, A]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new CoproductImplIso[F, G, A]()(cF, cG, eA))
    lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[CoproductImplIso[F, G, A]]
    }
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
  val dump = "H4sIAAAAAAAAALVWS4gjRRj+k0wmk2Rcx1UXFxRnhvjWZHHAPQwyxEwmuGQeTK/sEhfXSncl02t3VdtVGToeVhHZg3oS8eZhQfGyF/EuHhRkEUERETx7WldkD+5Jsar6HfPYBTeHoqv7z//4vu//q65chzxz4VGmIwuRqo05qmrquc54RWsSbvLhNjUGFt7EvR+e+oqW3n5/JwtLHZg/QGyTWR0o+g9Nz4meNW60oYiIjhmnLuOw0lYRajq1LKxzk5KaadsDjroWrrVNxtfbMNelxvB1uAiZNizplOgu5lhrWIgxzIL3C1hmZEb7otoPd504BqnJKmqJKk67yOQifRFjybffx442JJQMbQ5HgtR2HZmWsCljzxE1vGg7lgqTa0PBtB3q8jBqQUQ4oEa4nSNIvICj7QvoENVE1H5N465J+tKZg/TXUB/vCBNpPidqYNjqnR46OHBeZtxIxfMcABCsPKsSq8aYVSPMqhKzioZdE1nmG0h+3HOpNwT/l8kBeI5w8fQMF6EH3CRG5d1z+ss3tbKdlX/2ZCoFldC8cPTwBIUoegS23+5/wG60Lp/MQqkDJZPVu4y7SOdJGQRwlREhlKucIwSR2xcMrk5iUEWpC5sRmRR1ajuICE8BlouCKMvUTS6N5bvFgJ4J2Be4g0PTjOdkonqXJ9SrtNRAlrV37fgzj/zePJuFbDpEUbjURDO4oVMOxQZ1XNFEOg/8y/VuDpmtGGS5baW3dbWVS9GL18KU7CKcHrv2h/HNCTiXjdANkrk1QoWLPPvl5/JPT2xkYaGj9L9loX5HAMyaFrZ33QYlvAML9BC7/pfCIbLk01iCCwbuoYHFA9iTeOUEXhyWJzavgyWY66opMiEAZV/XO5TgytZe5S/t6odXpGxdWPS/+N38j3ny71+P9LhSNIecOyAhujkxA9JszDdNfoDdUYpG9klSYuKmGI1fZRElP1WN2vie1RvmK5ff44qvjJceJrvdC6J519X/lqdQF865zy9duv/PT87fq3pxoWtyGzmVE7fRiWHj3MFOgwghX+/H471cVgRbx6KukaOxkYy/kvhjAvoHM6FElBGHrL4VcjInBTu2/RJcjnHQmuagNdsBrkcOZOPMFAqHu1J1Kz9R1z40iXoF7rH99n3W9Y0vs5A/BfmeaEbWhnyXDogRsibOVY49/kL4LpNmTbCEXGRHLKnfMsSYj3SENtbi/Cgs481a0x3J5eytefovjAnXz0Ea9JxokfSb/3U+FxMnZ0LVar8WJDRb/EejlMYIP3WkJAUyGaEZpN0G1neSNbm+Ghe0JhRfnaD4TaxbyMWG7BFsi2udP8bWPto4c+qBMy+pQbpoKCP/S3TGjL+EbiNnXV2ZHp9yZRJGlabtiCuxeFj7+vkf3/zus0/V4RIzyKEUy0l0c5g/JchgUVmrE8rSgqkpNHrx5sc7T37/xW/qhlGS81ecdiS6hMZt640cZMVtFUvcKRMAi/kjJ3JChm/J5Z1/AVgfT0YCDAAA"
}
}

