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
    lazy val typeArgs = TypeArgs("F" -> (cF -> scalan.util.Invariant), "G" -> (cG -> scalan.util.Invariant), "A" -> (eA -> scalan.util.Invariant))
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
    override lazy val typeArgs = TypeArgs("F" -> (cF -> scalan.util.Invariant), "G" -> (cG -> scalan.util.Invariant), "A" -> (eA -> scalan.util.Invariant))

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
    lazy val typeArgs = TypeArgs("F" -> (cF -> scalan.util.Invariant), "G" -> (cG -> scalan.util.Invariant), "A" -> (eA -> scalan.util.Invariant))
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
  val dump = "H4sIAAAAAAAAAL1WS4gcRRj+Z2Z3Z2dmjeuKMQGDu8vEtzPBBSMsIuPszJow+yAdjZkNCTXdNbMdu6vb7ppNj4d4ECLqTcSD4CGieFkEyUU8K4hIDl49e0oMkoMBQfGv6ucM84iizqGoqqn+H9/3f3/V/i8w7TrwiKsSg7CSSTkpKXJecXlRqTGu896GpXUNukbb51/65Pcd850H0zDfhJld4q65RhNy/qTm2dFc4VoDcoSp1OWW43JYakgPZdUyDKpy3WJl3TS7nLQMWm7oLl9twFTL0nqvw2VINWBetZjqUE6VqkFcl7rB/iwVEenROifXvS079sHKIotyIovTDtE5ho8+5v3zp6it9JjFeiaHA0FoW7YIC88UqGdjDidM25BuMg3I6qZtOTz0mkUPu5YWLqcYwQ1YaFwke6SMXjtlhTs66whjNlFfIx26iUfE8SnMwaVG+3TPpoHxgsu1Pn+eDQDIyjMysFKMWSnCrCQwKyrU0Ymhv0HEn9uO5fXA/6UyAJ6NJp6aYCK0QGtMK757Tt25oxTMtPjYE6FkZUAzaOjhERUi6UFsvzv1vnt7/erxNOSbkNfdSsvlDlF5sgwCuAqEMYvLmCMEidNBBpdHMSi9VPDMQJnkVMu0CUNLAZZzSJShqzoXh8XeXEDPCOyz3Kbh0ZRnp6J8F0fkK2upSgxj+8bhp4/erL2ahnS/ixyaVFAMTmiUQ65q2Q6KSOWBfTHeyyFVj0EWy/X+ZUUuxZDz4jE7JroIp0dv3NK+PQbn0hG6QTB3RyiaWHjuo6+P0u0v0jDblPVfN0hHUivgW6Ou2oRZa486/n52jxhiNpTerEbbpGvwAPQkWhlEi8PiSOnaVEC5KiWRCtMv+FW9aTFarG8Xf1O+/2BfFK0Dc/4/vpb/1I//8dOBNpf1zCHjdFmIbQY7QD8XMzWd71JnkKCBdZKSmLYxh4aPIom8H6pimfS+5dv6+avvcclWyutvJVutiyjdVfnd4hjiwi735ZUrD/z66YX7pRJnWzo3iV089jd0GMrmP9QZRAj51X44XothCdk6GGlGNMZq0v9S4sME9A+lwhKRhzik1XrIyVTVYsPFl+ByiIH1cQbWJxuglchAzaDmxELhcE9f3tJOpNkjo6iX4J69qZUO3TpyKQ0zJ2G6jWJ0GzDdsrpMC1nDW5VTj78Y7qX6WUOWiEPM6LLdI3g7YNUgG6FAu1w3yq8E+74s8bcIMS8DqlGGnrgQquBgkJKwWzrBfI+8+ORX+5f064/XpXRjgHbGmFwf71QMZ6PZzt3ZrAz7ICDb9/Qs9HOaQQX27/yrzT+XuJYTopHrlSCgydpaiEIaoqu++ypZf6NhmsD3P4L+f6RTjGqc6QqWZWmE0taoahCHakKb1MTHpN8+Vz584czJQ2delg18TpOH/H+iu23403eD2KvyofbYmIcaHirWTBsf4jhZ+eb5H9/84fPPImVkg+zycZ1hFwnjtxjR3Cit5RFpKUG3xuK9fOfjzSeuX/tZvmvyou/jLcuip2/cLryBCzS3IX3hSzZRudj3xE2QqM+3xPD2XyM9Mcp4DAAA"
}
}

