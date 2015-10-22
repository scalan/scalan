package scalan.monads

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait CoproductsAbs extends Coproducts with scalan.Scalan {
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
    lazy val entityDef: STraitOrClassDef = {
      val module = getModules("Coproducts")
      module.entities.find(_.name == "Coproduct").get
    }
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("F" -> Right(cF.asInstanceOf[SomeCont]), "G" -> Right(cG.asInstanceOf[SomeCont]), "A" -> Left(eA))
    }
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
      x.selfType1.asInstanceOf[Element[_]] match {
        case _: CoproductElem[_, _, _, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have CoproductElem[_, _, _, _], but got $e")
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
  implicit def proxyCoproductCompanion(p: Rep[CoproductCompanion]): CoproductCompanion =
    proxyOps[CoproductCompanion](p)

  abstract class AbsCoproductImpl[F[_], G[_], A]
      (run: Rep[Either[F[A], G[A]]])(implicit cF: Cont[F], cG: Cont[G], eA: Elem[A])
    extends CoproductImpl[F, G, A](run) with Def[CoproductImpl[F, G, A]] {
    lazy val selfType = element[CoproductImpl[F, G, A]]
  }
  // elem for concrete class
  class CoproductImplElem[F[_], G[_], A](val iso: Iso[CoproductImplData[F, G, A], CoproductImpl[F, G, A]])(implicit cF: Cont[F], cG: Cont[G], eA: Elem[A])
    extends CoproductElem[F, G, A, CoproductImpl[F, G, A]]
    with ConcreteElem[CoproductImplData[F, G, A], CoproductImpl[F, G, A]] {
    override lazy val parent: Option[Elem[_]] = Some(coproductElement(container[F], container[G], element[A]))
    override lazy val entityDef = {
      val module = getModules("Coproducts")
      module.concreteSClasses.find(_.name == "CoproductImpl").get
    }
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("F" -> Right(cF.asInstanceOf[SomeCont]), "G" -> Right(cG.asInstanceOf[SomeCont]), "A" -> Left(eA))
    }

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
    extends Iso[CoproductImplData[F, G, A], CoproductImpl[F, G, A]] {
    override def from(p: Rep[CoproductImpl[F, G, A]]) =
      p.run
    override def to(p: Rep[Either[F[A], G[A]]]) = {
      val run = p
      CoproductImpl(run)
    }
    lazy val eTo = new CoproductImplElem[F, G, A](this)
  }
  // 4) constructor and deconstructor
  class CoproductImplCompanionAbs extends CompanionDef[CoproductImplCompanionAbs] with CoproductImplCompanion {
    def selfType = CoproductImplCompanionElem
    override def toString = "CoproductImpl"

    def apply[F[_], G[_], A](run: Rep[Either[F[A], G[A]]])(implicit cF: Cont[F], cG: Cont[G], eA: Elem[A]): Rep[CoproductImpl[F, G, A]] =
      mkCoproductImpl(run)
  }
  object CoproductImplMatcher {
    def unapply[F[_], G[_], A](p: Rep[Coproduct[F, G, A]]) = unmkCoproductImpl(p)
  }
  lazy val CoproductImpl: Rep[CoproductImplCompanionAbs] = new CoproductImplCompanionAbs
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
    cachedIso[CoproductImplIso[F, G, A]](cF, cG, eA)

  // 6) smart constructor and deconstructor
  def mkCoproductImpl[F[_], G[_], A](run: Rep[Either[F[A], G[A]]])(implicit cF: Cont[F], cG: Cont[G], eA: Elem[A]): Rep[CoproductImpl[F, G, A]]
  def unmkCoproductImpl[F[_], G[_], A](p: Rep[Coproduct[F, G, A]]): Option[(Rep[Either[F[A], G[A]]])]

  registerModule(scalan.meta.ScalanCodegen.loadModule(Coproducts_Module.dump))
}

// Seq -----------------------------------
trait CoproductsSeq extends CoproductsDsl with scalan.ScalanSeq {
  self: MonadsDslSeq =>
  lazy val Coproduct: Rep[CoproductCompanionAbs] = new CoproductCompanionAbs {
  }

  case class SeqCoproductImpl[F[_], G[_], A]
      (override val run: Rep[Either[F[A], G[A]]])(implicit cF: Cont[F], cG: Cont[G], eA: Elem[A])
    extends AbsCoproductImpl[F, G, A](run) {
  }

  def mkCoproductImpl[F[_], G[_], A]
    (run: Rep[Either[F[A], G[A]]])(implicit cF: Cont[F], cG: Cont[G], eA: Elem[A]): Rep[CoproductImpl[F, G, A]] =
    new SeqCoproductImpl[F, G, A](run)
  def unmkCoproductImpl[F[_], G[_], A](p: Rep[Coproduct[F, G, A]]) = p match {
    case p: CoproductImpl[F, G, A] @unchecked =>
      Some((p.run))
    case _ => None
  }
}

// Exp -----------------------------------
trait CoproductsExp extends CoproductsDsl with scalan.ScalanExp {
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
        case MethodCall(receiver, method, _, _) if (receiver.elem.asInstanceOf[Element[_]] match { case _: CoproductElem[_, _, _, _] => true; case _ => false }) && method.getName == "run" =>
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

object Coproducts_Module {
  val packageName = "scalan.monads"
  val name = "Coproducts"
  val dump = "H4sIAAAAAAAAALVWPYwbRRR+Xp/PZ/tIQoROOgnE3cmAQGCfQCLFFZHj+CyQ70e3kYhMRDRej30bdmf3ZsanNUUKSugQLULp09FQ0SEhCioESNRUIRQRkArEm/H+Gtt3SGSL0c7s2++9+b73ze79h1AQHF4UFnEIq7lUkpqp7xtCVs0Wk7Yc73n9kUOv08GHa19ae+yaMOBiF5aPibgunC6UJjetwI/vTXrSgRJhFhXS40LCZkdnqFue41BL2h6r2647kqTn0HrHFnKnA0s9rz8+gbuQ68Aly2MWp5KaTYcIQUW4vkJVRXY8L+n5+MBPcrC62kU9tYsbnNgSy8cclybxR9Q3x8xjY1fChbC0A1+VhTFF2/U9LqMURYQ79vrRdIkRXIDLnTvklNQxxbBuSm6zIb5Z8Yn1PhnSfQxR4UtYsKDO4MbY1/N8B8qCniBBb7m+o1cCHwBQgdd1EbWEn1rMT03xUzUpt4ljf0DUw0PuBWOYXLk8QOAjxKtnQEQItMX61Y9uWe8+NiuuoV4OVClFvcNlBHp+TjdoKZDHb44+EY/a964YUO5C2RaNnpCcWDItechWhTDmSV1zTCDhQ1Rra55aOksDY6ZaomR5rk8YIoVUrqJOjm3ZUgWrtdVQnTnUF6VPo9Bc4Ofi/W7M2a/umyZxnMMH66+98GvrpgFGNkUJIU1sfB6BSig1PZ+jYSwZ4qvxooTcbkKymraz04aeqqEUJGNxQXUxTy89+K3/9TbcMmJ2w2LOJyhCFMRPP1S+f/mqAStd3f67Dhl2kWDRcqh7wJsek11Y8U4pnzwpnhJH3c0UuNinAzJyZEh7mq888iVhY65RfarI3NGmyEUEVCZ9ve8xWt09rP5pfvvpfdW2HFYnTybO/du+8tfPFwZSd7SEPB+xiN08+j2rxnLLlseUT0s0NU+Lkgi3IGj2qDZRnpRqei59euuR/d69j6XWKxdkz5KD3h00745+b2OBdNGZ9kd32/h9/cfPDSihQj1busSvbp/TiU/QXRCzkgybKMtabA91BDbTSTcTR6ynOH42F/WCDpJgWLsR+UuqM2f6LCXaDID2IoD22QC0EQMoh5zZERKeyuxb48T2fG6exprRtaPOM87Dq18ZUHgbCgN0nehAoeeNWD+SCj+WkgbyWrSWy0qF0hBO3FgafW1AwvlU6x/NjLg9TcvssPZiIDW8cz6kf9OYgn4TsqTn0QvZlf/1IA6NnIS+EVYxp80vx8lntHjmK5FuhflcnCHPf2D1SeqjxttZLAwsJ0KgD6Je9xjpi5BKDltzLGCGhwyqe/fxZ/uvfPfFL/ojXFbHFX4QWPybljR8MHXWl/Z0LvzrShWMzlUHmC72H/cLHqkFCwAA"
}
}

