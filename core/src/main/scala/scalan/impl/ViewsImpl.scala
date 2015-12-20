package scalan

import java.lang.reflect.Method
import scala.language.higherKinds
import scala.collection.mutable.{Map=>MutMap}
import scala.reflect.ClassTag
import scalan.common.Lazy
import scalan.staged.{BaseExp, Transforming}
import scalan.meta.ScalanAst.STraitOrClassDef
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait ViewsAbs extends Views {
  self: ViewsDsl with Scalan =>

  // single proxy for each type family
  implicit def proxyIsoUR[From, To](p: Rep[IsoUR[From, To]]): IsoUR[From, To] = {
    proxyOps[IsoUR[From, To]](p)(scala.reflect.classTag[IsoUR[From, To]])
  }

  // familyElem
  class IsoURElem[From, To, To0 <: IsoUR[From, To]](implicit _eFrom: Elem[From], _eTo: Elem[To])
    extends EntityElem[To0] {
    def eFrom = _eFrom
    def eTo = _eTo
    lazy val parent: Option[Elem[_]] = None
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("From" -> Left(eFrom), "To" -> Left(eTo))
    }
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagFrom = eFrom.tag
      implicit val tagTo = eTo.tag
      weakTypeTag[IsoUR[From, To]].asInstanceOf[WeakTypeTag[To0]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo0: Elem[To0] = this
      val conv = fun {x: Rep[IsoUR[From, To]] => convertIsoUR(x) }
      tryConvert(element[IsoUR[From, To]], this, x, conv)
    }

    def convertIsoUR(x: Rep[IsoUR[From, To]]): Rep[To0] = {
      x.selfType1 match {
        case _: IsoURElem[_, _, _] => x.asRep[To0]
        case e => !!!(s"Expected $x to have IsoURElem[_, _, _], but got $e", x)
      }
    }

    override def getDefaultRep: Rep[To0] = ???
  }

  implicit def isoURElement[From, To](implicit eFrom: Elem[From], eTo: Elem[To]): Elem[IsoUR[From, To]] =
    cachedElem[IsoURElem[From, To, IsoUR[From, To]]](eFrom, eTo)

  implicit case object IsoURCompanionElem extends CompanionElem[IsoURCompanionAbs] {
    lazy val tag = weakTypeTag[IsoURCompanionAbs]
    protected def getDefaultRep = IsoUR
  }

  abstract class IsoURCompanionAbs extends CompanionDef[IsoURCompanionAbs] {
    def selfType = IsoURCompanionElem
    override def toString = "IsoUR"
  }
  def IsoUR: Rep[IsoURCompanionAbs]
  implicit def proxyIsoURCompanionAbs(p: Rep[IsoURCompanionAbs]): IsoURCompanionAbs =
    proxyOps[IsoURCompanionAbs](p)

  // single proxy for each type family
  implicit def proxyIso1UR[A, B, C[_]](p: Rep[Iso1UR[A, B, C]]): Iso1UR[A, B, C] = {
    proxyOps[Iso1UR[A, B, C]](p)(scala.reflect.classTag[Iso1UR[A, B, C]])
  }
  // familyElem
  class Iso1URElem[A, B, C[_], To <: Iso1UR[A, B, C]](implicit _eA: Elem[A], _eB: Elem[B], _cC: Cont[C])
    extends IsoURElem[C[A], C[B], To] {
    def eA = _eA
    def eB = _eB
    def cC = _cC
    override lazy val parent: Option[Elem[_]] = Some(isoURElement(element[C[A]], element[C[B]]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eA), "B" -> Left(eB), "C" -> Right(cC.asInstanceOf[SomeCont]))
    }
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[Iso1UR[A, B, C]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[Iso1UR[A, B, C]] => convertIso1UR(x) }
      tryConvert(element[Iso1UR[A, B, C]], this, x, conv)
    }

    def convertIso1UR(x: Rep[Iso1UR[A, B, C]]): Rep[To] = {
      x.selfType1.asInstanceOf[Elem[_]] match {
        case _: Iso1URElem[_, _, _, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have Iso1URElem[_, _, _, _], but got $e", x)
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def iso1URElement[A, B, C[_]](implicit eA: Elem[A], eB: Elem[B], cC: Cont[C]): Elem[Iso1UR[A, B, C]] =
    cachedElem[Iso1URElem[A, B, C, Iso1UR[A, B, C]]](eA, eB, cC)

  abstract class AbsIdentityIso[A]
      ()(implicit eA: Elem[A])
    extends IdentityIso[A]() with Def[IdentityIso[A]] {
    lazy val selfType = element[IdentityIso[A]]
  }
  // elem for concrete class
  class IdentityIsoElem[A](val iso: Iso[IdentityIsoData[A], IdentityIso[A]])(implicit  val eA: Elem[A])
    extends IsoURElem[A, A, IdentityIso[A]]
    with ConcreteElem[IdentityIsoData[A], IdentityIso[A]] {
    override lazy val parent: Option[Elem[_]] = Some(isoURElement(element[A], element[A]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eA))
    }

    override def convertIsoUR(x: Rep[IsoUR[A, A]]) = IdentityIso()
    override def getDefaultRep = IdentityIso()
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[IdentityIso[A]]
    }
  }

  // state representation type
  type IdentityIsoData[A] = Unit

  // 3) Iso for concrete class
  class IdentityIsoIso[A](implicit eA: Elem[A])
    extends EntityIso[IdentityIsoData[A], IdentityIso[A]] with Def[IdentityIsoIso[A]] {
    override def from(p: Rep[IdentityIso[A]]) =
      ()
    override def to(p: Rep[Unit]) = {
      val unit = p
      IdentityIso()
    }
    lazy val eFrom = UnitElement
    lazy val eTo = new IdentityIsoElem[A](self)
    lazy val selfType = new IdentityIsoIsoElem[A](eA)
    def productArity = 1
    def productElement(n: Int) = eA
  }
  case class IdentityIsoIsoElem[A](eA: Elem[A]) extends Elem[IdentityIsoIso[A]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new IdentityIsoIso[A]()(eA))
    lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[IdentityIsoIso[A]]
    }
  }
  // 4) constructor and deconstructor
  class IdentityIsoCompanionAbs extends CompanionDef[IdentityIsoCompanionAbs] {
    def selfType = IdentityIsoCompanionElem
    override def toString = "IdentityIso"
    def apply[A](p: Rep[IdentityIsoData[A]])(implicit eA: Elem[A]): Rep[IdentityIso[A]] =
      isoIdentityIso(eA).to(p)
    def apply[A]()(implicit eA: Elem[A]): Rep[IdentityIso[A]] =
      mkIdentityIso()
  }
  object IdentityIsoMatcher {
    def unapply[A](p: Rep[IsoUR[A, A]]) = unmkIdentityIso(p)
  }
  lazy val IdentityIso: Rep[IdentityIsoCompanionAbs] = new IdentityIsoCompanionAbs
  implicit def proxyIdentityIsoCompanion(p: Rep[IdentityIsoCompanionAbs]): IdentityIsoCompanionAbs = {
    proxyOps[IdentityIsoCompanionAbs](p)
  }

  implicit case object IdentityIsoCompanionElem extends CompanionElem[IdentityIsoCompanionAbs] {
    lazy val tag = weakTypeTag[IdentityIsoCompanionAbs]
    protected def getDefaultRep = IdentityIso
  }

  implicit def proxyIdentityIso[A](p: Rep[IdentityIso[A]]): IdentityIso[A] =
    proxyOps[IdentityIso[A]](p)

  implicit class ExtendedIdentityIso[A](p: Rep[IdentityIso[A]])(implicit eA: Elem[A]) {
    def toData: Rep[IdentityIsoData[A]] = isoIdentityIso(eA).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoIdentityIso[A](implicit eA: Elem[A]): Iso[IdentityIsoData[A], IdentityIso[A]] =
    reifyObject(new IdentityIsoIso[A]()(eA))

  // 6) smart constructor and deconstructor
  def mkIdentityIso[A]()(implicit eA: Elem[A]): Rep[IdentityIso[A]]
  def unmkIdentityIso[A](p: Rep[IsoUR[A, A]]): Option[(Rep[Unit])]

  abstract class AbsPairIso[A1, A2, B1, B2]
      (iso1: Iso[A1, B1], iso2: Iso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends PairIso[A1, A2, B1, B2](iso1, iso2) with Def[PairIso[A1, A2, B1, B2]] {
    lazy val selfType = element[PairIso[A1, A2, B1, B2]]
  }
  // elem for concrete class
  class PairIsoElem[A1, A2, B1, B2](val iso: Iso[PairIsoData[A1, A2, B1, B2], PairIso[A1, A2, B1, B2]])(implicit  val eA1: Elem[A1],  val eA2: Elem[A2],  val eB1: Elem[B1],  val eB2: Elem[B2])
    extends IsoURElem[(A1, A2), (B1, B2), PairIso[A1, A2, B1, B2]]
    with ConcreteElem[PairIsoData[A1, A2, B1, B2], PairIso[A1, A2, B1, B2]] {
    override lazy val parent: Option[Elem[_]] = Some(isoURElement(pairElement(element[A1],element[A2]), pairElement(element[B1],element[B2])))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A1" -> Left(eA1), "A2" -> Left(eA2), "B1" -> Left(eB1), "B2" -> Left(eB2))
    }

    override def convertIsoUR(x: Rep[IsoUR[(A1, A2), (B1, B2)]]) = // Converter is not generated by meta
!!!("Cannot convert from IsoUR to PairIso: missing fields List(iso1, iso2)")
    override def getDefaultRep = PairIso(element[IsoUR[A1, B1]].defaultRepValue, element[IsoUR[A2, B2]].defaultRepValue)
    override lazy val tag = {
      implicit val tagA1 = eA1.tag
      implicit val tagA2 = eA2.tag
      implicit val tagB1 = eB1.tag
      implicit val tagB2 = eB2.tag
      weakTypeTag[PairIso[A1, A2, B1, B2]]
    }
  }

  // state representation type
  type PairIsoData[A1, A2, B1, B2] = (IsoUR[A1, B1], IsoUR[A2, B2])

  // 3) Iso for concrete class
  class PairIsoIso[A1, A2, B1, B2](implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends EntityIso[PairIsoData[A1, A2, B1, B2], PairIso[A1, A2, B1, B2]] with Def[PairIsoIso[A1, A2, B1, B2]] {
    override def from(p: Rep[PairIso[A1, A2, B1, B2]]) =
      (p.iso1, p.iso2)
    override def to(p: Rep[(IsoUR[A1, B1], IsoUR[A2, B2])]) = {
      val Pair(iso1, iso2) = p
      PairIso(iso1, iso2)
    }
    lazy val eFrom = pairElement(element[IsoUR[A1, B1]], element[IsoUR[A2, B2]])
    lazy val eTo = new PairIsoElem[A1, A2, B1, B2](self)
    lazy val selfType = new PairIsoIsoElem[A1, A2, B1, B2](eA1, eA2, eB1, eB2)
    def productArity = 4
    def productElement(n: Int) = (eA1, eA2, eB1, eB2).productElement(n)
  }
  case class PairIsoIsoElem[A1, A2, B1, B2](eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]) extends Elem[PairIsoIso[A1, A2, B1, B2]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new PairIsoIso[A1, A2, B1, B2]()(eA1, eA2, eB1, eB2))
    lazy val tag = {
      implicit val tagA1 = eA1.tag
      implicit val tagA2 = eA2.tag
      implicit val tagB1 = eB1.tag
      implicit val tagB2 = eB2.tag
      weakTypeTag[PairIsoIso[A1, A2, B1, B2]]
    }
  }
  // 4) constructor and deconstructor
  class PairIsoCompanionAbs extends CompanionDef[PairIsoCompanionAbs] with PairIsoCompanion {
    def selfType = PairIsoCompanionElem
    override def toString = "PairIso"
    def apply[A1, A2, B1, B2](p: Rep[PairIsoData[A1, A2, B1, B2]])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[PairIso[A1, A2, B1, B2]] =
      isoPairIso(eA1, eA2, eB1, eB2).to(p)
    def apply[A1, A2, B1, B2](iso1: Iso[A1, B1], iso2: Iso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[PairIso[A1, A2, B1, B2]] =
      mkPairIso(iso1, iso2)
  }
  object PairIsoMatcher {
    def unapply[A1, A2, B1, B2](p: Rep[IsoUR[(A1, A2), (B1, B2)]]) = unmkPairIso(p)
  }
  lazy val PairIso: Rep[PairIsoCompanionAbs] = new PairIsoCompanionAbs
  implicit def proxyPairIsoCompanion(p: Rep[PairIsoCompanionAbs]): PairIsoCompanionAbs = {
    proxyOps[PairIsoCompanionAbs](p)
  }

  implicit case object PairIsoCompanionElem extends CompanionElem[PairIsoCompanionAbs] {
    lazy val tag = weakTypeTag[PairIsoCompanionAbs]
    protected def getDefaultRep = PairIso
  }

  implicit def proxyPairIso[A1, A2, B1, B2](p: Rep[PairIso[A1, A2, B1, B2]]): PairIso[A1, A2, B1, B2] =
    proxyOps[PairIso[A1, A2, B1, B2]](p)

  implicit class ExtendedPairIso[A1, A2, B1, B2](p: Rep[PairIso[A1, A2, B1, B2]])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]) {
    def toData: Rep[PairIsoData[A1, A2, B1, B2]] = isoPairIso(eA1, eA2, eB1, eB2).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoPairIso[A1, A2, B1, B2](implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Iso[PairIsoData[A1, A2, B1, B2], PairIso[A1, A2, B1, B2]] =
    reifyObject(new PairIsoIso[A1, A2, B1, B2]()(eA1, eA2, eB1, eB2))

  // 6) smart constructor and deconstructor
  def mkPairIso[A1, A2, B1, B2](iso1: Iso[A1, B1], iso2: Iso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[PairIso[A1, A2, B1, B2]]
  def unmkPairIso[A1, A2, B1, B2](p: Rep[IsoUR[(A1, A2), (B1, B2)]]): Option[(Rep[IsoUR[A1, B1]], Rep[IsoUR[A2, B2]])]

  abstract class AbsSumIso[A1, A2, B1, B2]
      (iso1: Iso[A1, B1], iso2: Iso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends SumIso[A1, A2, B1, B2](iso1, iso2) with Def[SumIso[A1, A2, B1, B2]] {
    lazy val selfType = element[SumIso[A1, A2, B1, B2]]
  }
  // elem for concrete class
  class SumIsoElem[A1, A2, B1, B2](val iso: Iso[SumIsoData[A1, A2, B1, B2], SumIso[A1, A2, B1, B2]])(implicit  val eA1: Elem[A1],  val eA2: Elem[A2],  val eB1: Elem[B1],  val eB2: Elem[B2])
    extends IsoURElem[$bar[A1, A2], $bar[B1, B2], SumIso[A1, A2, B1, B2]]
    with ConcreteElem[SumIsoData[A1, A2, B1, B2], SumIso[A1, A2, B1, B2]] {
    override lazy val parent: Option[Elem[_]] = Some(isoURElement(sumElement(element[A1],element[A2]), sumElement(element[B1],element[B2])))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A1" -> Left(eA1), "A2" -> Left(eA2), "B1" -> Left(eB1), "B2" -> Left(eB2))
    }

    override def convertIsoUR(x: Rep[IsoUR[$bar[A1, A2], $bar[B1, B2]]]) = // Converter is not generated by meta
!!!("Cannot convert from IsoUR to SumIso: missing fields List(iso1, iso2)")
    override def getDefaultRep = SumIso(element[IsoUR[A1, B1]].defaultRepValue, element[IsoUR[A2, B2]].defaultRepValue)
    override lazy val tag = {
      implicit val tagA1 = eA1.tag
      implicit val tagA2 = eA2.tag
      implicit val tagB1 = eB1.tag
      implicit val tagB2 = eB2.tag
      weakTypeTag[SumIso[A1, A2, B1, B2]]
    }
  }

  // state representation type
  type SumIsoData[A1, A2, B1, B2] = (IsoUR[A1, B1], IsoUR[A2, B2])

  // 3) Iso for concrete class
  class SumIsoIso[A1, A2, B1, B2](implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends EntityIso[SumIsoData[A1, A2, B1, B2], SumIso[A1, A2, B1, B2]] with Def[SumIsoIso[A1, A2, B1, B2]] {
    override def from(p: Rep[SumIso[A1, A2, B1, B2]]) =
      (p.iso1, p.iso2)
    override def to(p: Rep[(IsoUR[A1, B1], IsoUR[A2, B2])]) = {
      val Pair(iso1, iso2) = p
      SumIso(iso1, iso2)
    }
    lazy val eFrom = pairElement(element[IsoUR[A1, B1]], element[IsoUR[A2, B2]])
    lazy val eTo = new SumIsoElem[A1, A2, B1, B2](self)
    lazy val selfType = new SumIsoIsoElem[A1, A2, B1, B2](eA1, eA2, eB1, eB2)
    def productArity = 4
    def productElement(n: Int) = (eA1, eA2, eB1, eB2).productElement(n)
  }
  case class SumIsoIsoElem[A1, A2, B1, B2](eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]) extends Elem[SumIsoIso[A1, A2, B1, B2]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new SumIsoIso[A1, A2, B1, B2]()(eA1, eA2, eB1, eB2))
    lazy val tag = {
      implicit val tagA1 = eA1.tag
      implicit val tagA2 = eA2.tag
      implicit val tagB1 = eB1.tag
      implicit val tagB2 = eB2.tag
      weakTypeTag[SumIsoIso[A1, A2, B1, B2]]
    }
  }
  // 4) constructor and deconstructor
  class SumIsoCompanionAbs extends CompanionDef[SumIsoCompanionAbs] {
    def selfType = SumIsoCompanionElem
    override def toString = "SumIso"
    def apply[A1, A2, B1, B2](p: Rep[SumIsoData[A1, A2, B1, B2]])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[SumIso[A1, A2, B1, B2]] =
      isoSumIso(eA1, eA2, eB1, eB2).to(p)
    def apply[A1, A2, B1, B2](iso1: Iso[A1, B1], iso2: Iso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[SumIso[A1, A2, B1, B2]] =
      mkSumIso(iso1, iso2)
  }
  object SumIsoMatcher {
    def unapply[A1, A2, B1, B2](p: Rep[IsoUR[$bar[A1, A2], $bar[B1, B2]]]) = unmkSumIso(p)
  }
  lazy val SumIso: Rep[SumIsoCompanionAbs] = new SumIsoCompanionAbs
  implicit def proxySumIsoCompanion(p: Rep[SumIsoCompanionAbs]): SumIsoCompanionAbs = {
    proxyOps[SumIsoCompanionAbs](p)
  }

  implicit case object SumIsoCompanionElem extends CompanionElem[SumIsoCompanionAbs] {
    lazy val tag = weakTypeTag[SumIsoCompanionAbs]
    protected def getDefaultRep = SumIso
  }

  implicit def proxySumIso[A1, A2, B1, B2](p: Rep[SumIso[A1, A2, B1, B2]]): SumIso[A1, A2, B1, B2] =
    proxyOps[SumIso[A1, A2, B1, B2]](p)

  implicit class ExtendedSumIso[A1, A2, B1, B2](p: Rep[SumIso[A1, A2, B1, B2]])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]) {
    def toData: Rep[SumIsoData[A1, A2, B1, B2]] = isoSumIso(eA1, eA2, eB1, eB2).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoSumIso[A1, A2, B1, B2](implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Iso[SumIsoData[A1, A2, B1, B2], SumIso[A1, A2, B1, B2]] =
    reifyObject(new SumIsoIso[A1, A2, B1, B2]()(eA1, eA2, eB1, eB2))

  // 6) smart constructor and deconstructor
  def mkSumIso[A1, A2, B1, B2](iso1: Iso[A1, B1], iso2: Iso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[SumIso[A1, A2, B1, B2]]
  def unmkSumIso[A1, A2, B1, B2](p: Rep[IsoUR[$bar[A1, A2], $bar[B1, B2]]]): Option[(Rep[IsoUR[A1, B1]], Rep[IsoUR[A2, B2]])]

  abstract class AbsComposeIso[A, B, C]
      (iso2: Iso[B, C], iso1: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C])
    extends ComposeIso[A, B, C](iso2, iso1) with Def[ComposeIso[A, B, C]] {
    lazy val selfType = element[ComposeIso[A, B, C]]
  }
  // elem for concrete class
  class ComposeIsoElem[A, B, C](val iso: Iso[ComposeIsoData[A, B, C], ComposeIso[A, B, C]])(implicit  val eA: Elem[A],  val eB: Elem[B],  val eC: Elem[C])
    extends IsoURElem[A, C, ComposeIso[A, B, C]]
    with ConcreteElem[ComposeIsoData[A, B, C], ComposeIso[A, B, C]] {
    override lazy val parent: Option[Elem[_]] = Some(isoURElement(element[A], element[C]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eA), "B" -> Left(eB), "C" -> Left(eC))
    }

    override def convertIsoUR(x: Rep[IsoUR[A, C]]) = // Converter is not generated by meta
!!!("Cannot convert from IsoUR to ComposeIso: missing fields List(iso2, iso1)")
    override def getDefaultRep = ComposeIso(element[IsoUR[B, C]].defaultRepValue, element[IsoUR[A, B]].defaultRepValue)
    override lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      implicit val tagC = eC.tag
      weakTypeTag[ComposeIso[A, B, C]]
    }
  }

  // state representation type
  type ComposeIsoData[A, B, C] = (IsoUR[B, C], IsoUR[A, B])

  // 3) Iso for concrete class
  class ComposeIsoIso[A, B, C](implicit eA: Elem[A], eB: Elem[B], eC: Elem[C])
    extends EntityIso[ComposeIsoData[A, B, C], ComposeIso[A, B, C]] with Def[ComposeIsoIso[A, B, C]] {
    override def from(p: Rep[ComposeIso[A, B, C]]) =
      (p.iso2, p.iso1)
    override def to(p: Rep[(IsoUR[B, C], IsoUR[A, B])]) = {
      val Pair(iso2, iso1) = p
      ComposeIso(iso2, iso1)
    }
    lazy val eFrom = pairElement(element[IsoUR[B, C]], element[IsoUR[A, B]])
    lazy val eTo = new ComposeIsoElem[A, B, C](self)
    lazy val selfType = new ComposeIsoIsoElem[A, B, C](eA, eB, eC)
    def productArity = 3
    def productElement(n: Int) = (eA, eB, eC).productElement(n)
  }
  case class ComposeIsoIsoElem[A, B, C](eA: Elem[A], eB: Elem[B], eC: Elem[C]) extends Elem[ComposeIsoIso[A, B, C]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new ComposeIsoIso[A, B, C]()(eA, eB, eC))
    lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      implicit val tagC = eC.tag
      weakTypeTag[ComposeIsoIso[A, B, C]]
    }
  }
  // 4) constructor and deconstructor
  class ComposeIsoCompanionAbs extends CompanionDef[ComposeIsoCompanionAbs] {
    def selfType = ComposeIsoCompanionElem
    override def toString = "ComposeIso"
    def apply[A, B, C](p: Rep[ComposeIsoData[A, B, C]])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]): Rep[ComposeIso[A, B, C]] =
      isoComposeIso(eA, eB, eC).to(p)
    def apply[A, B, C](iso2: Iso[B, C], iso1: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]): Rep[ComposeIso[A, B, C]] =
      mkComposeIso(iso2, iso1)
  }
  object ComposeIsoMatcher {
    def unapply[A, B, C](p: Rep[IsoUR[A, C]]) = unmkComposeIso(p)
  }
  lazy val ComposeIso: Rep[ComposeIsoCompanionAbs] = new ComposeIsoCompanionAbs
  implicit def proxyComposeIsoCompanion(p: Rep[ComposeIsoCompanionAbs]): ComposeIsoCompanionAbs = {
    proxyOps[ComposeIsoCompanionAbs](p)
  }

  implicit case object ComposeIsoCompanionElem extends CompanionElem[ComposeIsoCompanionAbs] {
    lazy val tag = weakTypeTag[ComposeIsoCompanionAbs]
    protected def getDefaultRep = ComposeIso
  }

  implicit def proxyComposeIso[A, B, C](p: Rep[ComposeIso[A, B, C]]): ComposeIso[A, B, C] =
    proxyOps[ComposeIso[A, B, C]](p)

  implicit class ExtendedComposeIso[A, B, C](p: Rep[ComposeIso[A, B, C]])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]) {
    def toData: Rep[ComposeIsoData[A, B, C]] = isoComposeIso(eA, eB, eC).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoComposeIso[A, B, C](implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]): Iso[ComposeIsoData[A, B, C], ComposeIso[A, B, C]] =
    reifyObject(new ComposeIsoIso[A, B, C]()(eA, eB, eC))

  // 6) smart constructor and deconstructor
  def mkComposeIso[A, B, C](iso2: Iso[B, C], iso1: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]): Rep[ComposeIso[A, B, C]]
  def unmkComposeIso[A, B, C](p: Rep[IsoUR[A, C]]): Option[(Rep[IsoUR[B, C]], Rep[IsoUR[A, B]])]

  abstract class AbsFuncIso[A, B, C, D]
      (iso1: Iso[A, B], iso2: Iso[C, D])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D])
    extends FuncIso[A, B, C, D](iso1, iso2) with Def[FuncIso[A, B, C, D]] {
    lazy val selfType = element[FuncIso[A, B, C, D]]
  }
  // elem for concrete class
  class FuncIsoElem[A, B, C, D](val iso: Iso[FuncIsoData[A, B, C, D], FuncIso[A, B, C, D]])(implicit  val eA: Elem[A],  val eB: Elem[B],  val eC: Elem[C],  val eD: Elem[D])
    extends IsoURElem[A => C, B => D, FuncIso[A, B, C, D]]
    with ConcreteElem[FuncIsoData[A, B, C, D], FuncIso[A, B, C, D]] {
    override lazy val parent: Option[Elem[_]] = Some(isoURElement(funcElement(element[A],element[C]), funcElement(element[B],element[D])))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eA), "B" -> Left(eB), "C" -> Left(eC), "D" -> Left(eD))
    }

    override def convertIsoUR(x: Rep[IsoUR[A => C, B => D]]) = // Converter is not generated by meta
!!!("Cannot convert from IsoUR to FuncIso: missing fields List(iso1, iso2)")
    override def getDefaultRep = FuncIso(element[IsoUR[A, B]].defaultRepValue, element[IsoUR[C, D]].defaultRepValue)
    override lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      implicit val tagC = eC.tag
      implicit val tagD = eD.tag
      weakTypeTag[FuncIso[A, B, C, D]]
    }
  }

  // state representation type
  type FuncIsoData[A, B, C, D] = (IsoUR[A, B], IsoUR[C, D])

  // 3) Iso for concrete class
  class FuncIsoIso[A, B, C, D](implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D])
    extends EntityIso[FuncIsoData[A, B, C, D], FuncIso[A, B, C, D]] with Def[FuncIsoIso[A, B, C, D]] {
    override def from(p: Rep[FuncIso[A, B, C, D]]) =
      (p.iso1, p.iso2)
    override def to(p: Rep[(IsoUR[A, B], IsoUR[C, D])]) = {
      val Pair(iso1, iso2) = p
      FuncIso(iso1, iso2)
    }
    lazy val eFrom = pairElement(element[IsoUR[A, B]], element[IsoUR[C, D]])
    lazy val eTo = new FuncIsoElem[A, B, C, D](self)
    lazy val selfType = new FuncIsoIsoElem[A, B, C, D](eA, eB, eC, eD)
    def productArity = 4
    def productElement(n: Int) = (eA, eB, eC, eD).productElement(n)
  }
  case class FuncIsoIsoElem[A, B, C, D](eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D]) extends Elem[FuncIsoIso[A, B, C, D]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new FuncIsoIso[A, B, C, D]()(eA, eB, eC, eD))
    lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      implicit val tagC = eC.tag
      implicit val tagD = eD.tag
      weakTypeTag[FuncIsoIso[A, B, C, D]]
    }
  }
  // 4) constructor and deconstructor
  class FuncIsoCompanionAbs extends CompanionDef[FuncIsoCompanionAbs] {
    def selfType = FuncIsoCompanionElem
    override def toString = "FuncIso"
    def apply[A, B, C, D](p: Rep[FuncIsoData[A, B, C, D]])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D]): Rep[FuncIso[A, B, C, D]] =
      isoFuncIso(eA, eB, eC, eD).to(p)
    def apply[A, B, C, D](iso1: Iso[A, B], iso2: Iso[C, D])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D]): Rep[FuncIso[A, B, C, D]] =
      mkFuncIso(iso1, iso2)
  }
  object FuncIsoMatcher {
    def unapply[A, B, C, D](p: Rep[IsoUR[A => C, B => D]]) = unmkFuncIso(p)
  }
  lazy val FuncIso: Rep[FuncIsoCompanionAbs] = new FuncIsoCompanionAbs
  implicit def proxyFuncIsoCompanion(p: Rep[FuncIsoCompanionAbs]): FuncIsoCompanionAbs = {
    proxyOps[FuncIsoCompanionAbs](p)
  }

  implicit case object FuncIsoCompanionElem extends CompanionElem[FuncIsoCompanionAbs] {
    lazy val tag = weakTypeTag[FuncIsoCompanionAbs]
    protected def getDefaultRep = FuncIso
  }

  implicit def proxyFuncIso[A, B, C, D](p: Rep[FuncIso[A, B, C, D]]): FuncIso[A, B, C, D] =
    proxyOps[FuncIso[A, B, C, D]](p)

  implicit class ExtendedFuncIso[A, B, C, D](p: Rep[FuncIso[A, B, C, D]])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D]) {
    def toData: Rep[FuncIsoData[A, B, C, D]] = isoFuncIso(eA, eB, eC, eD).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoFuncIso[A, B, C, D](implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D]): Iso[FuncIsoData[A, B, C, D], FuncIso[A, B, C, D]] =
    reifyObject(new FuncIsoIso[A, B, C, D]()(eA, eB, eC, eD))

  // 6) smart constructor and deconstructor
  def mkFuncIso[A, B, C, D](iso1: Iso[A, B], iso2: Iso[C, D])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D]): Rep[FuncIso[A, B, C, D]]
  def unmkFuncIso[A, B, C, D](p: Rep[IsoUR[A => C, B => D]]): Option[(Rep[IsoUR[A, B]], Rep[IsoUR[C, D]])]

  abstract class AbsConverterIso[A, B]
      (convTo: Conv[A, B], convFrom: Conv[B, A])(implicit eA: Elem[A], eB: Elem[B])
    extends ConverterIso[A, B](convTo, convFrom) with Def[ConverterIso[A, B]] {
    lazy val selfType = element[ConverterIso[A, B]]
  }
  // elem for concrete class
  class ConverterIsoElem[A, B](val iso: Iso[ConverterIsoData[A, B], ConverterIso[A, B]])(implicit  val eA: Elem[A],  val eB: Elem[B])
    extends IsoURElem[A, B, ConverterIso[A, B]]
    with ConcreteElem[ConverterIsoData[A, B], ConverterIso[A, B]] {
    override lazy val parent: Option[Elem[_]] = Some(isoURElement(element[A], element[B]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eA), "B" -> Left(eB))
    }

    override def convertIsoUR(x: Rep[IsoUR[A, B]]) = // Converter is not generated by meta
!!!("Cannot convert from IsoUR to ConverterIso: missing fields List(convTo, convFrom)")
    override def getDefaultRep = ConverterIso(element[Converter[A, B]].defaultRepValue, element[Converter[B, A]].defaultRepValue)
    override lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[ConverterIso[A, B]]
    }
  }

  // state representation type
  type ConverterIsoData[A, B] = (Converter[A, B], Converter[B, A])

  // 3) Iso for concrete class
  class ConverterIsoIso[A, B](implicit eA: Elem[A], eB: Elem[B])
    extends EntityIso[ConverterIsoData[A, B], ConverterIso[A, B]] with Def[ConverterIsoIso[A, B]] {
    override def from(p: Rep[ConverterIso[A, B]]) =
      (p.convTo, p.convFrom)
    override def to(p: Rep[(Converter[A, B], Converter[B, A])]) = {
      val Pair(convTo, convFrom) = p
      ConverterIso(convTo, convFrom)
    }
    lazy val eFrom = pairElement(element[Converter[A, B]], element[Converter[B, A]])
    lazy val eTo = new ConverterIsoElem[A, B](self)
    lazy val selfType = new ConverterIsoIsoElem[A, B](eA, eB)
    def productArity = 2
    def productElement(n: Int) = (eA, eB).productElement(n)
  }
  case class ConverterIsoIsoElem[A, B](eA: Elem[A], eB: Elem[B]) extends Elem[ConverterIsoIso[A, B]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new ConverterIsoIso[A, B]()(eA, eB))
    lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[ConverterIsoIso[A, B]]
    }
  }
  // 4) constructor and deconstructor
  class ConverterIsoCompanionAbs extends CompanionDef[ConverterIsoCompanionAbs] {
    def selfType = ConverterIsoCompanionElem
    override def toString = "ConverterIso"
    def apply[A, B](p: Rep[ConverterIsoData[A, B]])(implicit eA: Elem[A], eB: Elem[B]): Rep[ConverterIso[A, B]] =
      isoConverterIso(eA, eB).to(p)
    def apply[A, B](convTo: Conv[A, B], convFrom: Conv[B, A])(implicit eA: Elem[A], eB: Elem[B]): Rep[ConverterIso[A, B]] =
      mkConverterIso(convTo, convFrom)
  }
  object ConverterIsoMatcher {
    def unapply[A, B](p: Rep[IsoUR[A, B]]) = unmkConverterIso(p)
  }
  lazy val ConverterIso: Rep[ConverterIsoCompanionAbs] = new ConverterIsoCompanionAbs
  implicit def proxyConverterIsoCompanion(p: Rep[ConverterIsoCompanionAbs]): ConverterIsoCompanionAbs = {
    proxyOps[ConverterIsoCompanionAbs](p)
  }

  implicit case object ConverterIsoCompanionElem extends CompanionElem[ConverterIsoCompanionAbs] {
    lazy val tag = weakTypeTag[ConverterIsoCompanionAbs]
    protected def getDefaultRep = ConverterIso
  }

  implicit def proxyConverterIso[A, B](p: Rep[ConverterIso[A, B]]): ConverterIso[A, B] =
    proxyOps[ConverterIso[A, B]](p)

  implicit class ExtendedConverterIso[A, B](p: Rep[ConverterIso[A, B]])(implicit eA: Elem[A], eB: Elem[B]) {
    def toData: Rep[ConverterIsoData[A, B]] = isoConverterIso(eA, eB).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoConverterIso[A, B](implicit eA: Elem[A], eB: Elem[B]): Iso[ConverterIsoData[A, B], ConverterIso[A, B]] =
    reifyObject(new ConverterIsoIso[A, B]()(eA, eB))

  // 6) smart constructor and deconstructor
  def mkConverterIso[A, B](convTo: Conv[A, B], convFrom: Conv[B, A])(implicit eA: Elem[A], eB: Elem[B]): Rep[ConverterIso[A, B]]
  def unmkConverterIso[A, B](p: Rep[IsoUR[A, B]]): Option[(Rep[Converter[A, B]], Rep[Converter[B, A]])]

  abstract class AbsArrayIso[A, B]
      (innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B])
    extends ArrayIso[A, B](innerIso) with Def[ArrayIso[A, B]] {
    lazy val selfType = element[ArrayIso[A, B]]
  }
  // elem for concrete class
  class ArrayIsoElem[A, B](val iso: Iso[ArrayIsoData[A, B], ArrayIso[A, B]])(implicit override  val eA: Elem[A], override  val eB: Elem[B])
    extends Iso1URElem[A, B, Array, ArrayIso[A, B]]
    with ConcreteElem[ArrayIsoData[A, B], ArrayIso[A, B]] {
    override lazy val parent: Option[Elem[_]] = Some(iso1URElement(element[A], element[B], container[Array]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eA), "B" -> Left(eB))
    }

    override def convertIso1UR(x: Rep[Iso1UR[A, B, Array]]) = ArrayIso(x.innerIso)
    override def getDefaultRep = ArrayIso(element[IsoUR[A, B]].defaultRepValue)
    override lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[ArrayIso[A, B]]
    }
  }

  // state representation type
  type ArrayIsoData[A, B] = IsoUR[A, B]

  // 3) Iso for concrete class
  class ArrayIsoIso[A, B](implicit eA: Elem[A], eB: Elem[B])
    extends EntityIso[ArrayIsoData[A, B], ArrayIso[A, B]] with Def[ArrayIsoIso[A, B]] {
    override def from(p: Rep[ArrayIso[A, B]]) =
      p.innerIso
    override def to(p: Rep[IsoUR[A, B]]) = {
      val innerIso = p
      ArrayIso(innerIso)
    }
    lazy val eFrom = element[IsoUR[A, B]]
    lazy val eTo = new ArrayIsoElem[A, B](self)
    lazy val selfType = new ArrayIsoIsoElem[A, B](eA, eB)
    def productArity = 2
    def productElement(n: Int) = (eA, eB).productElement(n)
  }
  case class ArrayIsoIsoElem[A, B](eA: Elem[A], eB: Elem[B]) extends Elem[ArrayIsoIso[A, B]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new ArrayIsoIso[A, B]()(eA, eB))
    lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[ArrayIsoIso[A, B]]
    }
  }
  // 4) constructor and deconstructor
  class ArrayIsoCompanionAbs extends CompanionDef[ArrayIsoCompanionAbs] {
    def selfType = ArrayIsoCompanionElem
    override def toString = "ArrayIso"

    def apply[A, B](innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B]): Rep[ArrayIso[A, B]] =
      mkArrayIso(innerIso)
  }
  object ArrayIsoMatcher {
    def unapply[A, B](p: Rep[Iso1UR[A, B, Array]]) = unmkArrayIso(p)
  }
  lazy val ArrayIso: Rep[ArrayIsoCompanionAbs] = new ArrayIsoCompanionAbs
  implicit def proxyArrayIsoCompanion(p: Rep[ArrayIsoCompanionAbs]): ArrayIsoCompanionAbs = {
    proxyOps[ArrayIsoCompanionAbs](p)
  }

  implicit case object ArrayIsoCompanionElem extends CompanionElem[ArrayIsoCompanionAbs] {
    lazy val tag = weakTypeTag[ArrayIsoCompanionAbs]
    protected def getDefaultRep = ArrayIso
  }

  implicit def proxyArrayIso[A, B](p: Rep[ArrayIso[A, B]]): ArrayIso[A, B] =
    proxyOps[ArrayIso[A, B]](p)

  implicit class ExtendedArrayIso[A, B](p: Rep[ArrayIso[A, B]])(implicit eA: Elem[A], eB: Elem[B]) {
    def toData: Rep[ArrayIsoData[A, B]] = isoArrayIso(eA, eB).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoArrayIso[A, B](implicit eA: Elem[A], eB: Elem[B]): Iso[ArrayIsoData[A, B], ArrayIso[A, B]] =
    reifyObject(new ArrayIsoIso[A, B]()(eA, eB))

  // 6) smart constructor and deconstructor
  def mkArrayIso[A, B](innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B]): Rep[ArrayIso[A, B]]
  def unmkArrayIso[A, B](p: Rep[Iso1UR[A, B, Array]]): Option[(Rep[IsoUR[A, B]])]

  abstract class AbsListIso[A, B]
      (innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B])
    extends ListIso[A, B](innerIso) with Def[ListIso[A, B]] {
    lazy val selfType = element[ListIso[A, B]]
  }
  // elem for concrete class
  class ListIsoElem[A, B](val iso: Iso[ListIsoData[A, B], ListIso[A, B]])(implicit override  val eA: Elem[A], override  val eB: Elem[B])
    extends Iso1URElem[A, B, List, ListIso[A, B]]
    with ConcreteElem[ListIsoData[A, B], ListIso[A, B]] {
    override lazy val parent: Option[Elem[_]] = Some(iso1URElement(element[A], element[B], container[List]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eA), "B" -> Left(eB))
    }

    override def convertIso1UR(x: Rep[Iso1UR[A, B, List]]) = ListIso(x.innerIso)
    override def getDefaultRep = ListIso(element[IsoUR[A, B]].defaultRepValue)
    override lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[ListIso[A, B]]
    }
  }

  // state representation type
  type ListIsoData[A, B] = IsoUR[A, B]

  // 3) Iso for concrete class
  class ListIsoIso[A, B](implicit eA: Elem[A], eB: Elem[B])
    extends EntityIso[ListIsoData[A, B], ListIso[A, B]] with Def[ListIsoIso[A, B]] {
    override def from(p: Rep[ListIso[A, B]]) =
      p.innerIso
    override def to(p: Rep[IsoUR[A, B]]) = {
      val innerIso = p
      ListIso(innerIso)
    }
    lazy val eFrom = element[IsoUR[A, B]]
    lazy val eTo = new ListIsoElem[A, B](self)
    lazy val selfType = new ListIsoIsoElem[A, B](eA, eB)
    def productArity = 2
    def productElement(n: Int) = (eA, eB).productElement(n)
  }
  case class ListIsoIsoElem[A, B](eA: Elem[A], eB: Elem[B]) extends Elem[ListIsoIso[A, B]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new ListIsoIso[A, B]()(eA, eB))
    lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[ListIsoIso[A, B]]
    }
  }
  // 4) constructor and deconstructor
  class ListIsoCompanionAbs extends CompanionDef[ListIsoCompanionAbs] {
    def selfType = ListIsoCompanionElem
    override def toString = "ListIso"

    def apply[A, B](innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B]): Rep[ListIso[A, B]] =
      mkListIso(innerIso)
  }
  object ListIsoMatcher {
    def unapply[A, B](p: Rep[Iso1UR[A, B, List]]) = unmkListIso(p)
  }
  lazy val ListIso: Rep[ListIsoCompanionAbs] = new ListIsoCompanionAbs
  implicit def proxyListIsoCompanion(p: Rep[ListIsoCompanionAbs]): ListIsoCompanionAbs = {
    proxyOps[ListIsoCompanionAbs](p)
  }

  implicit case object ListIsoCompanionElem extends CompanionElem[ListIsoCompanionAbs] {
    lazy val tag = weakTypeTag[ListIsoCompanionAbs]
    protected def getDefaultRep = ListIso
  }

  implicit def proxyListIso[A, B](p: Rep[ListIso[A, B]]): ListIso[A, B] =
    proxyOps[ListIso[A, B]](p)

  implicit class ExtendedListIso[A, B](p: Rep[ListIso[A, B]])(implicit eA: Elem[A], eB: Elem[B]) {
    def toData: Rep[ListIsoData[A, B]] = isoListIso(eA, eB).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoListIso[A, B](implicit eA: Elem[A], eB: Elem[B]): Iso[ListIsoData[A, B], ListIso[A, B]] =
    reifyObject(new ListIsoIso[A, B]()(eA, eB))

  // 6) smart constructor and deconstructor
  def mkListIso[A, B](innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B]): Rep[ListIso[A, B]]
  def unmkListIso[A, B](p: Rep[Iso1UR[A, B, List]]): Option[(Rep[IsoUR[A, B]])]

  abstract class AbsArrayBufferIso[A, B]
      (innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B])
    extends ArrayBufferIso[A, B](innerIso) with Def[ArrayBufferIso[A, B]] {
    lazy val selfType = element[ArrayBufferIso[A, B]]
  }
  // elem for concrete class
  class ArrayBufferIsoElem[A, B](val iso: Iso[ArrayBufferIsoData[A, B], ArrayBufferIso[A, B]])(implicit override  val eA: Elem[A], override  val eB: Elem[B])
    extends Iso1URElem[A, B, ArrayBuffer, ArrayBufferIso[A, B]]
    with ConcreteElem[ArrayBufferIsoData[A, B], ArrayBufferIso[A, B]] {
    override lazy val parent: Option[Elem[_]] = Some(iso1URElement(element[A], element[B], container[ArrayBuffer]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eA), "B" -> Left(eB))
    }

    override def convertIso1UR(x: Rep[Iso1UR[A, B, ArrayBuffer]]) = ArrayBufferIso(x.innerIso)
    override def getDefaultRep = ArrayBufferIso(element[IsoUR[A, B]].defaultRepValue)
    override lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[ArrayBufferIso[A, B]]
    }
  }

  // state representation type
  type ArrayBufferIsoData[A, B] = IsoUR[A, B]

  // 3) Iso for concrete class
  class ArrayBufferIsoIso[A, B](implicit eA: Elem[A], eB: Elem[B])
    extends EntityIso[ArrayBufferIsoData[A, B], ArrayBufferIso[A, B]] with Def[ArrayBufferIsoIso[A, B]] {
    override def from(p: Rep[ArrayBufferIso[A, B]]) =
      p.innerIso
    override def to(p: Rep[IsoUR[A, B]]) = {
      val innerIso = p
      ArrayBufferIso(innerIso)
    }
    lazy val eFrom = element[IsoUR[A, B]]
    lazy val eTo = new ArrayBufferIsoElem[A, B](self)
    lazy val selfType = new ArrayBufferIsoIsoElem[A, B](eA, eB)
    def productArity = 2
    def productElement(n: Int) = (eA, eB).productElement(n)
  }
  case class ArrayBufferIsoIsoElem[A, B](eA: Elem[A], eB: Elem[B]) extends Elem[ArrayBufferIsoIso[A, B]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new ArrayBufferIsoIso[A, B]()(eA, eB))
    lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[ArrayBufferIsoIso[A, B]]
    }
  }
  // 4) constructor and deconstructor
  class ArrayBufferIsoCompanionAbs extends CompanionDef[ArrayBufferIsoCompanionAbs] {
    def selfType = ArrayBufferIsoCompanionElem
    override def toString = "ArrayBufferIso"

    def apply[A, B](innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B]): Rep[ArrayBufferIso[A, B]] =
      mkArrayBufferIso(innerIso)
  }
  object ArrayBufferIsoMatcher {
    def unapply[A, B](p: Rep[Iso1UR[A, B, ArrayBuffer]]) = unmkArrayBufferIso(p)
  }
  lazy val ArrayBufferIso: Rep[ArrayBufferIsoCompanionAbs] = new ArrayBufferIsoCompanionAbs
  implicit def proxyArrayBufferIsoCompanion(p: Rep[ArrayBufferIsoCompanionAbs]): ArrayBufferIsoCompanionAbs = {
    proxyOps[ArrayBufferIsoCompanionAbs](p)
  }

  implicit case object ArrayBufferIsoCompanionElem extends CompanionElem[ArrayBufferIsoCompanionAbs] {
    lazy val tag = weakTypeTag[ArrayBufferIsoCompanionAbs]
    protected def getDefaultRep = ArrayBufferIso
  }

  implicit def proxyArrayBufferIso[A, B](p: Rep[ArrayBufferIso[A, B]]): ArrayBufferIso[A, B] =
    proxyOps[ArrayBufferIso[A, B]](p)

  implicit class ExtendedArrayBufferIso[A, B](p: Rep[ArrayBufferIso[A, B]])(implicit eA: Elem[A], eB: Elem[B]) {
    def toData: Rep[ArrayBufferIsoData[A, B]] = isoArrayBufferIso(eA, eB).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoArrayBufferIso[A, B](implicit eA: Elem[A], eB: Elem[B]): Iso[ArrayBufferIsoData[A, B], ArrayBufferIso[A, B]] =
    reifyObject(new ArrayBufferIsoIso[A, B]()(eA, eB))

  // 6) smart constructor and deconstructor
  def mkArrayBufferIso[A, B](innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B]): Rep[ArrayBufferIso[A, B]]
  def unmkArrayBufferIso[A, B](p: Rep[Iso1UR[A, B, ArrayBuffer]]): Option[(Rep[IsoUR[A, B]])]

  abstract class AbsThunkIso[A, B]
      (innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B])
    extends ThunkIso[A, B](innerIso) with Def[ThunkIso[A, B]] {
    lazy val selfType = element[ThunkIso[A, B]]
  }
  // elem for concrete class
  class ThunkIsoElem[A, B](val iso: Iso[ThunkIsoData[A, B], ThunkIso[A, B]])(implicit override  val eA: Elem[A], override  val eB: Elem[B])
    extends Iso1URElem[A, B, Thunk, ThunkIso[A, B]]
    with ConcreteElem[ThunkIsoData[A, B], ThunkIso[A, B]] {
    override lazy val parent: Option[Elem[_]] = Some(iso1URElement(element[A], element[B], container[Thunk]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eA), "B" -> Left(eB))
    }

    override def convertIso1UR(x: Rep[Iso1UR[A, B, Thunk]]) = ThunkIso(x.innerIso)
    override def getDefaultRep = ThunkIso(element[IsoUR[A, B]].defaultRepValue)
    override lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[ThunkIso[A, B]]
    }
  }

  // state representation type
  type ThunkIsoData[A, B] = IsoUR[A, B]

  // 3) Iso for concrete class
  class ThunkIsoIso[A, B](implicit eA: Elem[A], eB: Elem[B])
    extends EntityIso[ThunkIsoData[A, B], ThunkIso[A, B]] with Def[ThunkIsoIso[A, B]] {
    override def from(p: Rep[ThunkIso[A, B]]) =
      p.innerIso
    override def to(p: Rep[IsoUR[A, B]]) = {
      val innerIso = p
      ThunkIso(innerIso)
    }
    lazy val eFrom = element[IsoUR[A, B]]
    lazy val eTo = new ThunkIsoElem[A, B](self)
    lazy val selfType = new ThunkIsoIsoElem[A, B](eA, eB)
    def productArity = 2
    def productElement(n: Int) = (eA, eB).productElement(n)
  }
  case class ThunkIsoIsoElem[A, B](eA: Elem[A], eB: Elem[B]) extends Elem[ThunkIsoIso[A, B]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new ThunkIsoIso[A, B]()(eA, eB))
    lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[ThunkIsoIso[A, B]]
    }
  }
  // 4) constructor and deconstructor
  class ThunkIsoCompanionAbs extends CompanionDef[ThunkIsoCompanionAbs] {
    def selfType = ThunkIsoCompanionElem
    override def toString = "ThunkIso"

    def apply[A, B](innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B]): Rep[ThunkIso[A, B]] =
      mkThunkIso(innerIso)
  }
  object ThunkIsoMatcher {
    def unapply[A, B](p: Rep[Iso1UR[A, B, Thunk]]) = unmkThunkIso(p)
  }
  lazy val ThunkIso: Rep[ThunkIsoCompanionAbs] = new ThunkIsoCompanionAbs
  implicit def proxyThunkIsoCompanion(p: Rep[ThunkIsoCompanionAbs]): ThunkIsoCompanionAbs = {
    proxyOps[ThunkIsoCompanionAbs](p)
  }

  implicit case object ThunkIsoCompanionElem extends CompanionElem[ThunkIsoCompanionAbs] {
    lazy val tag = weakTypeTag[ThunkIsoCompanionAbs]
    protected def getDefaultRep = ThunkIso
  }

  implicit def proxyThunkIso[A, B](p: Rep[ThunkIso[A, B]]): ThunkIso[A, B] =
    proxyOps[ThunkIso[A, B]](p)

  implicit class ExtendedThunkIso[A, B](p: Rep[ThunkIso[A, B]])(implicit eA: Elem[A], eB: Elem[B]) {
    def toData: Rep[ThunkIsoData[A, B]] = isoThunkIso(eA, eB).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoThunkIso[A, B](implicit eA: Elem[A], eB: Elem[B]): Iso[ThunkIsoData[A, B], ThunkIso[A, B]] =
    reifyObject(new ThunkIsoIso[A, B]()(eA, eB))

  // 6) smart constructor and deconstructor
  def mkThunkIso[A, B](innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B]): Rep[ThunkIso[A, B]]
  def unmkThunkIso[A, B](p: Rep[Iso1UR[A, B, Thunk]]): Option[(Rep[IsoUR[A, B]])]

  registerModule(Views_Module)
}

// Seq -----------------------------------
trait ViewsSeq extends ViewsDsl {
  self: ViewsDsl with ScalanSeq =>
  lazy val IsoUR: Rep[IsoURCompanionAbs] = new IsoURCompanionAbs {
  }

  case class SeqIdentityIso[A]
      ()(implicit eA: Elem[A])
    extends AbsIdentityIso[A]() {
  }

  def mkIdentityIso[A]
    ()(implicit eA: Elem[A]): Rep[IdentityIso[A]] =
    new SeqIdentityIso[A]()
  def unmkIdentityIso[A](p: Rep[IsoUR[A, A]]) = p match {
    case p: IdentityIso[A] @unchecked =>
      Some(())
    case _ => None
  }

  case class SeqPairIso[A1, A2, B1, B2]
      (override val iso1: Iso[A1, B1], override val iso2: Iso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends AbsPairIso[A1, A2, B1, B2](iso1, iso2) {
  }

  def mkPairIso[A1, A2, B1, B2]
    (iso1: Iso[A1, B1], iso2: Iso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[PairIso[A1, A2, B1, B2]] =
    new SeqPairIso[A1, A2, B1, B2](iso1, iso2)
  def unmkPairIso[A1, A2, B1, B2](p: Rep[IsoUR[(A1, A2), (B1, B2)]]) = p match {
    case p: PairIso[A1, A2, B1, B2] @unchecked =>
      Some((p.iso1, p.iso2))
    case _ => None
  }

  case class SeqSumIso[A1, A2, B1, B2]
      (override val iso1: Iso[A1, B1], override val iso2: Iso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends AbsSumIso[A1, A2, B1, B2](iso1, iso2) {
  }

  def mkSumIso[A1, A2, B1, B2]
    (iso1: Iso[A1, B1], iso2: Iso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[SumIso[A1, A2, B1, B2]] =
    new SeqSumIso[A1, A2, B1, B2](iso1, iso2)
  def unmkSumIso[A1, A2, B1, B2](p: Rep[IsoUR[$bar[A1, A2], $bar[B1, B2]]]) = p match {
    case p: SumIso[A1, A2, B1, B2] @unchecked =>
      Some((p.iso1, p.iso2))
    case _ => None
  }

  case class SeqComposeIso[A, B, C]
      (override val iso2: Iso[B, C], override val iso1: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C])
    extends AbsComposeIso[A, B, C](iso2, iso1) {
  }

  def mkComposeIso[A, B, C]
    (iso2: Iso[B, C], iso1: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]): Rep[ComposeIso[A, B, C]] =
    new SeqComposeIso[A, B, C](iso2, iso1)
  def unmkComposeIso[A, B, C](p: Rep[IsoUR[A, C]]) = p match {
    case p: ComposeIso[A, B, C] @unchecked =>
      Some((p.iso2, p.iso1))
    case _ => None
  }

  case class SeqFuncIso[A, B, C, D]
      (override val iso1: Iso[A, B], override val iso2: Iso[C, D])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D])
    extends AbsFuncIso[A, B, C, D](iso1, iso2) {
  }

  def mkFuncIso[A, B, C, D]
    (iso1: Iso[A, B], iso2: Iso[C, D])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D]): Rep[FuncIso[A, B, C, D]] =
    new SeqFuncIso[A, B, C, D](iso1, iso2)
  def unmkFuncIso[A, B, C, D](p: Rep[IsoUR[A => C, B => D]]) = p match {
    case p: FuncIso[A, B, C, D] @unchecked =>
      Some((p.iso1, p.iso2))
    case _ => None
  }

  case class SeqConverterIso[A, B]
      (override val convTo: Conv[A, B], override val convFrom: Conv[B, A])(implicit eA: Elem[A], eB: Elem[B])
    extends AbsConverterIso[A, B](convTo, convFrom) {
  }

  def mkConverterIso[A, B]
    (convTo: Conv[A, B], convFrom: Conv[B, A])(implicit eA: Elem[A], eB: Elem[B]): Rep[ConverterIso[A, B]] =
    new SeqConverterIso[A, B](convTo, convFrom)
  def unmkConverterIso[A, B](p: Rep[IsoUR[A, B]]) = p match {
    case p: ConverterIso[A, B] @unchecked =>
      Some((p.convTo, p.convFrom))
    case _ => None
  }

  case class SeqArrayIso[A, B]
      (override val innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B])
    extends AbsArrayIso[A, B](innerIso) {
  }

  def mkArrayIso[A, B]
    (innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B]): Rep[ArrayIso[A, B]] =
    new SeqArrayIso[A, B](innerIso)
  def unmkArrayIso[A, B](p: Rep[Iso1UR[A, B, Array]]) = p match {
    case p: ArrayIso[A, B] @unchecked =>
      Some((p.innerIso))
    case _ => None
  }

  case class SeqListIso[A, B]
      (override val innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B])
    extends AbsListIso[A, B](innerIso) {
  }

  def mkListIso[A, B]
    (innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B]): Rep[ListIso[A, B]] =
    new SeqListIso[A, B](innerIso)
  def unmkListIso[A, B](p: Rep[Iso1UR[A, B, List]]) = p match {
    case p: ListIso[A, B] @unchecked =>
      Some((p.innerIso))
    case _ => None
  }

  case class SeqArrayBufferIso[A, B]
      (override val innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B])
    extends AbsArrayBufferIso[A, B](innerIso) {
  }

  def mkArrayBufferIso[A, B]
    (innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B]): Rep[ArrayBufferIso[A, B]] =
    new SeqArrayBufferIso[A, B](innerIso)
  def unmkArrayBufferIso[A, B](p: Rep[Iso1UR[A, B, ArrayBuffer]]) = p match {
    case p: ArrayBufferIso[A, B] @unchecked =>
      Some((p.innerIso))
    case _ => None
  }

  case class SeqThunkIso[A, B]
      (override val innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B])
    extends AbsThunkIso[A, B](innerIso) {
  }

  def mkThunkIso[A, B]
    (innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B]): Rep[ThunkIso[A, B]] =
    new SeqThunkIso[A, B](innerIso)
  def unmkThunkIso[A, B](p: Rep[Iso1UR[A, B, Thunk]]) = p match {
    case p: ThunkIso[A, B] @unchecked =>
      Some((p.innerIso))
    case _ => None
  }
}

// Exp -----------------------------------
trait ViewsExp extends ViewsDsl {
  self: ViewsDsl with ScalanExp =>
  lazy val IsoUR: Rep[IsoURCompanionAbs] = new IsoURCompanionAbs {
  }

  case class ExpIdentityIso[A]
      ()(implicit eA: Elem[A])
    extends AbsIdentityIso[A]()

  object IdentityIsoMethods {
    object from {
      def unapply(d: Def[_]): Option[(Rep[IdentityIso[A]], Rep[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(x, _*), _) if receiver.elem.isInstanceOf[IdentityIsoElem[_]] && method.getName == "from" =>
          Some((receiver, x)).asInstanceOf[Option[(Rep[IdentityIso[A]], Rep[A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[IdentityIso[A]], Rep[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object to {
      def unapply(d: Def[_]): Option[(Rep[IdentityIso[A]], Rep[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(x, _*), _) if receiver.elem.isInstanceOf[IdentityIsoElem[_]] && method.getName == "to" =>
          Some((receiver, x)).asInstanceOf[Option[(Rep[IdentityIso[A]], Rep[A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[IdentityIso[A]], Rep[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object isIdentity {
      def unapply(d: Def[_]): Option[Rep[IdentityIso[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[IdentityIsoElem[_]] && method.getName == "isIdentity" =>
          Some(receiver).asInstanceOf[Option[Rep[IdentityIso[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[IdentityIso[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `equals`: Overrides Object method
  }

  def mkIdentityIso[A]
    ()(implicit eA: Elem[A]): Rep[IdentityIso[A]] =
    new ExpIdentityIso[A]()
  def unmkIdentityIso[A](p: Rep[IsoUR[A, A]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: IdentityIsoElem[A] @unchecked =>
      Some(())
    case _ =>
      None
  }

  case class ExpPairIso[A1, A2, B1, B2]
      (override val iso1: Iso[A1, B1], override val iso2: Iso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends AbsPairIso[A1, A2, B1, B2](iso1, iso2)

  object PairIsoMethods {
    object from {
      def unapply(d: Def[_]): Option[(Rep[PairIso[A1, A2, B1, B2]], Rep[(B1, B2)]) forSome {type A1; type A2; type B1; type B2}] = d match {
        case MethodCall(receiver, method, Seq(b, _*), _) if receiver.elem.isInstanceOf[PairIsoElem[_, _, _, _]] && method.getName == "from" =>
          Some((receiver, b)).asInstanceOf[Option[(Rep[PairIso[A1, A2, B1, B2]], Rep[(B1, B2)]) forSome {type A1; type A2; type B1; type B2}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairIso[A1, A2, B1, B2]], Rep[(B1, B2)]) forSome {type A1; type A2; type B1; type B2}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object to {
      def unapply(d: Def[_]): Option[(Rep[PairIso[A1, A2, B1, B2]], Rep[(A1, A2)]) forSome {type A1; type A2; type B1; type B2}] = d match {
        case MethodCall(receiver, method, Seq(a, _*), _) if receiver.elem.isInstanceOf[PairIsoElem[_, _, _, _]] && method.getName == "to" =>
          Some((receiver, a)).asInstanceOf[Option[(Rep[PairIso[A1, A2, B1, B2]], Rep[(A1, A2)]) forSome {type A1; type A2; type B1; type B2}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairIso[A1, A2, B1, B2]], Rep[(A1, A2)]) forSome {type A1; type A2; type B1; type B2}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object isIdentity {
      def unapply(d: Def[_]): Option[Rep[PairIso[A1, A2, B1, B2]] forSome {type A1; type A2; type B1; type B2}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairIsoElem[_, _, _, _]] && method.getName == "isIdentity" =>
          Some(receiver).asInstanceOf[Option[Rep[PairIso[A1, A2, B1, B2]] forSome {type A1; type A2; type B1; type B2}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[PairIso[A1, A2, B1, B2]] forSome {type A1; type A2; type B1; type B2}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `equals`: Overrides Object method
  }

  object PairIsoCompanionMethods {
  }

  def mkPairIso[A1, A2, B1, B2]
    (iso1: Iso[A1, B1], iso2: Iso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[PairIso[A1, A2, B1, B2]] =
    new ExpPairIso[A1, A2, B1, B2](iso1, iso2)
  def unmkPairIso[A1, A2, B1, B2](p: Rep[IsoUR[(A1, A2), (B1, B2)]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: PairIsoElem[A1, A2, B1, B2] @unchecked =>
      Some((p.asRep[PairIso[A1, A2, B1, B2]].iso1, p.asRep[PairIso[A1, A2, B1, B2]].iso2))
    case _ =>
      None
  }

  case class ExpSumIso[A1, A2, B1, B2]
      (override val iso1: Iso[A1, B1], override val iso2: Iso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends AbsSumIso[A1, A2, B1, B2](iso1, iso2)

  object SumIsoMethods {
    object from {
      def unapply(d: Def[_]): Option[(Rep[SumIso[A1, A2, B1, B2]], Rep[$bar[B1, B2]]) forSome {type A1; type A2; type B1; type B2}] = d match {
        case MethodCall(receiver, method, Seq(b, _*), _) if receiver.elem.isInstanceOf[SumIsoElem[_, _, _, _]] && method.getName == "from" =>
          Some((receiver, b)).asInstanceOf[Option[(Rep[SumIso[A1, A2, B1, B2]], Rep[$bar[B1, B2]]) forSome {type A1; type A2; type B1; type B2}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SumIso[A1, A2, B1, B2]], Rep[$bar[B1, B2]]) forSome {type A1; type A2; type B1; type B2}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object to {
      def unapply(d: Def[_]): Option[(Rep[SumIso[A1, A2, B1, B2]], Rep[$bar[A1, A2]]) forSome {type A1; type A2; type B1; type B2}] = d match {
        case MethodCall(receiver, method, Seq(a, _*), _) if receiver.elem.isInstanceOf[SumIsoElem[_, _, _, _]] && method.getName == "to" =>
          Some((receiver, a)).asInstanceOf[Option[(Rep[SumIso[A1, A2, B1, B2]], Rep[$bar[A1, A2]]) forSome {type A1; type A2; type B1; type B2}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SumIso[A1, A2, B1, B2]], Rep[$bar[A1, A2]]) forSome {type A1; type A2; type B1; type B2}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object isIdentity {
      def unapply(d: Def[_]): Option[Rep[SumIso[A1, A2, B1, B2]] forSome {type A1; type A2; type B1; type B2}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SumIsoElem[_, _, _, _]] && method.getName == "isIdentity" =>
          Some(receiver).asInstanceOf[Option[Rep[SumIso[A1, A2, B1, B2]] forSome {type A1; type A2; type B1; type B2}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[SumIso[A1, A2, B1, B2]] forSome {type A1; type A2; type B1; type B2}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `equals`: Overrides Object method
  }

  def mkSumIso[A1, A2, B1, B2]
    (iso1: Iso[A1, B1], iso2: Iso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[SumIso[A1, A2, B1, B2]] =
    new ExpSumIso[A1, A2, B1, B2](iso1, iso2)
  def unmkSumIso[A1, A2, B1, B2](p: Rep[IsoUR[$bar[A1, A2], $bar[B1, B2]]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: SumIsoElem[A1, A2, B1, B2] @unchecked =>
      Some((p.asRep[SumIso[A1, A2, B1, B2]].iso1, p.asRep[SumIso[A1, A2, B1, B2]].iso2))
    case _ =>
      None
  }

  case class ExpComposeIso[A, B, C]
      (override val iso2: Iso[B, C], override val iso1: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C])
    extends AbsComposeIso[A, B, C](iso2, iso1)

  object ComposeIsoMethods {
    object from {
      def unapply(d: Def[_]): Option[(Rep[ComposeIso[A, B, C]], Rep[C]) forSome {type A; type B; type C}] = d match {
        case MethodCall(receiver, method, Seq(c, _*), _) if receiver.elem.isInstanceOf[ComposeIsoElem[_, _, _]] && method.getName == "from" =>
          Some((receiver, c)).asInstanceOf[Option[(Rep[ComposeIso[A, B, C]], Rep[C]) forSome {type A; type B; type C}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ComposeIso[A, B, C]], Rep[C]) forSome {type A; type B; type C}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object to {
      def unapply(d: Def[_]): Option[(Rep[ComposeIso[A, B, C]], Rep[A]) forSome {type A; type B; type C}] = d match {
        case MethodCall(receiver, method, Seq(a, _*), _) if receiver.elem.isInstanceOf[ComposeIsoElem[_, _, _]] && method.getName == "to" =>
          Some((receiver, a)).asInstanceOf[Option[(Rep[ComposeIso[A, B, C]], Rep[A]) forSome {type A; type B; type C}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ComposeIso[A, B, C]], Rep[A]) forSome {type A; type B; type C}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object isIdentity {
      def unapply(d: Def[_]): Option[Rep[ComposeIso[A, B, C]] forSome {type A; type B; type C}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ComposeIsoElem[_, _, _]] && method.getName == "isIdentity" =>
          Some(receiver).asInstanceOf[Option[Rep[ComposeIso[A, B, C]] forSome {type A; type B; type C}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[ComposeIso[A, B, C]] forSome {type A; type B; type C}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `equals`: Overrides Object method
  }

  def mkComposeIso[A, B, C]
    (iso2: Iso[B, C], iso1: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]): Rep[ComposeIso[A, B, C]] =
    new ExpComposeIso[A, B, C](iso2, iso1)
  def unmkComposeIso[A, B, C](p: Rep[IsoUR[A, C]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ComposeIsoElem[A, B, C] @unchecked =>
      Some((p.asRep[ComposeIso[A, B, C]].iso2, p.asRep[ComposeIso[A, B, C]].iso1))
    case _ =>
      None
  }

  case class ExpFuncIso[A, B, C, D]
      (override val iso1: Iso[A, B], override val iso2: Iso[C, D])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D])
    extends AbsFuncIso[A, B, C, D](iso1, iso2)

  object FuncIsoMethods {
    object from {
      def unapply(d: Def[_]): Option[(Rep[FuncIso[A, B, C, D]], Rep[B => D]) forSome {type A; type B; type C; type D}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[FuncIsoElem[_, _, _, _]] && method.getName == "from" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[FuncIso[A, B, C, D]], Rep[B => D]) forSome {type A; type B; type C; type D}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[FuncIso[A, B, C, D]], Rep[B => D]) forSome {type A; type B; type C; type D}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object to {
      def unapply(d: Def[_]): Option[(Rep[FuncIso[A, B, C, D]], Rep[A => C]) forSome {type A; type B; type C; type D}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[FuncIsoElem[_, _, _, _]] && method.getName == "to" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[FuncIso[A, B, C, D]], Rep[A => C]) forSome {type A; type B; type C; type D}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[FuncIso[A, B, C, D]], Rep[A => C]) forSome {type A; type B; type C; type D}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object isIdentity {
      def unapply(d: Def[_]): Option[Rep[FuncIso[A, B, C, D]] forSome {type A; type B; type C; type D}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[FuncIsoElem[_, _, _, _]] && method.getName == "isIdentity" =>
          Some(receiver).asInstanceOf[Option[Rep[FuncIso[A, B, C, D]] forSome {type A; type B; type C; type D}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[FuncIso[A, B, C, D]] forSome {type A; type B; type C; type D}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `equals`: Overrides Object method
  }

  def mkFuncIso[A, B, C, D]
    (iso1: Iso[A, B], iso2: Iso[C, D])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D]): Rep[FuncIso[A, B, C, D]] =
    new ExpFuncIso[A, B, C, D](iso1, iso2)
  def unmkFuncIso[A, B, C, D](p: Rep[IsoUR[A => C, B => D]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: FuncIsoElem[A, B, C, D] @unchecked =>
      Some((p.asRep[FuncIso[A, B, C, D]].iso1, p.asRep[FuncIso[A, B, C, D]].iso2))
    case _ =>
      None
  }

  case class ExpConverterIso[A, B]
      (override val convTo: Conv[A, B], override val convFrom: Conv[B, A])(implicit eA: Elem[A], eB: Elem[B])
    extends AbsConverterIso[A, B](convTo, convFrom)

  object ConverterIsoMethods {
    object to {
      def unapply(d: Def[_]): Option[(Rep[ConverterIso[A, B]], Rep[A]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(a, _*), _) if receiver.elem.isInstanceOf[ConverterIsoElem[_, _]] && method.getName == "to" =>
          Some((receiver, a)).asInstanceOf[Option[(Rep[ConverterIso[A, B]], Rep[A]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConverterIso[A, B]], Rep[A]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object from {
      def unapply(d: Def[_]): Option[(Rep[ConverterIso[A, B]], Rep[B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(b, _*), _) if receiver.elem.isInstanceOf[ConverterIsoElem[_, _]] && method.getName == "from" =>
          Some((receiver, b)).asInstanceOf[Option[(Rep[ConverterIso[A, B]], Rep[B]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConverterIso[A, B]], Rep[B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object isIdentity {
      def unapply(d: Def[_]): Option[Rep[ConverterIso[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ConverterIsoElem[_, _]] && method.getName == "isIdentity" =>
          Some(receiver).asInstanceOf[Option[Rep[ConverterIso[A, B]] forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[ConverterIso[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `equals`: Overrides Object method
  }

  def mkConverterIso[A, B]
    (convTo: Conv[A, B], convFrom: Conv[B, A])(implicit eA: Elem[A], eB: Elem[B]): Rep[ConverterIso[A, B]] =
    new ExpConverterIso[A, B](convTo, convFrom)
  def unmkConverterIso[A, B](p: Rep[IsoUR[A, B]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ConverterIsoElem[A, B] @unchecked =>
      Some((p.asRep[ConverterIso[A, B]].convTo, p.asRep[ConverterIso[A, B]].convFrom))
    case _ =>
      None
  }

  case class ExpArrayIso[A, B]
      (override val innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B])
    extends AbsArrayIso[A, B](innerIso)

  object ArrayIsoMethods {
    object cC {
      def unapply(d: Def[_]): Option[Rep[ArrayIso[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ArrayIsoElem[_, _]] && method.getName == "cC" =>
          Some(receiver).asInstanceOf[Option[Rep[ArrayIso[A, B]] forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[ArrayIso[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object from {
      def unapply(d: Def[_]): Option[(Rep[ArrayIso[A, B]], Arr[B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(x, _*), _) if receiver.elem.isInstanceOf[ArrayIsoElem[_, _]] && method.getName == "from" =>
          Some((receiver, x)).asInstanceOf[Option[(Rep[ArrayIso[A, B]], Arr[B]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ArrayIso[A, B]], Arr[B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object to {
      def unapply(d: Def[_]): Option[(Rep[ArrayIso[A, B]], Arr[A]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(x, _*), _) if receiver.elem.isInstanceOf[ArrayIsoElem[_, _]] && method.getName == "to" =>
          Some((receiver, x)).asInstanceOf[Option[(Rep[ArrayIso[A, B]], Arr[A]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ArrayIso[A, B]], Arr[A]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkArrayIso[A, B]
    (innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B]): Rep[ArrayIso[A, B]] =
    new ExpArrayIso[A, B](innerIso)
  def unmkArrayIso[A, B](p: Rep[Iso1UR[A, B, Array]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ArrayIsoElem[A, B] @unchecked =>
      Some((p.asRep[ArrayIso[A, B]].innerIso))
    case _ =>
      None
  }

  case class ExpListIso[A, B]
      (override val innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B])
    extends AbsListIso[A, B](innerIso)

  object ListIsoMethods {
    object cC {
      def unapply(d: Def[_]): Option[Rep[ListIso[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ListIsoElem[_, _]] && method.getName == "cC" =>
          Some(receiver).asInstanceOf[Option[Rep[ListIso[A, B]] forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[ListIso[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object from {
      def unapply(d: Def[_]): Option[(Rep[ListIso[A, B]], Lst[B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(x, _*), _) if receiver.elem.isInstanceOf[ListIsoElem[_, _]] && method.getName == "from" =>
          Some((receiver, x)).asInstanceOf[Option[(Rep[ListIso[A, B]], Lst[B]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ListIso[A, B]], Lst[B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object to {
      def unapply(d: Def[_]): Option[(Rep[ListIso[A, B]], Lst[A]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(x, _*), _) if receiver.elem.isInstanceOf[ListIsoElem[_, _]] && method.getName == "to" =>
          Some((receiver, x)).asInstanceOf[Option[(Rep[ListIso[A, B]], Lst[A]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ListIso[A, B]], Lst[A]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkListIso[A, B]
    (innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B]): Rep[ListIso[A, B]] =
    new ExpListIso[A, B](innerIso)
  def unmkListIso[A, B](p: Rep[Iso1UR[A, B, List]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ListIsoElem[A, B] @unchecked =>
      Some((p.asRep[ListIso[A, B]].innerIso))
    case _ =>
      None
  }

  case class ExpArrayBufferIso[A, B]
      (override val innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B])
    extends AbsArrayBufferIso[A, B](innerIso)

  object ArrayBufferIsoMethods {
    object cC {
      def unapply(d: Def[_]): Option[Rep[ArrayBufferIso[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ArrayBufferIsoElem[_, _]] && method.getName == "cC" =>
          Some(receiver).asInstanceOf[Option[Rep[ArrayBufferIso[A, B]] forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[ArrayBufferIso[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object from {
      def unapply(d: Def[_]): Option[(Rep[ArrayBufferIso[A, B]], Rep[ArrayBuffer[B]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(x, _*), _) if receiver.elem.isInstanceOf[ArrayBufferIsoElem[_, _]] && method.getName == "from" =>
          Some((receiver, x)).asInstanceOf[Option[(Rep[ArrayBufferIso[A, B]], Rep[ArrayBuffer[B]]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ArrayBufferIso[A, B]], Rep[ArrayBuffer[B]]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object to {
      def unapply(d: Def[_]): Option[(Rep[ArrayBufferIso[A, B]], Rep[ArrayBuffer[A]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(x, _*), _) if receiver.elem.isInstanceOf[ArrayBufferIsoElem[_, _]] && method.getName == "to" =>
          Some((receiver, x)).asInstanceOf[Option[(Rep[ArrayBufferIso[A, B]], Rep[ArrayBuffer[A]]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ArrayBufferIso[A, B]], Rep[ArrayBuffer[A]]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkArrayBufferIso[A, B]
    (innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B]): Rep[ArrayBufferIso[A, B]] =
    new ExpArrayBufferIso[A, B](innerIso)
  def unmkArrayBufferIso[A, B](p: Rep[Iso1UR[A, B, ArrayBuffer]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ArrayBufferIsoElem[A, B] @unchecked =>
      Some((p.asRep[ArrayBufferIso[A, B]].innerIso))
    case _ =>
      None
  }

  case class ExpThunkIso[A, B]
      (override val innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B])
    extends AbsThunkIso[A, B](innerIso)

  object ThunkIsoMethods {
    object cC {
      def unapply(d: Def[_]): Option[Rep[ThunkIso[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ThunkIsoElem[_, _]] && method.getName == "cC" =>
          Some(receiver).asInstanceOf[Option[Rep[ThunkIso[A, B]] forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[ThunkIso[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object from {
      def unapply(d: Def[_]): Option[(Rep[ThunkIso[A, B]], Th[B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(x, _*), _) if receiver.elem.isInstanceOf[ThunkIsoElem[_, _]] && method.getName == "from" =>
          Some((receiver, x)).asInstanceOf[Option[(Rep[ThunkIso[A, B]], Th[B]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ThunkIso[A, B]], Th[B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object to {
      def unapply(d: Def[_]): Option[(Rep[ThunkIso[A, B]], Th[A]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(x, _*), _) if receiver.elem.isInstanceOf[ThunkIsoElem[_, _]] && method.getName == "to" =>
          Some((receiver, x)).asInstanceOf[Option[(Rep[ThunkIso[A, B]], Th[A]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ThunkIso[A, B]], Th[A]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkThunkIso[A, B]
    (innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B]): Rep[ThunkIso[A, B]] =
    new ExpThunkIso[A, B](innerIso)
  def unmkThunkIso[A, B](p: Rep[Iso1UR[A, B, Thunk]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ThunkIsoElem[A, B] @unchecked =>
      Some((p.asRep[ThunkIso[A, B]].innerIso))
    case _ =>
      None
  }

  object IsoURMethods {
    object from {
      def unapply(d: Def[_]): Option[(Rep[IsoUR[From, To]], Rep[To]) forSome {type From; type To}] = d match {
        case MethodCall(receiver, method, Seq(p, _*), _) if receiver.elem.isInstanceOf[IsoURElem[_, _, _]] && method.getName == "from" =>
          Some((receiver, p)).asInstanceOf[Option[(Rep[IsoUR[From, To]], Rep[To]) forSome {type From; type To}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[IsoUR[From, To]], Rep[To]) forSome {type From; type To}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object to {
      def unapply(d: Def[_]): Option[(Rep[IsoUR[From, To]], Rep[From]) forSome {type From; type To}] = d match {
        case MethodCall(receiver, method, Seq(p, _*), _) if receiver.elem.isInstanceOf[IsoURElem[_, _, _]] && method.getName == "to" =>
          Some((receiver, p)).asInstanceOf[Option[(Rep[IsoUR[From, To]], Rep[From]) forSome {type From; type To}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[IsoUR[From, To]], Rep[From]) forSome {type From; type To}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `toString`: Overrides Object method

    // WARNING: Cannot generate matcher for method `equals`: Overrides Object method

    // WARNING: Cannot generate matcher for method `isIdentity`: Method's return type Boolean is not a Rep
  }
}

object Views_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAAO1ZTWzbVBx/cZqPNlXbMT43bS1VAA1Bs0ZIOxRpipMWikJb1R1CYVr1kry03mw/134pCYeJ04TghrhwQGISF6RdECeEhLggIQ6cJoTEiQOnMYR2YOIA4r3nj9iO7aQtQ1VpD5bt/P37f/x+/7/fc2/dBSnTAE+bDahAbU5FBM5J/Lxkkry0qBGZdF/FzbaCKqj1OP764/lPT30hgMkaSG9Ds2IqNTBqnSx2dPdcQjtVMAq1BjIJNkwCnqxyD4UGVhTUIDLWCrKqtgmsK6hQlU2yUAUjddzs7oDrIFEFUw2sNQxEkFRWoGki076fRSwi2b0e5dfdVb3nQyuwLAqeLDYMKBMaPvUxZdmvI13qaljrqgRM2KGt6iwsapORVR0bxHGRoXDbuOlcjmiQ3gAPVa/CXVigLrYKEjFkbYs+mdNh4xrcQivUhJmP0IBNpLQ2ujq/TlbBmIl2aIGWVV3hdzo6AIAyUORBzPXqM+fWZ47VJy8hQ4aK/BZkP64ZuNMF1l8iCUBHpxDPDYBwENCi1sy/e7nxxn0ppwrs4Q4LJcMzTFOg6Qg1cCpoHb9df9+899LNCwIYq4Ex2SzVTWLABvFSblcrBzUNEx6zW0BobFG2ZqPY4l5K1CYgidEGVnWoUSS7lOOUJ0VuyIQZs3vjNjsRpc8QHTmmiY6ecPOdiciX66YMFWXtzhPPP/Xr4usCEPwuRimkRIVvOKAEpJZNfGndxmbHSQISpV6Be5fsMNrpHTMxobhFeebOb81vzoPLgltK27MLyWBylhRWsIbyS2v5P6TvPrjFmDbAuPWLJfa/5Qt//TTRIlwEHOix4URAI0mZP/6Qu33uogCyNd4ySwrcqlFSzEUFqatGGWukBrJ4FxnWL5ldqLCzUFFkmqgF2wqxqfLWOElrTMBMZHPriBGwQAmlveBW4TQBAio5FR9hMYVy4iWBgNxy0xoQlESO4pbjTJRGuKYeXa8+rNy9+JUAUq+AVItmaVZBqo7bWtMRKx1oBHWI6NxL+LOk4oQGVB02rdaeATwIHmggYm6YS/hz8mkvmkIa80ZbV9ALX/555Z23X9a5lvqk7IcWSvM+DQulYjCa+cATYuAJsdhXca/4vfJjx7MgwOeIbOJ5By1JUx0Qoe3fE2EoZDEOsjhUCqzlxqzGkrCKTszek6/cfI/wuiY6/pfFav0qnc4L/LlTcSTZL63Pbtx45PdPNk/yYZuty0SFev78HkatMxkf4CgFnuHDaexd80LTsTi1BmWDFrjs9Xw6gvlgJydRySU+pJUd5r1Mh0AUYyH65BwCIcZGEaK3EIjYKEL0RUDGLp13ILHjubAh4euCCIviIAtxIIbYV67YWeSknK9DY89zJeLB/9F46TF/3CaRbZKW2uqR6JLgarEcLorhFB0jv4QY7WdfLRIMXDyQmvewfIsCEOMAxCEAynEA/bwQMMbeb9hEwwrRW7JwA3GQQR9tA1aFZ6NXhUttrXF7+cOTk2c2f+Z7nXQTq1Dm7+ppuqA16AKGLwGm7eVhpFrZZeJElNIqB1H0PgW4vxYpDxX4UVR0FEAlDqC/PnT5wlT1n/dDjEElmOV+hnLEbBss4TTdCO5uYLeKdJ+8e1AVZxnmkoHVONRAD0Z8gziEUnaENM6SQgZBQy+Gh1VTjBbS1Nf8cGJw5VMyDNjdrz6ysqbxDI/ie9ahMstLdLhpHGHfTI9ZjGExwyp0uEnMcaGJ7VYLGcdcxnA54SnU4aY0tbHd1q4dkxlDZpaX6MHQ6AnVwrwE/BElK6jlvxP6KYgtV3x1FjZwaKF91LoW7odFC3yoRAM+w/fsvihCEwxdHpYH8RpmFK6sAQnbLfOvUhtjUB7oYzM6C+HFnqVtnnpNRm+aNA1rG8px1+m2dDZiWyrZX56pqq7f/2jl2e8//4VvTcfYN2ysIc39R23vg3XwnxdZ7rNiKj7BpS0vnuipRNiHbh7zP9ae0CAaHwAA"
}
}

