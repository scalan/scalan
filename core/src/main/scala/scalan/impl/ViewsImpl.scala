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
    lazy val typeArgs = TypeArgs("From" -> eFrom, "To" -> eTo)
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
    override lazy val typeArgs = TypeArgs("A" -> eA, "B" -> eB, "C" -> cC)
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

  implicit case object Iso1URCompanionElem extends CompanionElem[Iso1URCompanionAbs] {
    lazy val tag = weakTypeTag[Iso1URCompanionAbs]
    protected def getDefaultRep = Iso1UR
  }

  abstract class Iso1URCompanionAbs extends CompanionDef[Iso1URCompanionAbs] {
    def selfType = Iso1URCompanionElem
    override def toString = "Iso1UR"
  }
  def Iso1UR: Rep[Iso1URCompanionAbs]
  implicit def proxyIso1URCompanionAbs(p: Rep[Iso1URCompanionAbs]): Iso1URCompanionAbs =
    proxyOps[Iso1URCompanionAbs](p)

  abstract class AbsIdentityIso[A]
      ()(implicit eA: Elem[A])
    extends IdentityIso[A]() with Def[IdentityIso[A]] {
    lazy val selfType = element[IdentityIso[A]]
  }
  // elem for concrete class
  class IdentityIsoElem[A](val iso: Iso[IdentityIsoData[A], IdentityIso[A]])(implicit val eA: Elem[A])
    extends IsoURElem[A, A, IdentityIso[A]]
    with ConcreteElem[IdentityIsoData[A], IdentityIso[A]] {
    override lazy val parent: Option[Elem[_]] = Some(isoURElement(element[A], element[A]))
    override lazy val typeArgs = TypeArgs("A" -> eA)

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
    lazy val typeArgs = TypeArgs("A" -> eA)
  }
  // 4) constructor and deconstructor
  class IdentityIsoCompanionAbs extends CompanionDef[IdentityIsoCompanionAbs] {
    def selfType = IdentityIsoCompanionElem
    override def toString = "IdentityIso"
    @scalan.OverloadId("fromData")
    def apply[A](p: Rep[IdentityIsoData[A]])(implicit eA: Elem[A]): Rep[IdentityIso[A]] =
      isoIdentityIso(eA).to(p)
    @scalan.OverloadId("fromFields")
    def apply[A]()(implicit eA: Elem[A]): Rep[IdentityIso[A]] =
      mkIdentityIso()

    def unapply[A](p: Rep[IsoUR[A, A]]) = unmkIdentityIso(p)
  }
  lazy val IdentityIsoRep: Rep[IdentityIsoCompanionAbs] = new IdentityIsoCompanionAbs
  lazy val IdentityIso: IdentityIsoCompanionAbs = proxyIdentityIsoCompanion(IdentityIsoRep)
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
  class PairIsoElem[A1, A2, B1, B2](val iso: Iso[PairIsoData[A1, A2, B1, B2], PairIso[A1, A2, B1, B2]])(implicit val eA1: Elem[A1], val eA2: Elem[A2], val eB1: Elem[B1], val eB2: Elem[B2])
    extends IsoURElem[(A1, A2), (B1, B2), PairIso[A1, A2, B1, B2]]
    with ConcreteElem[PairIsoData[A1, A2, B1, B2], PairIso[A1, A2, B1, B2]] {
    override lazy val parent: Option[Elem[_]] = Some(isoURElement(pairElement(element[A1],element[A2]), pairElement(element[B1],element[B2])))
    override lazy val typeArgs = TypeArgs("A1" -> eA1, "A2" -> eA2, "B1" -> eB1, "B2" -> eB2)

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
    def productElement(n: Int) = n match {
      case 0 => eA1
      case 1 => eA2
      case 2 => eB1
      case 3 => eB2
    }
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
    lazy val typeArgs = TypeArgs("A1" -> eA1, "A2" -> eA2, "B1" -> eB1, "B2" -> eB2)
  }
  // 4) constructor and deconstructor
  class PairIsoCompanionAbs extends CompanionDef[PairIsoCompanionAbs] with PairIsoCompanion {
    def selfType = PairIsoCompanionElem
    override def toString = "PairIso"
    @scalan.OverloadId("fromData")
    def apply[A1, A2, B1, B2](p: Rep[PairIsoData[A1, A2, B1, B2]])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[PairIso[A1, A2, B1, B2]] =
      isoPairIso(eA1, eA2, eB1, eB2).to(p)
    @scalan.OverloadId("fromFields")
    def apply[A1, A2, B1, B2](iso1: Iso[A1, B1], iso2: Iso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[PairIso[A1, A2, B1, B2]] =
      mkPairIso(iso1, iso2)

    def unapply[A1, A2, B1, B2](p: Rep[IsoUR[(A1, A2), (B1, B2)]]) = unmkPairIso(p)
  }
  lazy val PairIsoRep: Rep[PairIsoCompanionAbs] = new PairIsoCompanionAbs
  lazy val PairIso: PairIsoCompanionAbs = proxyPairIsoCompanion(PairIsoRep)
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

  abstract class AbsAbsorbFirstUnitIso[A2, B2]
      (iso2: Iso[A2, B2])(implicit eA2: Elem[A2], eB2: Elem[B2])
    extends AbsorbFirstUnitIso[A2, B2](iso2) with Def[AbsorbFirstUnitIso[A2, B2]] {
    lazy val selfType = element[AbsorbFirstUnitIso[A2, B2]]
  }
  // elem for concrete class
  class AbsorbFirstUnitIsoElem[A2, B2](val iso: Iso[AbsorbFirstUnitIsoData[A2, B2], AbsorbFirstUnitIso[A2, B2]])(implicit val eA2: Elem[A2], val eB2: Elem[B2])
    extends IsoURElem[A2, (Unit, B2), AbsorbFirstUnitIso[A2, B2]]
    with ConcreteElem[AbsorbFirstUnitIsoData[A2, B2], AbsorbFirstUnitIso[A2, B2]] {
    override lazy val parent: Option[Elem[_]] = Some(isoURElement(element[A2], pairElement(UnitElement,element[B2])))
    override lazy val typeArgs = TypeArgs("A2" -> eA2, "B2" -> eB2)

    override def convertIsoUR(x: Rep[IsoUR[A2, (Unit, B2)]]) = // Converter is not generated by meta
!!!("Cannot convert from IsoUR to AbsorbFirstUnitIso: missing fields List(iso2)")
    override def getDefaultRep = AbsorbFirstUnitIso(element[IsoUR[A2, B2]].defaultRepValue)
    override lazy val tag = {
      implicit val tagA2 = eA2.tag
      implicit val tagB2 = eB2.tag
      weakTypeTag[AbsorbFirstUnitIso[A2, B2]]
    }
  }

  // state representation type
  type AbsorbFirstUnitIsoData[A2, B2] = IsoUR[A2, B2]

  // 3) Iso for concrete class
  class AbsorbFirstUnitIsoIso[A2, B2](implicit eA2: Elem[A2], eB2: Elem[B2])
    extends EntityIso[AbsorbFirstUnitIsoData[A2, B2], AbsorbFirstUnitIso[A2, B2]] with Def[AbsorbFirstUnitIsoIso[A2, B2]] {
    override def from(p: Rep[AbsorbFirstUnitIso[A2, B2]]) =
      p.iso2
    override def to(p: Rep[IsoUR[A2, B2]]) = {
      val iso2 = p
      AbsorbFirstUnitIso(iso2)
    }
    lazy val eFrom = element[IsoUR[A2, B2]]
    lazy val eTo = new AbsorbFirstUnitIsoElem[A2, B2](self)
    lazy val selfType = new AbsorbFirstUnitIsoIsoElem[A2, B2](eA2, eB2)
    def productArity = 2
    def productElement(n: Int) = n match {
      case 0 => eA2
      case 1 => eB2
    }
  }
  case class AbsorbFirstUnitIsoIsoElem[A2, B2](eA2: Elem[A2], eB2: Elem[B2]) extends Elem[AbsorbFirstUnitIsoIso[A2, B2]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new AbsorbFirstUnitIsoIso[A2, B2]()(eA2, eB2))
    lazy val tag = {
      implicit val tagA2 = eA2.tag
      implicit val tagB2 = eB2.tag
      weakTypeTag[AbsorbFirstUnitIsoIso[A2, B2]]
    }
    lazy val typeArgs = TypeArgs("A2" -> eA2, "B2" -> eB2)
  }
  // 4) constructor and deconstructor
  class AbsorbFirstUnitIsoCompanionAbs extends CompanionDef[AbsorbFirstUnitIsoCompanionAbs] {
    def selfType = AbsorbFirstUnitIsoCompanionElem
    override def toString = "AbsorbFirstUnitIso"

    @scalan.OverloadId("fromFields")
    def apply[A2, B2](iso2: Iso[A2, B2])(implicit eA2: Elem[A2], eB2: Elem[B2]): Rep[AbsorbFirstUnitIso[A2, B2]] =
      mkAbsorbFirstUnitIso(iso2)

    def unapply[A2, B2](p: Rep[IsoUR[A2, (Unit, B2)]]) = unmkAbsorbFirstUnitIso(p)
  }
  lazy val AbsorbFirstUnitIsoRep: Rep[AbsorbFirstUnitIsoCompanionAbs] = new AbsorbFirstUnitIsoCompanionAbs
  lazy val AbsorbFirstUnitIso: AbsorbFirstUnitIsoCompanionAbs = proxyAbsorbFirstUnitIsoCompanion(AbsorbFirstUnitIsoRep)
  implicit def proxyAbsorbFirstUnitIsoCompanion(p: Rep[AbsorbFirstUnitIsoCompanionAbs]): AbsorbFirstUnitIsoCompanionAbs = {
    proxyOps[AbsorbFirstUnitIsoCompanionAbs](p)
  }

  implicit case object AbsorbFirstUnitIsoCompanionElem extends CompanionElem[AbsorbFirstUnitIsoCompanionAbs] {
    lazy val tag = weakTypeTag[AbsorbFirstUnitIsoCompanionAbs]
    protected def getDefaultRep = AbsorbFirstUnitIso
  }

  implicit def proxyAbsorbFirstUnitIso[A2, B2](p: Rep[AbsorbFirstUnitIso[A2, B2]]): AbsorbFirstUnitIso[A2, B2] =
    proxyOps[AbsorbFirstUnitIso[A2, B2]](p)

  implicit class ExtendedAbsorbFirstUnitIso[A2, B2](p: Rep[AbsorbFirstUnitIso[A2, B2]])(implicit eA2: Elem[A2], eB2: Elem[B2]) {
    def toData: Rep[AbsorbFirstUnitIsoData[A2, B2]] = isoAbsorbFirstUnitIso(eA2, eB2).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoAbsorbFirstUnitIso[A2, B2](implicit eA2: Elem[A2], eB2: Elem[B2]): Iso[AbsorbFirstUnitIsoData[A2, B2], AbsorbFirstUnitIso[A2, B2]] =
    reifyObject(new AbsorbFirstUnitIsoIso[A2, B2]()(eA2, eB2))

  // 6) smart constructor and deconstructor
  def mkAbsorbFirstUnitIso[A2, B2](iso2: Iso[A2, B2])(implicit eA2: Elem[A2], eB2: Elem[B2]): Rep[AbsorbFirstUnitIso[A2, B2]]
  def unmkAbsorbFirstUnitIso[A2, B2](p: Rep[IsoUR[A2, (Unit, B2)]]): Option[(Rep[IsoUR[A2, B2]])]

  abstract class AbsAbsorbSecondUnitIso[A1, B1]
      (iso1: Iso[A1, B1])(implicit eA1: Elem[A1], eB1: Elem[B1])
    extends AbsorbSecondUnitIso[A1, B1](iso1) with Def[AbsorbSecondUnitIso[A1, B1]] {
    lazy val selfType = element[AbsorbSecondUnitIso[A1, B1]]
  }
  // elem for concrete class
  class AbsorbSecondUnitIsoElem[A1, B1](val iso: Iso[AbsorbSecondUnitIsoData[A1, B1], AbsorbSecondUnitIso[A1, B1]])(implicit val eA1: Elem[A1], val eB1: Elem[B1])
    extends IsoURElem[A1, (B1, Unit), AbsorbSecondUnitIso[A1, B1]]
    with ConcreteElem[AbsorbSecondUnitIsoData[A1, B1], AbsorbSecondUnitIso[A1, B1]] {
    override lazy val parent: Option[Elem[_]] = Some(isoURElement(element[A1], pairElement(element[B1],UnitElement)))
    override lazy val typeArgs = TypeArgs("A1" -> eA1, "B1" -> eB1)

    override def convertIsoUR(x: Rep[IsoUR[A1, (B1, Unit)]]) = // Converter is not generated by meta
!!!("Cannot convert from IsoUR to AbsorbSecondUnitIso: missing fields List(iso1)")
    override def getDefaultRep = AbsorbSecondUnitIso(element[IsoUR[A1, B1]].defaultRepValue)
    override lazy val tag = {
      implicit val tagA1 = eA1.tag
      implicit val tagB1 = eB1.tag
      weakTypeTag[AbsorbSecondUnitIso[A1, B1]]
    }
  }

  // state representation type
  type AbsorbSecondUnitIsoData[A1, B1] = IsoUR[A1, B1]

  // 3) Iso for concrete class
  class AbsorbSecondUnitIsoIso[A1, B1](implicit eA1: Elem[A1], eB1: Elem[B1])
    extends EntityIso[AbsorbSecondUnitIsoData[A1, B1], AbsorbSecondUnitIso[A1, B1]] with Def[AbsorbSecondUnitIsoIso[A1, B1]] {
    override def from(p: Rep[AbsorbSecondUnitIso[A1, B1]]) =
      p.iso1
    override def to(p: Rep[IsoUR[A1, B1]]) = {
      val iso1 = p
      AbsorbSecondUnitIso(iso1)
    }
    lazy val eFrom = element[IsoUR[A1, B1]]
    lazy val eTo = new AbsorbSecondUnitIsoElem[A1, B1](self)
    lazy val selfType = new AbsorbSecondUnitIsoIsoElem[A1, B1](eA1, eB1)
    def productArity = 2
    def productElement(n: Int) = n match {
      case 0 => eA1
      case 1 => eB1
    }
  }
  case class AbsorbSecondUnitIsoIsoElem[A1, B1](eA1: Elem[A1], eB1: Elem[B1]) extends Elem[AbsorbSecondUnitIsoIso[A1, B1]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new AbsorbSecondUnitIsoIso[A1, B1]()(eA1, eB1))
    lazy val tag = {
      implicit val tagA1 = eA1.tag
      implicit val tagB1 = eB1.tag
      weakTypeTag[AbsorbSecondUnitIsoIso[A1, B1]]
    }
    lazy val typeArgs = TypeArgs("A1" -> eA1, "B1" -> eB1)
  }
  // 4) constructor and deconstructor
  class AbsorbSecondUnitIsoCompanionAbs extends CompanionDef[AbsorbSecondUnitIsoCompanionAbs] {
    def selfType = AbsorbSecondUnitIsoCompanionElem
    override def toString = "AbsorbSecondUnitIso"

    @scalan.OverloadId("fromFields")
    def apply[A1, B1](iso1: Iso[A1, B1])(implicit eA1: Elem[A1], eB1: Elem[B1]): Rep[AbsorbSecondUnitIso[A1, B1]] =
      mkAbsorbSecondUnitIso(iso1)

    def unapply[A1, B1](p: Rep[IsoUR[A1, (B1, Unit)]]) = unmkAbsorbSecondUnitIso(p)
  }
  lazy val AbsorbSecondUnitIsoRep: Rep[AbsorbSecondUnitIsoCompanionAbs] = new AbsorbSecondUnitIsoCompanionAbs
  lazy val AbsorbSecondUnitIso: AbsorbSecondUnitIsoCompanionAbs = proxyAbsorbSecondUnitIsoCompanion(AbsorbSecondUnitIsoRep)
  implicit def proxyAbsorbSecondUnitIsoCompanion(p: Rep[AbsorbSecondUnitIsoCompanionAbs]): AbsorbSecondUnitIsoCompanionAbs = {
    proxyOps[AbsorbSecondUnitIsoCompanionAbs](p)
  }

  implicit case object AbsorbSecondUnitIsoCompanionElem extends CompanionElem[AbsorbSecondUnitIsoCompanionAbs] {
    lazy val tag = weakTypeTag[AbsorbSecondUnitIsoCompanionAbs]
    protected def getDefaultRep = AbsorbSecondUnitIso
  }

  implicit def proxyAbsorbSecondUnitIso[A1, B1](p: Rep[AbsorbSecondUnitIso[A1, B1]]): AbsorbSecondUnitIso[A1, B1] =
    proxyOps[AbsorbSecondUnitIso[A1, B1]](p)

  implicit class ExtendedAbsorbSecondUnitIso[A1, B1](p: Rep[AbsorbSecondUnitIso[A1, B1]])(implicit eA1: Elem[A1], eB1: Elem[B1]) {
    def toData: Rep[AbsorbSecondUnitIsoData[A1, B1]] = isoAbsorbSecondUnitIso(eA1, eB1).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoAbsorbSecondUnitIso[A1, B1](implicit eA1: Elem[A1], eB1: Elem[B1]): Iso[AbsorbSecondUnitIsoData[A1, B1], AbsorbSecondUnitIso[A1, B1]] =
    reifyObject(new AbsorbSecondUnitIsoIso[A1, B1]()(eA1, eB1))

  // 6) smart constructor and deconstructor
  def mkAbsorbSecondUnitIso[A1, B1](iso1: Iso[A1, B1])(implicit eA1: Elem[A1], eB1: Elem[B1]): Rep[AbsorbSecondUnitIso[A1, B1]]
  def unmkAbsorbSecondUnitIso[A1, B1](p: Rep[IsoUR[A1, (B1, Unit)]]): Option[(Rep[IsoUR[A1, B1]])]

  abstract class AbsSumIso[A1, A2, B1, B2]
      (iso1: Iso[A1, B1], iso2: Iso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends SumIso[A1, A2, B1, B2](iso1, iso2) with Def[SumIso[A1, A2, B1, B2]] {
    lazy val selfType = element[SumIso[A1, A2, B1, B2]]
  }
  // elem for concrete class
  class SumIsoElem[A1, A2, B1, B2](val iso: Iso[SumIsoData[A1, A2, B1, B2], SumIso[A1, A2, B1, B2]])(implicit val eA1: Elem[A1], val eA2: Elem[A2], val eB1: Elem[B1], val eB2: Elem[B2])
    extends IsoURElem[$bar[A1, A2], $bar[B1, B2], SumIso[A1, A2, B1, B2]]
    with ConcreteElem[SumIsoData[A1, A2, B1, B2], SumIso[A1, A2, B1, B2]] {
    override lazy val parent: Option[Elem[_]] = Some(isoURElement(sumElement(element[A1],element[A2]), sumElement(element[B1],element[B2])))
    override lazy val typeArgs = TypeArgs("A1" -> eA1, "A2" -> eA2, "B1" -> eB1, "B2" -> eB2)

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
    def productElement(n: Int) = n match {
      case 0 => eA1
      case 1 => eA2
      case 2 => eB1
      case 3 => eB2
    }
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
    lazy val typeArgs = TypeArgs("A1" -> eA1, "A2" -> eA2, "B1" -> eB1, "B2" -> eB2)
  }
  // 4) constructor and deconstructor
  class SumIsoCompanionAbs extends CompanionDef[SumIsoCompanionAbs] {
    def selfType = SumIsoCompanionElem
    override def toString = "SumIso"
    @scalan.OverloadId("fromData")
    def apply[A1, A2, B1, B2](p: Rep[SumIsoData[A1, A2, B1, B2]])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[SumIso[A1, A2, B1, B2]] =
      isoSumIso(eA1, eA2, eB1, eB2).to(p)
    @scalan.OverloadId("fromFields")
    def apply[A1, A2, B1, B2](iso1: Iso[A1, B1], iso2: Iso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[SumIso[A1, A2, B1, B2]] =
      mkSumIso(iso1, iso2)

    def unapply[A1, A2, B1, B2](p: Rep[IsoUR[$bar[A1, A2], $bar[B1, B2]]]) = unmkSumIso(p)
  }
  lazy val SumIsoRep: Rep[SumIsoCompanionAbs] = new SumIsoCompanionAbs
  lazy val SumIso: SumIsoCompanionAbs = proxySumIsoCompanion(SumIsoRep)
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
  class ComposeIsoElem[A, B, C](val iso: Iso[ComposeIsoData[A, B, C], ComposeIso[A, B, C]])(implicit val eA: Elem[A], val eB: Elem[B], val eC: Elem[C])
    extends IsoURElem[A, C, ComposeIso[A, B, C]]
    with ConcreteElem[ComposeIsoData[A, B, C], ComposeIso[A, B, C]] {
    override lazy val parent: Option[Elem[_]] = Some(isoURElement(element[A], element[C]))
    override lazy val typeArgs = TypeArgs("A" -> eA, "B" -> eB, "C" -> eC)

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
    def productElement(n: Int) = n match {
      case 0 => eA
      case 1 => eB
      case 2 => eC
    }
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
    lazy val typeArgs = TypeArgs("A" -> eA, "B" -> eB, "C" -> eC)
  }
  // 4) constructor and deconstructor
  class ComposeIsoCompanionAbs extends CompanionDef[ComposeIsoCompanionAbs] {
    def selfType = ComposeIsoCompanionElem
    override def toString = "ComposeIso"
    @scalan.OverloadId("fromData")
    def apply[A, B, C](p: Rep[ComposeIsoData[A, B, C]])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]): Rep[ComposeIso[A, B, C]] =
      isoComposeIso(eA, eB, eC).to(p)
    @scalan.OverloadId("fromFields")
    def apply[A, B, C](iso2: Iso[B, C], iso1: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]): Rep[ComposeIso[A, B, C]] =
      mkComposeIso(iso2, iso1)

    def unapply[A, B, C](p: Rep[IsoUR[A, C]]) = unmkComposeIso(p)
  }
  lazy val ComposeIsoRep: Rep[ComposeIsoCompanionAbs] = new ComposeIsoCompanionAbs
  lazy val ComposeIso: ComposeIsoCompanionAbs = proxyComposeIsoCompanion(ComposeIsoRep)
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
  class FuncIsoElem[A, B, C, D](val iso: Iso[FuncIsoData[A, B, C, D], FuncIso[A, B, C, D]])(implicit val eA: Elem[A], val eB: Elem[B], val eC: Elem[C], val eD: Elem[D])
    extends IsoURElem[A => C, B => D, FuncIso[A, B, C, D]]
    with ConcreteElem[FuncIsoData[A, B, C, D], FuncIso[A, B, C, D]] {
    override lazy val parent: Option[Elem[_]] = Some(isoURElement(funcElement(element[A],element[C]), funcElement(element[B],element[D])))
    override lazy val typeArgs = TypeArgs("A" -> eA, "B" -> eB, "C" -> eC, "D" -> eD)

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
    def productElement(n: Int) = n match {
      case 0 => eA
      case 1 => eB
      case 2 => eC
      case 3 => eD
    }
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
    lazy val typeArgs = TypeArgs("A" -> eA, "B" -> eB, "C" -> eC, "D" -> eD)
  }
  // 4) constructor and deconstructor
  class FuncIsoCompanionAbs extends CompanionDef[FuncIsoCompanionAbs] {
    def selfType = FuncIsoCompanionElem
    override def toString = "FuncIso"
    @scalan.OverloadId("fromData")
    def apply[A, B, C, D](p: Rep[FuncIsoData[A, B, C, D]])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D]): Rep[FuncIso[A, B, C, D]] =
      isoFuncIso(eA, eB, eC, eD).to(p)
    @scalan.OverloadId("fromFields")
    def apply[A, B, C, D](iso1: Iso[A, B], iso2: Iso[C, D])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D]): Rep[FuncIso[A, B, C, D]] =
      mkFuncIso(iso1, iso2)

    def unapply[A, B, C, D](p: Rep[IsoUR[A => C, B => D]]) = unmkFuncIso(p)
  }
  lazy val FuncIsoRep: Rep[FuncIsoCompanionAbs] = new FuncIsoCompanionAbs
  lazy val FuncIso: FuncIsoCompanionAbs = proxyFuncIsoCompanion(FuncIsoRep)
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
  class ConverterIsoElem[A, B](val iso: Iso[ConverterIsoData[A, B], ConverterIso[A, B]])(implicit val eA: Elem[A], val eB: Elem[B])
    extends IsoURElem[A, B, ConverterIso[A, B]]
    with ConcreteElem[ConverterIsoData[A, B], ConverterIso[A, B]] {
    override lazy val parent: Option[Elem[_]] = Some(isoURElement(element[A], element[B]))
    override lazy val typeArgs = TypeArgs("A" -> eA, "B" -> eB)

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
    def productElement(n: Int) = n match {
      case 0 => eA
      case 1 => eB
    }
  }
  case class ConverterIsoIsoElem[A, B](eA: Elem[A], eB: Elem[B]) extends Elem[ConverterIsoIso[A, B]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new ConverterIsoIso[A, B]()(eA, eB))
    lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[ConverterIsoIso[A, B]]
    }
    lazy val typeArgs = TypeArgs("A" -> eA, "B" -> eB)
  }
  // 4) constructor and deconstructor
  class ConverterIsoCompanionAbs extends CompanionDef[ConverterIsoCompanionAbs] {
    def selfType = ConverterIsoCompanionElem
    override def toString = "ConverterIso"
    @scalan.OverloadId("fromData")
    def apply[A, B](p: Rep[ConverterIsoData[A, B]])(implicit eA: Elem[A], eB: Elem[B]): Rep[ConverterIso[A, B]] =
      isoConverterIso(eA, eB).to(p)
    @scalan.OverloadId("fromFields")
    def apply[A, B](convTo: Conv[A, B], convFrom: Conv[B, A])(implicit eA: Elem[A], eB: Elem[B]): Rep[ConverterIso[A, B]] =
      mkConverterIso(convTo, convFrom)

    def unapply[A, B](p: Rep[IsoUR[A, B]]) = unmkConverterIso(p)
  }
  lazy val ConverterIsoRep: Rep[ConverterIsoCompanionAbs] = new ConverterIsoCompanionAbs
  lazy val ConverterIso: ConverterIsoCompanionAbs = proxyConverterIsoCompanion(ConverterIsoRep)
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
  class ArrayIsoElem[A, B](val iso: Iso[ArrayIsoData[A, B], ArrayIso[A, B]])(implicit override val eA: Elem[A], override val eB: Elem[B])
    extends Iso1URElem[A, B, Array, ArrayIso[A, B]]
    with ConcreteElem[ArrayIsoData[A, B], ArrayIso[A, B]] {
    override lazy val parent: Option[Elem[_]] = Some(iso1URElement(element[A], element[B], container[Array]))
    override lazy val typeArgs = TypeArgs("A" -> eA, "B" -> eB)

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
    def productElement(n: Int) = n match {
      case 0 => eA
      case 1 => eB
    }
  }
  case class ArrayIsoIsoElem[A, B](eA: Elem[A], eB: Elem[B]) extends Elem[ArrayIsoIso[A, B]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new ArrayIsoIso[A, B]()(eA, eB))
    lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[ArrayIsoIso[A, B]]
    }
    lazy val typeArgs = TypeArgs("A" -> eA, "B" -> eB)
  }
  // 4) constructor and deconstructor
  class ArrayIsoCompanionAbs extends CompanionDef[ArrayIsoCompanionAbs] {
    def selfType = ArrayIsoCompanionElem
    override def toString = "ArrayIso"

    @scalan.OverloadId("fromFields")
    def apply[A, B](innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B]): Rep[ArrayIso[A, B]] =
      mkArrayIso(innerIso)

    def unapply[A, B](p: Rep[Iso1UR[A, B, Array]]) = unmkArrayIso(p)
  }
  lazy val ArrayIsoRep: Rep[ArrayIsoCompanionAbs] = new ArrayIsoCompanionAbs
  lazy val ArrayIso: ArrayIsoCompanionAbs = proxyArrayIsoCompanion(ArrayIsoRep)
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
  class ListIsoElem[A, B](val iso: Iso[ListIsoData[A, B], ListIso[A, B]])(implicit override val eA: Elem[A], override val eB: Elem[B])
    extends Iso1URElem[A, B, List, ListIso[A, B]]
    with ConcreteElem[ListIsoData[A, B], ListIso[A, B]] {
    override lazy val parent: Option[Elem[_]] = Some(iso1URElement(element[A], element[B], container[List]))
    override lazy val typeArgs = TypeArgs("A" -> eA, "B" -> eB)

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
    def productElement(n: Int) = n match {
      case 0 => eA
      case 1 => eB
    }
  }
  case class ListIsoIsoElem[A, B](eA: Elem[A], eB: Elem[B]) extends Elem[ListIsoIso[A, B]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new ListIsoIso[A, B]()(eA, eB))
    lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[ListIsoIso[A, B]]
    }
    lazy val typeArgs = TypeArgs("A" -> eA, "B" -> eB)
  }
  // 4) constructor and deconstructor
  class ListIsoCompanionAbs extends CompanionDef[ListIsoCompanionAbs] {
    def selfType = ListIsoCompanionElem
    override def toString = "ListIso"

    @scalan.OverloadId("fromFields")
    def apply[A, B](innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B]): Rep[ListIso[A, B]] =
      mkListIso(innerIso)

    def unapply[A, B](p: Rep[Iso1UR[A, B, List]]) = unmkListIso(p)
  }
  lazy val ListIsoRep: Rep[ListIsoCompanionAbs] = new ListIsoCompanionAbs
  lazy val ListIso: ListIsoCompanionAbs = proxyListIsoCompanion(ListIsoRep)
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
  class ArrayBufferIsoElem[A, B](val iso: Iso[ArrayBufferIsoData[A, B], ArrayBufferIso[A, B]])(implicit override val eA: Elem[A], override val eB: Elem[B])
    extends Iso1URElem[A, B, ArrayBuffer, ArrayBufferIso[A, B]]
    with ConcreteElem[ArrayBufferIsoData[A, B], ArrayBufferIso[A, B]] {
    override lazy val parent: Option[Elem[_]] = Some(iso1URElement(element[A], element[B], container[ArrayBuffer]))
    override lazy val typeArgs = TypeArgs("A" -> eA, "B" -> eB)

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
    def productElement(n: Int) = n match {
      case 0 => eA
      case 1 => eB
    }
  }
  case class ArrayBufferIsoIsoElem[A, B](eA: Elem[A], eB: Elem[B]) extends Elem[ArrayBufferIsoIso[A, B]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new ArrayBufferIsoIso[A, B]()(eA, eB))
    lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[ArrayBufferIsoIso[A, B]]
    }
    lazy val typeArgs = TypeArgs("A" -> eA, "B" -> eB)
  }
  // 4) constructor and deconstructor
  class ArrayBufferIsoCompanionAbs extends CompanionDef[ArrayBufferIsoCompanionAbs] {
    def selfType = ArrayBufferIsoCompanionElem
    override def toString = "ArrayBufferIso"

    @scalan.OverloadId("fromFields")
    def apply[A, B](innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B]): Rep[ArrayBufferIso[A, B]] =
      mkArrayBufferIso(innerIso)

    def unapply[A, B](p: Rep[Iso1UR[A, B, ArrayBuffer]]) = unmkArrayBufferIso(p)
  }
  lazy val ArrayBufferIsoRep: Rep[ArrayBufferIsoCompanionAbs] = new ArrayBufferIsoCompanionAbs
  lazy val ArrayBufferIso: ArrayBufferIsoCompanionAbs = proxyArrayBufferIsoCompanion(ArrayBufferIsoRep)
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
  class ThunkIsoElem[A, B](val iso: Iso[ThunkIsoData[A, B], ThunkIso[A, B]])(implicit override val eA: Elem[A], override val eB: Elem[B])
    extends Iso1URElem[A, B, Thunk, ThunkIso[A, B]]
    with ConcreteElem[ThunkIsoData[A, B], ThunkIso[A, B]] {
    override lazy val parent: Option[Elem[_]] = Some(iso1URElement(element[A], element[B], container[Thunk]))
    override lazy val typeArgs = TypeArgs("A" -> eA, "B" -> eB)

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
    def productElement(n: Int) = n match {
      case 0 => eA
      case 1 => eB
    }
  }
  case class ThunkIsoIsoElem[A, B](eA: Elem[A], eB: Elem[B]) extends Elem[ThunkIsoIso[A, B]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new ThunkIsoIso[A, B]()(eA, eB))
    lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[ThunkIsoIso[A, B]]
    }
    lazy val typeArgs = TypeArgs("A" -> eA, "B" -> eB)
  }
  // 4) constructor and deconstructor
  class ThunkIsoCompanionAbs extends CompanionDef[ThunkIsoCompanionAbs] {
    def selfType = ThunkIsoCompanionElem
    override def toString = "ThunkIso"

    @scalan.OverloadId("fromFields")
    def apply[A, B](innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B]): Rep[ThunkIso[A, B]] =
      mkThunkIso(innerIso)

    def unapply[A, B](p: Rep[Iso1UR[A, B, Thunk]]) = unmkThunkIso(p)
  }
  lazy val ThunkIsoRep: Rep[ThunkIsoCompanionAbs] = new ThunkIsoCompanionAbs
  lazy val ThunkIso: ThunkIsoCompanionAbs = proxyThunkIsoCompanion(ThunkIsoRep)
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

  abstract class AbsMapIso[K1, V1, K2, V2]
      (iso1: Iso[K1, K2], iso2: Iso[V1, V2])(implicit eK1: Elem[K1], eV1: Elem[V1], eK2: Elem[K2], eV2: Elem[V2])
    extends MapIso[K1, V1, K2, V2](iso1, iso2) with Def[MapIso[K1, V1, K2, V2]] {
    lazy val selfType = element[MapIso[K1, V1, K2, V2]]
  }
  // elem for concrete class
  class MapIsoElem[K1, V1, K2, V2](val iso: Iso[MapIsoData[K1, V1, K2, V2], MapIso[K1, V1, K2, V2]])(implicit val eK1: Elem[K1], val eV1: Elem[V1], val eK2: Elem[K2], val eV2: Elem[V2])
    extends IsoURElem[MMap[K1, V1], MMap[K2, V2], MapIso[K1, V1, K2, V2]]
    with ConcreteElem[MapIsoData[K1, V1, K2, V2], MapIso[K1, V1, K2, V2]] {
    override lazy val parent: Option[Elem[_]] = Some(isoURElement(mMapElement(element[K1], element[V1]), mMapElement(element[K2], element[V2])))
    override lazy val typeArgs = TypeArgs("K1" -> eK1, "V1" -> eV1, "K2" -> eK2, "V2" -> eV2)

    override def convertIsoUR(x: Rep[IsoUR[MMap[K1, V1], MMap[K2, V2]]]) = // Converter is not generated by meta
!!!("Cannot convert from IsoUR to MapIso: missing fields List(iso1, iso2)")
    override def getDefaultRep = MapIso(element[IsoUR[K1, K2]].defaultRepValue, element[IsoUR[V1, V2]].defaultRepValue)
    override lazy val tag = {
      implicit val tagK1 = eK1.tag
      implicit val tagV1 = eV1.tag
      implicit val tagK2 = eK2.tag
      implicit val tagV2 = eV2.tag
      weakTypeTag[MapIso[K1, V1, K2, V2]]
    }
  }

  // state representation type
  type MapIsoData[K1, V1, K2, V2] = (IsoUR[K1, K2], IsoUR[V1, V2])

  // 3) Iso for concrete class
  class MapIsoIso[K1, V1, K2, V2](implicit eK1: Elem[K1], eV1: Elem[V1], eK2: Elem[K2], eV2: Elem[V2])
    extends EntityIso[MapIsoData[K1, V1, K2, V2], MapIso[K1, V1, K2, V2]] with Def[MapIsoIso[K1, V1, K2, V2]] {
    override def from(p: Rep[MapIso[K1, V1, K2, V2]]) =
      (p.iso1, p.iso2)
    override def to(p: Rep[(IsoUR[K1, K2], IsoUR[V1, V2])]) = {
      val Pair(iso1, iso2) = p
      MapIso(iso1, iso2)
    }
    lazy val eFrom = pairElement(element[IsoUR[K1, K2]], element[IsoUR[V1, V2]])
    lazy val eTo = new MapIsoElem[K1, V1, K2, V2](self)
    lazy val selfType = new MapIsoIsoElem[K1, V1, K2, V2](eK1, eV1, eK2, eV2)
    def productArity = 4
    def productElement(n: Int) = n match {
      case 0 => eK1
      case 1 => eV1
      case 2 => eK2
      case 3 => eV2
    }
  }
  case class MapIsoIsoElem[K1, V1, K2, V2](eK1: Elem[K1], eV1: Elem[V1], eK2: Elem[K2], eV2: Elem[V2]) extends Elem[MapIsoIso[K1, V1, K2, V2]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new MapIsoIso[K1, V1, K2, V2]()(eK1, eV1, eK2, eV2))
    lazy val tag = {
      implicit val tagK1 = eK1.tag
      implicit val tagV1 = eV1.tag
      implicit val tagK2 = eK2.tag
      implicit val tagV2 = eV2.tag
      weakTypeTag[MapIsoIso[K1, V1, K2, V2]]
    }
    lazy val typeArgs = TypeArgs("K1" -> eK1, "V1" -> eV1, "K2" -> eK2, "V2" -> eV2)
  }
  // 4) constructor and deconstructor
  class MapIsoCompanionAbs extends CompanionDef[MapIsoCompanionAbs] {
    def selfType = MapIsoCompanionElem
    override def toString = "MapIso"
    @scalan.OverloadId("fromData")
    def apply[K1, V1, K2, V2](p: Rep[MapIsoData[K1, V1, K2, V2]])(implicit eK1: Elem[K1], eV1: Elem[V1], eK2: Elem[K2], eV2: Elem[V2]): Rep[MapIso[K1, V1, K2, V2]] =
      isoMapIso(eK1, eV1, eK2, eV2).to(p)
    @scalan.OverloadId("fromFields")
    def apply[K1, V1, K2, V2](iso1: Iso[K1, K2], iso2: Iso[V1, V2])(implicit eK1: Elem[K1], eV1: Elem[V1], eK2: Elem[K2], eV2: Elem[V2]): Rep[MapIso[K1, V1, K2, V2]] =
      mkMapIso(iso1, iso2)

    def unapply[K1, V1, K2, V2](p: Rep[IsoUR[MMap[K1, V1], MMap[K2, V2]]]) = unmkMapIso(p)
  }
  lazy val MapIsoRep: Rep[MapIsoCompanionAbs] = new MapIsoCompanionAbs
  lazy val MapIso: MapIsoCompanionAbs = proxyMapIsoCompanion(MapIsoRep)
  implicit def proxyMapIsoCompanion(p: Rep[MapIsoCompanionAbs]): MapIsoCompanionAbs = {
    proxyOps[MapIsoCompanionAbs](p)
  }

  implicit case object MapIsoCompanionElem extends CompanionElem[MapIsoCompanionAbs] {
    lazy val tag = weakTypeTag[MapIsoCompanionAbs]
    protected def getDefaultRep = MapIso
  }

  implicit def proxyMapIso[K1, V1, K2, V2](p: Rep[MapIso[K1, V1, K2, V2]]): MapIso[K1, V1, K2, V2] =
    proxyOps[MapIso[K1, V1, K2, V2]](p)

  implicit class ExtendedMapIso[K1, V1, K2, V2](p: Rep[MapIso[K1, V1, K2, V2]])(implicit eK1: Elem[K1], eV1: Elem[V1], eK2: Elem[K2], eV2: Elem[V2]) {
    def toData: Rep[MapIsoData[K1, V1, K2, V2]] = isoMapIso(eK1, eV1, eK2, eV2).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoMapIso[K1, V1, K2, V2](implicit eK1: Elem[K1], eV1: Elem[V1], eK2: Elem[K2], eV2: Elem[V2]): Iso[MapIsoData[K1, V1, K2, V2], MapIso[K1, V1, K2, V2]] =
    reifyObject(new MapIsoIso[K1, V1, K2, V2]()(eK1, eV1, eK2, eV2))

  // 6) smart constructor and deconstructor
  def mkMapIso[K1, V1, K2, V2](iso1: Iso[K1, K2], iso2: Iso[V1, V2])(implicit eK1: Elem[K1], eV1: Elem[V1], eK2: Elem[K2], eV2: Elem[V2]): Rep[MapIso[K1, V1, K2, V2]]
  def unmkMapIso[K1, V1, K2, V2](p: Rep[IsoUR[MMap[K1, V1], MMap[K2, V2]]]): Option[(Rep[IsoUR[K1, K2]], Rep[IsoUR[V1, V2]])]

  registerModule(Views_Module)
}

// Std -----------------------------------
trait ViewsStd extends ViewsDsl {
  self: ViewsDsl with ScalanStd =>

  lazy val IsoUR: Rep[IsoURCompanionAbs] = new IsoURCompanionAbs {
  }

  lazy val Iso1UR: Rep[Iso1URCompanionAbs] = new Iso1URCompanionAbs {
  }

  case class StdIdentityIso[A]
      ()(implicit eA: Elem[A])
    extends AbsIdentityIso[A]() {
  }

  def mkIdentityIso[A]
    ()(implicit eA: Elem[A]): Rep[IdentityIso[A]] =
    new StdIdentityIso[A]()
  def unmkIdentityIso[A](p: Rep[IsoUR[A, A]]) = p match {
    case p: IdentityIso[A] @unchecked =>
      Some(())
    case _ => None
  }

  case class StdPairIso[A1, A2, B1, B2]
      (override val iso1: Iso[A1, B1], override val iso2: Iso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends AbsPairIso[A1, A2, B1, B2](iso1, iso2) {
  }

  def mkPairIso[A1, A2, B1, B2]
    (iso1: Iso[A1, B1], iso2: Iso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[PairIso[A1, A2, B1, B2]] =
    new StdPairIso[A1, A2, B1, B2](iso1, iso2)
  def unmkPairIso[A1, A2, B1, B2](p: Rep[IsoUR[(A1, A2), (B1, B2)]]) = p match {
    case p: PairIso[A1, A2, B1, B2] @unchecked =>
      Some((p.iso1, p.iso2))
    case _ => None
  }

  case class StdAbsorbFirstUnitIso[A2, B2]
      (override val iso2: Iso[A2, B2])(implicit eA2: Elem[A2], eB2: Elem[B2])
    extends AbsAbsorbFirstUnitIso[A2, B2](iso2) {
  }

  def mkAbsorbFirstUnitIso[A2, B2]
    (iso2: Iso[A2, B2])(implicit eA2: Elem[A2], eB2: Elem[B2]): Rep[AbsorbFirstUnitIso[A2, B2]] =
    new StdAbsorbFirstUnitIso[A2, B2](iso2)
  def unmkAbsorbFirstUnitIso[A2, B2](p: Rep[IsoUR[A2, (Unit, B2)]]) = p match {
    case p: AbsorbFirstUnitIso[A2, B2] @unchecked =>
      Some((p.iso2))
    case _ => None
  }

  case class StdAbsorbSecondUnitIso[A1, B1]
      (override val iso1: Iso[A1, B1])(implicit eA1: Elem[A1], eB1: Elem[B1])
    extends AbsAbsorbSecondUnitIso[A1, B1](iso1) {
  }

  def mkAbsorbSecondUnitIso[A1, B1]
    (iso1: Iso[A1, B1])(implicit eA1: Elem[A1], eB1: Elem[B1]): Rep[AbsorbSecondUnitIso[A1, B1]] =
    new StdAbsorbSecondUnitIso[A1, B1](iso1)
  def unmkAbsorbSecondUnitIso[A1, B1](p: Rep[IsoUR[A1, (B1, Unit)]]) = p match {
    case p: AbsorbSecondUnitIso[A1, B1] @unchecked =>
      Some((p.iso1))
    case _ => None
  }

  case class StdSumIso[A1, A2, B1, B2]
      (override val iso1: Iso[A1, B1], override val iso2: Iso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends AbsSumIso[A1, A2, B1, B2](iso1, iso2) {
  }

  def mkSumIso[A1, A2, B1, B2]
    (iso1: Iso[A1, B1], iso2: Iso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[SumIso[A1, A2, B1, B2]] =
    new StdSumIso[A1, A2, B1, B2](iso1, iso2)
  def unmkSumIso[A1, A2, B1, B2](p: Rep[IsoUR[$bar[A1, A2], $bar[B1, B2]]]) = p match {
    case p: SumIso[A1, A2, B1, B2] @unchecked =>
      Some((p.iso1, p.iso2))
    case _ => None
  }

  case class StdComposeIso[A, B, C]
      (override val iso2: Iso[B, C], override val iso1: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C])
    extends AbsComposeIso[A, B, C](iso2, iso1) {
  }

  def mkComposeIso[A, B, C]
    (iso2: Iso[B, C], iso1: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]): Rep[ComposeIso[A, B, C]] =
    new StdComposeIso[A, B, C](iso2, iso1)
  def unmkComposeIso[A, B, C](p: Rep[IsoUR[A, C]]) = p match {
    case p: ComposeIso[A, B, C] @unchecked =>
      Some((p.iso2, p.iso1))
    case _ => None
  }

  case class StdFuncIso[A, B, C, D]
      (override val iso1: Iso[A, B], override val iso2: Iso[C, D])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D])
    extends AbsFuncIso[A, B, C, D](iso1, iso2) {
  }

  def mkFuncIso[A, B, C, D]
    (iso1: Iso[A, B], iso2: Iso[C, D])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D]): Rep[FuncIso[A, B, C, D]] =
    new StdFuncIso[A, B, C, D](iso1, iso2)
  def unmkFuncIso[A, B, C, D](p: Rep[IsoUR[A => C, B => D]]) = p match {
    case p: FuncIso[A, B, C, D] @unchecked =>
      Some((p.iso1, p.iso2))
    case _ => None
  }

  case class StdConverterIso[A, B]
      (override val convTo: Conv[A, B], override val convFrom: Conv[B, A])(implicit eA: Elem[A], eB: Elem[B])
    extends AbsConverterIso[A, B](convTo, convFrom) {
  }

  def mkConverterIso[A, B]
    (convTo: Conv[A, B], convFrom: Conv[B, A])(implicit eA: Elem[A], eB: Elem[B]): Rep[ConverterIso[A, B]] =
    new StdConverterIso[A, B](convTo, convFrom)
  def unmkConverterIso[A, B](p: Rep[IsoUR[A, B]]) = p match {
    case p: ConverterIso[A, B] @unchecked =>
      Some((p.convTo, p.convFrom))
    case _ => None
  }

  case class StdArrayIso[A, B]
      (override val innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B])
    extends AbsArrayIso[A, B](innerIso) {
  }

  def mkArrayIso[A, B]
    (innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B]): Rep[ArrayIso[A, B]] =
    new StdArrayIso[A, B](innerIso)
  def unmkArrayIso[A, B](p: Rep[Iso1UR[A, B, Array]]) = p match {
    case p: ArrayIso[A, B] @unchecked =>
      Some((p.innerIso))
    case _ => None
  }

  case class StdListIso[A, B]
      (override val innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B])
    extends AbsListIso[A, B](innerIso) {
  }

  def mkListIso[A, B]
    (innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B]): Rep[ListIso[A, B]] =
    new StdListIso[A, B](innerIso)
  def unmkListIso[A, B](p: Rep[Iso1UR[A, B, List]]) = p match {
    case p: ListIso[A, B] @unchecked =>
      Some((p.innerIso))
    case _ => None
  }

  case class StdArrayBufferIso[A, B]
      (override val innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B])
    extends AbsArrayBufferIso[A, B](innerIso) {
  }

  def mkArrayBufferIso[A, B]
    (innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B]): Rep[ArrayBufferIso[A, B]] =
    new StdArrayBufferIso[A, B](innerIso)
  def unmkArrayBufferIso[A, B](p: Rep[Iso1UR[A, B, ArrayBuffer]]) = p match {
    case p: ArrayBufferIso[A, B] @unchecked =>
      Some((p.innerIso))
    case _ => None
  }

  case class StdThunkIso[A, B]
      (override val innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B])
    extends AbsThunkIso[A, B](innerIso) {
  }

  def mkThunkIso[A, B]
    (innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B]): Rep[ThunkIso[A, B]] =
    new StdThunkIso[A, B](innerIso)
  def unmkThunkIso[A, B](p: Rep[Iso1UR[A, B, Thunk]]) = p match {
    case p: ThunkIso[A, B] @unchecked =>
      Some((p.innerIso))
    case _ => None
  }

  case class StdMapIso[K1, V1, K2, V2]
      (override val iso1: Iso[K1, K2], override val iso2: Iso[V1, V2])(implicit eK1: Elem[K1], eV1: Elem[V1], eK2: Elem[K2], eV2: Elem[V2])
    extends AbsMapIso[K1, V1, K2, V2](iso1, iso2) {
  }

  def mkMapIso[K1, V1, K2, V2]
    (iso1: Iso[K1, K2], iso2: Iso[V1, V2])(implicit eK1: Elem[K1], eV1: Elem[V1], eK2: Elem[K2], eV2: Elem[V2]): Rep[MapIso[K1, V1, K2, V2]] =
    new StdMapIso[K1, V1, K2, V2](iso1, iso2)
  def unmkMapIso[K1, V1, K2, V2](p: Rep[IsoUR[MMap[K1, V1], MMap[K2, V2]]]) = p match {
    case p: MapIso[K1, V1, K2, V2] @unchecked =>
      Some((p.iso1, p.iso2))
    case _ => None
  }
}

// Exp -----------------------------------
trait ViewsExp extends ViewsDsl {
  self: ViewsDsl with ScalanExp =>

  lazy val IsoUR: Rep[IsoURCompanionAbs] = new IsoURCompanionAbs {
  }

  lazy val Iso1UR: Rep[Iso1URCompanionAbs] = new Iso1URCompanionAbs {
  }

  object Iso1URMethods {
    object innerIso {
      def unapply(d: Def[_]): Option[Rep[Iso1UR[A, B, C]] forSome {type A; type B; type C[_]}] = d match {
        case MethodCall(receiver, method, _, _) if (receiver.elem.asInstanceOf[Elem[_]] match { case _: Iso1URElem[_, _, _, _] => true; case _ => false }) && method.getName == "innerIso" =>
          Some(receiver).asInstanceOf[Option[Rep[Iso1UR[A, B, C]] forSome {type A; type B; type C[_]}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Iso1UR[A, B, C]] forSome {type A; type B; type C[_]}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object isIdentity {
      def unapply(d: Def[_]): Option[Rep[Iso1UR[A, B, C]] forSome {type A; type B; type C[_]}] = d match {
        case MethodCall(receiver, method, _, _) if (receiver.elem.asInstanceOf[Elem[_]] match { case _: Iso1URElem[_, _, _, _] => true; case _ => false }) && method.getName == "isIdentity" =>
          Some(receiver).asInstanceOf[Option[Rep[Iso1UR[A, B, C]] forSome {type A; type B; type C[_]}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Iso1UR[A, B, C]] forSome {type A; type B; type C[_]}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `equals`: Overrides Object method
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

  case class ExpAbsorbFirstUnitIso[A2, B2]
      (override val iso2: Iso[A2, B2])(implicit eA2: Elem[A2], eB2: Elem[B2])
    extends AbsAbsorbFirstUnitIso[A2, B2](iso2)

  object AbsorbFirstUnitIsoMethods {
    object from {
      def unapply(d: Def[_]): Option[(Rep[AbsorbFirstUnitIso[A2, B2]], Rep[(Unit, B2)]) forSome {type A2; type B2}] = d match {
        case MethodCall(receiver, method, Seq(b, _*), _) if receiver.elem.isInstanceOf[AbsorbFirstUnitIsoElem[_, _]] && method.getName == "from" =>
          Some((receiver, b)).asInstanceOf[Option[(Rep[AbsorbFirstUnitIso[A2, B2]], Rep[(Unit, B2)]) forSome {type A2; type B2}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbsorbFirstUnitIso[A2, B2]], Rep[(Unit, B2)]) forSome {type A2; type B2}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object to {
      def unapply(d: Def[_]): Option[(Rep[AbsorbFirstUnitIso[A2, B2]], Rep[A2]) forSome {type A2; type B2}] = d match {
        case MethodCall(receiver, method, Seq(a, _*), _) if receiver.elem.isInstanceOf[AbsorbFirstUnitIsoElem[_, _]] && method.getName == "to" =>
          Some((receiver, a)).asInstanceOf[Option[(Rep[AbsorbFirstUnitIso[A2, B2]], Rep[A2]) forSome {type A2; type B2}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbsorbFirstUnitIso[A2, B2]], Rep[A2]) forSome {type A2; type B2}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object isIdentity {
      def unapply(d: Def[_]): Option[Rep[AbsorbFirstUnitIso[A2, B2]] forSome {type A2; type B2}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AbsorbFirstUnitIsoElem[_, _]] && method.getName == "isIdentity" =>
          Some(receiver).asInstanceOf[Option[Rep[AbsorbFirstUnitIso[A2, B2]] forSome {type A2; type B2}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[AbsorbFirstUnitIso[A2, B2]] forSome {type A2; type B2}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `equals`: Overrides Object method
  }

  def mkAbsorbFirstUnitIso[A2, B2]
    (iso2: Iso[A2, B2])(implicit eA2: Elem[A2], eB2: Elem[B2]): Rep[AbsorbFirstUnitIso[A2, B2]] =
    new ExpAbsorbFirstUnitIso[A2, B2](iso2)
  def unmkAbsorbFirstUnitIso[A2, B2](p: Rep[IsoUR[A2, (Unit, B2)]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: AbsorbFirstUnitIsoElem[A2, B2] @unchecked =>
      Some((p.asRep[AbsorbFirstUnitIso[A2, B2]].iso2))
    case _ =>
      None
  }

  case class ExpAbsorbSecondUnitIso[A1, B1]
      (override val iso1: Iso[A1, B1])(implicit eA1: Elem[A1], eB1: Elem[B1])
    extends AbsAbsorbSecondUnitIso[A1, B1](iso1)

  object AbsorbSecondUnitIsoMethods {
    object from {
      def unapply(d: Def[_]): Option[(Rep[AbsorbSecondUnitIso[A1, B1]], Rep[(B1, Unit)]) forSome {type A1; type B1}] = d match {
        case MethodCall(receiver, method, Seq(b, _*), _) if receiver.elem.isInstanceOf[AbsorbSecondUnitIsoElem[_, _]] && method.getName == "from" =>
          Some((receiver, b)).asInstanceOf[Option[(Rep[AbsorbSecondUnitIso[A1, B1]], Rep[(B1, Unit)]) forSome {type A1; type B1}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbsorbSecondUnitIso[A1, B1]], Rep[(B1, Unit)]) forSome {type A1; type B1}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object to {
      def unapply(d: Def[_]): Option[(Rep[AbsorbSecondUnitIso[A1, B1]], Rep[A1]) forSome {type A1; type B1}] = d match {
        case MethodCall(receiver, method, Seq(a, _*), _) if receiver.elem.isInstanceOf[AbsorbSecondUnitIsoElem[_, _]] && method.getName == "to" =>
          Some((receiver, a)).asInstanceOf[Option[(Rep[AbsorbSecondUnitIso[A1, B1]], Rep[A1]) forSome {type A1; type B1}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AbsorbSecondUnitIso[A1, B1]], Rep[A1]) forSome {type A1; type B1}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object isIdentity {
      def unapply(d: Def[_]): Option[Rep[AbsorbSecondUnitIso[A1, B1]] forSome {type A1; type B1}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AbsorbSecondUnitIsoElem[_, _]] && method.getName == "isIdentity" =>
          Some(receiver).asInstanceOf[Option[Rep[AbsorbSecondUnitIso[A1, B1]] forSome {type A1; type B1}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[AbsorbSecondUnitIso[A1, B1]] forSome {type A1; type B1}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `equals`: Overrides Object method
  }

  def mkAbsorbSecondUnitIso[A1, B1]
    (iso1: Iso[A1, B1])(implicit eA1: Elem[A1], eB1: Elem[B1]): Rep[AbsorbSecondUnitIso[A1, B1]] =
    new ExpAbsorbSecondUnitIso[A1, B1](iso1)
  def unmkAbsorbSecondUnitIso[A1, B1](p: Rep[IsoUR[A1, (B1, Unit)]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: AbsorbSecondUnitIsoElem[A1, B1] @unchecked =>
      Some((p.asRep[AbsorbSecondUnitIso[A1, B1]].iso1))
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

  case class ExpMapIso[K1, V1, K2, V2]
      (override val iso1: Iso[K1, K2], override val iso2: Iso[V1, V2])(implicit eK1: Elem[K1], eV1: Elem[V1], eK2: Elem[K2], eV2: Elem[V2])
    extends AbsMapIso[K1, V1, K2, V2](iso1, iso2)

  object MapIsoMethods {
    object from {
      def unapply(d: Def[_]): Option[(Rep[MapIso[K1, V1, K2, V2]], MM[K2, V2]) forSome {type K1; type V1; type K2; type V2}] = d match {
        case MethodCall(receiver, method, Seq(b, _*), _) if receiver.elem.isInstanceOf[MapIsoElem[_, _, _, _]] && method.getName == "from" =>
          Some((receiver, b)).asInstanceOf[Option[(Rep[MapIso[K1, V1, K2, V2]], MM[K2, V2]) forSome {type K1; type V1; type K2; type V2}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[MapIso[K1, V1, K2, V2]], MM[K2, V2]) forSome {type K1; type V1; type K2; type V2}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object to {
      def unapply(d: Def[_]): Option[(Rep[MapIso[K1, V1, K2, V2]], MM[K1, V1]) forSome {type K1; type V1; type K2; type V2}] = d match {
        case MethodCall(receiver, method, Seq(a, _*), _) if receiver.elem.isInstanceOf[MapIsoElem[_, _, _, _]] && method.getName == "to" =>
          Some((receiver, a)).asInstanceOf[Option[(Rep[MapIso[K1, V1, K2, V2]], MM[K1, V1]) forSome {type K1; type V1; type K2; type V2}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[MapIso[K1, V1, K2, V2]], MM[K1, V1]) forSome {type K1; type V1; type K2; type V2}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object isIdentity {
      def unapply(d: Def[_]): Option[Rep[MapIso[K1, V1, K2, V2]] forSome {type K1; type V1; type K2; type V2}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MapIsoElem[_, _, _, _]] && method.getName == "isIdentity" =>
          Some(receiver).asInstanceOf[Option[Rep[MapIso[K1, V1, K2, V2]] forSome {type K1; type V1; type K2; type V2}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[MapIso[K1, V1, K2, V2]] forSome {type K1; type V1; type K2; type V2}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `equals`: Overrides Object method
  }

  def mkMapIso[K1, V1, K2, V2]
    (iso1: Iso[K1, K2], iso2: Iso[V1, V2])(implicit eK1: Elem[K1], eV1: Elem[V1], eK2: Elem[K2], eV2: Elem[V2]): Rep[MapIso[K1, V1, K2, V2]] =
    new ExpMapIso[K1, V1, K2, V2](iso1, iso2)
  def unmkMapIso[K1, V1, K2, V2](p: Rep[IsoUR[MMap[K1, V1], MMap[K2, V2]]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: MapIsoElem[K1, V1, K2, V2] @unchecked =>
      Some((p.asRep[MapIso[K1, V1, K2, V2]].iso1, p.asRep[MapIso[K1, V1, K2, V2]].iso2))
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
  val dump = "H4sIAAAAAAAAAO1aW2gcVRg+u9lbsqGNqVZt6cW61dZLtkmVCkHKXhJNs01DNtlKLC1nd8+m087NmbNx14cqPhSxPokICj4UFF+KIL5ZKIIKIiIovvnkg/hQK6UPFh8Uzzlz2ZnZue0mLbE2D8PO5Mx/+b7v/8+ZM3PpGoirCnhErUEeimMCwnCszH7nVJwpT4mYw+2jUr3JoyJq/PD4F9LQ62/NRcHIMkichmpR5ZfBoPZjqiWbv8u4XgKDUKwhFUuKisFDJeYhW5N4HtUwJ4lZThCaGFZ5lC1xKp4sgVhVqrdfAudApARGapJYUxBG5QIPVRWp+vUUohFx5vkgO28fkzs+xCzNImvJYlGBHCbhEx8j2vgFJJfboiS2BQw26aEdk2lYZEwatWSSw4wg88zNQAkkOUGWFGx4TRIPp6W6cRoTIbkARktn4CrMEq8r2TJWOHGFGpNh7SxcQXNkCB0eIzmoiG8stmWkG0+ruG7z15IBAISVCRbYWAezMROzMYpZpowUDvLcK5D+c16RWm2g/UUGAGjJxMQTASYMC2hKrGfePFF78WY5LUTpzS0aSpIFlCCGdnkohNFDsP1m4W31xnMXD0XB0DIY4tRcVcUKrGGrDHS40lAUJcxiNhGEygphcI8Xg8xLjoxxyGSwJgkyFIklHcthQhTP1ThMB9Nrwzo9HtgnsYyMoZGWHDHz3e2RL9NSAfL8/NUHn9z7+9QLURC1uxgkJsukGBTDKAbxGVVaWtBt0+NmDCK5DsCdU3oYbHWOSZ9QTFAevfpH/esD4ETUhFL3bJqkZtKaFOYkEWWm5zN/lr995xJlWgHD2n+0AviHO/T3z5samImAGbo/nAhIJKPPvP/5XjT/SRSkllnNTPNwhcmBQl5Eam0ZpKRVpGjXk6uQp79cJZGsowZs8lgnyorwAEEYg92e5S4jCv8koZNUgonBdgyiKGfgHZvikeDKiJUCDNIzda1lEAqZFROMHV4KYYraulC6l792+EoUxI+AeINkqZZAvCo1xbohVdLiMGrhvHEtYs+SSBMqUDC41Ap7N2BBsEAdEbOB6Yg9J5vyvAkkMS82ZR49dfmvk2+89rzMlNQlZLvpaG7cpuBobsIZzbjjjrzjjvxEF+JW6VvFR487gYPPGKdK44a1AZJqQIS6f0uEriYn/ExOhEqBFtyQVlZlSUD37LnBnbx4ATNcIy37XHGseob05kl23zY/kvRp7NPz5++7/uGpLazVpqocFqCcOdBDozX64i1spMDSehiNnXMGNGmKI/OQUwjABavn7R7MOyt5AOVM4l1K2WDeyrSLiQlfE11ydjGR943CRW8uJnyjcNEXBkkdOmtDosf9bk3CVgUeIyaCRuQDbeS74PLtRW71ZLQLBTzs3afmFU4gK8BV9PSXl5euX5mLswl4VJ8tKpBvIm3xpQu1I1o6E0f37Sc4L4kcXtcmtB4do0NmkPLXQba9a26UrOgkpTrNkeU8xS+8/ILF1ad0LJr0mGno4cKamO17eumL2bX3tB4aksHsFo3ZMiILknpv1AZ3ha6Yw1Aby1Sh0vNqw+PG/9Gi43YK7T87eSbKTeGOmDudT5AFd1GseQaL5L399FUizsDza1JzDw91XgbyfgbyIQwU/Ax084LBEF31SioKK0QrZO4D8kEDumgLeFbc6b0Gm26KtZ9m3tuyecepX9jyK1GXBMixFfwu8pirkMcatubapT80eqqVnkYEL6UV16LoPgXYX4kUQgV+Jyray0DRz0A3PuShhqrqtteDz4CiM8t+mrJHbwuWcIKsxlYXJRPFAjldq4pT1Oa0Igl+Vh016LEvuQGlbAhpmCaFFIxCPyKHVZOPFhLE13g4MZjyySkKbPerjxQniizDO3GeNahMMYg2No0x+h7lLos+LCYpQhubxDQTWr7ZaCDlLpc+XG6yALWxKY0vnm6KZ++S6UNmikF0G2l03ew5ehQ6Xy3NOvZQKl17AV43OnZKKrd4s8cZ6Gz3vkevmz0VZ+5r3eyZ9d1mmQ2zzVLxNdHNjlsUvjs1Lri5ROFrwgUn0jeIREJv9swGbtRUAkfMBm4HucRpiWwJ2NMaKKJGiBqii3q7bBalYOWbIzSMzS8VQsHl8OmerC0K1wRdH6IKQd3PbZB7/w1IWJ9YQmW8Hg+dgT5OeWcR/a0T5oKqgDGPHZoiqvFQQXX6RRESkKi/Aj747uHjRx44vsReQg/X2SDtP+bHFO7fZ5EammRfE+3z+ZqIDMpMCTJu0x8Hv3r2x1e/+/gj9hFJB3TCdoVDLxN/CS10M5U9HqmU9bfMpArO3fxg7rHvP/uVbTgN0ffVkkhyYyYi9vd8dmGkmM+iytsKJKF5saBNJE1fausxkePAVnrY9i+MQwdnJCcAAA=="
}
}

