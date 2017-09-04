package scalan

import java.lang.reflect.Method
import scala.language.higherKinds
import scala.collection.mutable.{Map=>MutMap}
import scala.reflect.ClassTag
import scalan.common.Lazy
import scalan.meta.ScalanAst.STraitOrClassDef
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait ViewsDefs extends Views {
  self: ViewsDsl with Scalan =>

  // entityProxy: single proxy for each type family
  implicit def proxyIsoUR[From, To](p: Rep[IsoUR[From, To]]): IsoUR[From, To] = {
    proxyOps[IsoUR[From, To]](p)(scala.reflect.classTag[IsoUR[From, To]])
  }

  // familyElem
  class IsoURElem[From, To, To0 <: IsoUR[From, To]](implicit _eFrom: Elem[From], _eTo: Elem[To])
    extends EntityElem[To0] {
    def eFrom = _eFrom
    def eTo = _eTo
    lazy val parent: Option[Elem[_]] = None
    lazy val typeArgs = TypeArgs("From" -> (eFrom -> scalan.util.Invariant), "To" -> (eTo -> scalan.util.Invariant))
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
      x.elem match {
        case _: IsoURElem[_, _, _] => x.asRep[To0]
        case e => !!!(s"Expected $x to have IsoURElem[_, _, _], but got $e", x)
      }
    }

    override def getDefaultRep: Rep[To0] = ???
  }

  implicit def isoURElement[From, To](implicit eFrom: Elem[From], eTo: Elem[To]): Elem[IsoUR[From, To]] =
    cachedElem[IsoURElem[From, To, IsoUR[From, To]]](eFrom, eTo)

  implicit case object IsoURCompanionElem extends CompanionElem[IsoURCompanionCtor] {
    lazy val tag = weakTypeTag[IsoURCompanionCtor]
    protected def getDefaultRep = IsoUR
  }

  abstract class IsoURCompanionCtor extends CompanionDef[IsoURCompanionCtor] {
    def selfType = IsoURCompanionElem
    override def toString = "IsoUR"
  }
  implicit def proxyIsoURCompanionCtor(p: Rep[IsoURCompanionCtor]): IsoURCompanionCtor =
    proxyOps[IsoURCompanionCtor](p)

  // entityProxy: single proxy for each type family
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
    override lazy val typeArgs = TypeArgs("A" -> (eA -> scalan.util.Invariant), "B" -> (eB -> scalan.util.Invariant), "C" -> (cC -> scalan.util.Invariant))
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
      x.elem.asInstanceOf[Elem[_]] match {
        case _: Iso1URElem[_, _, _, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have Iso1URElem[_, _, _, _], but got $e", x)
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def iso1URElement[A, B, C[_]](implicit eA: Elem[A], eB: Elem[B], cC: Cont[C]): Elem[Iso1UR[A, B, C]] =
    cachedElem[Iso1URElem[A, B, C, Iso1UR[A, B, C]]](eA, eB, cC)

  implicit case object Iso1URCompanionElem extends CompanionElem[Iso1URCompanionCtor] {
    lazy val tag = weakTypeTag[Iso1URCompanionCtor]
    protected def getDefaultRep = Iso1UR
  }

  abstract class Iso1URCompanionCtor extends CompanionDef[Iso1URCompanionCtor] {
    def selfType = Iso1URCompanionElem
    override def toString = "Iso1UR"
  }
  implicit def proxyIso1URCompanionCtor(p: Rep[Iso1URCompanionCtor]): Iso1URCompanionCtor =
    proxyOps[Iso1URCompanionCtor](p)

  case class IdentityIsoCtor[A]
      ()(implicit eA: Elem[A])
    extends IdentityIso[A]() with Def[IdentityIso[A]] {
    lazy val selfType = element[IdentityIso[A]]
  }
  // elem for concrete class
  class IdentityIsoElem[A](val iso: Iso[IdentityIsoData[A], IdentityIso[A]])(implicit val eA: Elem[A])
    extends IsoURElem[A, A, IdentityIso[A]]
    with ConcreteElem[IdentityIsoData[A], IdentityIso[A]] {
    override lazy val parent: Option[Elem[_]] = Some(isoURElement(element[A], element[A]))
    override lazy val typeArgs = TypeArgs("A" -> (eA -> scalan.util.Invariant))

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
    def getDefaultRep = reifyObject(new IdentityIsoIso[A]()(eA))
    lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[IdentityIsoIso[A]]
    }
    lazy val typeArgs = TypeArgs("A" -> (eA -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class IdentityIsoCompanionCtor extends CompanionDef[IdentityIsoCompanionCtor] {
    def selfType = IdentityIsoCompanionElem
    override def toString = "IdentityIsoCompanion"
    @scalan.OverloadId("fromData")
    def apply[A](p: Rep[IdentityIsoData[A]])(implicit eA: Elem[A]): Rep[IdentityIso[A]] = {
      isoIdentityIso[A].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[A]()(implicit eA: Elem[A]): Rep[IdentityIso[A]] =
      mkIdentityIso()

    def unapply[A](p: Rep[IsoUR[A, A]]) = unmkIdentityIso(p)
  }
  lazy val IdentityIsoRep: Rep[IdentityIsoCompanionCtor] = new IdentityIsoCompanionCtor
  lazy val IdentityIso: IdentityIsoCompanionCtor = proxyIdentityIsoCompanion(IdentityIsoRep)
  implicit def proxyIdentityIsoCompanion(p: Rep[IdentityIsoCompanionCtor]): IdentityIsoCompanionCtor = {
    proxyOps[IdentityIsoCompanionCtor](p)
  }

  implicit case object IdentityIsoCompanionElem extends CompanionElem[IdentityIsoCompanionCtor] {
    lazy val tag = weakTypeTag[IdentityIsoCompanionCtor]
    protected def getDefaultRep = IdentityIsoRep
  }

  implicit def proxyIdentityIso[A](p: Rep[IdentityIso[A]]): IdentityIso[A] =
    proxyOps[IdentityIso[A]](p)

  implicit class ExtendedIdentityIso[A](p: Rep[IdentityIso[A]])(implicit eA: Elem[A]) {
    def toData: Rep[IdentityIsoData[A]] = isoIdentityIso(eA).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoIdentityIso[A](implicit eA: Elem[A]): Iso[IdentityIsoData[A], IdentityIso[A]] =
    reifyObject(new IdentityIsoIso[A]()(eA))

  case class PairIsoCtor[A1, A2, B1, B2]
      (override val iso1: Iso[A1, B1], override val iso2: Iso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends PairIso[A1, A2, B1, B2](iso1, iso2) with Def[PairIso[A1, A2, B1, B2]] {
    lazy val selfType = element[PairIso[A1, A2, B1, B2]]
  }
  // elem for concrete class
  class PairIsoElem[A1, A2, B1, B2](val iso: Iso[PairIsoData[A1, A2, B1, B2], PairIso[A1, A2, B1, B2]])(implicit val eA1: Elem[A1], val eA2: Elem[A2], val eB1: Elem[B1], val eB2: Elem[B2])
    extends IsoURElem[(A1, A2), (B1, B2), PairIso[A1, A2, B1, B2]]
    with ConcreteElem[PairIsoData[A1, A2, B1, B2], PairIso[A1, A2, B1, B2]] {
    override lazy val parent: Option[Elem[_]] = Some(isoURElement(pairElement(element[A1],element[A2]), pairElement(element[B1],element[B2])))
    override lazy val typeArgs = TypeArgs("A1" -> (eA1 -> scalan.util.Invariant), "A2" -> (eA2 -> scalan.util.Invariant), "B1" -> (eB1 -> scalan.util.Invariant), "B2" -> (eB2 -> scalan.util.Invariant))

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
    def getDefaultRep = reifyObject(new PairIsoIso[A1, A2, B1, B2]()(eA1, eA2, eB1, eB2))
    lazy val tag = {
      implicit val tagA1 = eA1.tag
      implicit val tagA2 = eA2.tag
      implicit val tagB1 = eB1.tag
      implicit val tagB2 = eB2.tag
      weakTypeTag[PairIsoIso[A1, A2, B1, B2]]
    }
    lazy val typeArgs = TypeArgs("A1" -> (eA1 -> scalan.util.Invariant), "A2" -> (eA2 -> scalan.util.Invariant), "B1" -> (eB1 -> scalan.util.Invariant), "B2" -> (eB2 -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class PairIsoCompanionCtor extends CompanionDef[PairIsoCompanionCtor] with PairIsoCompanion {
    def selfType = PairIsoCompanionElem
    override def toString = "PairIsoCompanion"
    @scalan.OverloadId("fromData")
    def apply[A1, A2, B1, B2](p: Rep[PairIsoData[A1, A2, B1, B2]])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[PairIso[A1, A2, B1, B2]] = {
      isoPairIso[A1, A2, B1, B2].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[A1, A2, B1, B2](iso1: Iso[A1, B1], iso2: Iso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[PairIso[A1, A2, B1, B2]] =
      mkPairIso(iso1, iso2)

    def unapply[A1, A2, B1, B2](p: Rep[IsoUR[(A1, A2), (B1, B2)]]) = unmkPairIso(p)
  }
  lazy val PairIsoRep: Rep[PairIsoCompanionCtor] = new PairIsoCompanionCtor
  lazy val PairIso: PairIsoCompanionCtor = proxyPairIsoCompanion(PairIsoRep)
  implicit def proxyPairIsoCompanion(p: Rep[PairIsoCompanionCtor]): PairIsoCompanionCtor = {
    proxyOps[PairIsoCompanionCtor](p)
  }

  implicit case object PairIsoCompanionElem extends CompanionElem[PairIsoCompanionCtor] {
    lazy val tag = weakTypeTag[PairIsoCompanionCtor]
    protected def getDefaultRep = PairIsoRep
  }

  implicit def proxyPairIso[A1, A2, B1, B2](p: Rep[PairIso[A1, A2, B1, B2]]): PairIso[A1, A2, B1, B2] =
    proxyOps[PairIso[A1, A2, B1, B2]](p)

  implicit class ExtendedPairIso[A1, A2, B1, B2](p: Rep[PairIso[A1, A2, B1, B2]])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]) {
    def toData: Rep[PairIsoData[A1, A2, B1, B2]] = isoPairIso(eA1, eA2, eB1, eB2).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoPairIso[A1, A2, B1, B2](implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Iso[PairIsoData[A1, A2, B1, B2], PairIso[A1, A2, B1, B2]] =
    reifyObject(new PairIsoIso[A1, A2, B1, B2]()(eA1, eA2, eB1, eB2))

  case class AbsorbFirstUnitIsoCtor[A2, B2]
      (override val iso2: Iso[A2, B2])(implicit eA2: Elem[A2], eB2: Elem[B2])
    extends AbsorbFirstUnitIso[A2, B2](iso2) with Def[AbsorbFirstUnitIso[A2, B2]] {
    lazy val selfType = element[AbsorbFirstUnitIso[A2, B2]]
  }
  // elem for concrete class
  class AbsorbFirstUnitIsoElem[A2, B2](val iso: Iso[AbsorbFirstUnitIsoData[A2, B2], AbsorbFirstUnitIso[A2, B2]])(implicit val eA2: Elem[A2], val eB2: Elem[B2])
    extends IsoURElem[A2, (Unit, B2), AbsorbFirstUnitIso[A2, B2]]
    with ConcreteElem[AbsorbFirstUnitIsoData[A2, B2], AbsorbFirstUnitIso[A2, B2]] {
    override lazy val parent: Option[Elem[_]] = Some(isoURElement(element[A2], pairElement(UnitElement,element[B2])))
    override lazy val typeArgs = TypeArgs("A2" -> (eA2 -> scalan.util.Invariant), "B2" -> (eB2 -> scalan.util.Invariant))

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
    def getDefaultRep = reifyObject(new AbsorbFirstUnitIsoIso[A2, B2]()(eA2, eB2))
    lazy val tag = {
      implicit val tagA2 = eA2.tag
      implicit val tagB2 = eB2.tag
      weakTypeTag[AbsorbFirstUnitIsoIso[A2, B2]]
    }
    lazy val typeArgs = TypeArgs("A2" -> (eA2 -> scalan.util.Invariant), "B2" -> (eB2 -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class AbsorbFirstUnitIsoCompanionCtor extends CompanionDef[AbsorbFirstUnitIsoCompanionCtor] {
    def selfType = AbsorbFirstUnitIsoCompanionElem
    override def toString = "AbsorbFirstUnitIsoCompanion"

    @scalan.OverloadId("fromFields")
    def apply[A2, B2](iso2: Iso[A2, B2])(implicit eA2: Elem[A2], eB2: Elem[B2]): Rep[AbsorbFirstUnitIso[A2, B2]] =
      mkAbsorbFirstUnitIso(iso2)

    def unapply[A2, B2](p: Rep[IsoUR[A2, (Unit, B2)]]) = unmkAbsorbFirstUnitIso(p)
  }
  lazy val AbsorbFirstUnitIsoRep: Rep[AbsorbFirstUnitIsoCompanionCtor] = new AbsorbFirstUnitIsoCompanionCtor
  lazy val AbsorbFirstUnitIso: AbsorbFirstUnitIsoCompanionCtor = proxyAbsorbFirstUnitIsoCompanion(AbsorbFirstUnitIsoRep)
  implicit def proxyAbsorbFirstUnitIsoCompanion(p: Rep[AbsorbFirstUnitIsoCompanionCtor]): AbsorbFirstUnitIsoCompanionCtor = {
    proxyOps[AbsorbFirstUnitIsoCompanionCtor](p)
  }

  implicit case object AbsorbFirstUnitIsoCompanionElem extends CompanionElem[AbsorbFirstUnitIsoCompanionCtor] {
    lazy val tag = weakTypeTag[AbsorbFirstUnitIsoCompanionCtor]
    protected def getDefaultRep = AbsorbFirstUnitIsoRep
  }

  implicit def proxyAbsorbFirstUnitIso[A2, B2](p: Rep[AbsorbFirstUnitIso[A2, B2]]): AbsorbFirstUnitIso[A2, B2] =
    proxyOps[AbsorbFirstUnitIso[A2, B2]](p)

  implicit class ExtendedAbsorbFirstUnitIso[A2, B2](p: Rep[AbsorbFirstUnitIso[A2, B2]])(implicit eA2: Elem[A2], eB2: Elem[B2]) {
    def toData: Rep[AbsorbFirstUnitIsoData[A2, B2]] = isoAbsorbFirstUnitIso(eA2, eB2).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoAbsorbFirstUnitIso[A2, B2](implicit eA2: Elem[A2], eB2: Elem[B2]): Iso[AbsorbFirstUnitIsoData[A2, B2], AbsorbFirstUnitIso[A2, B2]] =
    reifyObject(new AbsorbFirstUnitIsoIso[A2, B2]()(eA2, eB2))

  case class AbsorbSecondUnitIsoCtor[A1, B1]
      (override val iso1: Iso[A1, B1])(implicit eA1: Elem[A1], eB1: Elem[B1])
    extends AbsorbSecondUnitIso[A1, B1](iso1) with Def[AbsorbSecondUnitIso[A1, B1]] {
    lazy val selfType = element[AbsorbSecondUnitIso[A1, B1]]
  }
  // elem for concrete class
  class AbsorbSecondUnitIsoElem[A1, B1](val iso: Iso[AbsorbSecondUnitIsoData[A1, B1], AbsorbSecondUnitIso[A1, B1]])(implicit val eA1: Elem[A1], val eB1: Elem[B1])
    extends IsoURElem[A1, (B1, Unit), AbsorbSecondUnitIso[A1, B1]]
    with ConcreteElem[AbsorbSecondUnitIsoData[A1, B1], AbsorbSecondUnitIso[A1, B1]] {
    override lazy val parent: Option[Elem[_]] = Some(isoURElement(element[A1], pairElement(element[B1],UnitElement)))
    override lazy val typeArgs = TypeArgs("A1" -> (eA1 -> scalan.util.Invariant), "B1" -> (eB1 -> scalan.util.Invariant))

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
    def getDefaultRep = reifyObject(new AbsorbSecondUnitIsoIso[A1, B1]()(eA1, eB1))
    lazy val tag = {
      implicit val tagA1 = eA1.tag
      implicit val tagB1 = eB1.tag
      weakTypeTag[AbsorbSecondUnitIsoIso[A1, B1]]
    }
    lazy val typeArgs = TypeArgs("A1" -> (eA1 -> scalan.util.Invariant), "B1" -> (eB1 -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class AbsorbSecondUnitIsoCompanionCtor extends CompanionDef[AbsorbSecondUnitIsoCompanionCtor] {
    def selfType = AbsorbSecondUnitIsoCompanionElem
    override def toString = "AbsorbSecondUnitIsoCompanion"

    @scalan.OverloadId("fromFields")
    def apply[A1, B1](iso1: Iso[A1, B1])(implicit eA1: Elem[A1], eB1: Elem[B1]): Rep[AbsorbSecondUnitIso[A1, B1]] =
      mkAbsorbSecondUnitIso(iso1)

    def unapply[A1, B1](p: Rep[IsoUR[A1, (B1, Unit)]]) = unmkAbsorbSecondUnitIso(p)
  }
  lazy val AbsorbSecondUnitIsoRep: Rep[AbsorbSecondUnitIsoCompanionCtor] = new AbsorbSecondUnitIsoCompanionCtor
  lazy val AbsorbSecondUnitIso: AbsorbSecondUnitIsoCompanionCtor = proxyAbsorbSecondUnitIsoCompanion(AbsorbSecondUnitIsoRep)
  implicit def proxyAbsorbSecondUnitIsoCompanion(p: Rep[AbsorbSecondUnitIsoCompanionCtor]): AbsorbSecondUnitIsoCompanionCtor = {
    proxyOps[AbsorbSecondUnitIsoCompanionCtor](p)
  }

  implicit case object AbsorbSecondUnitIsoCompanionElem extends CompanionElem[AbsorbSecondUnitIsoCompanionCtor] {
    lazy val tag = weakTypeTag[AbsorbSecondUnitIsoCompanionCtor]
    protected def getDefaultRep = AbsorbSecondUnitIsoRep
  }

  implicit def proxyAbsorbSecondUnitIso[A1, B1](p: Rep[AbsorbSecondUnitIso[A1, B1]]): AbsorbSecondUnitIso[A1, B1] =
    proxyOps[AbsorbSecondUnitIso[A1, B1]](p)

  implicit class ExtendedAbsorbSecondUnitIso[A1, B1](p: Rep[AbsorbSecondUnitIso[A1, B1]])(implicit eA1: Elem[A1], eB1: Elem[B1]) {
    def toData: Rep[AbsorbSecondUnitIsoData[A1, B1]] = isoAbsorbSecondUnitIso(eA1, eB1).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoAbsorbSecondUnitIso[A1, B1](implicit eA1: Elem[A1], eB1: Elem[B1]): Iso[AbsorbSecondUnitIsoData[A1, B1], AbsorbSecondUnitIso[A1, B1]] =
    reifyObject(new AbsorbSecondUnitIsoIso[A1, B1]()(eA1, eB1))

  case class SumIsoCtor[A1, A2, B1, B2]
      (override val iso1: Iso[A1, B1], override val iso2: Iso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends SumIso[A1, A2, B1, B2](iso1, iso2) with Def[SumIso[A1, A2, B1, B2]] {
    lazy val selfType = element[SumIso[A1, A2, B1, B2]]
  }
  // elem for concrete class
  class SumIsoElem[A1, A2, B1, B2](val iso: Iso[SumIsoData[A1, A2, B1, B2], SumIso[A1, A2, B1, B2]])(implicit val eA1: Elem[A1], val eA2: Elem[A2], val eB1: Elem[B1], val eB2: Elem[B2])
    extends IsoURElem[$bar[A1, A2], $bar[B1, B2], SumIso[A1, A2, B1, B2]]
    with ConcreteElem[SumIsoData[A1, A2, B1, B2], SumIso[A1, A2, B1, B2]] {
    override lazy val parent: Option[Elem[_]] = Some(isoURElement(sumElement(element[A1],element[A2]), sumElement(element[B1],element[B2])))
    override lazy val typeArgs = TypeArgs("A1" -> (eA1 -> scalan.util.Invariant), "A2" -> (eA2 -> scalan.util.Invariant), "B1" -> (eB1 -> scalan.util.Invariant), "B2" -> (eB2 -> scalan.util.Invariant))

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
    def getDefaultRep = reifyObject(new SumIsoIso[A1, A2, B1, B2]()(eA1, eA2, eB1, eB2))
    lazy val tag = {
      implicit val tagA1 = eA1.tag
      implicit val tagA2 = eA2.tag
      implicit val tagB1 = eB1.tag
      implicit val tagB2 = eB2.tag
      weakTypeTag[SumIsoIso[A1, A2, B1, B2]]
    }
    lazy val typeArgs = TypeArgs("A1" -> (eA1 -> scalan.util.Invariant), "A2" -> (eA2 -> scalan.util.Invariant), "B1" -> (eB1 -> scalan.util.Invariant), "B2" -> (eB2 -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class SumIsoCompanionCtor extends CompanionDef[SumIsoCompanionCtor] {
    def selfType = SumIsoCompanionElem
    override def toString = "SumIsoCompanion"
    @scalan.OverloadId("fromData")
    def apply[A1, A2, B1, B2](p: Rep[SumIsoData[A1, A2, B1, B2]])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[SumIso[A1, A2, B1, B2]] = {
      isoSumIso[A1, A2, B1, B2].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[A1, A2, B1, B2](iso1: Iso[A1, B1], iso2: Iso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[SumIso[A1, A2, B1, B2]] =
      mkSumIso(iso1, iso2)

    def unapply[A1, A2, B1, B2](p: Rep[IsoUR[$bar[A1, A2], $bar[B1, B2]]]) = unmkSumIso(p)
  }
  lazy val SumIsoRep: Rep[SumIsoCompanionCtor] = new SumIsoCompanionCtor
  lazy val SumIso: SumIsoCompanionCtor = proxySumIsoCompanion(SumIsoRep)
  implicit def proxySumIsoCompanion(p: Rep[SumIsoCompanionCtor]): SumIsoCompanionCtor = {
    proxyOps[SumIsoCompanionCtor](p)
  }

  implicit case object SumIsoCompanionElem extends CompanionElem[SumIsoCompanionCtor] {
    lazy val tag = weakTypeTag[SumIsoCompanionCtor]
    protected def getDefaultRep = SumIsoRep
  }

  implicit def proxySumIso[A1, A2, B1, B2](p: Rep[SumIso[A1, A2, B1, B2]]): SumIso[A1, A2, B1, B2] =
    proxyOps[SumIso[A1, A2, B1, B2]](p)

  implicit class ExtendedSumIso[A1, A2, B1, B2](p: Rep[SumIso[A1, A2, B1, B2]])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]) {
    def toData: Rep[SumIsoData[A1, A2, B1, B2]] = isoSumIso(eA1, eA2, eB1, eB2).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoSumIso[A1, A2, B1, B2](implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Iso[SumIsoData[A1, A2, B1, B2], SumIso[A1, A2, B1, B2]] =
    reifyObject(new SumIsoIso[A1, A2, B1, B2]()(eA1, eA2, eB1, eB2))

  case class ComposeIsoCtor[A, B, C]
      (override val iso2: Iso[B, C], override val iso1: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C])
    extends ComposeIso[A, B, C](iso2, iso1) with Def[ComposeIso[A, B, C]] {
    lazy val selfType = element[ComposeIso[A, B, C]]
  }
  // elem for concrete class
  class ComposeIsoElem[A, B, C](val iso: Iso[ComposeIsoData[A, B, C], ComposeIso[A, B, C]])(implicit val eA: Elem[A], val eB: Elem[B], val eC: Elem[C])
    extends IsoURElem[A, C, ComposeIso[A, B, C]]
    with ConcreteElem[ComposeIsoData[A, B, C], ComposeIso[A, B, C]] {
    override lazy val parent: Option[Elem[_]] = Some(isoURElement(element[A], element[C]))
    override lazy val typeArgs = TypeArgs("A" -> (eA -> scalan.util.Invariant), "B" -> (eB -> scalan.util.Invariant), "C" -> (eC -> scalan.util.Invariant))

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
    def getDefaultRep = reifyObject(new ComposeIsoIso[A, B, C]()(eA, eB, eC))
    lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      implicit val tagC = eC.tag
      weakTypeTag[ComposeIsoIso[A, B, C]]
    }
    lazy val typeArgs = TypeArgs("A" -> (eA -> scalan.util.Invariant), "B" -> (eB -> scalan.util.Invariant), "C" -> (eC -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class ComposeIsoCompanionCtor extends CompanionDef[ComposeIsoCompanionCtor] {
    def selfType = ComposeIsoCompanionElem
    override def toString = "ComposeIsoCompanion"
    @scalan.OverloadId("fromData")
    def apply[A, B, C](p: Rep[ComposeIsoData[A, B, C]])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]): Rep[ComposeIso[A, B, C]] = {
      isoComposeIso[A, B, C].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[A, B, C](iso2: Iso[B, C], iso1: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]): Rep[ComposeIso[A, B, C]] =
      mkComposeIso(iso2, iso1)

    def unapply[A, B, C](p: Rep[IsoUR[A, C]]) = unmkComposeIso(p)
  }
  lazy val ComposeIsoRep: Rep[ComposeIsoCompanionCtor] = new ComposeIsoCompanionCtor
  lazy val ComposeIso: ComposeIsoCompanionCtor = proxyComposeIsoCompanion(ComposeIsoRep)
  implicit def proxyComposeIsoCompanion(p: Rep[ComposeIsoCompanionCtor]): ComposeIsoCompanionCtor = {
    proxyOps[ComposeIsoCompanionCtor](p)
  }

  implicit case object ComposeIsoCompanionElem extends CompanionElem[ComposeIsoCompanionCtor] {
    lazy val tag = weakTypeTag[ComposeIsoCompanionCtor]
    protected def getDefaultRep = ComposeIsoRep
  }

  implicit def proxyComposeIso[A, B, C](p: Rep[ComposeIso[A, B, C]]): ComposeIso[A, B, C] =
    proxyOps[ComposeIso[A, B, C]](p)

  implicit class ExtendedComposeIso[A, B, C](p: Rep[ComposeIso[A, B, C]])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]) {
    def toData: Rep[ComposeIsoData[A, B, C]] = isoComposeIso(eA, eB, eC).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoComposeIso[A, B, C](implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]): Iso[ComposeIsoData[A, B, C], ComposeIso[A, B, C]] =
    reifyObject(new ComposeIsoIso[A, B, C]()(eA, eB, eC))

  case class FuncIsoCtor[A, B, C, D]
      (override val iso1: Iso[A, B], override val iso2: Iso[C, D])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D])
    extends FuncIso[A, B, C, D](iso1, iso2) with Def[FuncIso[A, B, C, D]] {
    lazy val selfType = element[FuncIso[A, B, C, D]]
  }
  // elem for concrete class
  class FuncIsoElem[A, B, C, D](val iso: Iso[FuncIsoData[A, B, C, D], FuncIso[A, B, C, D]])(implicit val eA: Elem[A], val eB: Elem[B], val eC: Elem[C], val eD: Elem[D])
    extends IsoURElem[A => C, B => D, FuncIso[A, B, C, D]]
    with ConcreteElem[FuncIsoData[A, B, C, D], FuncIso[A, B, C, D]] {
    override lazy val parent: Option[Elem[_]] = Some(isoURElement(funcElement(element[A],element[C]), funcElement(element[B],element[D])))
    override lazy val typeArgs = TypeArgs("A" -> (eA -> scalan.util.Invariant), "B" -> (eB -> scalan.util.Invariant), "C" -> (eC -> scalan.util.Invariant), "D" -> (eD -> scalan.util.Invariant))

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
    def getDefaultRep = reifyObject(new FuncIsoIso[A, B, C, D]()(eA, eB, eC, eD))
    lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      implicit val tagC = eC.tag
      implicit val tagD = eD.tag
      weakTypeTag[FuncIsoIso[A, B, C, D]]
    }
    lazy val typeArgs = TypeArgs("A" -> (eA -> scalan.util.Invariant), "B" -> (eB -> scalan.util.Invariant), "C" -> (eC -> scalan.util.Invariant), "D" -> (eD -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class FuncIsoCompanionCtor extends CompanionDef[FuncIsoCompanionCtor] {
    def selfType = FuncIsoCompanionElem
    override def toString = "FuncIsoCompanion"
    @scalan.OverloadId("fromData")
    def apply[A, B, C, D](p: Rep[FuncIsoData[A, B, C, D]])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D]): Rep[FuncIso[A, B, C, D]] = {
      isoFuncIso[A, B, C, D].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[A, B, C, D](iso1: Iso[A, B], iso2: Iso[C, D])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D]): Rep[FuncIso[A, B, C, D]] =
      mkFuncIso(iso1, iso2)

    def unapply[A, B, C, D](p: Rep[IsoUR[A => C, B => D]]) = unmkFuncIso(p)
  }
  lazy val FuncIsoRep: Rep[FuncIsoCompanionCtor] = new FuncIsoCompanionCtor
  lazy val FuncIso: FuncIsoCompanionCtor = proxyFuncIsoCompanion(FuncIsoRep)
  implicit def proxyFuncIsoCompanion(p: Rep[FuncIsoCompanionCtor]): FuncIsoCompanionCtor = {
    proxyOps[FuncIsoCompanionCtor](p)
  }

  implicit case object FuncIsoCompanionElem extends CompanionElem[FuncIsoCompanionCtor] {
    lazy val tag = weakTypeTag[FuncIsoCompanionCtor]
    protected def getDefaultRep = FuncIsoRep
  }

  implicit def proxyFuncIso[A, B, C, D](p: Rep[FuncIso[A, B, C, D]]): FuncIso[A, B, C, D] =
    proxyOps[FuncIso[A, B, C, D]](p)

  implicit class ExtendedFuncIso[A, B, C, D](p: Rep[FuncIso[A, B, C, D]])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D]) {
    def toData: Rep[FuncIsoData[A, B, C, D]] = isoFuncIso(eA, eB, eC, eD).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoFuncIso[A, B, C, D](implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D]): Iso[FuncIsoData[A, B, C, D], FuncIso[A, B, C, D]] =
    reifyObject(new FuncIsoIso[A, B, C, D]()(eA, eB, eC, eD))

  case class ConverterIsoCtor[A, B]
      (override val convTo: Conv[A, B], override val convFrom: Conv[B, A])(implicit eA: Elem[A], eB: Elem[B])
    extends ConverterIso[A, B](convTo, convFrom) with Def[ConverterIso[A, B]] {
    lazy val selfType = element[ConverterIso[A, B]]
  }
  // elem for concrete class
  class ConverterIsoElem[A, B](val iso: Iso[ConverterIsoData[A, B], ConverterIso[A, B]])(implicit val eA: Elem[A], val eB: Elem[B])
    extends IsoURElem[A, B, ConverterIso[A, B]]
    with ConcreteElem[ConverterIsoData[A, B], ConverterIso[A, B]] {
    override lazy val parent: Option[Elem[_]] = Some(isoURElement(element[A], element[B]))
    override lazy val typeArgs = TypeArgs("A" -> (eA -> scalan.util.Invariant), "B" -> (eB -> scalan.util.Invariant))

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
    def getDefaultRep = reifyObject(new ConverterIsoIso[A, B]()(eA, eB))
    lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[ConverterIsoIso[A, B]]
    }
    lazy val typeArgs = TypeArgs("A" -> (eA -> scalan.util.Invariant), "B" -> (eB -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class ConverterIsoCompanionCtor extends CompanionDef[ConverterIsoCompanionCtor] {
    def selfType = ConverterIsoCompanionElem
    override def toString = "ConverterIsoCompanion"
    @scalan.OverloadId("fromData")
    def apply[A, B](p: Rep[ConverterIsoData[A, B]])(implicit eA: Elem[A], eB: Elem[B]): Rep[ConverterIso[A, B]] = {
      isoConverterIso[A, B].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[A, B](convTo: Conv[A, B], convFrom: Conv[B, A])(implicit eA: Elem[A], eB: Elem[B]): Rep[ConverterIso[A, B]] =
      mkConverterIso(convTo, convFrom)

    def unapply[A, B](p: Rep[IsoUR[A, B]]) = unmkConverterIso(p)
  }
  lazy val ConverterIsoRep: Rep[ConverterIsoCompanionCtor] = new ConverterIsoCompanionCtor
  lazy val ConverterIso: ConverterIsoCompanionCtor = proxyConverterIsoCompanion(ConverterIsoRep)
  implicit def proxyConverterIsoCompanion(p: Rep[ConverterIsoCompanionCtor]): ConverterIsoCompanionCtor = {
    proxyOps[ConverterIsoCompanionCtor](p)
  }

  implicit case object ConverterIsoCompanionElem extends CompanionElem[ConverterIsoCompanionCtor] {
    lazy val tag = weakTypeTag[ConverterIsoCompanionCtor]
    protected def getDefaultRep = ConverterIsoRep
  }

  implicit def proxyConverterIso[A, B](p: Rep[ConverterIso[A, B]]): ConverterIso[A, B] =
    proxyOps[ConverterIso[A, B]](p)

  implicit class ExtendedConverterIso[A, B](p: Rep[ConverterIso[A, B]])(implicit eA: Elem[A], eB: Elem[B]) {
    def toData: Rep[ConverterIsoData[A, B]] = isoConverterIso(eA, eB).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoConverterIso[A, B](implicit eA: Elem[A], eB: Elem[B]): Iso[ConverterIsoData[A, B], ConverterIso[A, B]] =
    reifyObject(new ConverterIsoIso[A, B]()(eA, eB))

  case class ThunkIsoCtor[A, B]
      (override val innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B])
    extends ThunkIso[A, B](innerIso) with Def[ThunkIso[A, B]] {
    lazy val selfType = element[ThunkIso[A, B]]
  }
  // elem for concrete class
  class ThunkIsoElem[A, B](val iso: Iso[ThunkIsoData[A, B], ThunkIso[A, B]])(implicit override val eA: Elem[A], override val eB: Elem[B])
    extends Iso1URElem[A, B, Thunk, ThunkIso[A, B]]
    with ConcreteElem[ThunkIsoData[A, B], ThunkIso[A, B]] {
    override lazy val parent: Option[Elem[_]] = Some(iso1URElement(element[A], element[B], container[Thunk]))
    override lazy val typeArgs = TypeArgs("A" -> (eA -> scalan.util.Invariant), "B" -> (eB -> scalan.util.Invariant))

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
    def getDefaultRep = reifyObject(new ThunkIsoIso[A, B]()(eA, eB))
    lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[ThunkIsoIso[A, B]]
    }
    lazy val typeArgs = TypeArgs("A" -> (eA -> scalan.util.Invariant), "B" -> (eB -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class ThunkIsoCompanionCtor extends CompanionDef[ThunkIsoCompanionCtor] {
    def selfType = ThunkIsoCompanionElem
    override def toString = "ThunkIsoCompanion"

    @scalan.OverloadId("fromFields")
    def apply[A, B](innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B]): Rep[ThunkIso[A, B]] =
      mkThunkIso(innerIso)

    def unapply[A, B](p: Rep[Iso1UR[A, B, Thunk]]) = unmkThunkIso(p)
  }
  lazy val ThunkIsoRep: Rep[ThunkIsoCompanionCtor] = new ThunkIsoCompanionCtor
  lazy val ThunkIso: ThunkIsoCompanionCtor = proxyThunkIsoCompanion(ThunkIsoRep)
  implicit def proxyThunkIsoCompanion(p: Rep[ThunkIsoCompanionCtor]): ThunkIsoCompanionCtor = {
    proxyOps[ThunkIsoCompanionCtor](p)
  }

  implicit case object ThunkIsoCompanionElem extends CompanionElem[ThunkIsoCompanionCtor] {
    lazy val tag = weakTypeTag[ThunkIsoCompanionCtor]
    protected def getDefaultRep = ThunkIsoRep
  }

  implicit def proxyThunkIso[A, B](p: Rep[ThunkIso[A, B]]): ThunkIso[A, B] =
    proxyOps[ThunkIso[A, B]](p)

  implicit class ExtendedThunkIso[A, B](p: Rep[ThunkIso[A, B]])(implicit eA: Elem[A], eB: Elem[B]) {
    def toData: Rep[ThunkIsoData[A, B]] = isoThunkIso(eA, eB).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoThunkIso[A, B](implicit eA: Elem[A], eB: Elem[B]): Iso[ThunkIsoData[A, B], ThunkIso[A, B]] =
    reifyObject(new ThunkIsoIso[A, B]()(eA, eB))

  registerModule(Views_Module)

  lazy val IsoUR: Rep[IsoURCompanionCtor] = new IsoURCompanionCtor {
  }

  lazy val Iso1UR: Rep[Iso1URCompanionCtor] = new Iso1URCompanionCtor {
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
    ()(implicit eA: Elem[A]): Rep[IdentityIso[A]] = {
    new IdentityIsoCtor[A]()
  }
  def unmkIdentityIso[A](p: Rep[IsoUR[A, A]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: IdentityIsoElem[A] @unchecked =>
      Some(())
    case _ =>
      None
  }

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
    (iso1: Iso[A1, B1], iso2: Iso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[PairIso[A1, A2, B1, B2]] = {
    new PairIsoCtor[A1, A2, B1, B2](iso1, iso2)
  }
  def unmkPairIso[A1, A2, B1, B2](p: Rep[IsoUR[(A1, A2), (B1, B2)]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: PairIsoElem[A1, A2, B1, B2] @unchecked =>
      Some((p.asRep[PairIso[A1, A2, B1, B2]].iso1, p.asRep[PairIso[A1, A2, B1, B2]].iso2))
    case _ =>
      None
  }

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
    (iso2: Iso[A2, B2])(implicit eA2: Elem[A2], eB2: Elem[B2]): Rep[AbsorbFirstUnitIso[A2, B2]] = {
    new AbsorbFirstUnitIsoCtor[A2, B2](iso2)
  }
  def unmkAbsorbFirstUnitIso[A2, B2](p: Rep[IsoUR[A2, (Unit, B2)]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: AbsorbFirstUnitIsoElem[A2, B2] @unchecked =>
      Some((p.asRep[AbsorbFirstUnitIso[A2, B2]].iso2))
    case _ =>
      None
  }

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
    (iso1: Iso[A1, B1])(implicit eA1: Elem[A1], eB1: Elem[B1]): Rep[AbsorbSecondUnitIso[A1, B1]] = {
    new AbsorbSecondUnitIsoCtor[A1, B1](iso1)
  }
  def unmkAbsorbSecondUnitIso[A1, B1](p: Rep[IsoUR[A1, (B1, Unit)]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: AbsorbSecondUnitIsoElem[A1, B1] @unchecked =>
      Some((p.asRep[AbsorbSecondUnitIso[A1, B1]].iso1))
    case _ =>
      None
  }

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
    (iso1: Iso[A1, B1], iso2: Iso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[SumIso[A1, A2, B1, B2]] = {
    new SumIsoCtor[A1, A2, B1, B2](iso1, iso2)
  }
  def unmkSumIso[A1, A2, B1, B2](p: Rep[IsoUR[$bar[A1, A2], $bar[B1, B2]]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: SumIsoElem[A1, A2, B1, B2] @unchecked =>
      Some((p.asRep[SumIso[A1, A2, B1, B2]].iso1, p.asRep[SumIso[A1, A2, B1, B2]].iso2))
    case _ =>
      None
  }

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
    (iso2: Iso[B, C], iso1: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]): Rep[ComposeIso[A, B, C]] = {
    new ComposeIsoCtor[A, B, C](iso2, iso1)
  }
  def unmkComposeIso[A, B, C](p: Rep[IsoUR[A, C]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ComposeIsoElem[A, B, C] @unchecked =>
      Some((p.asRep[ComposeIso[A, B, C]].iso2, p.asRep[ComposeIso[A, B, C]].iso1))
    case _ =>
      None
  }

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
    (iso1: Iso[A, B], iso2: Iso[C, D])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D]): Rep[FuncIso[A, B, C, D]] = {
    new FuncIsoCtor[A, B, C, D](iso1, iso2)
  }
  def unmkFuncIso[A, B, C, D](p: Rep[IsoUR[A => C, B => D]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: FuncIsoElem[A, B, C, D] @unchecked =>
      Some((p.asRep[FuncIso[A, B, C, D]].iso1, p.asRep[FuncIso[A, B, C, D]].iso2))
    case _ =>
      None
  }

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
    (convTo: Conv[A, B], convFrom: Conv[B, A])(implicit eA: Elem[A], eB: Elem[B]): Rep[ConverterIso[A, B]] = {
    new ConverterIsoCtor[A, B](convTo, convFrom)
  }
  def unmkConverterIso[A, B](p: Rep[IsoUR[A, B]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ConverterIsoElem[A, B] @unchecked =>
      Some((p.asRep[ConverterIso[A, B]].convTo, p.asRep[ConverterIso[A, B]].convFrom))
    case _ =>
      None
  }

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
    (innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B]): Rep[ThunkIso[A, B]] = {
    new ThunkIsoCtor[A, B](innerIso)
  }
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
  val dump = "H4sIAAAAAAAAAOVZS4wURRiu6Z3XPsRlN5I18lhxiIA4A4MEk40x88TFYXdD7y6yEkjNdO3S0C+6a5YZD+hFYvRgYoyJJh6IGi/ExHgxknhRE2MMB+PNkwcPBiSEA8SDxqrqx/TMdvf07IAPnEOnu6f6+x/f9/9V1X35NxAzdDBp1KAElbSMMEzz7Dxn4BR/RBXqEiqi5dnrr2799YM9tzmwcQnET0OjaEhLYNA8KTU055zHQgVsLIuKUFKwiJspmUFgkK6YNjLURsbLRsr11FQFDEKlhgys6gYGj5oPZ2qqJKEaFlUlI8pyHcOqhDIV0cBkfLSqCs1z4ALgKmC0pio1HWHEFyRoGMiw7icRhRed60F23ZzVWjbWOjivQxET/4iNUXP8UaTxTUVVmjIGGyzXZjXqFhkzjBoaScS0rEnMTLQCEqKsqTq2rSaIhdOqYF9GFUhugLHKGbgKM8TqSobHuqisUDAN1s7CFTRDhtDhMRKDgaTl+aaGLPBhAwtt9hoaAEAjrGaZZ+lW0tJO0tI0aSke6SKUxJcg/XNOVxtNYP4iAwA0KMSeLhA2AiopQur1E7UX7/DDMkcfblBfksyjBAHa5qMwxg9J7rdH3zJuHbp0kANDS2BINHJVA+uwht06sPI1DBVFxcxnJ4VQXyEUbvejkFnJkTEdOhmsqbIGFYJkJXOEMCWJNRHTwfTeAxY/PslPYA3ZQ7mGFnHi9asoJqYClKS5aw8/ueN66QXOkYBlYpBA8qSkdBsUg9i0oS4ctbDpcRSDSK6V4NYlPQw1WsdkgCtOUh6/dkP4Zi84wYGIlUrLsgNJYYZNKcyoCkqV51K3+e/evkyZ1sGI+Y9ZAX+KB//4acMyZiJgQBPhREA8GXv6vS92oLlPOJBcYkVTluAKkwNNeREZtSWQVFeRbt5PrEKJnnlKIiGgZViXsEWUO8MDJMMYTPrWu4Zo+qcInaQSnBxsxoBDOTvf0ZKEZE9G3BRgMDwtmD2DUMhQnGRs8VMIU9Tx60J64saW8xyIHwaxZRKlUQGxqlpXBFuqpMdh1MB5+16Hjog0oQ5lp/WtQlKqpJQw2GRHXseilFm07k+xvkF+k4A5yoJp6UwHmyyH6VPpacXEw6knPr98Xry6q8wIN8NmNkci7elpE7G/Fkj483VNQk9d+f3ka688pzFRrqmJdmgut6+tGLhc1kUFu3ug44l8xxP57Bry3FXk1jE9bgMd0oiKhrrPRhsgoXbx0LLv8tATMhsEmQ0VAqVuyKxQXpXRxu23xJOX3sAsr5FG+7wzWz1D2vwUe+6RIJKsKfHTixcfuvnhqXHWtZNVEctQS+3toWfbLfYe9mTg6mKMxtY1SzTpr6NzUNRJggtuy5t9mO9sCgMo5xDv0RVs5t1Me0BkAyHWyNkDIh/ohYfePCACvfDQFwYJK3Xu3kaPu716iTsXmaBh2VDD8uHQXH5nwvYnrxqzW4gOHvPvXXO6KJMV5io68NWVhZtfzsTY/D5mTUaLUKojc3FnibclZDrRczt3kdwvKCK+q43pbnSRFsHdquEuSLl3HY6RBaOqV8si2S7Q/IWXZEit9SMiV+/3mYfo4c2+OF735LMujvvveD20K5vjcZNjHpGVj9AbySE7hWtYjyRHU1Wo97wq8Xnwf7Q4+Tsl95+dZON8Xb5v59jOjWzBWyh9z3SRvL+ddZVNp+P5vhTew97SDyAfBJAPAVAIAljLCwZDdMWsGiisOF1hBKjJ5WvQqJZDoSWng63+S7dyXan9OP3u+INbTv3MVm1xQZWhyDYDk2TzrZMdEluqTVr7T1/x0svIOT/hFfsR+Dr1uL6KKYRy/H4UuB9AMQhgbX7I/oiq6p8uj4BRLZ/77ds+7a+7rONkObc6rzqZLZDLfpWdpJhlXZWDUDvq0ucN6r9Q3ra4RmhQSMco9A68d4UFqCJOrO4LJwtHSPOn68rZ9SolKSoKi/V+nJRtUpMsRfeaUJfTJvpx0O7bQBEtd28CUVpjbRnn5lXPlLeR7IxwXsGZ4KFCbtnsErXbFbeOO0P17G6Fblx7DfJWW5fQrTK6R3QHzRBdrZ3ykk2mIzJuouX6IllipX2WWEVUk6COBPqlEslIsV4H73/n2WOHJ44tsBfSIwIbZP7jfKPx/u57BGpT7CPlzoCPlGRQqiRruElP9n/9zA8vf//xR86niqQVaGxRROeJvbjpuhPKdp9QeOuNM6mRC3fen9l99bNf2IpxiL67VhUSG4Pg2t/vtYslyWwWDamtfAZNKyWXf7QpROk7bsstmvJD9PD8X1qlUga+HwAA"
}
}

