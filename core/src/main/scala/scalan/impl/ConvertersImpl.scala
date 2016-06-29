package scalan

import scalan.staged.Expressions
import scalan.common.{Lazy, OverloadHack}
import OverloadHack.Overloaded2
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait ConvertersAbs extends Converters {
  self: Scalan =>

  // single proxy for each type family
  implicit def proxyConverter[T, R](p: Rep[Converter[T, R]]): Converter[T, R] = {
    proxyOps[Converter[T, R]](p)(scala.reflect.classTag[Converter[T, R]])
  }

  // familyElem
  class ConverterElem[T, R, To <: Converter[T, R]](implicit _eT: Elem[T], _eR: Elem[R])
    extends EntityElem[To] {
    def eT = _eT
    def eR = _eR
    lazy val parent: Option[Elem[_]] = None
    lazy val typeArgs = TypeArgs("T" -> eT, "R" -> eR)
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagT = eT.tag
      implicit val tagR = eR.tag
      weakTypeTag[Converter[T, R]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[Converter[T, R]] => convertConverter(x) }
      tryConvert(element[Converter[T, R]], this, x, conv)
    }

    def convertConverter(x: Rep[Converter[T, R]]): Rep[To] = {
      x.selfType1 match {
        case _: ConverterElem[_, _, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have ConverterElem[_, _, _], but got $e", x)
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def converterElement[T, R](implicit eT: Elem[T], eR: Elem[R]): Elem[Converter[T, R]] =
    cachedElem[ConverterElem[T, R, Converter[T, R]]](eT, eR)

  implicit case object ConverterCompanionElem extends CompanionElem[ConverterCompanionAbs] {
    lazy val tag = weakTypeTag[ConverterCompanionAbs]
    protected def getDefaultRep = Converter
  }

  abstract class ConverterCompanionAbs extends CompanionDef[ConverterCompanionAbs] with ConverterCompanion {
    def selfType = ConverterCompanionElem
    override def toString = "Converter"
  }
  def Converter: Rep[ConverterCompanionAbs]
  implicit def proxyConverterCompanionAbs(p: Rep[ConverterCompanionAbs]): ConverterCompanionAbs =
    proxyOps[ConverterCompanionAbs](p)

  abstract class AbsIdentityConv[A]
      ()(implicit eA: Elem[A])
    extends IdentityConv[A]() with Def[IdentityConv[A]] {
    lazy val selfType = element[IdentityConv[A]]
  }
  // elem for concrete class
  class IdentityConvElem[A](val iso: Iso[IdentityConvData[A], IdentityConv[A]])(implicit val eA: Elem[A])
    extends ConverterElem[A, A, IdentityConv[A]]
    with ConcreteElem[IdentityConvData[A], IdentityConv[A]] {
    override lazy val parent: Option[Elem[_]] = Some(converterElement(element[A], element[A]))
    override lazy val typeArgs = TypeArgs("A" -> eA)

    override def convertConverter(x: Rep[Converter[A, A]]) = IdentityConv()
    override def getDefaultRep = IdentityConv()
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[IdentityConv[A]]
    }
  }

  // state representation type
  type IdentityConvData[A] = Unit

  // 3) Iso for concrete class
  class IdentityConvIso[A](implicit eA: Elem[A])
    extends EntityIso[IdentityConvData[A], IdentityConv[A]] with Def[IdentityConvIso[A]] {
    override def from(p: Rep[IdentityConv[A]]) =
      ()
    override def to(p: Rep[Unit]) = {
      val unit = p
      IdentityConv()
    }
    lazy val eFrom = UnitElement
    lazy val eTo = new IdentityConvElem[A](self)
    lazy val selfType = new IdentityConvIsoElem[A](eA)
    def productArity = 1
    def productElement(n: Int) = eA
  }
  case class IdentityConvIsoElem[A](eA: Elem[A]) extends Elem[IdentityConvIso[A]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new IdentityConvIso[A]()(eA))
    lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[IdentityConvIso[A]]
    }
    lazy val typeArgs = TypeArgs("A" -> eA)
  }
  // 4) constructor and deconstructor
  class IdentityConvCompanionAbs extends CompanionDef[IdentityConvCompanionAbs] {
    def selfType = IdentityConvCompanionElem
    override def toString = "IdentityConv"
    @scalan.OverloadId("fromData")
    def apply[A](p: Rep[IdentityConvData[A]])(implicit eA: Elem[A]): Rep[IdentityConv[A]] =
      isoIdentityConv(eA).to(p)
    @scalan.OverloadId("fromFields")
    def apply[A]()(implicit eA: Elem[A]): Rep[IdentityConv[A]] =
      mkIdentityConv()

    def unapply[A](p: Rep[Converter[A, A]]) = unmkIdentityConv(p)
  }
  lazy val IdentityConvRep: Rep[IdentityConvCompanionAbs] = new IdentityConvCompanionAbs
  lazy val IdentityConv: IdentityConvCompanionAbs = proxyIdentityConvCompanion(IdentityConvRep)
  implicit def proxyIdentityConvCompanion(p: Rep[IdentityConvCompanionAbs]): IdentityConvCompanionAbs = {
    proxyOps[IdentityConvCompanionAbs](p)
  }

  implicit case object IdentityConvCompanionElem extends CompanionElem[IdentityConvCompanionAbs] {
    lazy val tag = weakTypeTag[IdentityConvCompanionAbs]
    protected def getDefaultRep = IdentityConv
  }

  implicit def proxyIdentityConv[A](p: Rep[IdentityConv[A]]): IdentityConv[A] =
    proxyOps[IdentityConv[A]](p)

  implicit class ExtendedIdentityConv[A](p: Rep[IdentityConv[A]])(implicit eA: Elem[A]) {
    def toData: Rep[IdentityConvData[A]] = isoIdentityConv(eA).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoIdentityConv[A](implicit eA: Elem[A]): Iso[IdentityConvData[A], IdentityConv[A]] =
    reifyObject(new IdentityConvIso[A]()(eA))

  // 6) smart constructor and deconstructor
  def mkIdentityConv[A]()(implicit eA: Elem[A]): Rep[IdentityConv[A]]
  def unmkIdentityConv[A](p: Rep[Converter[A, A]]): Option[(Rep[Unit])]

  abstract class AbsBaseConverter[T, R]
      (convFun: Rep[T => R])(implicit eT: Elem[T], eR: Elem[R])
    extends BaseConverter[T, R](convFun) with Def[BaseConverter[T, R]] {
    lazy val selfType = element[BaseConverter[T, R]]
  }
  // elem for concrete class
  class BaseConverterElem[T, R](val iso: Iso[BaseConverterData[T, R], BaseConverter[T, R]])(implicit override val eT: Elem[T], override val eR: Elem[R])
    extends ConverterElem[T, R, BaseConverter[T, R]]
    with ConcreteElem[BaseConverterData[T, R], BaseConverter[T, R]] {
    override lazy val parent: Option[Elem[_]] = Some(converterElement(element[T], element[R]))
    override lazy val typeArgs = TypeArgs("T" -> eT, "R" -> eR)

    override def convertConverter(x: Rep[Converter[T, R]]) = BaseConverter(x.convFun)
    override def getDefaultRep = BaseConverter(constFun[T, R](element[R].defaultRepValue))
    override lazy val tag = {
      implicit val tagT = eT.tag
      implicit val tagR = eR.tag
      weakTypeTag[BaseConverter[T, R]]
    }
  }

  // state representation type
  type BaseConverterData[T, R] = T => R

  // 3) Iso for concrete class
  class BaseConverterIso[T, R](implicit eT: Elem[T], eR: Elem[R])
    extends EntityIso[BaseConverterData[T, R], BaseConverter[T, R]] with Def[BaseConverterIso[T, R]] {
    override def from(p: Rep[BaseConverter[T, R]]) =
      p.convFun
    override def to(p: Rep[T => R]) = {
      val convFun = p
      BaseConverter(convFun)
    }
    lazy val eFrom = element[T => R]
    lazy val eTo = new BaseConverterElem[T, R](self)
    lazy val selfType = new BaseConverterIsoElem[T, R](eT, eR)
    def productArity = 2
    def productElement(n: Int) = (eT, eR).productElement(n)
  }
  case class BaseConverterIsoElem[T, R](eT: Elem[T], eR: Elem[R]) extends Elem[BaseConverterIso[T, R]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new BaseConverterIso[T, R]()(eT, eR))
    lazy val tag = {
      implicit val tagT = eT.tag
      implicit val tagR = eR.tag
      weakTypeTag[BaseConverterIso[T, R]]
    }
    lazy val typeArgs = TypeArgs("T" -> eT, "R" -> eR)
  }
  // 4) constructor and deconstructor
  class BaseConverterCompanionAbs extends CompanionDef[BaseConverterCompanionAbs] with BaseConverterCompanion {
    def selfType = BaseConverterCompanionElem
    override def toString = "BaseConverter"

    @scalan.OverloadId("fromFields")
    def apply[T, R](convFun: Rep[T => R])(implicit eT: Elem[T], eR: Elem[R]): Rep[BaseConverter[T, R]] =
      mkBaseConverter(convFun)

    def unapply[T, R](p: Rep[Converter[T, R]]) = unmkBaseConverter(p)
  }
  lazy val BaseConverterRep: Rep[BaseConverterCompanionAbs] = new BaseConverterCompanionAbs
  lazy val BaseConverter: BaseConverterCompanionAbs = proxyBaseConverterCompanion(BaseConverterRep)
  implicit def proxyBaseConverterCompanion(p: Rep[BaseConverterCompanionAbs]): BaseConverterCompanionAbs = {
    proxyOps[BaseConverterCompanionAbs](p)
  }

  implicit case object BaseConverterCompanionElem extends CompanionElem[BaseConverterCompanionAbs] {
    lazy val tag = weakTypeTag[BaseConverterCompanionAbs]
    protected def getDefaultRep = BaseConverter
  }

  implicit def proxyBaseConverter[T, R](p: Rep[BaseConverter[T, R]]): BaseConverter[T, R] =
    proxyOps[BaseConverter[T, R]](p)

  implicit class ExtendedBaseConverter[T, R](p: Rep[BaseConverter[T, R]])(implicit eT: Elem[T], eR: Elem[R]) {
    def toData: Rep[BaseConverterData[T, R]] = isoBaseConverter(eT, eR).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoBaseConverter[T, R](implicit eT: Elem[T], eR: Elem[R]): Iso[BaseConverterData[T, R], BaseConverter[T, R]] =
    reifyObject(new BaseConverterIso[T, R]()(eT, eR))

  // 6) smart constructor and deconstructor
  def mkBaseConverter[T, R](convFun: Rep[T => R])(implicit eT: Elem[T], eR: Elem[R]): Rep[BaseConverter[T, R]]
  def unmkBaseConverter[T, R](p: Rep[Converter[T, R]]): Option[(Rep[T => R])]

  abstract class AbsPairConverter[A1, A2, B1, B2]
      (conv1: Conv[A1, B1], conv2: Conv[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends PairConverter[A1, A2, B1, B2](conv1, conv2) with Def[PairConverter[A1, A2, B1, B2]] {
    lazy val selfType = element[PairConverter[A1, A2, B1, B2]]
  }
  // elem for concrete class
  class PairConverterElem[A1, A2, B1, B2](val iso: Iso[PairConverterData[A1, A2, B1, B2], PairConverter[A1, A2, B1, B2]])(implicit val eA1: Elem[A1], val eA2: Elem[A2], val eB1: Elem[B1], val eB2: Elem[B2])
    extends ConverterElem[(A1, A2), (B1, B2), PairConverter[A1, A2, B1, B2]]
    with ConcreteElem[PairConverterData[A1, A2, B1, B2], PairConverter[A1, A2, B1, B2]] {
    override lazy val parent: Option[Elem[_]] = Some(converterElement(pairElement(element[A1],element[A2]), pairElement(element[B1],element[B2])))
    override lazy val typeArgs = TypeArgs("A1" -> eA1, "A2" -> eA2, "B1" -> eB1, "B2" -> eB2)

    override def convertConverter(x: Rep[Converter[(A1, A2), (B1, B2)]]) = // Converter is not generated by meta
!!!("Cannot convert from Converter to PairConverter: missing fields List(conv1, conv2)")
    override def getDefaultRep = PairConverter(element[Converter[A1, B1]].defaultRepValue, element[Converter[A2, B2]].defaultRepValue)
    override lazy val tag = {
      implicit val tagA1 = eA1.tag
      implicit val tagA2 = eA2.tag
      implicit val tagB1 = eB1.tag
      implicit val tagB2 = eB2.tag
      weakTypeTag[PairConverter[A1, A2, B1, B2]]
    }
  }

  // state representation type
  type PairConverterData[A1, A2, B1, B2] = (Converter[A1, B1], Converter[A2, B2])

  // 3) Iso for concrete class
  class PairConverterIso[A1, A2, B1, B2](implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends EntityIso[PairConverterData[A1, A2, B1, B2], PairConverter[A1, A2, B1, B2]] with Def[PairConverterIso[A1, A2, B1, B2]] {
    override def from(p: Rep[PairConverter[A1, A2, B1, B2]]) =
      (p.conv1, p.conv2)
    override def to(p: Rep[(Converter[A1, B1], Converter[A2, B2])]) = {
      val Pair(conv1, conv2) = p
      PairConverter(conv1, conv2)
    }
    lazy val eFrom = pairElement(element[Converter[A1, B1]], element[Converter[A2, B2]])
    lazy val eTo = new PairConverterElem[A1, A2, B1, B2](self)
    lazy val selfType = new PairConverterIsoElem[A1, A2, B1, B2](eA1, eA2, eB1, eB2)
    def productArity = 4
    def productElement(n: Int) = (eA1, eA2, eB1, eB2).productElement(n)
  }
  case class PairConverterIsoElem[A1, A2, B1, B2](eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]) extends Elem[PairConverterIso[A1, A2, B1, B2]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new PairConverterIso[A1, A2, B1, B2]()(eA1, eA2, eB1, eB2))
    lazy val tag = {
      implicit val tagA1 = eA1.tag
      implicit val tagA2 = eA2.tag
      implicit val tagB1 = eB1.tag
      implicit val tagB2 = eB2.tag
      weakTypeTag[PairConverterIso[A1, A2, B1, B2]]
    }
    lazy val typeArgs = TypeArgs("A1" -> eA1, "A2" -> eA2, "B1" -> eB1, "B2" -> eB2)
  }
  // 4) constructor and deconstructor
  class PairConverterCompanionAbs extends CompanionDef[PairConverterCompanionAbs] with PairConverterCompanion {
    def selfType = PairConverterCompanionElem
    override def toString = "PairConverter"
    @scalan.OverloadId("fromData")
    def apply[A1, A2, B1, B2](p: Rep[PairConverterData[A1, A2, B1, B2]])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[PairConverter[A1, A2, B1, B2]] =
      isoPairConverter(eA1, eA2, eB1, eB2).to(p)
    @scalan.OverloadId("fromFields")
    def apply[A1, A2, B1, B2](conv1: Conv[A1, B1], conv2: Conv[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[PairConverter[A1, A2, B1, B2]] =
      mkPairConverter(conv1, conv2)

    def unapply[A1, A2, B1, B2](p: Rep[Converter[(A1, A2), (B1, B2)]]) = unmkPairConverter(p)
  }
  lazy val PairConverterRep: Rep[PairConverterCompanionAbs] = new PairConverterCompanionAbs
  lazy val PairConverter: PairConverterCompanionAbs = proxyPairConverterCompanion(PairConverterRep)
  implicit def proxyPairConverterCompanion(p: Rep[PairConverterCompanionAbs]): PairConverterCompanionAbs = {
    proxyOps[PairConverterCompanionAbs](p)
  }

  implicit case object PairConverterCompanionElem extends CompanionElem[PairConverterCompanionAbs] {
    lazy val tag = weakTypeTag[PairConverterCompanionAbs]
    protected def getDefaultRep = PairConverter
  }

  implicit def proxyPairConverter[A1, A2, B1, B2](p: Rep[PairConverter[A1, A2, B1, B2]]): PairConverter[A1, A2, B1, B2] =
    proxyOps[PairConverter[A1, A2, B1, B2]](p)

  implicit class ExtendedPairConverter[A1, A2, B1, B2](p: Rep[PairConverter[A1, A2, B1, B2]])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]) {
    def toData: Rep[PairConverterData[A1, A2, B1, B2]] = isoPairConverter(eA1, eA2, eB1, eB2).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoPairConverter[A1, A2, B1, B2](implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Iso[PairConverterData[A1, A2, B1, B2], PairConverter[A1, A2, B1, B2]] =
    reifyObject(new PairConverterIso[A1, A2, B1, B2]()(eA1, eA2, eB1, eB2))

  // 6) smart constructor and deconstructor
  def mkPairConverter[A1, A2, B1, B2](conv1: Conv[A1, B1], conv2: Conv[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[PairConverter[A1, A2, B1, B2]]
  def unmkPairConverter[A1, A2, B1, B2](p: Rep[Converter[(A1, A2), (B1, B2)]]): Option[(Rep[Converter[A1, B1]], Rep[Converter[A2, B2]])]

  abstract class AbsSumConverter[A1, A2, B1, B2]
      (conv1: Conv[A1, B1], conv2: Conv[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends SumConverter[A1, A2, B1, B2](conv1, conv2) with Def[SumConverter[A1, A2, B1, B2]] {
    lazy val selfType = element[SumConverter[A1, A2, B1, B2]]
  }
  // elem for concrete class
  class SumConverterElem[A1, A2, B1, B2](val iso: Iso[SumConverterData[A1, A2, B1, B2], SumConverter[A1, A2, B1, B2]])(implicit val eA1: Elem[A1], val eA2: Elem[A2], val eB1: Elem[B1], val eB2: Elem[B2])
    extends ConverterElem[$bar[A1, A2], $bar[B1, B2], SumConverter[A1, A2, B1, B2]]
    with ConcreteElem[SumConverterData[A1, A2, B1, B2], SumConverter[A1, A2, B1, B2]] {
    override lazy val parent: Option[Elem[_]] = Some(converterElement(sumElement(element[A1],element[A2]), sumElement(element[B1],element[B2])))
    override lazy val typeArgs = TypeArgs("A1" -> eA1, "A2" -> eA2, "B1" -> eB1, "B2" -> eB2)

    override def convertConverter(x: Rep[Converter[$bar[A1, A2], $bar[B1, B2]]]) = // Converter is not generated by meta
!!!("Cannot convert from Converter to SumConverter: missing fields List(conv1, conv2)")
    override def getDefaultRep = SumConverter(element[Converter[A1, B1]].defaultRepValue, element[Converter[A2, B2]].defaultRepValue)
    override lazy val tag = {
      implicit val tagA1 = eA1.tag
      implicit val tagA2 = eA2.tag
      implicit val tagB1 = eB1.tag
      implicit val tagB2 = eB2.tag
      weakTypeTag[SumConverter[A1, A2, B1, B2]]
    }
  }

  // state representation type
  type SumConverterData[A1, A2, B1, B2] = (Converter[A1, B1], Converter[A2, B2])

  // 3) Iso for concrete class
  class SumConverterIso[A1, A2, B1, B2](implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends EntityIso[SumConverterData[A1, A2, B1, B2], SumConverter[A1, A2, B1, B2]] with Def[SumConverterIso[A1, A2, B1, B2]] {
    override def from(p: Rep[SumConverter[A1, A2, B1, B2]]) =
      (p.conv1, p.conv2)
    override def to(p: Rep[(Converter[A1, B1], Converter[A2, B2])]) = {
      val Pair(conv1, conv2) = p
      SumConverter(conv1, conv2)
    }
    lazy val eFrom = pairElement(element[Converter[A1, B1]], element[Converter[A2, B2]])
    lazy val eTo = new SumConverterElem[A1, A2, B1, B2](self)
    lazy val selfType = new SumConverterIsoElem[A1, A2, B1, B2](eA1, eA2, eB1, eB2)
    def productArity = 4
    def productElement(n: Int) = (eA1, eA2, eB1, eB2).productElement(n)
  }
  case class SumConverterIsoElem[A1, A2, B1, B2](eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]) extends Elem[SumConverterIso[A1, A2, B1, B2]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new SumConverterIso[A1, A2, B1, B2]()(eA1, eA2, eB1, eB2))
    lazy val tag = {
      implicit val tagA1 = eA1.tag
      implicit val tagA2 = eA2.tag
      implicit val tagB1 = eB1.tag
      implicit val tagB2 = eB2.tag
      weakTypeTag[SumConverterIso[A1, A2, B1, B2]]
    }
    lazy val typeArgs = TypeArgs("A1" -> eA1, "A2" -> eA2, "B1" -> eB1, "B2" -> eB2)
  }
  // 4) constructor and deconstructor
  class SumConverterCompanionAbs extends CompanionDef[SumConverterCompanionAbs] with SumConverterCompanion {
    def selfType = SumConverterCompanionElem
    override def toString = "SumConverter"
    @scalan.OverloadId("fromData")
    def apply[A1, A2, B1, B2](p: Rep[SumConverterData[A1, A2, B1, B2]])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[SumConverter[A1, A2, B1, B2]] =
      isoSumConverter(eA1, eA2, eB1, eB2).to(p)
    @scalan.OverloadId("fromFields")
    def apply[A1, A2, B1, B2](conv1: Conv[A1, B1], conv2: Conv[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[SumConverter[A1, A2, B1, B2]] =
      mkSumConverter(conv1, conv2)

    def unapply[A1, A2, B1, B2](p: Rep[Converter[$bar[A1, A2], $bar[B1, B2]]]) = unmkSumConverter(p)
  }
  lazy val SumConverterRep: Rep[SumConverterCompanionAbs] = new SumConverterCompanionAbs
  lazy val SumConverter: SumConverterCompanionAbs = proxySumConverterCompanion(SumConverterRep)
  implicit def proxySumConverterCompanion(p: Rep[SumConverterCompanionAbs]): SumConverterCompanionAbs = {
    proxyOps[SumConverterCompanionAbs](p)
  }

  implicit case object SumConverterCompanionElem extends CompanionElem[SumConverterCompanionAbs] {
    lazy val tag = weakTypeTag[SumConverterCompanionAbs]
    protected def getDefaultRep = SumConverter
  }

  implicit def proxySumConverter[A1, A2, B1, B2](p: Rep[SumConverter[A1, A2, B1, B2]]): SumConverter[A1, A2, B1, B2] =
    proxyOps[SumConverter[A1, A2, B1, B2]](p)

  implicit class ExtendedSumConverter[A1, A2, B1, B2](p: Rep[SumConverter[A1, A2, B1, B2]])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]) {
    def toData: Rep[SumConverterData[A1, A2, B1, B2]] = isoSumConverter(eA1, eA2, eB1, eB2).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoSumConverter[A1, A2, B1, B2](implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Iso[SumConverterData[A1, A2, B1, B2], SumConverter[A1, A2, B1, B2]] =
    reifyObject(new SumConverterIso[A1, A2, B1, B2]()(eA1, eA2, eB1, eB2))

  // 6) smart constructor and deconstructor
  def mkSumConverter[A1, A2, B1, B2](conv1: Conv[A1, B1], conv2: Conv[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[SumConverter[A1, A2, B1, B2]]
  def unmkSumConverter[A1, A2, B1, B2](p: Rep[Converter[$bar[A1, A2], $bar[B1, B2]]]): Option[(Rep[Converter[A1, B1]], Rep[Converter[A2, B2]])]

  abstract class AbsComposeConverter[A, B, C]
      (conv2: Conv[B, C], conv1: Conv[A, B])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C])
    extends ComposeConverter[A, B, C](conv2, conv1) with Def[ComposeConverter[A, B, C]] {
    lazy val selfType = element[ComposeConverter[A, B, C]]
  }
  // elem for concrete class
  class ComposeConverterElem[A, B, C](val iso: Iso[ComposeConverterData[A, B, C], ComposeConverter[A, B, C]])(implicit val eA: Elem[A], val eB: Elem[B], val eC: Elem[C])
    extends ConverterElem[A, C, ComposeConverter[A, B, C]]
    with ConcreteElem[ComposeConverterData[A, B, C], ComposeConverter[A, B, C]] {
    override lazy val parent: Option[Elem[_]] = Some(converterElement(element[A], element[C]))
    override lazy val typeArgs = TypeArgs("A" -> eA, "B" -> eB, "C" -> eC)

    override def convertConverter(x: Rep[Converter[A, C]]) = // Converter is not generated by meta
!!!("Cannot convert from Converter to ComposeConverter: missing fields List(conv2, conv1)")
    override def getDefaultRep = ComposeConverter(element[Converter[B, C]].defaultRepValue, element[Converter[A, B]].defaultRepValue)
    override lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      implicit val tagC = eC.tag
      weakTypeTag[ComposeConverter[A, B, C]]
    }
  }

  // state representation type
  type ComposeConverterData[A, B, C] = (Converter[B, C], Converter[A, B])

  // 3) Iso for concrete class
  class ComposeConverterIso[A, B, C](implicit eA: Elem[A], eB: Elem[B], eC: Elem[C])
    extends EntityIso[ComposeConverterData[A, B, C], ComposeConverter[A, B, C]] with Def[ComposeConverterIso[A, B, C]] {
    override def from(p: Rep[ComposeConverter[A, B, C]]) =
      (p.conv2, p.conv1)
    override def to(p: Rep[(Converter[B, C], Converter[A, B])]) = {
      val Pair(conv2, conv1) = p
      ComposeConverter(conv2, conv1)
    }
    lazy val eFrom = pairElement(element[Converter[B, C]], element[Converter[A, B]])
    lazy val eTo = new ComposeConverterElem[A, B, C](self)
    lazy val selfType = new ComposeConverterIsoElem[A, B, C](eA, eB, eC)
    def productArity = 3
    def productElement(n: Int) = (eA, eB, eC).productElement(n)
  }
  case class ComposeConverterIsoElem[A, B, C](eA: Elem[A], eB: Elem[B], eC: Elem[C]) extends Elem[ComposeConverterIso[A, B, C]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new ComposeConverterIso[A, B, C]()(eA, eB, eC))
    lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      implicit val tagC = eC.tag
      weakTypeTag[ComposeConverterIso[A, B, C]]
    }
    lazy val typeArgs = TypeArgs("A" -> eA, "B" -> eB, "C" -> eC)
  }
  // 4) constructor and deconstructor
  class ComposeConverterCompanionAbs extends CompanionDef[ComposeConverterCompanionAbs] {
    def selfType = ComposeConverterCompanionElem
    override def toString = "ComposeConverter"
    @scalan.OverloadId("fromData")
    def apply[A, B, C](p: Rep[ComposeConverterData[A, B, C]])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]): Rep[ComposeConverter[A, B, C]] =
      isoComposeConverter(eA, eB, eC).to(p)
    @scalan.OverloadId("fromFields")
    def apply[A, B, C](conv2: Conv[B, C], conv1: Conv[A, B])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]): Rep[ComposeConverter[A, B, C]] =
      mkComposeConverter(conv2, conv1)

    def unapply[A, B, C](p: Rep[Converter[A, C]]) = unmkComposeConverter(p)
  }
  lazy val ComposeConverterRep: Rep[ComposeConverterCompanionAbs] = new ComposeConverterCompanionAbs
  lazy val ComposeConverter: ComposeConverterCompanionAbs = proxyComposeConverterCompanion(ComposeConverterRep)
  implicit def proxyComposeConverterCompanion(p: Rep[ComposeConverterCompanionAbs]): ComposeConverterCompanionAbs = {
    proxyOps[ComposeConverterCompanionAbs](p)
  }

  implicit case object ComposeConverterCompanionElem extends CompanionElem[ComposeConverterCompanionAbs] {
    lazy val tag = weakTypeTag[ComposeConverterCompanionAbs]
    protected def getDefaultRep = ComposeConverter
  }

  implicit def proxyComposeConverter[A, B, C](p: Rep[ComposeConverter[A, B, C]]): ComposeConverter[A, B, C] =
    proxyOps[ComposeConverter[A, B, C]](p)

  implicit class ExtendedComposeConverter[A, B, C](p: Rep[ComposeConverter[A, B, C]])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]) {
    def toData: Rep[ComposeConverterData[A, B, C]] = isoComposeConverter(eA, eB, eC).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoComposeConverter[A, B, C](implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]): Iso[ComposeConverterData[A, B, C], ComposeConverter[A, B, C]] =
    reifyObject(new ComposeConverterIso[A, B, C]()(eA, eB, eC))

  // 6) smart constructor and deconstructor
  def mkComposeConverter[A, B, C](conv2: Conv[B, C], conv1: Conv[A, B])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]): Rep[ComposeConverter[A, B, C]]
  def unmkComposeConverter[A, B, C](p: Rep[Converter[A, C]]): Option[(Rep[Converter[B, C]], Rep[Converter[A, B]])]

  abstract class AbsFunctorConverter[A, B, F[_]]
      (itemConv: Conv[A, B])(implicit eA: Elem[A], eB: Elem[B], F: Functor[F])
    extends FunctorConverter[A, B, F](itemConv) with Def[FunctorConverter[A, B, F]] {
    lazy val selfType = element[FunctorConverter[A, B, F]]
  }
  // elem for concrete class
  class FunctorConverterElem[A, B, F[_]](val iso: Iso[FunctorConverterData[A, B, F], FunctorConverter[A, B, F]])(implicit val eA: Elem[A], val eB: Elem[B], val F: Functor[F])
    extends ConverterElem[F[A], F[B], FunctorConverter[A, B, F]]
    with ConcreteElem[FunctorConverterData[A, B, F], FunctorConverter[A, B, F]] {
    override lazy val parent: Option[Elem[_]] = Some(converterElement(element[F[A]], element[F[B]]))
    override lazy val typeArgs = TypeArgs("A" -> eA, "B" -> eB)

    override def convertConverter(x: Rep[Converter[F[A], F[B]]]) = // Converter is not generated by meta
!!!("Cannot convert from Converter to FunctorConverter: missing fields List(itemConv)")
    override def getDefaultRep = FunctorConverter(element[Converter[A, B]].defaultRepValue)
    override lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[FunctorConverter[A, B, F]]
    }
  }

  // state representation type
  type FunctorConverterData[A, B, F[_]] = Converter[A, B]

  // 3) Iso for concrete class
  class FunctorConverterIso[A, B, F[_]](implicit eA: Elem[A], eB: Elem[B], F: Functor[F])
    extends EntityIso[FunctorConverterData[A, B, F], FunctorConverter[A, B, F]] with Def[FunctorConverterIso[A, B, F]] {
    override def from(p: Rep[FunctorConverter[A, B, F]]) =
      p.itemConv
    override def to(p: Rep[Converter[A, B]]) = {
      val itemConv = p
      FunctorConverter(itemConv)
    }
    lazy val eFrom = element[Converter[A, B]]
    lazy val eTo = new FunctorConverterElem[A, B, F](self)
    lazy val selfType = new FunctorConverterIsoElem[A, B, F](eA, eB, F)
    def productArity = 3
    def productElement(n: Int) = (eA, eB, F).productElement(n)
  }
  case class FunctorConverterIsoElem[A, B, F[_]](eA: Elem[A], eB: Elem[B], F: Functor[F]) extends Elem[FunctorConverterIso[A, B, F]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new FunctorConverterIso[A, B, F]()(eA, eB, F))
    lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[FunctorConverterIso[A, B, F]]
    }
    lazy val typeArgs = TypeArgs("A" -> eA, "B" -> eB)
  }
  // 4) constructor and deconstructor
  class FunctorConverterCompanionAbs extends CompanionDef[FunctorConverterCompanionAbs] with FunctorConverterCompanion {
    def selfType = FunctorConverterCompanionElem
    override def toString = "FunctorConverter"

    @scalan.OverloadId("fromFields")
    def apply[A, B, F[_]](itemConv: Conv[A, B])(implicit eA: Elem[A], eB: Elem[B], F: Functor[F]): Rep[FunctorConverter[A, B, F]] =
      mkFunctorConverter(itemConv)

    def unapply[A, B, F[_]](p: Rep[Converter[F[A], F[B]]]) = unmkFunctorConverter(p)
  }
  lazy val FunctorConverterRep: Rep[FunctorConverterCompanionAbs] = new FunctorConverterCompanionAbs
  lazy val FunctorConverter: FunctorConverterCompanionAbs = proxyFunctorConverterCompanion(FunctorConverterRep)
  implicit def proxyFunctorConverterCompanion(p: Rep[FunctorConverterCompanionAbs]): FunctorConverterCompanionAbs = {
    proxyOps[FunctorConverterCompanionAbs](p)
  }

  implicit case object FunctorConverterCompanionElem extends CompanionElem[FunctorConverterCompanionAbs] {
    lazy val tag = weakTypeTag[FunctorConverterCompanionAbs]
    protected def getDefaultRep = FunctorConverter
  }

  implicit def proxyFunctorConverter[A, B, F[_]](p: Rep[FunctorConverter[A, B, F]]): FunctorConverter[A, B, F] =
    proxyOps[FunctorConverter[A, B, F]](p)

  implicit class ExtendedFunctorConverter[A, B, F[_]](p: Rep[FunctorConverter[A, B, F]])(implicit eA: Elem[A], eB: Elem[B], F: Functor[F]) {
    def toData: Rep[FunctorConverterData[A, B, F]] = isoFunctorConverter(eA, eB, F).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoFunctorConverter[A, B, F[_]](implicit eA: Elem[A], eB: Elem[B], F: Functor[F]): Iso[FunctorConverterData[A, B, F], FunctorConverter[A, B, F]] =
    reifyObject(new FunctorConverterIso[A, B, F]()(eA, eB, F))

  // 6) smart constructor and deconstructor
  def mkFunctorConverter[A, B, F[_]](itemConv: Conv[A, B])(implicit eA: Elem[A], eB: Elem[B], F: Functor[F]): Rep[FunctorConverter[A, B, F]]
  def unmkFunctorConverter[A, B, F[_]](p: Rep[Converter[F[A], F[B]]]): Option[(Rep[Converter[A, B]])]

  abstract class AbsNaturalConverter[A, F[_], G[_]]
      (convFun: Rep[F[A] => G[A]])(implicit eA: Elem[A], cF: Cont[F], cG: Cont[G])
    extends NaturalConverter[A, F, G](convFun) with Def[NaturalConverter[A, F, G]] {
    lazy val selfType = element[NaturalConverter[A, F, G]]
  }
  // elem for concrete class
  class NaturalConverterElem[A, F[_], G[_]](val iso: Iso[NaturalConverterData[A, F, G], NaturalConverter[A, F, G]])(implicit val eA: Elem[A], val cF: Cont[F], val cG: Cont[G])
    extends ConverterElem[F[A], G[A], NaturalConverter[A, F, G]]
    with ConcreteElem[NaturalConverterData[A, F, G], NaturalConverter[A, F, G]] {
    override lazy val parent: Option[Elem[_]] = Some(converterElement(element[F[A]], element[G[A]]))
    override lazy val typeArgs = TypeArgs("A" -> eA, "F" -> cF, "G" -> cG)

    override def convertConverter(x: Rep[Converter[F[A], G[A]]]) = NaturalConverter(x.convFun)
    override def getDefaultRep = NaturalConverter(constFun[F[A], G[A]](element[G[A]].defaultRepValue))
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[NaturalConverter[A, F, G]]
    }
  }

  // state representation type
  type NaturalConverterData[A, F[_], G[_]] = F[A] => G[A]

  // 3) Iso for concrete class
  class NaturalConverterIso[A, F[_], G[_]](implicit eA: Elem[A], cF: Cont[F], cG: Cont[G])
    extends EntityIso[NaturalConverterData[A, F, G], NaturalConverter[A, F, G]] with Def[NaturalConverterIso[A, F, G]] {
    override def from(p: Rep[NaturalConverter[A, F, G]]) =
      p.convFun
    override def to(p: Rep[F[A] => G[A]]) = {
      val convFun = p
      NaturalConverter(convFun)
    }
    lazy val eFrom = element[F[A] => G[A]]
    lazy val eTo = new NaturalConverterElem[A, F, G](self)
    lazy val selfType = new NaturalConverterIsoElem[A, F, G](eA, cF, cG)
    def productArity = 3
    def productElement(n: Int) = (eA, cF, cG).productElement(n)
  }
  case class NaturalConverterIsoElem[A, F[_], G[_]](eA: Elem[A], cF: Cont[F], cG: Cont[G]) extends Elem[NaturalConverterIso[A, F, G]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new NaturalConverterIso[A, F, G]()(eA, cF, cG))
    lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[NaturalConverterIso[A, F, G]]
    }
    lazy val typeArgs = TypeArgs("A" -> eA, "F" -> cF, "G" -> cG)
  }
  // 4) constructor and deconstructor
  class NaturalConverterCompanionAbs extends CompanionDef[NaturalConverterCompanionAbs] {
    def selfType = NaturalConverterCompanionElem
    override def toString = "NaturalConverter"

    @scalan.OverloadId("fromFields")
    def apply[A, F[_], G[_]](convFun: Rep[F[A] => G[A]])(implicit eA: Elem[A], cF: Cont[F], cG: Cont[G]): Rep[NaturalConverter[A, F, G]] =
      mkNaturalConverter(convFun)

    def unapply[A, F[_], G[_]](p: Rep[Converter[F[A], G[A]]]) = unmkNaturalConverter(p)
  }
  lazy val NaturalConverterRep: Rep[NaturalConverterCompanionAbs] = new NaturalConverterCompanionAbs
  lazy val NaturalConverter: NaturalConverterCompanionAbs = proxyNaturalConverterCompanion(NaturalConverterRep)
  implicit def proxyNaturalConverterCompanion(p: Rep[NaturalConverterCompanionAbs]): NaturalConverterCompanionAbs = {
    proxyOps[NaturalConverterCompanionAbs](p)
  }

  implicit case object NaturalConverterCompanionElem extends CompanionElem[NaturalConverterCompanionAbs] {
    lazy val tag = weakTypeTag[NaturalConverterCompanionAbs]
    protected def getDefaultRep = NaturalConverter
  }

  implicit def proxyNaturalConverter[A, F[_], G[_]](p: Rep[NaturalConverter[A, F, G]]): NaturalConverter[A, F, G] =
    proxyOps[NaturalConverter[A, F, G]](p)

  implicit class ExtendedNaturalConverter[A, F[_], G[_]](p: Rep[NaturalConverter[A, F, G]])(implicit eA: Elem[A], cF: Cont[F], cG: Cont[G]) {
    def toData: Rep[NaturalConverterData[A, F, G]] = isoNaturalConverter(eA, cF, cG).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoNaturalConverter[A, F[_], G[_]](implicit eA: Elem[A], cF: Cont[F], cG: Cont[G]): Iso[NaturalConverterData[A, F, G], NaturalConverter[A, F, G]] =
    reifyObject(new NaturalConverterIso[A, F, G]()(eA, cF, cG))

  // 6) smart constructor and deconstructor
  def mkNaturalConverter[A, F[_], G[_]](convFun: Rep[F[A] => G[A]])(implicit eA: Elem[A], cF: Cont[F], cG: Cont[G]): Rep[NaturalConverter[A, F, G]]
  def unmkNaturalConverter[A, F[_], G[_]](p: Rep[Converter[F[A], G[A]]]): Option[(Rep[F[A] => G[A]])]

  registerModule(Converters_Module)
}

// Std -----------------------------------
trait ConvertersStd extends ConvertersDsl {
  self: ScalanStd =>
  lazy val Converter: Rep[ConverterCompanionAbs] = new ConverterCompanionAbs {
  }

  case class StdIdentityConv[A]
      ()(implicit eA: Elem[A])
    extends AbsIdentityConv[A]() {
  }

  def mkIdentityConv[A]
    ()(implicit eA: Elem[A]): Rep[IdentityConv[A]] =
    new StdIdentityConv[A]()
  def unmkIdentityConv[A](p: Rep[Converter[A, A]]) = p match {
    case p: IdentityConv[A] @unchecked =>
      Some(())
    case _ => None
  }

  case class StdBaseConverter[T, R]
      (override val convFun: Rep[T => R])(implicit eT: Elem[T], eR: Elem[R])
    extends AbsBaseConverter[T, R](convFun) {
  }

  def mkBaseConverter[T, R]
    (convFun: Rep[T => R])(implicit eT: Elem[T], eR: Elem[R]): Rep[BaseConverter[T, R]] =
    new StdBaseConverter[T, R](convFun)
  def unmkBaseConverter[T, R](p: Rep[Converter[T, R]]) = p match {
    case p: BaseConverter[T, R] @unchecked =>
      Some((p.convFun))
    case _ => None
  }

  case class StdPairConverter[A1, A2, B1, B2]
      (override val conv1: Conv[A1, B1], override val conv2: Conv[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends AbsPairConverter[A1, A2, B1, B2](conv1, conv2) {
  }

  def mkPairConverter[A1, A2, B1, B2]
    (conv1: Conv[A1, B1], conv2: Conv[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[PairConverter[A1, A2, B1, B2]] =
    new StdPairConverter[A1, A2, B1, B2](conv1, conv2)
  def unmkPairConverter[A1, A2, B1, B2](p: Rep[Converter[(A1, A2), (B1, B2)]]) = p match {
    case p: PairConverter[A1, A2, B1, B2] @unchecked =>
      Some((p.conv1, p.conv2))
    case _ => None
  }

  case class StdSumConverter[A1, A2, B1, B2]
      (override val conv1: Conv[A1, B1], override val conv2: Conv[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends AbsSumConverter[A1, A2, B1, B2](conv1, conv2) {
  }

  def mkSumConverter[A1, A2, B1, B2]
    (conv1: Conv[A1, B1], conv2: Conv[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[SumConverter[A1, A2, B1, B2]] =
    new StdSumConverter[A1, A2, B1, B2](conv1, conv2)
  def unmkSumConverter[A1, A2, B1, B2](p: Rep[Converter[$bar[A1, A2], $bar[B1, B2]]]) = p match {
    case p: SumConverter[A1, A2, B1, B2] @unchecked =>
      Some((p.conv1, p.conv2))
    case _ => None
  }

  case class StdComposeConverter[A, B, C]
      (override val conv2: Conv[B, C], override val conv1: Conv[A, B])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C])
    extends AbsComposeConverter[A, B, C](conv2, conv1) {
  }

  def mkComposeConverter[A, B, C]
    (conv2: Conv[B, C], conv1: Conv[A, B])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]): Rep[ComposeConverter[A, B, C]] =
    new StdComposeConverter[A, B, C](conv2, conv1)
  def unmkComposeConverter[A, B, C](p: Rep[Converter[A, C]]) = p match {
    case p: ComposeConverter[A, B, C] @unchecked =>
      Some((p.conv2, p.conv1))
    case _ => None
  }

  case class StdFunctorConverter[A, B, F[_]]
      (override val itemConv: Conv[A, B])(implicit eA: Elem[A], eB: Elem[B], F: Functor[F])
    extends AbsFunctorConverter[A, B, F](itemConv) {
  }

  def mkFunctorConverter[A, B, F[_]]
    (itemConv: Conv[A, B])(implicit eA: Elem[A], eB: Elem[B], F: Functor[F]): Rep[FunctorConverter[A, B, F]] =
    new StdFunctorConverter[A, B, F](itemConv)
  def unmkFunctorConverter[A, B, F[_]](p: Rep[Converter[F[A], F[B]]]) = p match {
    case p: FunctorConverter[A, B, F] @unchecked =>
      Some((p.itemConv))
    case _ => None
  }

  case class StdNaturalConverter[A, F[_], G[_]]
      (override val convFun: Rep[F[A] => G[A]])(implicit eA: Elem[A], cF: Cont[F], cG: Cont[G])
    extends AbsNaturalConverter[A, F, G](convFun) {
  }

  def mkNaturalConverter[A, F[_], G[_]]
    (convFun: Rep[F[A] => G[A]])(implicit eA: Elem[A], cF: Cont[F], cG: Cont[G]): Rep[NaturalConverter[A, F, G]] =
    new StdNaturalConverter[A, F, G](convFun)
  def unmkNaturalConverter[A, F[_], G[_]](p: Rep[Converter[F[A], G[A]]]) = p match {
    case p: NaturalConverter[A, F, G] @unchecked =>
      Some((p.convFun))
    case _ => None
  }
}

// Exp -----------------------------------
trait ConvertersExp extends ConvertersDsl {
  self: ScalanExp =>
  lazy val Converter: Rep[ConverterCompanionAbs] = new ConverterCompanionAbs {
  }

  case class ExpIdentityConv[A]
      ()(implicit eA: Elem[A])
    extends AbsIdentityConv[A]()

  object IdentityConvMethods {
    object apply {
      def unapply(d: Def[_]): Option[(Rep[IdentityConv[A]], Rep[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(x, _*), _) if receiver.elem.isInstanceOf[IdentityConvElem[_]] && method.getName == "apply" =>
          Some((receiver, x)).asInstanceOf[Option[(Rep[IdentityConv[A]], Rep[A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[IdentityConv[A]], Rep[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object isIdentity {
      def unapply(d: Def[_]): Option[Rep[IdentityConv[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[IdentityConvElem[_]] && method.getName == "isIdentity" =>
          Some(receiver).asInstanceOf[Option[Rep[IdentityConv[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[IdentityConv[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `equals`: Overrides Object method
  }

  def mkIdentityConv[A]
    ()(implicit eA: Elem[A]): Rep[IdentityConv[A]] =
    new ExpIdentityConv[A]()
  def unmkIdentityConv[A](p: Rep[Converter[A, A]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: IdentityConvElem[A] @unchecked =>
      Some(())
    case _ =>
      None
  }

  case class ExpBaseConverter[T, R]
      (override val convFun: Rep[T => R])(implicit eT: Elem[T], eR: Elem[R])
    extends AbsBaseConverter[T, R](convFun)

  object BaseConverterMethods {
    object apply {
      def unapply(d: Def[_]): Option[(Rep[BaseConverter[T, R]], Rep[T]) forSome {type T; type R}] = d match {
        case MethodCall(receiver, method, Seq(x, _*), _) if receiver.elem.isInstanceOf[BaseConverterElem[_, _]] && method.getName == "apply" =>
          Some((receiver, x)).asInstanceOf[Option[(Rep[BaseConverter[T, R]], Rep[T]) forSome {type T; type R}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[BaseConverter[T, R]], Rep[T]) forSome {type T; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `equals`: Overrides Object method
  }

  object BaseConverterCompanionMethods {
  }

  def mkBaseConverter[T, R]
    (convFun: Rep[T => R])(implicit eT: Elem[T], eR: Elem[R]): Rep[BaseConverter[T, R]] =
    new ExpBaseConverter[T, R](convFun)
  def unmkBaseConverter[T, R](p: Rep[Converter[T, R]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: BaseConverterElem[T, R] @unchecked =>
      Some((p.asRep[BaseConverter[T, R]].convFun))
    case _ =>
      None
  }

  case class ExpPairConverter[A1, A2, B1, B2]
      (override val conv1: Conv[A1, B1], override val conv2: Conv[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends AbsPairConverter[A1, A2, B1, B2](conv1, conv2)

  object PairConverterMethods {
    object apply {
      def unapply(d: Def[_]): Option[(Rep[PairConverter[A1, A2, B1, B2]], Rep[(A1, A2)]) forSome {type A1; type A2; type B1; type B2}] = d match {
        case MethodCall(receiver, method, Seq(x, _*), _) if receiver.elem.isInstanceOf[PairConverterElem[_, _, _, _]] && method.getName == "apply" =>
          Some((receiver, x)).asInstanceOf[Option[(Rep[PairConverter[A1, A2, B1, B2]], Rep[(A1, A2)]) forSome {type A1; type A2; type B1; type B2}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairConverter[A1, A2, B1, B2]], Rep[(A1, A2)]) forSome {type A1; type A2; type B1; type B2}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object isIdentity {
      def unapply(d: Def[_]): Option[Rep[PairConverter[A1, A2, B1, B2]] forSome {type A1; type A2; type B1; type B2}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairConverterElem[_, _, _, _]] && method.getName == "isIdentity" =>
          Some(receiver).asInstanceOf[Option[Rep[PairConverter[A1, A2, B1, B2]] forSome {type A1; type A2; type B1; type B2}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[PairConverter[A1, A2, B1, B2]] forSome {type A1; type A2; type B1; type B2}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object PairConverterCompanionMethods {
  }

  def mkPairConverter[A1, A2, B1, B2]
    (conv1: Conv[A1, B1], conv2: Conv[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[PairConverter[A1, A2, B1, B2]] =
    new ExpPairConverter[A1, A2, B1, B2](conv1, conv2)
  def unmkPairConverter[A1, A2, B1, B2](p: Rep[Converter[(A1, A2), (B1, B2)]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: PairConverterElem[A1, A2, B1, B2] @unchecked =>
      Some((p.asRep[PairConverter[A1, A2, B1, B2]].conv1, p.asRep[PairConverter[A1, A2, B1, B2]].conv2))
    case _ =>
      None
  }

  case class ExpSumConverter[A1, A2, B1, B2]
      (override val conv1: Conv[A1, B1], override val conv2: Conv[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends AbsSumConverter[A1, A2, B1, B2](conv1, conv2)

  object SumConverterMethods {
    object apply {
      def unapply(d: Def[_]): Option[(Rep[SumConverter[A1, A2, B1, B2]], Rep[$bar[A1, A2]]) forSome {type A1; type A2; type B1; type B2}] = d match {
        case MethodCall(receiver, method, Seq(x, _*), _) if receiver.elem.isInstanceOf[SumConverterElem[_, _, _, _]] && method.getName == "apply" =>
          Some((receiver, x)).asInstanceOf[Option[(Rep[SumConverter[A1, A2, B1, B2]], Rep[$bar[A1, A2]]) forSome {type A1; type A2; type B1; type B2}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SumConverter[A1, A2, B1, B2]], Rep[$bar[A1, A2]]) forSome {type A1; type A2; type B1; type B2}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object isIdentity {
      def unapply(d: Def[_]): Option[Rep[SumConverter[A1, A2, B1, B2]] forSome {type A1; type A2; type B1; type B2}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SumConverterElem[_, _, _, _]] && method.getName == "isIdentity" =>
          Some(receiver).asInstanceOf[Option[Rep[SumConverter[A1, A2, B1, B2]] forSome {type A1; type A2; type B1; type B2}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[SumConverter[A1, A2, B1, B2]] forSome {type A1; type A2; type B1; type B2}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object SumConverterCompanionMethods {
  }

  def mkSumConverter[A1, A2, B1, B2]
    (conv1: Conv[A1, B1], conv2: Conv[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[SumConverter[A1, A2, B1, B2]] =
    new ExpSumConverter[A1, A2, B1, B2](conv1, conv2)
  def unmkSumConverter[A1, A2, B1, B2](p: Rep[Converter[$bar[A1, A2], $bar[B1, B2]]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: SumConverterElem[A1, A2, B1, B2] @unchecked =>
      Some((p.asRep[SumConverter[A1, A2, B1, B2]].conv1, p.asRep[SumConverter[A1, A2, B1, B2]].conv2))
    case _ =>
      None
  }

  case class ExpComposeConverter[A, B, C]
      (override val conv2: Conv[B, C], override val conv1: Conv[A, B])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C])
    extends AbsComposeConverter[A, B, C](conv2, conv1)

  object ComposeConverterMethods {
    object apply {
      def unapply(d: Def[_]): Option[(Rep[ComposeConverter[A, B, C]], Rep[A]) forSome {type A; type B; type C}] = d match {
        case MethodCall(receiver, method, Seq(a, _*), _) if receiver.elem.isInstanceOf[ComposeConverterElem[_, _, _]] && method.getName == "apply" =>
          Some((receiver, a)).asInstanceOf[Option[(Rep[ComposeConverter[A, B, C]], Rep[A]) forSome {type A; type B; type C}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ComposeConverter[A, B, C]], Rep[A]) forSome {type A; type B; type C}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object isIdentity {
      def unapply(d: Def[_]): Option[Rep[ComposeConverter[A, B, C]] forSome {type A; type B; type C}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ComposeConverterElem[_, _, _]] && method.getName == "isIdentity" =>
          Some(receiver).asInstanceOf[Option[Rep[ComposeConverter[A, B, C]] forSome {type A; type B; type C}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[ComposeConverter[A, B, C]] forSome {type A; type B; type C}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `equals`: Overrides Object method
  }

  def mkComposeConverter[A, B, C]
    (conv2: Conv[B, C], conv1: Conv[A, B])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]): Rep[ComposeConverter[A, B, C]] =
    new ExpComposeConverter[A, B, C](conv2, conv1)
  def unmkComposeConverter[A, B, C](p: Rep[Converter[A, C]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ComposeConverterElem[A, B, C] @unchecked =>
      Some((p.asRep[ComposeConverter[A, B, C]].conv2, p.asRep[ComposeConverter[A, B, C]].conv1))
    case _ =>
      None
  }

  case class ExpFunctorConverter[A, B, F[_]]
      (override val itemConv: Conv[A, B])(implicit eA: Elem[A], eB: Elem[B], F: Functor[F])
    extends AbsFunctorConverter[A, B, F](itemConv)

  object FunctorConverterMethods {
    object convFun {
      def unapply(d: Def[_]): Option[Rep[FunctorConverter[A, B, F]] forSome {type A; type B; type F[_]}] = d match {
        case MethodCall(receiver, method, _, _) if (receiver.elem.asInstanceOf[Elem[_]] match { case _: FunctorConverterElem[_, _, _] => true; case _ => false }) && method.getName == "convFun" =>
          Some(receiver).asInstanceOf[Option[Rep[FunctorConverter[A, B, F]] forSome {type A; type B; type F[_]}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[FunctorConverter[A, B, F]] forSome {type A; type B; type F[_]}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[FunctorConverter[A, B, F]], Rep[F[A]]) forSome {type A; type B; type F[_]}] = d match {
        case MethodCall(receiver, method, Seq(xs, _*), _) if (receiver.elem.asInstanceOf[Elem[_]] match { case _: FunctorConverterElem[_, _, _] => true; case _ => false }) && method.getName == "apply" =>
          Some((receiver, xs)).asInstanceOf[Option[(Rep[FunctorConverter[A, B, F]], Rep[F[A]]) forSome {type A; type B; type F[_]}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[FunctorConverter[A, B, F]], Rep[F[A]]) forSome {type A; type B; type F[_]}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object isIdentity {
      def unapply(d: Def[_]): Option[Rep[FunctorConverter[A, B, F]] forSome {type A; type B; type F[_]}] = d match {
        case MethodCall(receiver, method, _, _) if (receiver.elem.asInstanceOf[Elem[_]] match { case _: FunctorConverterElem[_, _, _] => true; case _ => false }) && method.getName == "isIdentity" =>
          Some(receiver).asInstanceOf[Option[Rep[FunctorConverter[A, B, F]] forSome {type A; type B; type F[_]}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[FunctorConverter[A, B, F]] forSome {type A; type B; type F[_]}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `equals`: Overrides Object method
  }

  object FunctorConverterCompanionMethods {
  }

  def mkFunctorConverter[A, B, F[_]]
    (itemConv: Conv[A, B])(implicit eA: Elem[A], eB: Elem[B], F: Functor[F]): Rep[FunctorConverter[A, B, F]] =
    new ExpFunctorConverter[A, B, F](itemConv)
  def unmkFunctorConverter[A, B, F[_]](p: Rep[Converter[F[A], F[B]]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: FunctorConverterElem[A, B, F] @unchecked =>
      Some((p.asRep[FunctorConverter[A, B, F]].itemConv))
    case _ =>
      None
  }

  case class ExpNaturalConverter[A, F[_], G[_]]
      (override val convFun: Rep[F[A] => G[A]])(implicit eA: Elem[A], cF: Cont[F], cG: Cont[G])
    extends AbsNaturalConverter[A, F, G](convFun)

  object NaturalConverterMethods {
    object apply {
      def unapply(d: Def[_]): Option[(Rep[NaturalConverter[A, F, G]], Rep[F[A]]) forSome {type A; type F[_]; type G[_]}] = d match {
        case MethodCall(receiver, method, Seq(xs, _*), _) if (receiver.elem.asInstanceOf[Elem[_]] match { case _: NaturalConverterElem[_, _, _] => true; case _ => false }) && method.getName == "apply" =>
          Some((receiver, xs)).asInstanceOf[Option[(Rep[NaturalConverter[A, F, G]], Rep[F[A]]) forSome {type A; type F[_]; type G[_]}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[NaturalConverter[A, F, G]], Rep[F[A]]) forSome {type A; type F[_]; type G[_]}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `equals`: Overrides Object method
  }

  def mkNaturalConverter[A, F[_], G[_]]
    (convFun: Rep[F[A] => G[A]])(implicit eA: Elem[A], cF: Cont[F], cG: Cont[G]): Rep[NaturalConverter[A, F, G]] =
    new ExpNaturalConverter[A, F, G](convFun)
  def unmkNaturalConverter[A, F[_], G[_]](p: Rep[Converter[F[A], G[A]]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: NaturalConverterElem[A, F, G] @unchecked =>
      Some((p.asRep[NaturalConverter[A, F, G]].convFun))
    case _ =>
      None
  }

  object ConverterMethods {
    object convFun {
      def unapply(d: Def[_]): Option[Rep[Converter[T, R]] forSome {type T; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ConverterElem[_, _, _]] && method.getName == "convFun" =>
          Some(receiver).asInstanceOf[Option[Rep[Converter[T, R]] forSome {type T; type R}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Converter[T, R]] forSome {type T; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[Converter[T, R]], Rep[T]) forSome {type T; type R}] = d match {
        case MethodCall(receiver, method, Seq(x, _*), _) if receiver.elem.isInstanceOf[ConverterElem[_, _, _]] && method.getName == "apply" =>
          Some((receiver, x)).asInstanceOf[Option[(Rep[Converter[T, R]], Rep[T]) forSome {type T; type R}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Converter[T, R]], Rep[T]) forSome {type T; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `isIdentity`: Method's return type Boolean is not a Rep

    // WARNING: Cannot generate matcher for method `toString`: Overrides Object method
  }

  object ConverterCompanionMethods {
  }
}

object Converters_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAAO1ZTYgbVRx/yX5ksxvbdWurtbS7LqnfbtpUqLBKyWSz65Z0d9mJVNbS5WXmZTt1vpx5WRIPrXgoop5EBAUPBcVLKYg3BfFgQUQExZt46MFTrUgPFg+K7735nsxMkl1XQZrDY2byn9/7/3+/3/8l8+bKTTBkGuBBU4AyVGcUhOEMz45LJs7zFRVLuH1KE5symkON7x77Qht99c2lNBhfA8PnoDlnymsgax1UWrp7zGOxCrJQFZCJNcPE4IEqm6EgaLKMBCxpakFSlCaGdRkVqpKJZ6tgsK6J7ZfABZCqgnFBUwUDYcSXZWiayLSvjyCakeSeZ9l5e1n35lALtIqCr4qaASVM0idzjFvxq0jn26qmthUMdtmpLes0LRIzhlo6qWFR0WU2zUAVZCRF1wzszJohM5zTROd0UIXkApionoebsEBm3Sjw2JDUDQqmQ+FFuIGWSAgNHyQ1mEhu1No6ssHHTCwG5mvpAACiSpElNuNxNuNyNkM5y/PIkKAsvQzplyuG1moD65MaAKClE4jHu0A4CKiiivnXzwgv3ObHlDS9uUVTybCEhgnQZIxDmDyE269W3zJvLVw+ngaja2BUMkt1ExtQwH4b2HSNQVXVMMvZZRAaG0TB6TgF2SwlEhOySVbQFB2qBMnmMkeEkiVBwjSYXsvZ8sRwn8E6ckJTLT3l1jsVUy/zUhnK8sqN/U8c/qXyfBqkg1NkCSRPmsFwQDHIljV1ExkYGTY+HXdjkCp5JHundMi2vDGTkI5LzEM3fhWvHQFn0i6d9uwuJIUZs+ywpKkoP7+S/53/+u0rVG0D5KxvrCb4Szr+54+7GpgZgQHd25sRSCYTT7336WG0cjUNRtZY38zLcINZgtI+h0xhDYxohA3remYTyvQo0hYZETVgU8a2WH6WBwjLGEzFtryOqASzRFLSDS4HBzBIo5LD92BFRkqkIn4JMMgtitayQWVkMC4bB+Nswmy1b7V6j3zzxOdpMHQSDDVImWYVDNW1pio6fiXrHEYtzDnXUsEyiT+hARVHTKu7pwBLgmUaSpkFjqWCRcXarxa032qM/fwGoOMhEOI0Q6rYnG+qDtIAWV9dig7FU0RuEX5YfHfP7oPr11kfDYuaAiXWzJOEKYMspYyJSduGfSRtgFHL0rymoLunb0lnL7+BWXekWsG1erl+nqyNs+y++xMc7vyMfHzp0t7fPljfw5a6kbqEFajnj/Sx0Dnr0g4uZMDX9kw+75xpRhalfRw0keuMsn/+AzG6d/ZSLamXamFjRgCsJgF0aovBXYG0/d1Ix0ciO8RnmeiA1XCmXVsowSY6qjV1GT352R9nX3vlWZ15ruNHIQifLh0NuDpdKoYzEkN3cKE7uGIHWX318BDt4aOuGrTeLjnaGfj1jQQtJoIWeyrDCuDYWOnF3CtQMrZn7gFU8ujoNKdDR5K9CUQxEaJD5QgILjGLCBEiIBKziKCctFmAwJ7aLGCQmIhitwiuKwbXQVqvv3eD+To0olVMkCTmxjv9l9R/e/mmcqf9tt5+OT9//4/uCz/slBOM1r1Tkkyd4uJn2mLzhZPnopP3lOryv6nnZ5A4AC4JgOsBoJwE0KkNBuO0jbW+/3v5iIsO4LoFdMjXs+PmuxEbFRQtbc/uHJEwUlwDbctNfa65++njFHn82O5/+v/am7YiJDhjVxSWNcqd4eL/LXfOd51jvSPdHXDxQkLQFlyc8ERPx6d3MDlPuB32qeD6jLYoTjBZHMBCEsBClEuXIG4aUP6nXdrFhGRIXQzXE4200B9Sh6uH7eUqSMbAHGr06PaetncCPnYj+l0wJ5JWysC2bW8y9bXN4I6pax46ZxpgJmZfYQ4JMjSQSN8TIAWp9sbSsXdOnD553+nn2DZDTmRB1jfu9mj0W5dTUJ9l7wgeTnhHQILyFUXHbXpw7Mtnvr/4zUcfsm1hj04MRj0xMWGI5ezWMx1TD29vYBFrXLj9/tKj337yM9sDHKVbYZqKVPd1i7fvFd43GbbwfLSSXqQ7Y54TUj/R4frfHz0s++kaAAA="
}
}

