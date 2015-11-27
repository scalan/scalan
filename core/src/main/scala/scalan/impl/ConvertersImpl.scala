package scalan

import scalan.staged.Expressions
import scalan.common.Lazy
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
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("T" -> Left(eT), "R" -> Left(eR))
    }
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
        case e => !!!(s"Expected $x to have ConverterElem[_, _, _], but got $e")
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

  abstract class AbsBaseConverter[T, R]
      (convFun: Rep[T => R])(implicit eT: Elem[T], eR: Elem[R])
    extends BaseConverter[T, R](convFun) with Def[BaseConverter[T, R]] {
    lazy val selfType = element[BaseConverter[T, R]]
  }
  // elem for concrete class
  class BaseConverterElem[T, R](val iso: Iso[BaseConverterData[T, R], BaseConverter[T, R]])(implicit eT: Elem[T], eR: Elem[R])
    extends ConverterElem[T, R, BaseConverter[T, R]]
    with ConcreteElem[BaseConverterData[T, R], BaseConverter[T, R]] {
    override lazy val parent: Option[Elem[_]] = Some(converterElement(element[T], element[R]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("T" -> Left(eT), "R" -> Left(eR))
    }

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
    extends IsoUR[BaseConverterData[T, R], BaseConverter[T, R]] with Def[BaseConverterIso[T, R]] {
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
  }
  // 4) constructor and deconstructor
  class BaseConverterCompanionAbs extends CompanionDef[BaseConverterCompanionAbs] with BaseConverterCompanion {
    def selfType = BaseConverterCompanionElem
    override def toString = "BaseConverter"

    def apply[T, R](convFun: Rep[T => R])(implicit eT: Elem[T], eR: Elem[R]): Rep[BaseConverter[T, R]] =
      mkBaseConverter(convFun)
  }
  object BaseConverterMatcher {
    def unapply[T, R](p: Rep[Converter[T, R]]) = unmkBaseConverter(p)
  }
  lazy val BaseConverter: Rep[BaseConverterCompanionAbs] = new BaseConverterCompanionAbs
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
  class PairConverterElem[A1, A2, B1, B2](val iso: Iso[PairConverterData[A1, A2, B1, B2], PairConverter[A1, A2, B1, B2]])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends ConverterElem[(A1, A2), (B1, B2), PairConverter[A1, A2, B1, B2]]
    with ConcreteElem[PairConverterData[A1, A2, B1, B2], PairConverter[A1, A2, B1, B2]] {
    override lazy val parent: Option[Elem[_]] = Some(converterElement(pairElement(element[A1],element[A2]), pairElement(element[B1],element[B2])))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A1" -> Left(eA1), "A2" -> Left(eA2), "B1" -> Left(eB1), "B2" -> Left(eB2))
    }

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
    extends IsoUR[PairConverterData[A1, A2, B1, B2], PairConverter[A1, A2, B1, B2]] with Def[PairConverterIso[A1, A2, B1, B2]] {
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
  }
  // 4) constructor and deconstructor
  class PairConverterCompanionAbs extends CompanionDef[PairConverterCompanionAbs] with PairConverterCompanion {
    def selfType = PairConverterCompanionElem
    override def toString = "PairConverter"
    def apply[A1, A2, B1, B2](p: Rep[PairConverterData[A1, A2, B1, B2]])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[PairConverter[A1, A2, B1, B2]] =
      isoPairConverter(eA1, eA2, eB1, eB2).to(p)
    def apply[A1, A2, B1, B2](conv1: Conv[A1, B1], conv2: Conv[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[PairConverter[A1, A2, B1, B2]] =
      mkPairConverter(conv1, conv2)
  }
  object PairConverterMatcher {
    def unapply[A1, A2, B1, B2](p: Rep[Converter[(A1, A2), (B1, B2)]]) = unmkPairConverter(p)
  }
  lazy val PairConverter: Rep[PairConverterCompanionAbs] = new PairConverterCompanionAbs
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
  class SumConverterElem[A1, A2, B1, B2](val iso: Iso[SumConverterData[A1, A2, B1, B2], SumConverter[A1, A2, B1, B2]])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends ConverterElem[$bar[A1, A2], $bar[B1, B2], SumConverter[A1, A2, B1, B2]]
    with ConcreteElem[SumConverterData[A1, A2, B1, B2], SumConverter[A1, A2, B1, B2]] {
    override lazy val parent: Option[Elem[_]] = Some(converterElement(sumElement(element[A1],element[A2]), sumElement(element[B1],element[B2])))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A1" -> Left(eA1), "A2" -> Left(eA2), "B1" -> Left(eB1), "B2" -> Left(eB2))
    }

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
    extends IsoUR[SumConverterData[A1, A2, B1, B2], SumConverter[A1, A2, B1, B2]] with Def[SumConverterIso[A1, A2, B1, B2]] {
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
  }
  // 4) constructor and deconstructor
  class SumConverterCompanionAbs extends CompanionDef[SumConverterCompanionAbs] with SumConverterCompanion {
    def selfType = SumConverterCompanionElem
    override def toString = "SumConverter"
    def apply[A1, A2, B1, B2](p: Rep[SumConverterData[A1, A2, B1, B2]])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[SumConverter[A1, A2, B1, B2]] =
      isoSumConverter(eA1, eA2, eB1, eB2).to(p)
    def apply[A1, A2, B1, B2](conv1: Conv[A1, B1], conv2: Conv[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[SumConverter[A1, A2, B1, B2]] =
      mkSumConverter(conv1, conv2)
  }
  object SumConverterMatcher {
    def unapply[A1, A2, B1, B2](p: Rep[Converter[$bar[A1, A2], $bar[B1, B2]]]) = unmkSumConverter(p)
  }
  lazy val SumConverter: Rep[SumConverterCompanionAbs] = new SumConverterCompanionAbs
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

  abstract class AbsFunctorConverter[A, B, F[_]]
      (itemConv: Conv[A, B])(implicit eA: Elem[A], eB: Elem[B], F: Functor[F])
    extends FunctorConverter[A, B, F](itemConv) with Def[FunctorConverter[A, B, F]] {
    lazy val selfType = element[FunctorConverter[A, B, F]]
  }
  // elem for concrete class
  class FunctorConverterElem[A, B, F[_]](val iso: Iso[FunctorConverterData[A, B, F], FunctorConverter[A, B, F]])(implicit eA: Elem[A], eB: Elem[B], F: Functor[F])
    extends ConverterElem[F[A], F[B], FunctorConverter[A, B, F]]
    with ConcreteElem[FunctorConverterData[A, B, F], FunctorConverter[A, B, F]] {
    override lazy val parent: Option[Elem[_]] = Some(converterElement(element[F[A]], element[F[B]]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eA), "B" -> Left(eB))
    }

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
    extends IsoUR[FunctorConverterData[A, B, F], FunctorConverter[A, B, F]] with Def[FunctorConverterIso[A, B, F]] {
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
  }
  // 4) constructor and deconstructor
  class FunctorConverterCompanionAbs extends CompanionDef[FunctorConverterCompanionAbs] with FunctorConverterCompanion {
    def selfType = FunctorConverterCompanionElem
    override def toString = "FunctorConverter"

    def apply[A, B, F[_]](itemConv: Conv[A, B])(implicit eA: Elem[A], eB: Elem[B], F: Functor[F]): Rep[FunctorConverter[A, B, F]] =
      mkFunctorConverter(itemConv)
  }
  object FunctorConverterMatcher {
    def unapply[A, B, F[_]](p: Rep[Converter[F[A], F[B]]]) = unmkFunctorConverter(p)
  }
  lazy val FunctorConverter: Rep[FunctorConverterCompanionAbs] = new FunctorConverterCompanionAbs
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

  registerModule(Converters_Module)
}

// Seq -----------------------------------
trait ConvertersSeq extends ConvertersDsl {
  self: ScalanSeq =>
  lazy val Converter: Rep[ConverterCompanionAbs] = new ConverterCompanionAbs {
  }

  case class SeqBaseConverter[T, R]
      (override val convFun: Rep[T => R])(implicit eT: Elem[T], eR: Elem[R])
    extends AbsBaseConverter[T, R](convFun) {
  }

  def mkBaseConverter[T, R]
    (convFun: Rep[T => R])(implicit eT: Elem[T], eR: Elem[R]): Rep[BaseConverter[T, R]] =
    new SeqBaseConverter[T, R](convFun)
  def unmkBaseConverter[T, R](p: Rep[Converter[T, R]]) = p match {
    case p: BaseConverter[T, R] @unchecked =>
      Some((p.convFun))
    case _ => None
  }

  case class SeqPairConverter[A1, A2, B1, B2]
      (override val conv1: Conv[A1, B1], override val conv2: Conv[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends AbsPairConverter[A1, A2, B1, B2](conv1, conv2) {
  }

  def mkPairConverter[A1, A2, B1, B2]
    (conv1: Conv[A1, B1], conv2: Conv[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[PairConverter[A1, A2, B1, B2]] =
    new SeqPairConverter[A1, A2, B1, B2](conv1, conv2)
  def unmkPairConverter[A1, A2, B1, B2](p: Rep[Converter[(A1, A2), (B1, B2)]]) = p match {
    case p: PairConverter[A1, A2, B1, B2] @unchecked =>
      Some((p.conv1, p.conv2))
    case _ => None
  }

  case class SeqSumConverter[A1, A2, B1, B2]
      (override val conv1: Conv[A1, B1], override val conv2: Conv[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends AbsSumConverter[A1, A2, B1, B2](conv1, conv2) {
  }

  def mkSumConverter[A1, A2, B1, B2]
    (conv1: Conv[A1, B1], conv2: Conv[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[SumConverter[A1, A2, B1, B2]] =
    new SeqSumConverter[A1, A2, B1, B2](conv1, conv2)
  def unmkSumConverter[A1, A2, B1, B2](p: Rep[Converter[$bar[A1, A2], $bar[B1, B2]]]) = p match {
    case p: SumConverter[A1, A2, B1, B2] @unchecked =>
      Some((p.conv1, p.conv2))
    case _ => None
  }

  case class SeqFunctorConverter[A, B, F[_]]
      (override val itemConv: Conv[A, B])(implicit eA: Elem[A], eB: Elem[B], F: Functor[F])
    extends AbsFunctorConverter[A, B, F](itemConv) {
  }

  def mkFunctorConverter[A, B, F[_]]
    (itemConv: Conv[A, B])(implicit eA: Elem[A], eB: Elem[B], F: Functor[F]): Rep[FunctorConverter[A, B, F]] =
    new SeqFunctorConverter[A, B, F](itemConv)
  def unmkFunctorConverter[A, B, F[_]](p: Rep[Converter[F[A], F[B]]]) = p match {
    case p: FunctorConverter[A, B, F] @unchecked =>
      Some((p.itemConv))
    case _ => None
  }
}

// Exp -----------------------------------
trait ConvertersExp extends ConvertersDsl {
  self: ScalanExp =>
  lazy val Converter: Rep[ConverterCompanionAbs] = new ConverterCompanionAbs {
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

    // WARNING: Cannot generate matcher for method `toString`: Method's return type String is not a Rep

    // WARNING: Cannot generate matcher for method `equals`: Method's return type Boolean is not a Rep
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

    // WARNING: Cannot generate matcher for method `toString`: Method's return type String is not a Rep

    // WARNING: Cannot generate matcher for method `equals`: Method's return type Boolean is not a Rep
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
  }

  object ConverterCompanionMethods {
  }
}

object Converters_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAAO1YP2wbVRh/Z8dxbIcmtLTQVm3SyICoiN1YSB0yVL40gSI3iXwZkKkaPZ9f0iv3L3fPkc1QMVUINsTCgEQlFqQuiAkhIRYkxMBUISQmBqZSVHWgYgDxvXd/fHe+OztELKgeTvfuvve97/v9Ob3nuw9QzrbQC7aMVaxXNEJxReL3dZuWpVWdKrR/1eh0VXKZ7DxnfPPJ0menvsygmRaavIHty7baQgXnZrVn+vcS2WugAtZlYlPDsik61+ArVGVDVYlMFUOvKprWpbitkmpDselyA020jU5/D91CQgPNyoYuW4QSaUXFtk1s9/kUYRUp/rjAx/0Nc7CGXmVdVANdbFlYoVA+rDHrxDeJKfV1Q+9rFB1xS9swWVkQk1c007Cot0Qe0t0wOt5wQsfwAB1t3MT7uApL7FYlain6LswsmVh+C++SdQhh4RNQsE3Una2+ycfZBiraZA8AuqKZKn/SMxFCwECNF1EZ4FPx8akwfMoSsRSsKm9j9nLTMnp95PyELEI9E1K8PCKFl4Gs6p3ye9fkNx9LJS3DJvdYKXne4SQkmktQA6cCcPyu+YH96NU7FzOo2EJFxa63bWphmQYpd9EqYV03KK/ZBxBbu8DWQhJbfJU6xEQkUZANzcQ6ZHKhnAaeVEVWKAtmz6ZddhKgz1OTeKFCzxT8fucT+uW6WcGqunn/5OLzv62+kUGZ8BIFSCmB8C0vKUWFFUPfJxYllpufXWcoErYGILNhkw/ZpdAbXPMp5fjAvHj/9863F9C1jA+nu/p4DEKKnP3Tj6V7L13KoKkW1/uaindbgKi9qhJtw4IeaAtNGdCI8ya/j1V2F8tovkN2cFelLs5BgLIAEEXzic40CUNvmbtA8AAoOUJeN3RSXtss/yF9/+FdplMLTTtvHKv+rVz86+cjO5RLmKI8fDL217q6h3AWTO4DcjaJYpPAFPnelY+OzZzZ/oUTPNkxNKxwlZ1uoJwFFuftnHYBPhCZRadiydDI0wuPlOt33qecNqEX/oZstG+CaZf5vLMpDHrfss9v3z7+8NPtY9yDU22FatgsXziAAz3D/IcOQz4qDlLPDsbsMgeknRCxTXzHrATXnwtMDFjplOAphQdRlCFbHg8TTL2xtnNISUrQTEswzC1FT4XK5nl8qZ1Jlhpgc6LZeEZ9cOnrDMq9jnI7YCkbNNY2unrHAx10TEmPit4zIQw6gIwtrPkg8988GkAWludKbEAzikpJCLc99BlLkaRJtrqmSl756s/r777zmsn1PfRlDKfP1JdCDsrUa9GKrkZmiJEZYnDGsPWGdIMitOfY92LJZ571O6JGt4KglmKT1lKT1sZqwwk4z6+L4xhpEyvW4YyUJfUBHMNG8OBIsxKkqKWmGGI5JoWYWkUMCTEpUquIgRwsHQIwaOlEF4UEkhBRGxUhjswhDoE20qxe8+U2tuJZTKEkYeIT/6X577jU1Z7Y79/bbzqI3//DfcJaZFyPcdpQkHg4V00plGi+7OM94BYyYsmDOuAk20jDxvOwu7l6inaGIYxJIKYlEEcmcBmB4LzbUZTWGPnORpsfS8JBIuIDxFEBayPX2E4QVGDiIgq3mIUDxpgaH+sgFFJvREnjC+xomrJCJ+/xwD/QJtm/CpVBjBtYHIBDYUW+M3Z7s9BCwq5Zco9OAPWtxx+vn//hi1/56bPIDmFw8tX9P6AGm//oLnrSyRcoE0TPzmS8xH8AbIcRXNwTAAA="
}
}

