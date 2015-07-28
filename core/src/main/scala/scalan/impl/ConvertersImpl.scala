package scalan

import scalan.staged.Expressions
import scalan.common.Lazy
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}

package impl {
// Abs -----------------------------------
trait ConvertersAbs extends Converters  {
  self: Scalan =>

  // single proxy for each type family
  implicit def proxyConverter[T, R](p: Rep[Converter[T, R]]): Converter[T, R] = {
    proxyOps[Converter[T, R]](p)(scala.reflect.classTag[Converter[T, R]])
  }

  // familyElem
  class ConverterElem[T, R, To <: Converter[T, R]](implicit val eT: Elem[T], val eR: Elem[R])
    extends EntityElem[To] {
    val parent: Option[Elem[_]] = None
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagT = eT.tag
      implicit val tagR = eR.tag
      weakTypeTag[Converter[T, R]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Reifiable[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[Converter[T, R]] => convertConverter(x) }
      tryConvert(element[Converter[T, R]], this, x, conv)
    }

    def convertConverter(x : Rep[Converter[T, R]]): Rep[To] = {
      assert(x.selfType1 match { case _: ConverterElem[_, _, _] => true; case _ => false })
      x.asRep[To]
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def converterElement[T, R](implicit eT: Elem[T], eR: Elem[R]): Elem[Converter[T, R]] =
    new ConverterElem[T, R, Converter[T, R]]

  implicit case object ConverterCompanionElem extends CompanionElem[ConverterCompanionAbs] {
    lazy val tag = weakTypeTag[ConverterCompanionAbs]
    protected def getDefaultRep = Converter
  }

  abstract class ConverterCompanionAbs extends CompanionBase[ConverterCompanionAbs] with ConverterCompanion {
    override def toString = "Converter"
  }
  def Converter: Rep[ConverterCompanionAbs]
  implicit def proxyConverterCompanion(p: Rep[ConverterCompanion]): ConverterCompanion =
    proxyOps[ConverterCompanion](p)

  // elem for concrete class
  class BaseConverterElem[T, R](val iso: Iso[BaseConverterData[T, R], BaseConverter[T, R]])(implicit eT: Elem[T], eR: Elem[R])
    extends ConverterElem[T, R, BaseConverter[T, R]]
    with ConcreteElem[BaseConverterData[T, R], BaseConverter[T, R]] {
    override val parent: Option[Elem[_]] = Some(converterElement(element[T], element[R]))

    override def convertConverter(x: Rep[Converter[T, R]]) = BaseConverter(x.convFun)
    override def getDefaultRep = super[ConcreteElem].getDefaultRep
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
    extends Iso[BaseConverterData[T, R], BaseConverter[T, R]] {
    override def from(p: Rep[BaseConverter[T, R]]) =
      p.convFun
    override def to(p: Rep[T => R]) = {
      val convFun = p
      BaseConverter(convFun)
    }
    lazy val defaultRepTo: Rep[BaseConverter[T, R]] = BaseConverter(fun { (x: Rep[T]) => element[R].defaultRepValue })
    lazy val eTo = new BaseConverterElem[T, R](this)
  }
  // 4) constructor and deconstructor
  abstract class BaseConverterCompanionAbs extends CompanionBase[BaseConverterCompanionAbs] with BaseConverterCompanion {
    override def toString = "BaseConverter"

    def apply[T, R](convFun: Rep[T => R])(implicit eT: Elem[T], eR: Elem[R]): Rep[BaseConverter[T, R]] =
      mkBaseConverter(convFun)
  }
  object BaseConverterMatcher {
    def unapply[T, R](p: Rep[Converter[T, R]]) = unmkBaseConverter(p)
  }
  def BaseConverter: Rep[BaseConverterCompanionAbs]
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
    new BaseConverterIso[T, R]

  // 6) smart constructor and deconstructor
  def mkBaseConverter[T, R](convFun: Rep[T => R])(implicit eT: Elem[T], eR: Elem[R]): Rep[BaseConverter[T, R]]
  def unmkBaseConverter[T, R](p: Rep[Converter[T, R]]): Option[(Rep[T => R])]

  // elem for concrete class
  class PairConverterElem[A1, A2, B1, B2](val iso: Iso[PairConverterData[A1, A2, B1, B2], PairConverter[A1, A2, B1, B2]])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends ConverterElem[(A1, A2), (B1, B2), PairConverter[A1, A2, B1, B2]]
    with ConcreteElem[PairConverterData[A1, A2, B1, B2], PairConverter[A1, A2, B1, B2]] {
    override val parent: Option[Elem[_]] = Some(converterElement(pairElement(element[A1],element[A2]), pairElement(element[B1],element[B2])))

    override def convertConverter(x: Rep[Converter[(A1, A2), (B1, B2)]]) = // Converter is not generated by meta
!!!("Cannot convert from Converter to PairConverter: missing fields List(conv1, conv2)")
    override def getDefaultRep = super[ConcreteElem].getDefaultRep
    override lazy val tag = {
      implicit val tagA1 = eA1.tag
      implicit val tagA2 = eA2.tag
      implicit val tagB1 = eB1.tag
      implicit val tagB2 = eB2.tag
      weakTypeTag[PairConverter[A1, A2, B1, B2]]
    }
  }

  // state representation type
  type PairConverterData[A1, A2, B1, B2] = (Converter[A1,B1], Converter[A2,B2])

  // 3) Iso for concrete class
  class PairConverterIso[A1, A2, B1, B2](implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends Iso[PairConverterData[A1, A2, B1, B2], PairConverter[A1, A2, B1, B2]]()(pairElement(implicitly[Elem[Converter[A1,B1]]], implicitly[Elem[Converter[A2,B2]]])) {
    override def from(p: Rep[PairConverter[A1, A2, B1, B2]]) =
      (p.conv1, p.conv2)
    override def to(p: Rep[(Converter[A1,B1], Converter[A2,B2])]) = {
      val Pair(conv1, conv2) = p
      PairConverter(conv1, conv2)
    }
    lazy val defaultRepTo: Rep[PairConverter[A1, A2, B1, B2]] = PairConverter(element[Converter[A1,B1]].defaultRepValue, element[Converter[A2,B2]].defaultRepValue)
    lazy val eTo = new PairConverterElem[A1, A2, B1, B2](this)
  }
  // 4) constructor and deconstructor
  abstract class PairConverterCompanionAbs extends CompanionBase[PairConverterCompanionAbs] with PairConverterCompanion {
    override def toString = "PairConverter"
    def apply[A1, A2, B1, B2](p: Rep[PairConverterData[A1, A2, B1, B2]])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[PairConverter[A1, A2, B1, B2]] =
      isoPairConverter(eA1, eA2, eB1, eB2).to(p)
    def apply[A1, A2, B1, B2](conv1: Conv[A1,B1], conv2: Conv[A2,B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[PairConverter[A1, A2, B1, B2]] =
      mkPairConverter(conv1, conv2)
  }
  object PairConverterMatcher {
    def unapply[A1, A2, B1, B2](p: Rep[Converter[(A1, A2), (B1, B2)]]) = unmkPairConverter(p)
  }
  def PairConverter: Rep[PairConverterCompanionAbs]
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
    new PairConverterIso[A1, A2, B1, B2]

  // 6) smart constructor and deconstructor
  def mkPairConverter[A1, A2, B1, B2](conv1: Conv[A1,B1], conv2: Conv[A2,B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[PairConverter[A1, A2, B1, B2]]
  def unmkPairConverter[A1, A2, B1, B2](p: Rep[Converter[(A1, A2), (B1, B2)]]): Option[(Rep[Converter[A1,B1]], Rep[Converter[A2,B2]])]

  // elem for concrete class
  class SumConverterElem[A1, A2, B1, B2](val iso: Iso[SumConverterData[A1, A2, B1, B2], SumConverter[A1, A2, B1, B2]])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends ConverterElem[$bar[A1,A2], $bar[B1,B2], SumConverter[A1, A2, B1, B2]]
    with ConcreteElem[SumConverterData[A1, A2, B1, B2], SumConverter[A1, A2, B1, B2]] {
    override val parent: Option[Elem[_]] = Some(converterElement(sumElement(element[A1],element[A2]), sumElement(element[B1],element[B2])))

    override def convertConverter(x: Rep[Converter[$bar[A1,A2], $bar[B1,B2]]]) = // Converter is not generated by meta
!!!("Cannot convert from Converter to SumConverter: missing fields List(conv1, conv2)")
    override def getDefaultRep = super[ConcreteElem].getDefaultRep
    override lazy val tag = {
      implicit val tagA1 = eA1.tag
      implicit val tagA2 = eA2.tag
      implicit val tagB1 = eB1.tag
      implicit val tagB2 = eB2.tag
      weakTypeTag[SumConverter[A1, A2, B1, B2]]
    }
  }

  // state representation type
  type SumConverterData[A1, A2, B1, B2] = (Converter[A1,B1], Converter[A2,B2])

  // 3) Iso for concrete class
  class SumConverterIso[A1, A2, B1, B2](implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends Iso[SumConverterData[A1, A2, B1, B2], SumConverter[A1, A2, B1, B2]]()(pairElement(implicitly[Elem[Converter[A1,B1]]], implicitly[Elem[Converter[A2,B2]]])) {
    override def from(p: Rep[SumConverter[A1, A2, B1, B2]]) =
      (p.conv1, p.conv2)
    override def to(p: Rep[(Converter[A1,B1], Converter[A2,B2])]) = {
      val Pair(conv1, conv2) = p
      SumConverter(conv1, conv2)
    }
    lazy val defaultRepTo: Rep[SumConverter[A1, A2, B1, B2]] = SumConverter(element[Converter[A1,B1]].defaultRepValue, element[Converter[A2,B2]].defaultRepValue)
    lazy val eTo = new SumConverterElem[A1, A2, B1, B2](this)
  }
  // 4) constructor and deconstructor
  abstract class SumConverterCompanionAbs extends CompanionBase[SumConverterCompanionAbs] with SumConverterCompanion {
    override def toString = "SumConverter"
    def apply[A1, A2, B1, B2](p: Rep[SumConverterData[A1, A2, B1, B2]])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[SumConverter[A1, A2, B1, B2]] =
      isoSumConverter(eA1, eA2, eB1, eB2).to(p)
    def apply[A1, A2, B1, B2](conv1: Conv[A1,B1], conv2: Conv[A2,B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[SumConverter[A1, A2, B1, B2]] =
      mkSumConverter(conv1, conv2)
  }
  object SumConverterMatcher {
    def unapply[A1, A2, B1, B2](p: Rep[Converter[$bar[A1,A2], $bar[B1,B2]]]) = unmkSumConverter(p)
  }
  def SumConverter: Rep[SumConverterCompanionAbs]
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
    new SumConverterIso[A1, A2, B1, B2]

  // 6) smart constructor and deconstructor
  def mkSumConverter[A1, A2, B1, B2](conv1: Conv[A1,B1], conv2: Conv[A2,B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[SumConverter[A1, A2, B1, B2]]
  def unmkSumConverter[A1, A2, B1, B2](p: Rep[Converter[$bar[A1,A2], $bar[B1,B2]]]): Option[(Rep[Converter[A1,B1]], Rep[Converter[A2,B2]])]

  // elem for concrete class
  class FunctorConverterElem[A, B, F[_]](val iso: Iso[FunctorConverterData[A, B, F], FunctorConverter[A, B, F]])(implicit eA: Elem[A], eB: Elem[B], F: Functor[F])
    extends ConverterElem[F[A], F[B], FunctorConverter[A, B, F]]
    with ConcreteElem[FunctorConverterData[A, B, F], FunctorConverter[A, B, F]] {
    override val parent: Option[Elem[_]] = Some(converterElement(element[F[A]], element[F[B]]))

    override def convertConverter(x: Rep[Converter[F[A], F[B]]]) = // Converter is not generated by meta
!!!("Cannot convert from Converter to FunctorConverter: missing fields List(itemConv)")
    override def getDefaultRep = super[ConcreteElem].getDefaultRep
    override lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[FunctorConverter[A, B, F]]
    }
  }

  // state representation type
  type FunctorConverterData[A, B, F[_]] = Converter[A,B]

  // 3) Iso for concrete class
  class FunctorConverterIso[A, B, F[_]](implicit eA: Elem[A], eB: Elem[B], F: Functor[F])
    extends Iso[FunctorConverterData[A, B, F], FunctorConverter[A, B, F]] {
    override def from(p: Rep[FunctorConverter[A, B, F]]) =
      p.itemConv
    override def to(p: Rep[Converter[A,B]]) = {
      val itemConv = p
      FunctorConverter(itemConv)
    }
    lazy val defaultRepTo: Rep[FunctorConverter[A, B, F]] = FunctorConverter(element[Converter[A,B]].defaultRepValue)
    lazy val eTo = new FunctorConverterElem[A, B, F](this)
  }
  // 4) constructor and deconstructor
  abstract class FunctorConverterCompanionAbs extends CompanionBase[FunctorConverterCompanionAbs] with FunctorConverterCompanion {
    override def toString = "FunctorConverter"

    def apply[A, B, F[_]](itemConv: Conv[A,B])(implicit eA: Elem[A], eB: Elem[B], F: Functor[F]): Rep[FunctorConverter[A, B, F]] =
      mkFunctorConverter(itemConv)
  }
  object FunctorConverterMatcher {
    def unapply[A, B, F[_]](p: Rep[Converter[F[A], F[B]]]) = unmkFunctorConverter(p)
  }
  def FunctorConverter: Rep[FunctorConverterCompanionAbs]
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
    new FunctorConverterIso[A, B, F]

  // 6) smart constructor and deconstructor
  def mkFunctorConverter[A, B, F[_]](itemConv: Conv[A,B])(implicit eA: Elem[A], eB: Elem[B], F: Functor[F]): Rep[FunctorConverter[A, B, F]]
  def unmkFunctorConverter[A, B, F[_]](p: Rep[Converter[F[A], F[B]]]): Option[(Rep[Converter[A,B]])]

  registerModule(scalan.meta.ScalanCodegen.loadModule(Converters_Module.dump))
}

// Seq -----------------------------------
trait ConvertersSeq extends ConvertersDsl  {
  self: ScalanSeq =>
  lazy val Converter: Rep[ConverterCompanionAbs] = new ConverterCompanionAbs with UserTypeSeq[ConverterCompanionAbs] {
    lazy val selfType = element[ConverterCompanionAbs]
  }

  case class SeqBaseConverter[T, R]
      (override val convFun: Rep[T => R])
      (implicit eT: Elem[T], eR: Elem[R])
    extends BaseConverter[T, R](convFun)
        with UserTypeSeq[BaseConverter[T, R]] {
    lazy val selfType = element[BaseConverter[T, R]]
  }
  lazy val BaseConverter = new BaseConverterCompanionAbs with UserTypeSeq[BaseConverterCompanionAbs] {
    lazy val selfType = element[BaseConverterCompanionAbs]
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
      (override val conv1: Conv[A1,B1], override val conv2: Conv[A2,B2])
      (implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends PairConverter[A1, A2, B1, B2](conv1, conv2)
        with UserTypeSeq[PairConverter[A1, A2, B1, B2]] {
    lazy val selfType = element[PairConverter[A1, A2, B1, B2]]
  }
  lazy val PairConverter = new PairConverterCompanionAbs with UserTypeSeq[PairConverterCompanionAbs] {
    lazy val selfType = element[PairConverterCompanionAbs]
  }

  def mkPairConverter[A1, A2, B1, B2]
      (conv1: Conv[A1,B1], conv2: Conv[A2,B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[PairConverter[A1, A2, B1, B2]] =
      new SeqPairConverter[A1, A2, B1, B2](conv1, conv2)
  def unmkPairConverter[A1, A2, B1, B2](p: Rep[Converter[(A1, A2), (B1, B2)]]) = p match {
    case p: PairConverter[A1, A2, B1, B2] @unchecked =>
      Some((p.conv1, p.conv2))
    case _ => None
  }

  case class SeqSumConverter[A1, A2, B1, B2]
      (override val conv1: Conv[A1,B1], override val conv2: Conv[A2,B2])
      (implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends SumConverter[A1, A2, B1, B2](conv1, conv2)
        with UserTypeSeq[SumConverter[A1, A2, B1, B2]] {
    lazy val selfType = element[SumConverter[A1, A2, B1, B2]]
  }
  lazy val SumConverter = new SumConverterCompanionAbs with UserTypeSeq[SumConverterCompanionAbs] {
    lazy val selfType = element[SumConverterCompanionAbs]
  }

  def mkSumConverter[A1, A2, B1, B2]
      (conv1: Conv[A1,B1], conv2: Conv[A2,B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[SumConverter[A1, A2, B1, B2]] =
      new SeqSumConverter[A1, A2, B1, B2](conv1, conv2)
  def unmkSumConverter[A1, A2, B1, B2](p: Rep[Converter[$bar[A1,A2], $bar[B1,B2]]]) = p match {
    case p: SumConverter[A1, A2, B1, B2] @unchecked =>
      Some((p.conv1, p.conv2))
    case _ => None
  }

  case class SeqFunctorConverter[A, B, F[_]]
      (override val itemConv: Conv[A,B])
      (implicit eA: Elem[A], eB: Elem[B], F: Functor[F])
    extends FunctorConverter[A, B, F](itemConv)
        with UserTypeSeq[FunctorConverter[A, B, F]] {
    lazy val selfType = element[FunctorConverter[A, B, F]]
  }
  lazy val FunctorConverter = new FunctorConverterCompanionAbs with UserTypeSeq[FunctorConverterCompanionAbs] {
    lazy val selfType = element[FunctorConverterCompanionAbs]
  }

  def mkFunctorConverter[A, B, F[_]]
      (itemConv: Conv[A,B])(implicit eA: Elem[A], eB: Elem[B], F: Functor[F]): Rep[FunctorConverter[A, B, F]] =
      new SeqFunctorConverter[A, B, F](itemConv)
  def unmkFunctorConverter[A, B, F[_]](p: Rep[Converter[F[A], F[B]]]) = p match {
    case p: FunctorConverter[A, B, F] @unchecked =>
      Some((p.itemConv))
    case _ => None
  }
}

// Exp -----------------------------------
trait ConvertersExp extends ConvertersDsl  {
  self: ScalanExp =>
  lazy val Converter: Rep[ConverterCompanionAbs] = new ConverterCompanionAbs with UserTypeDef[ConverterCompanionAbs] {
    lazy val selfType = element[ConverterCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  case class ExpBaseConverter[T, R]
      (override val convFun: Rep[T => R])
      (implicit eT: Elem[T], eR: Elem[R])
    extends BaseConverter[T, R](convFun) with UserTypeDef[BaseConverter[T, R]] {
    lazy val selfType = element[BaseConverter[T, R]]
    override def mirror(t: Transformer) = ExpBaseConverter[T, R](t(convFun))
  }

  lazy val BaseConverter: Rep[BaseConverterCompanionAbs] = new BaseConverterCompanionAbs with UserTypeDef[BaseConverterCompanionAbs] {
    lazy val selfType = element[BaseConverterCompanionAbs]
    override def mirror(t: Transformer) = this
  }

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
      (override val conv1: Conv[A1,B1], override val conv2: Conv[A2,B2])
      (implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends PairConverter[A1, A2, B1, B2](conv1, conv2) with UserTypeDef[PairConverter[A1, A2, B1, B2]] {
    lazy val selfType = element[PairConverter[A1, A2, B1, B2]]
    override def mirror(t: Transformer) = ExpPairConverter[A1, A2, B1, B2](t(conv1), t(conv2))
  }

  lazy val PairConverter: Rep[PairConverterCompanionAbs] = new PairConverterCompanionAbs with UserTypeDef[PairConverterCompanionAbs] {
    lazy val selfType = element[PairConverterCompanionAbs]
    override def mirror(t: Transformer) = this
  }

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
    (conv1: Conv[A1,B1], conv2: Conv[A2,B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[PairConverter[A1, A2, B1, B2]] =
    new ExpPairConverter[A1, A2, B1, B2](conv1, conv2)
  def unmkPairConverter[A1, A2, B1, B2](p: Rep[Converter[(A1, A2), (B1, B2)]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: PairConverterElem[A1, A2, B1, B2] @unchecked =>
      Some((p.asRep[PairConverter[A1, A2, B1, B2]].conv1, p.asRep[PairConverter[A1, A2, B1, B2]].conv2))
    case _ =>
      None
  }

  case class ExpSumConverter[A1, A2, B1, B2]
      (override val conv1: Conv[A1,B1], override val conv2: Conv[A2,B2])
      (implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends SumConverter[A1, A2, B1, B2](conv1, conv2) with UserTypeDef[SumConverter[A1, A2, B1, B2]] {
    lazy val selfType = element[SumConverter[A1, A2, B1, B2]]
    override def mirror(t: Transformer) = ExpSumConverter[A1, A2, B1, B2](t(conv1), t(conv2))
  }

  lazy val SumConverter: Rep[SumConverterCompanionAbs] = new SumConverterCompanionAbs with UserTypeDef[SumConverterCompanionAbs] {
    lazy val selfType = element[SumConverterCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object SumConverterMethods {
    object apply {
      def unapply(d: Def[_]): Option[(Rep[SumConverter[A1, A2, B1, B2]], Rep[$bar[A1,A2]]) forSome {type A1; type A2; type B1; type B2}] = d match {
        case MethodCall(receiver, method, Seq(x, _*), _) if receiver.elem.isInstanceOf[SumConverterElem[_, _, _, _]] && method.getName == "apply" =>
          Some((receiver, x)).asInstanceOf[Option[(Rep[SumConverter[A1, A2, B1, B2]], Rep[$bar[A1,A2]]) forSome {type A1; type A2; type B1; type B2}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SumConverter[A1, A2, B1, B2]], Rep[$bar[A1,A2]]) forSome {type A1; type A2; type B1; type B2}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object SumConverterCompanionMethods {
  }

  def mkSumConverter[A1, A2, B1, B2]
    (conv1: Conv[A1,B1], conv2: Conv[A2,B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[SumConverter[A1, A2, B1, B2]] =
    new ExpSumConverter[A1, A2, B1, B2](conv1, conv2)
  def unmkSumConverter[A1, A2, B1, B2](p: Rep[Converter[$bar[A1,A2], $bar[B1,B2]]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: SumConverterElem[A1, A2, B1, B2] @unchecked =>
      Some((p.asRep[SumConverter[A1, A2, B1, B2]].conv1, p.asRep[SumConverter[A1, A2, B1, B2]].conv2))
    case _ =>
      None
  }

  case class ExpFunctorConverter[A, B, F[_]]
      (override val itemConv: Conv[A,B])
      (implicit eA: Elem[A], eB: Elem[B], F: Functor[F])
    extends FunctorConverter[A, B, F](itemConv) with UserTypeDef[FunctorConverter[A, B, F]] {
    lazy val selfType = element[FunctorConverter[A, B, F]]
    override def mirror(t: Transformer) = ExpFunctorConverter[A, B, F](t(itemConv))
  }

  lazy val FunctorConverter: Rep[FunctorConverterCompanionAbs] = new FunctorConverterCompanionAbs with UserTypeDef[FunctorConverterCompanionAbs] {
    lazy val selfType = element[FunctorConverterCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object FunctorConverterMethods {
    object convFun {
      def unapply(d: Def[_]): Option[Rep[FunctorConverter[A, B, F]] forSome {type A; type B; type F[_]}] = d match {
        case MethodCall(receiver, method, _, _) if (receiver.elem.asInstanceOf[Element[_]] match { case _: FunctorConverterElem[_, _, _] => true; case _ => false }) && method.getName == "convFun" =>
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
        case MethodCall(receiver, method, Seq(xs, _*), _) if (receiver.elem.asInstanceOf[Element[_]] match { case _: FunctorConverterElem[_, _, _] => true; case _ => false }) && method.getName == "apply" =>
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
    (itemConv: Conv[A,B])(implicit eA: Elem[A], eB: Elem[B], F: Functor[F]): Rep[FunctorConverter[A, B, F]] =
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

object Converters_Module {
  val packageName = "scalan"
  val name = "Converters"
  val dump = "H4sIAAAAAAAAAO1YS2wbRRieXSdxbIcmFAEKVZsSmSIiYqcWUpFyQN7EoZVMEnlzQG7VaryeJBv2ldlxZHOoOPIQl4oLQgj1wK03jpyREAdOFUXixIFTAYkKqDiAmJl9P2OIOCA1h5Vn559v/v/7v281kzs/gUkbgwu2AjVo1HREYE3mv5s2qcotg6hk9JrZH2hoHe2uH9x7GX+6/74IZrtgah/a67bWBSXnR2to+b9ldNgGJWgoyCYmtgl4ts13qCumpiGFqKZRV3V9QGBPQ/W2apPVNpjomf3RIbgJhDaYU0xDwYggeU2Dto1s9/00Yhmp/rjEx6MtK9jDqLMq6qEqdjBUCU2f7jHnxHeQJY8M0xjpBJxyU9uyWFo0pqjqlomJt0WRwu2bfW84YUD6ApxuH8AjWKdb7NVlglVjj66sWFB5A+6hTRrCwidowjbSdndGFh8X2qBso0NK0BXd0viboQUAoB1o8CRqAT81n58a46cqI6xCTX0TssltbA5HwPkTCgAMLQrx4jEQHgJqGf3qu9eUqw/lii6yxUOWSpFXOEWBFjLUwFtBefyyc8t+8OrtSyIod0FZtZs9m2CokHDLXbYq0DBMwnP2CYR4j3ZrMatbfJcmjYlJoqSYugUNiuRSOUP7pKmKSlgwezfjdieD+iKxkBcqDC3Br/d8Rr1cN2tQ07bvzy8/92PrdRGI0S1KFFKmwsceKAGlNdM4Qpgg7OKz5ywBwk5AMht2+JA9SsPgWcxJxyfm+fs/979YAddEn0539/E6SCEm7W+/qdx94RURTHe53jc0uNeljNotDelbmNZAumDapIU4M8UjqLFfqR0t9tEuHGjE5TlMUIESRMD5TGdaiLG3yl0geARUHCFvmgaqbmxXf5e/+uAO0ykGM86MY9W/1Et/fndql3AJE1Ckn4yjjYHhMVygJvcJOZfVYgvRJcrdKx8+MXv2xve8wVN9U4cqV9mZNpjE1OK8nDMuwf+omWUnY9nU0eOLD9Trt98jvG3CMPoN2eodUNOu8nXncjrofct+666Iv87f+0QEJdqonkp0aFVXxnTgf+gq4DMRPBZod56SoI18a6yFN10IuHw65JlnBE8SPIgAEe14hE8wmab6y2E/C6CTB5BsIgGPRdLmOL6mzmZrihJy6+qFy/iXj94RGWmTPXNg9D2GqVAJGhLJeydEGaaMQgx1j9GAoqjupMhEJ159RYiWl/gu5WjMQjsDS0Mvff7H9bffumxxwSY+dVF4sXkxYgmx2Yhn1I6tkGIrpPCKpJcS+gCx9k6yD8BFv8Os3mNydDMIayYVtJEL2hirDCdgiT+XM12yDVV8MpcUUDPgIKlyj4M8n1CIRi5EorUpEFJuFinMp0DkZpHCM/VrhMCwXxOWiaghNtPImpEy10gJUo51oFdctQdxepdyKM9Y+MhUCVM9KQ/0R576956aCfP3/7CUsBEbN1PskwiSTmaVaZUg3ddyurDdRI7ZcixZz7ODKz3onfRQ1cw7EyV4SwGQ8gCkYwHcNtDgoltRvJcpmpyLF5+ryzDr0Qkpa2IjE+tGhkpCC5ZBTKYdpO6q7CI+pnzHul64WhlbMKfzlBK5ueaTOdYZ1H8KS0GMG1gOyiZ0B37wdAvAYDHjUCq71w5697n58OPNpa8/+4Hf1srsAkNviob/D5vgLB0/pE45eKE0qWjZfYan+Ddm/XMhDBMAAA=="
}
}

