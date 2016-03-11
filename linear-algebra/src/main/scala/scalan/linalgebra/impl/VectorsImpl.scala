package scalan.linalgebra

import scalan._
import scalan.collections.{CollectionsDsl, CollectionsDslStd, CollectionsDslExp}
import scalan.common.OverloadHack.{Overloaded2, Overloaded1}
import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait VectorsAbs extends scalan.ScalanDsl with Vectors {
  self: LADsl =>

  // single proxy for each type family
  implicit def proxyVector[T](p: Rep[Vector[T]]): Vector[T] = {
    proxyOps[Vector[T]](p)(scala.reflect.classTag[Vector[T]])
  }

  // familyElem
  class VectorElem[T, To <: Vector[T]](implicit _eT: Elem[T])
    extends EntityElem[To] {
    def eT = _eT
    lazy val parent: Option[Elem[_]] = None
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("T" -> Left(eT))
    }
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[Vector[T]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[Vector[T]] => convertVector(x) }
      tryConvert(element[Vector[T]], this, x, conv)
    }

    def convertVector(x: Rep[Vector[T]]): Rep[To] = {
      x.selfType1 match {
        case _: VectorElem[_, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have VectorElem[_, _], but got $e", x)
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def vectorElement[T](implicit eT: Elem[T]): Elem[Vector[T]] =
    cachedElem[VectorElem[T, Vector[T]]](eT)

  implicit case object VectorCompanionElem extends CompanionElem[VectorCompanionAbs] {
    lazy val tag = weakTypeTag[VectorCompanionAbs]
    protected def getDefaultRep = Vector
  }

  abstract class VectorCompanionAbs extends CompanionDef[VectorCompanionAbs] {
    def selfType = VectorCompanionElem
    override def toString = "Vector"
  }
  def Vector: Rep[VectorCompanionAbs]
  implicit def proxyVectorCompanionAbs(p: Rep[VectorCompanionAbs]): VectorCompanionAbs =
    proxyOps[VectorCompanionAbs](p)

  abstract class AbsDenseVector[T]
      (items: Coll[T])(implicit eT: Elem[T])
    extends DenseVector[T](items) with Def[DenseVector[T]] {
    lazy val selfType = element[DenseVector[T]]
  }
  // elem for concrete class
  class DenseVectorElem[T](val iso: Iso[DenseVectorData[T], DenseVector[T]])(implicit override val eT: Elem[T])
    extends VectorElem[T, DenseVector[T]]
    with ConcreteElem[DenseVectorData[T], DenseVector[T]] {
    override lazy val parent: Option[Elem[_]] = Some(vectorElement(element[T]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("T" -> Left(eT))
    }

    override def convertVector(x: Rep[Vector[T]]) = DenseVector(x.items)
    override def getDefaultRep = DenseVector(element[Collection[T]].defaultRepValue)
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[DenseVector[T]]
    }
  }

  // state representation type
  type DenseVectorData[T] = Collection[T]

  // 3) Iso for concrete class
  class DenseVectorIso[T](implicit eT: Elem[T])
    extends EntityIso[DenseVectorData[T], DenseVector[T]] with Def[DenseVectorIso[T]] {
    override def from(p: Rep[DenseVector[T]]) =
      p.items
    override def to(p: Rep[Collection[T]]) = {
      val items = p
      DenseVector(items)
    }
    lazy val eFrom = element[Collection[T]]
    lazy val eTo = new DenseVectorElem[T](self)
    lazy val selfType = new DenseVectorIsoElem[T](eT)
    def productArity = 1
    def productElement(n: Int) = eT
  }
  case class DenseVectorIsoElem[T](eT: Elem[T]) extends Elem[DenseVectorIso[T]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new DenseVectorIso[T]()(eT))
    lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[DenseVectorIso[T]]
    }
  }
  // 4) constructor and deconstructor
  class DenseVectorCompanionAbs extends CompanionDef[DenseVectorCompanionAbs] with DenseVectorCompanion {
    def selfType = DenseVectorCompanionElem
    override def toString = "DenseVector"

    @scalan.OverloadId("fromFields")
    def apply[T](items: Coll[T])(implicit eT: Elem[T]): Rep[DenseVector[T]] =
      mkDenseVector(items)

    def unapply[T](p: Rep[Vector[T]]) = unmkDenseVector(p)
  }
  lazy val DenseVectorRep: Rep[DenseVectorCompanionAbs] = new DenseVectorCompanionAbs
  lazy val DenseVector: DenseVectorCompanionAbs = proxyDenseVectorCompanion(DenseVectorRep)
  implicit def proxyDenseVectorCompanion(p: Rep[DenseVectorCompanionAbs]): DenseVectorCompanionAbs = {
    proxyOps[DenseVectorCompanionAbs](p)
  }

  implicit case object DenseVectorCompanionElem extends CompanionElem[DenseVectorCompanionAbs] {
    lazy val tag = weakTypeTag[DenseVectorCompanionAbs]
    protected def getDefaultRep = DenseVector
  }

  implicit def proxyDenseVector[T](p: Rep[DenseVector[T]]): DenseVector[T] =
    proxyOps[DenseVector[T]](p)

  implicit class ExtendedDenseVector[T](p: Rep[DenseVector[T]])(implicit eT: Elem[T]) {
    def toData: Rep[DenseVectorData[T]] = isoDenseVector(eT).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoDenseVector[T](implicit eT: Elem[T]): Iso[DenseVectorData[T], DenseVector[T]] =
    reifyObject(new DenseVectorIso[T]()(eT))

  // 6) smart constructor and deconstructor
  def mkDenseVector[T](items: Coll[T])(implicit eT: Elem[T]): Rep[DenseVector[T]]
  def unmkDenseVector[T](p: Rep[Vector[T]]): Option[(Rep[Collection[T]])]

  abstract class AbsConstVector[T]
      (const: Rep[T], length: IntRep)(implicit eT: Elem[T])
    extends ConstVector[T](const, length) with Def[ConstVector[T]] {
    lazy val selfType = element[ConstVector[T]]
  }
  // elem for concrete class
  class ConstVectorElem[T](val iso: Iso[ConstVectorData[T], ConstVector[T]])(implicit override val eT: Elem[T])
    extends VectorElem[T, ConstVector[T]]
    with ConcreteElem[ConstVectorData[T], ConstVector[T]] {
    override lazy val parent: Option[Elem[_]] = Some(vectorElement(element[T]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("T" -> Left(eT))
    }

    override def convertVector(x: Rep[Vector[T]]) = // Converter is not generated by meta
!!!("Cannot convert from Vector to ConstVector: missing fields List(const)")
    override def getDefaultRep = ConstVector(element[T].defaultRepValue, element[Int].defaultRepValue)
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[ConstVector[T]]
    }
  }

  // state representation type
  type ConstVectorData[T] = (T, Int)

  // 3) Iso for concrete class
  class ConstVectorIso[T](implicit eT: Elem[T])
    extends EntityIso[ConstVectorData[T], ConstVector[T]] with Def[ConstVectorIso[T]] {
    override def from(p: Rep[ConstVector[T]]) =
      (p.const, p.length)
    override def to(p: Rep[(T, Int)]) = {
      val Pair(const, length) = p
      ConstVector(const, length)
    }
    lazy val eFrom = pairElement(element[T], element[Int])
    lazy val eTo = new ConstVectorElem[T](self)
    lazy val selfType = new ConstVectorIsoElem[T](eT)
    def productArity = 1
    def productElement(n: Int) = eT
  }
  case class ConstVectorIsoElem[T](eT: Elem[T]) extends Elem[ConstVectorIso[T]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new ConstVectorIso[T]()(eT))
    lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[ConstVectorIso[T]]
    }
  }
  // 4) constructor and deconstructor
  class ConstVectorCompanionAbs extends CompanionDef[ConstVectorCompanionAbs] with ConstVectorCompanion {
    def selfType = ConstVectorCompanionElem
    override def toString = "ConstVector"
    @scalan.OverloadId("fromData")
    def apply[T](p: Rep[ConstVectorData[T]])(implicit eT: Elem[T]): Rep[ConstVector[T]] =
      isoConstVector(eT).to(p)
    @scalan.OverloadId("fromFields")
    def apply[T](const: Rep[T], length: IntRep)(implicit eT: Elem[T]): Rep[ConstVector[T]] =
      mkConstVector(const, length)

    def unapply[T](p: Rep[Vector[T]]) = unmkConstVector(p)
  }
  lazy val ConstVectorRep: Rep[ConstVectorCompanionAbs] = new ConstVectorCompanionAbs
  lazy val ConstVector: ConstVectorCompanionAbs = proxyConstVectorCompanion(ConstVectorRep)
  implicit def proxyConstVectorCompanion(p: Rep[ConstVectorCompanionAbs]): ConstVectorCompanionAbs = {
    proxyOps[ConstVectorCompanionAbs](p)
  }

  implicit case object ConstVectorCompanionElem extends CompanionElem[ConstVectorCompanionAbs] {
    lazy val tag = weakTypeTag[ConstVectorCompanionAbs]
    protected def getDefaultRep = ConstVector
  }

  implicit def proxyConstVector[T](p: Rep[ConstVector[T]]): ConstVector[T] =
    proxyOps[ConstVector[T]](p)

  implicit class ExtendedConstVector[T](p: Rep[ConstVector[T]])(implicit eT: Elem[T]) {
    def toData: Rep[ConstVectorData[T]] = isoConstVector(eT).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoConstVector[T](implicit eT: Elem[T]): Iso[ConstVectorData[T], ConstVector[T]] =
    reifyObject(new ConstVectorIso[T]()(eT))

  // 6) smart constructor and deconstructor
  def mkConstVector[T](const: Rep[T], length: IntRep)(implicit eT: Elem[T]): Rep[ConstVector[T]]
  def unmkConstVector[T](p: Rep[Vector[T]]): Option[(Rep[T], Rep[Int])]

  abstract class AbsZeroVector[T]
      (length: IntRep)(implicit eT: Elem[T])
    extends ZeroVector[T](length) with Def[ZeroVector[T]] {
    lazy val selfType = element[ZeroVector[T]]
  }
  // elem for concrete class
  class ZeroVectorElem[T](val iso: Iso[ZeroVectorData[T], ZeroVector[T]])(implicit override val eT: Elem[T])
    extends VectorElem[T, ZeroVector[T]]
    with ConcreteElem[ZeroVectorData[T], ZeroVector[T]] {
    override lazy val parent: Option[Elem[_]] = Some(vectorElement(element[T]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("T" -> Left(eT))
    }

    override def convertVector(x: Rep[Vector[T]]) = ZeroVector(x.length)
    override def getDefaultRep = ZeroVector(element[Int].defaultRepValue)
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[ZeroVector[T]]
    }
  }

  // state representation type
  type ZeroVectorData[T] = Int

  // 3) Iso for concrete class
  class ZeroVectorIso[T](implicit eT: Elem[T])
    extends EntityIso[ZeroVectorData[T], ZeroVector[T]] with Def[ZeroVectorIso[T]] {
    override def from(p: Rep[ZeroVector[T]]) =
      p.length
    override def to(p: Rep[Int]) = {
      val length = p
      ZeroVector(length)
    }
    lazy val eFrom = element[Int]
    lazy val eTo = new ZeroVectorElem[T](self)
    lazy val selfType = new ZeroVectorIsoElem[T](eT)
    def productArity = 1
    def productElement(n: Int) = eT
  }
  case class ZeroVectorIsoElem[T](eT: Elem[T]) extends Elem[ZeroVectorIso[T]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new ZeroVectorIso[T]()(eT))
    lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[ZeroVectorIso[T]]
    }
  }
  // 4) constructor and deconstructor
  class ZeroVectorCompanionAbs extends CompanionDef[ZeroVectorCompanionAbs] {
    def selfType = ZeroVectorCompanionElem
    override def toString = "ZeroVector"

    @scalan.OverloadId("fromFields")
    def apply[T](length: IntRep)(implicit eT: Elem[T]): Rep[ZeroVector[T]] =
      mkZeroVector(length)

    def unapply[T](p: Rep[Vector[T]]) = unmkZeroVector(p)
  }
  lazy val ZeroVectorRep: Rep[ZeroVectorCompanionAbs] = new ZeroVectorCompanionAbs
  lazy val ZeroVector: ZeroVectorCompanionAbs = proxyZeroVectorCompanion(ZeroVectorRep)
  implicit def proxyZeroVectorCompanion(p: Rep[ZeroVectorCompanionAbs]): ZeroVectorCompanionAbs = {
    proxyOps[ZeroVectorCompanionAbs](p)
  }

  implicit case object ZeroVectorCompanionElem extends CompanionElem[ZeroVectorCompanionAbs] {
    lazy val tag = weakTypeTag[ZeroVectorCompanionAbs]
    protected def getDefaultRep = ZeroVector
  }

  implicit def proxyZeroVector[T](p: Rep[ZeroVector[T]]): ZeroVector[T] =
    proxyOps[ZeroVector[T]](p)

  implicit class ExtendedZeroVector[T](p: Rep[ZeroVector[T]])(implicit eT: Elem[T]) {
    def toData: Rep[ZeroVectorData[T]] = isoZeroVector(eT).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoZeroVector[T](implicit eT: Elem[T]): Iso[ZeroVectorData[T], ZeroVector[T]] =
    reifyObject(new ZeroVectorIso[T]()(eT))

  // 6) smart constructor and deconstructor
  def mkZeroVector[T](length: IntRep)(implicit eT: Elem[T]): Rep[ZeroVector[T]]
  def unmkZeroVector[T](p: Rep[Vector[T]]): Option[(Rep[Int])]

  abstract class AbsSparseVector[T]
      (nonZeroIndices: Coll[Int], nonZeroValues: Coll[T], length: IntRep)(implicit eT: Elem[T])
    extends SparseVector[T](nonZeroIndices, nonZeroValues, length) with Def[SparseVector[T]] {
    lazy val selfType = element[SparseVector[T]]
  }
  // elem for concrete class
  class SparseVectorElem[T](val iso: Iso[SparseVectorData[T], SparseVector[T]])(implicit override val eT: Elem[T])
    extends VectorElem[T, SparseVector[T]]
    with ConcreteElem[SparseVectorData[T], SparseVector[T]] {
    override lazy val parent: Option[Elem[_]] = Some(vectorElement(element[T]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("T" -> Left(eT))
    }

    override def convertVector(x: Rep[Vector[T]]) = SparseVector(x.nonZeroIndices, x.nonZeroValues, x.length)
    override def getDefaultRep = SparseVector(element[Collection[Int]].defaultRepValue, element[Collection[T]].defaultRepValue, element[Int].defaultRepValue)
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[SparseVector[T]]
    }
  }

  // state representation type
  type SparseVectorData[T] = (Collection[Int], (Collection[T], Int))

  // 3) Iso for concrete class
  class SparseVectorIso[T](implicit eT: Elem[T])
    extends EntityIso[SparseVectorData[T], SparseVector[T]] with Def[SparseVectorIso[T]] {
    override def from(p: Rep[SparseVector[T]]) =
      (p.nonZeroIndices, p.nonZeroValues, p.length)
    override def to(p: Rep[(Collection[Int], (Collection[T], Int))]) = {
      val Pair(nonZeroIndices, Pair(nonZeroValues, length)) = p
      SparseVector(nonZeroIndices, nonZeroValues, length)
    }
    lazy val eFrom = pairElement(element[Collection[Int]], pairElement(element[Collection[T]], element[Int]))
    lazy val eTo = new SparseVectorElem[T](self)
    lazy val selfType = new SparseVectorIsoElem[T](eT)
    def productArity = 1
    def productElement(n: Int) = eT
  }
  case class SparseVectorIsoElem[T](eT: Elem[T]) extends Elem[SparseVectorIso[T]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new SparseVectorIso[T]()(eT))
    lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[SparseVectorIso[T]]
    }
  }
  // 4) constructor and deconstructor
  class SparseVectorCompanionAbs extends CompanionDef[SparseVectorCompanionAbs] with SparseVectorCompanion {
    def selfType = SparseVectorCompanionElem
    override def toString = "SparseVector"
    @scalan.OverloadId("fromData")
    def apply[T](p: Rep[SparseVectorData[T]])(implicit eT: Elem[T]): Rep[SparseVector[T]] =
      isoSparseVector(eT).to(p)
    @scalan.OverloadId("fromFields")
    def apply[T](nonZeroIndices: Coll[Int], nonZeroValues: Coll[T], length: IntRep)(implicit eT: Elem[T]): Rep[SparseVector[T]] =
      mkSparseVector(nonZeroIndices, nonZeroValues, length)

    def unapply[T](p: Rep[Vector[T]]) = unmkSparseVector(p)
  }
  lazy val SparseVectorRep: Rep[SparseVectorCompanionAbs] = new SparseVectorCompanionAbs
  lazy val SparseVector: SparseVectorCompanionAbs = proxySparseVectorCompanion(SparseVectorRep)
  implicit def proxySparseVectorCompanion(p: Rep[SparseVectorCompanionAbs]): SparseVectorCompanionAbs = {
    proxyOps[SparseVectorCompanionAbs](p)
  }

  implicit case object SparseVectorCompanionElem extends CompanionElem[SparseVectorCompanionAbs] {
    lazy val tag = weakTypeTag[SparseVectorCompanionAbs]
    protected def getDefaultRep = SparseVector
  }

  implicit def proxySparseVector[T](p: Rep[SparseVector[T]]): SparseVector[T] =
    proxyOps[SparseVector[T]](p)

  implicit class ExtendedSparseVector[T](p: Rep[SparseVector[T]])(implicit eT: Elem[T]) {
    def toData: Rep[SparseVectorData[T]] = isoSparseVector(eT).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoSparseVector[T](implicit eT: Elem[T]): Iso[SparseVectorData[T], SparseVector[T]] =
    reifyObject(new SparseVectorIso[T]()(eT))

  // 6) smart constructor and deconstructor
  def mkSparseVector[T](nonZeroIndices: Coll[Int], nonZeroValues: Coll[T], length: IntRep)(implicit eT: Elem[T]): Rep[SparseVector[T]]
  def unmkSparseVector[T](p: Rep[Vector[T]]): Option[(Rep[Collection[Int]], Rep[Collection[T]], Rep[Int])]

  abstract class AbsSparseVectorBoxed[T]
      (nonZeroItems: Coll[(Int, T)], length: IntRep)(implicit eT: Elem[T])
    extends SparseVectorBoxed[T](nonZeroItems, length) with Def[SparseVectorBoxed[T]] {
    lazy val selfType = element[SparseVectorBoxed[T]]
  }
  // elem for concrete class
  class SparseVectorBoxedElem[T](val iso: Iso[SparseVectorBoxedData[T], SparseVectorBoxed[T]])(implicit override val eT: Elem[T])
    extends VectorElem[T, SparseVectorBoxed[T]]
    with ConcreteElem[SparseVectorBoxedData[T], SparseVectorBoxed[T]] {
    override lazy val parent: Option[Elem[_]] = Some(vectorElement(element[T]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("T" -> Left(eT))
    }

    override def convertVector(x: Rep[Vector[T]]) = SparseVectorBoxed(x.nonZeroItems, x.length)
    override def getDefaultRep = SparseVectorBoxed(element[Collection[(Int, T)]].defaultRepValue, element[Int].defaultRepValue)
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[SparseVectorBoxed[T]]
    }
  }

  // state representation type
  type SparseVectorBoxedData[T] = (Collection[(Int, T)], Int)

  // 3) Iso for concrete class
  class SparseVectorBoxedIso[T](implicit eT: Elem[T])
    extends EntityIso[SparseVectorBoxedData[T], SparseVectorBoxed[T]] with Def[SparseVectorBoxedIso[T]] {
    override def from(p: Rep[SparseVectorBoxed[T]]) =
      (p.nonZeroItems, p.length)
    override def to(p: Rep[(Collection[(Int, T)], Int)]) = {
      val Pair(nonZeroItems, length) = p
      SparseVectorBoxed(nonZeroItems, length)
    }
    lazy val eFrom = pairElement(element[Collection[(Int, T)]], element[Int])
    lazy val eTo = new SparseVectorBoxedElem[T](self)
    lazy val selfType = new SparseVectorBoxedIsoElem[T](eT)
    def productArity = 1
    def productElement(n: Int) = eT
  }
  case class SparseVectorBoxedIsoElem[T](eT: Elem[T]) extends Elem[SparseVectorBoxedIso[T]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new SparseVectorBoxedIso[T]()(eT))
    lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[SparseVectorBoxedIso[T]]
    }
  }
  // 4) constructor and deconstructor
  class SparseVectorBoxedCompanionAbs extends CompanionDef[SparseVectorBoxedCompanionAbs] with SparseVectorBoxedCompanion {
    def selfType = SparseVectorBoxedCompanionElem
    override def toString = "SparseVectorBoxed"
    @scalan.OverloadId("fromData")
    def apply[T](p: Rep[SparseVectorBoxedData[T]])(implicit eT: Elem[T]): Rep[SparseVectorBoxed[T]] =
      isoSparseVectorBoxed(eT).to(p)
    @scalan.OverloadId("fromFields")
    def apply[T](nonZeroItems: Coll[(Int, T)], length: IntRep)(implicit eT: Elem[T]): Rep[SparseVectorBoxed[T]] =
      mkSparseVectorBoxed(nonZeroItems, length)

    def unapply[T](p: Rep[Vector[T]]) = unmkSparseVectorBoxed(p)
  }
  lazy val SparseVectorBoxedRep: Rep[SparseVectorBoxedCompanionAbs] = new SparseVectorBoxedCompanionAbs
  lazy val SparseVectorBoxed: SparseVectorBoxedCompanionAbs = proxySparseVectorBoxedCompanion(SparseVectorBoxedRep)
  implicit def proxySparseVectorBoxedCompanion(p: Rep[SparseVectorBoxedCompanionAbs]): SparseVectorBoxedCompanionAbs = {
    proxyOps[SparseVectorBoxedCompanionAbs](p)
  }

  implicit case object SparseVectorBoxedCompanionElem extends CompanionElem[SparseVectorBoxedCompanionAbs] {
    lazy val tag = weakTypeTag[SparseVectorBoxedCompanionAbs]
    protected def getDefaultRep = SparseVectorBoxed
  }

  implicit def proxySparseVectorBoxed[T](p: Rep[SparseVectorBoxed[T]]): SparseVectorBoxed[T] =
    proxyOps[SparseVectorBoxed[T]](p)

  implicit class ExtendedSparseVectorBoxed[T](p: Rep[SparseVectorBoxed[T]])(implicit eT: Elem[T]) {
    def toData: Rep[SparseVectorBoxedData[T]] = isoSparseVectorBoxed(eT).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoSparseVectorBoxed[T](implicit eT: Elem[T]): Iso[SparseVectorBoxedData[T], SparseVectorBoxed[T]] =
    reifyObject(new SparseVectorBoxedIso[T]()(eT))

  // 6) smart constructor and deconstructor
  def mkSparseVectorBoxed[T](nonZeroItems: Coll[(Int, T)], length: IntRep)(implicit eT: Elem[T]): Rep[SparseVectorBoxed[T]]
  def unmkSparseVectorBoxed[T](p: Rep[Vector[T]]): Option[(Rep[Collection[(Int, T)]], Rep[Int])]

  abstract class AbsShiftVector[T]
      (nonZeroIndices: Coll[Int], nonZeroValues: Coll[T], offset: Rep[T], length: IntRep)(implicit eT: Elem[T])
    extends ShiftVector[T](nonZeroIndices, nonZeroValues, offset, length) with Def[ShiftVector[T]] {
    lazy val selfType = element[ShiftVector[T]]
  }
  // elem for concrete class
  class ShiftVectorElem[T](val iso: Iso[ShiftVectorData[T], ShiftVector[T]])(implicit override val eT: Elem[T])
    extends VectorElem[T, ShiftVector[T]]
    with ConcreteElem[ShiftVectorData[T], ShiftVector[T]] {
    override lazy val parent: Option[Elem[_]] = Some(vectorElement(element[T]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("T" -> Left(eT))
    }

    override def convertVector(x: Rep[Vector[T]]) = // Converter is not generated by meta
!!!("Cannot convert from Vector to ShiftVector: missing fields List(offset)")
    override def getDefaultRep = ShiftVector(element[Collection[Int]].defaultRepValue, element[Collection[T]].defaultRepValue, element[T].defaultRepValue, element[Int].defaultRepValue)
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[ShiftVector[T]]
    }
  }

  // state representation type
  type ShiftVectorData[T] = (Collection[Int], (Collection[T], (T, Int)))

  // 3) Iso for concrete class
  class ShiftVectorIso[T](implicit eT: Elem[T])
    extends EntityIso[ShiftVectorData[T], ShiftVector[T]] with Def[ShiftVectorIso[T]] {
    override def from(p: Rep[ShiftVector[T]]) =
      (p.nonZeroIndices, p.nonZeroValues, p.offset, p.length)
    override def to(p: Rep[(Collection[Int], (Collection[T], (T, Int)))]) = {
      val Pair(nonZeroIndices, Pair(nonZeroValues, Pair(offset, length))) = p
      ShiftVector(nonZeroIndices, nonZeroValues, offset, length)
    }
    lazy val eFrom = pairElement(element[Collection[Int]], pairElement(element[Collection[T]], pairElement(element[T], element[Int])))
    lazy val eTo = new ShiftVectorElem[T](self)
    lazy val selfType = new ShiftVectorIsoElem[T](eT)
    def productArity = 1
    def productElement(n: Int) = eT
  }
  case class ShiftVectorIsoElem[T](eT: Elem[T]) extends Elem[ShiftVectorIso[T]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new ShiftVectorIso[T]()(eT))
    lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[ShiftVectorIso[T]]
    }
  }
  // 4) constructor and deconstructor
  class ShiftVectorCompanionAbs extends CompanionDef[ShiftVectorCompanionAbs] {
    def selfType = ShiftVectorCompanionElem
    override def toString = "ShiftVector"
    @scalan.OverloadId("fromData")
    def apply[T](p: Rep[ShiftVectorData[T]])(implicit eT: Elem[T]): Rep[ShiftVector[T]] =
      isoShiftVector(eT).to(p)
    @scalan.OverloadId("fromFields")
    def apply[T](nonZeroIndices: Coll[Int], nonZeroValues: Coll[T], offset: Rep[T], length: IntRep)(implicit eT: Elem[T]): Rep[ShiftVector[T]] =
      mkShiftVector(nonZeroIndices, nonZeroValues, offset, length)

    def unapply[T](p: Rep[Vector[T]]) = unmkShiftVector(p)
  }
  lazy val ShiftVectorRep: Rep[ShiftVectorCompanionAbs] = new ShiftVectorCompanionAbs
  lazy val ShiftVector: ShiftVectorCompanionAbs = proxyShiftVectorCompanion(ShiftVectorRep)
  implicit def proxyShiftVectorCompanion(p: Rep[ShiftVectorCompanionAbs]): ShiftVectorCompanionAbs = {
    proxyOps[ShiftVectorCompanionAbs](p)
  }

  implicit case object ShiftVectorCompanionElem extends CompanionElem[ShiftVectorCompanionAbs] {
    lazy val tag = weakTypeTag[ShiftVectorCompanionAbs]
    protected def getDefaultRep = ShiftVector
  }

  implicit def proxyShiftVector[T](p: Rep[ShiftVector[T]]): ShiftVector[T] =
    proxyOps[ShiftVector[T]](p)

  implicit class ExtendedShiftVector[T](p: Rep[ShiftVector[T]])(implicit eT: Elem[T]) {
    def toData: Rep[ShiftVectorData[T]] = isoShiftVector(eT).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoShiftVector[T](implicit eT: Elem[T]): Iso[ShiftVectorData[T], ShiftVector[T]] =
    reifyObject(new ShiftVectorIso[T]()(eT))

  // 6) smart constructor and deconstructor
  def mkShiftVector[T](nonZeroIndices: Coll[Int], nonZeroValues: Coll[T], offset: Rep[T], length: IntRep)(implicit eT: Elem[T]): Rep[ShiftVector[T]]
  def unmkShiftVector[T](p: Rep[Vector[T]]): Option[(Rep[Collection[Int]], Rep[Collection[T]], Rep[T], Rep[Int])]

  abstract class AbsShiftVectorBoxed[T]
      (nonZeroItems: Coll[(Int, T)], offset: Rep[T], length: IntRep)(implicit eT: Elem[T])
    extends ShiftVectorBoxed[T](nonZeroItems, offset, length) with Def[ShiftVectorBoxed[T]] {
    lazy val selfType = element[ShiftVectorBoxed[T]]
  }
  // elem for concrete class
  class ShiftVectorBoxedElem[T](val iso: Iso[ShiftVectorBoxedData[T], ShiftVectorBoxed[T]])(implicit override val eT: Elem[T])
    extends VectorElem[T, ShiftVectorBoxed[T]]
    with ConcreteElem[ShiftVectorBoxedData[T], ShiftVectorBoxed[T]] {
    override lazy val parent: Option[Elem[_]] = Some(vectorElement(element[T]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("T" -> Left(eT))
    }

    override def convertVector(x: Rep[Vector[T]]) = // Converter is not generated by meta
!!!("Cannot convert from Vector to ShiftVectorBoxed: missing fields List(offset)")
    override def getDefaultRep = ShiftVectorBoxed(element[Collection[(Int, T)]].defaultRepValue, element[T].defaultRepValue, element[Int].defaultRepValue)
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[ShiftVectorBoxed[T]]
    }
  }

  // state representation type
  type ShiftVectorBoxedData[T] = (Collection[(Int, T)], (T, Int))

  // 3) Iso for concrete class
  class ShiftVectorBoxedIso[T](implicit eT: Elem[T])
    extends EntityIso[ShiftVectorBoxedData[T], ShiftVectorBoxed[T]] with Def[ShiftVectorBoxedIso[T]] {
    override def from(p: Rep[ShiftVectorBoxed[T]]) =
      (p.nonZeroItems, p.offset, p.length)
    override def to(p: Rep[(Collection[(Int, T)], (T, Int))]) = {
      val Pair(nonZeroItems, Pair(offset, length)) = p
      ShiftVectorBoxed(nonZeroItems, offset, length)
    }
    lazy val eFrom = pairElement(element[Collection[(Int, T)]], pairElement(element[T], element[Int]))
    lazy val eTo = new ShiftVectorBoxedElem[T](self)
    lazy val selfType = new ShiftVectorBoxedIsoElem[T](eT)
    def productArity = 1
    def productElement(n: Int) = eT
  }
  case class ShiftVectorBoxedIsoElem[T](eT: Elem[T]) extends Elem[ShiftVectorBoxedIso[T]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new ShiftVectorBoxedIso[T]()(eT))
    lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[ShiftVectorBoxedIso[T]]
    }
  }
  // 4) constructor and deconstructor
  class ShiftVectorBoxedCompanionAbs extends CompanionDef[ShiftVectorBoxedCompanionAbs] {
    def selfType = ShiftVectorBoxedCompanionElem
    override def toString = "ShiftVectorBoxed"
    @scalan.OverloadId("fromData")
    def apply[T](p: Rep[ShiftVectorBoxedData[T]])(implicit eT: Elem[T]): Rep[ShiftVectorBoxed[T]] =
      isoShiftVectorBoxed(eT).to(p)
    @scalan.OverloadId("fromFields")
    def apply[T](nonZeroItems: Coll[(Int, T)], offset: Rep[T], length: IntRep)(implicit eT: Elem[T]): Rep[ShiftVectorBoxed[T]] =
      mkShiftVectorBoxed(nonZeroItems, offset, length)

    def unapply[T](p: Rep[Vector[T]]) = unmkShiftVectorBoxed(p)
  }
  lazy val ShiftVectorBoxedRep: Rep[ShiftVectorBoxedCompanionAbs] = new ShiftVectorBoxedCompanionAbs
  lazy val ShiftVectorBoxed: ShiftVectorBoxedCompanionAbs = proxyShiftVectorBoxedCompanion(ShiftVectorBoxedRep)
  implicit def proxyShiftVectorBoxedCompanion(p: Rep[ShiftVectorBoxedCompanionAbs]): ShiftVectorBoxedCompanionAbs = {
    proxyOps[ShiftVectorBoxedCompanionAbs](p)
  }

  implicit case object ShiftVectorBoxedCompanionElem extends CompanionElem[ShiftVectorBoxedCompanionAbs] {
    lazy val tag = weakTypeTag[ShiftVectorBoxedCompanionAbs]
    protected def getDefaultRep = ShiftVectorBoxed
  }

  implicit def proxyShiftVectorBoxed[T](p: Rep[ShiftVectorBoxed[T]]): ShiftVectorBoxed[T] =
    proxyOps[ShiftVectorBoxed[T]](p)

  implicit class ExtendedShiftVectorBoxed[T](p: Rep[ShiftVectorBoxed[T]])(implicit eT: Elem[T]) {
    def toData: Rep[ShiftVectorBoxedData[T]] = isoShiftVectorBoxed(eT).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoShiftVectorBoxed[T](implicit eT: Elem[T]): Iso[ShiftVectorBoxedData[T], ShiftVectorBoxed[T]] =
    reifyObject(new ShiftVectorBoxedIso[T]()(eT))

  // 6) smart constructor and deconstructor
  def mkShiftVectorBoxed[T](nonZeroItems: Coll[(Int, T)], offset: Rep[T], length: IntRep)(implicit eT: Elem[T]): Rep[ShiftVectorBoxed[T]]
  def unmkShiftVectorBoxed[T](p: Rep[Vector[T]]): Option[(Rep[Collection[(Int, T)]], Rep[T], Rep[Int])]

  registerModule(Vectors_Module)
}

// Std -----------------------------------
trait VectorsStd extends scalan.ScalanDslStd with VectorsDsl {
  self: LADslStd =>
  lazy val Vector: Rep[VectorCompanionAbs] = new VectorCompanionAbs {
  }

  case class StdDenseVector[T]
      (override val items: Coll[T])(implicit eT: Elem[T])
    extends AbsDenseVector[T](items) {
  }

  def mkDenseVector[T]
    (items: Coll[T])(implicit eT: Elem[T]): Rep[DenseVector[T]] =
    new StdDenseVector[T](items)
  def unmkDenseVector[T](p: Rep[Vector[T]]) = p match {
    case p: DenseVector[T] @unchecked =>
      Some((p.items))
    case _ => None
  }

  case class StdConstVector[T]
      (override val const: Rep[T], override val length: IntRep)(implicit eT: Elem[T])
    extends AbsConstVector[T](const, length) {
  }

  def mkConstVector[T]
    (const: Rep[T], length: IntRep)(implicit eT: Elem[T]): Rep[ConstVector[T]] =
    new StdConstVector[T](const, length)
  def unmkConstVector[T](p: Rep[Vector[T]]) = p match {
    case p: ConstVector[T] @unchecked =>
      Some((p.const, p.length))
    case _ => None
  }

  case class StdZeroVector[T]
      (override val length: IntRep)(implicit eT: Elem[T])
    extends AbsZeroVector[T](length) {
  }

  def mkZeroVector[T]
    (length: IntRep)(implicit eT: Elem[T]): Rep[ZeroVector[T]] =
    new StdZeroVector[T](length)
  def unmkZeroVector[T](p: Rep[Vector[T]]) = p match {
    case p: ZeroVector[T] @unchecked =>
      Some((p.length))
    case _ => None
  }

  case class StdSparseVector[T]
      (override val nonZeroIndices: Coll[Int], override val nonZeroValues: Coll[T], override val length: IntRep)(implicit eT: Elem[T])
    extends AbsSparseVector[T](nonZeroIndices, nonZeroValues, length) {
  }

  def mkSparseVector[T]
    (nonZeroIndices: Coll[Int], nonZeroValues: Coll[T], length: IntRep)(implicit eT: Elem[T]): Rep[SparseVector[T]] =
    new StdSparseVector[T](nonZeroIndices, nonZeroValues, length)
  def unmkSparseVector[T](p: Rep[Vector[T]]) = p match {
    case p: SparseVector[T] @unchecked =>
      Some((p.nonZeroIndices, p.nonZeroValues, p.length))
    case _ => None
  }

  case class StdSparseVectorBoxed[T]
      (override val nonZeroItems: Coll[(Int, T)], override val length: IntRep)(implicit eT: Elem[T])
    extends AbsSparseVectorBoxed[T](nonZeroItems, length) {
  }

  def mkSparseVectorBoxed[T]
    (nonZeroItems: Coll[(Int, T)], length: IntRep)(implicit eT: Elem[T]): Rep[SparseVectorBoxed[T]] =
    new StdSparseVectorBoxed[T](nonZeroItems, length)
  def unmkSparseVectorBoxed[T](p: Rep[Vector[T]]) = p match {
    case p: SparseVectorBoxed[T] @unchecked =>
      Some((p.nonZeroItems, p.length))
    case _ => None
  }

  case class StdShiftVector[T]
      (override val nonZeroIndices: Coll[Int], override val nonZeroValues: Coll[T], override val offset: Rep[T], override val length: IntRep)(implicit eT: Elem[T])
    extends AbsShiftVector[T](nonZeroIndices, nonZeroValues, offset, length) {
  }

  def mkShiftVector[T]
    (nonZeroIndices: Coll[Int], nonZeroValues: Coll[T], offset: Rep[T], length: IntRep)(implicit eT: Elem[T]): Rep[ShiftVector[T]] =
    new StdShiftVector[T](nonZeroIndices, nonZeroValues, offset, length)
  def unmkShiftVector[T](p: Rep[Vector[T]]) = p match {
    case p: ShiftVector[T] @unchecked =>
      Some((p.nonZeroIndices, p.nonZeroValues, p.offset, p.length))
    case _ => None
  }

  case class StdShiftVectorBoxed[T]
      (override val nonZeroItems: Coll[(Int, T)], override val offset: Rep[T], override val length: IntRep)(implicit eT: Elem[T])
    extends AbsShiftVectorBoxed[T](nonZeroItems, offset, length) {
  }

  def mkShiftVectorBoxed[T]
    (nonZeroItems: Coll[(Int, T)], offset: Rep[T], length: IntRep)(implicit eT: Elem[T]): Rep[ShiftVectorBoxed[T]] =
    new StdShiftVectorBoxed[T](nonZeroItems, offset, length)
  def unmkShiftVectorBoxed[T](p: Rep[Vector[T]]) = p match {
    case p: ShiftVectorBoxed[T] @unchecked =>
      Some((p.nonZeroItems, p.offset, p.length))
    case _ => None
  }
}

// Exp -----------------------------------
trait VectorsExp extends scalan.ScalanDslExp with VectorsDsl {
  self: LADslExp =>
  lazy val Vector: Rep[VectorCompanionAbs] = new VectorCompanionAbs {
  }

  case class ExpDenseVector[T]
      (override val items: Coll[T])(implicit eT: Elem[T])
    extends AbsDenseVector[T](items)

  object DenseVectorMethods {
    object length {
      def unapply(d: Def[_]): Option[Rep[DenseVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[DenseVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[DenseVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object nonZeroIndices {
      def unapply(d: Def[_]): Option[Rep[DenseVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "nonZeroIndices" =>
          Some(receiver).asInstanceOf[Option[Rep[DenseVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[DenseVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object nonZeroValues {
      def unapply(d: Def[_]): Option[Rep[DenseVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "nonZeroValues" =>
          Some(receiver).asInstanceOf[Option[Rep[DenseVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[DenseVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object nonZeroItems {
      def unapply(d: Def[_]): Option[Rep[DenseVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "nonZeroItems" =>
          Some(receiver).asInstanceOf[Option[Rep[DenseVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[DenseVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[DenseVector[T]], IntRep) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "apply" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[DenseVector[T]], IntRep) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseVector[T]], IntRep) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_apply_by_collection {
      def unapply(d: Def[_]): Option[(Rep[DenseVector[T]], Coll[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(is, _*), _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "apply_by_collection" } =>
          Some((receiver, is)).asInstanceOf[Option[(Rep[DenseVector[T]], Coll[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseVector[T]], Coll[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object filterBy {
      def unapply(d: Def[_]): Option[(Rep[DenseVector[T]], Rep[T @uncheckedVariance => Boolean]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "filterBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[DenseVector[T]], Rep[T @uncheckedVariance => Boolean]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseVector[T]], Rep[T @uncheckedVariance => Boolean]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object +^ {
      def unapply(d: Def[_]): Option[(Rep[DenseVector[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, n, _*), _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "$plus$up" =>
          Some((receiver, vector, n)).asInstanceOf[Option[(Rep[DenseVector[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseVector[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object -^ {
      def unapply(d: Def[_]): Option[(Rep[DenseVector[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, n, _*), _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "$minus$up" =>
          Some((receiver, vector, n)).asInstanceOf[Option[(Rep[DenseVector[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseVector[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object *^ {
      def unapply(d: Def[_]): Option[(Rep[DenseVector[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, n, _*), _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "$times$up" =>
          Some((receiver, vector, n)).asInstanceOf[Option[(Rep[DenseVector[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseVector[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object /^ {
      def unapply(d: Def[_]): Option[(Rep[DenseVector[T]], Vec[T], Fractional[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, f, _*), _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "$div$up" =>
          Some((receiver, vector, f)).asInstanceOf[Option[(Rep[DenseVector[T]], Vec[T], Fractional[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseVector[T]], Vec[T], Fractional[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object dot {
      def unapply(d: Def[_]): Option[(Rep[DenseVector[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, n, _*), _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "dot" =>
          Some((receiver, vector, n)).asInstanceOf[Option[(Rep[DenseVector[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseVector[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object * {
      def unapply(d: Def[_]): Option[(Rep[DenseVector[T]], Matr[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(matrix, n, _*), _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "$times" =>
          Some((receiver, matrix, n)).asInstanceOf[Option[(Rep[DenseVector[T]], Matr[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseVector[T]], Matr[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object pow_^ {
      def unapply(d: Def[_]): Option[(Rep[DenseVector[T]], DoubleRep, Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(order, n, _*), _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "pow_$up" =>
          Some((receiver, order, n)).asInstanceOf[Option[(Rep[DenseVector[T]], DoubleRep, Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseVector[T]], DoubleRep, Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object sum {
      def unapply(d: Def[_]): Option[(Rep[DenseVector[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(n, _*), _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "sum" =>
          Some((receiver, n)).asInstanceOf[Option[(Rep[DenseVector[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseVector[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduce {
      def unapply(d: Def[_]): Option[(Rep[DenseVector[T]], RepMonoid[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(m, _*), _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "reduce" =>
          Some((receiver, m)).asInstanceOf[Option[(Rep[DenseVector[T]], RepMonoid[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseVector[T]], RepMonoid[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object euclideanNorm {
      def unapply(d: Def[_]): Option[(Rep[DenseVector[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(n, _*), _) if receiver.elem.isInstanceOf[DenseVectorElem[_]] && method.getName == "euclideanNorm" =>
          Some((receiver, n)).asInstanceOf[Option[(Rep[DenseVector[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[DenseVector[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object DenseVectorCompanionMethods {
  }

  def mkDenseVector[T]
    (items: Coll[T])(implicit eT: Elem[T]): Rep[DenseVector[T]] =
    new ExpDenseVector[T](items)
  def unmkDenseVector[T](p: Rep[Vector[T]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: DenseVectorElem[T] @unchecked =>
      Some((p.asRep[DenseVector[T]].items))
    case _ =>
      None
  }

  case class ExpConstVector[T]
      (override val const: Rep[T], override val length: IntRep)(implicit eT: Elem[T])
    extends AbsConstVector[T](const, length)

  object ConstVectorMethods {
    object items {
      def unapply(d: Def[_]): Option[Rep[ConstVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ConstVectorElem[_]] && method.getName == "items" =>
          Some(receiver).asInstanceOf[Option[Rep[ConstVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[ConstVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object nonZeroIndices {
      def unapply(d: Def[_]): Option[Rep[ConstVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ConstVectorElem[_]] && method.getName == "nonZeroIndices" =>
          Some(receiver).asInstanceOf[Option[Rep[ConstVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[ConstVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object nonZeroValues {
      def unapply(d: Def[_]): Option[Rep[ConstVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ConstVectorElem[_]] && method.getName == "nonZeroValues" =>
          Some(receiver).asInstanceOf[Option[Rep[ConstVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[ConstVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object nonZeroItems {
      def unapply(d: Def[_]): Option[Rep[ConstVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ConstVectorElem[_]] && method.getName == "nonZeroItems" =>
          Some(receiver).asInstanceOf[Option[Rep[ConstVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[ConstVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[ConstVector[T]], IntRep) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[ConstVectorElem[_]] && method.getName == "apply" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[ConstVector[T]], IntRep) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstVector[T]], IntRep) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_apply_by_collection {
      def unapply(d: Def[_]): Option[(Rep[ConstVector[T]], Coll[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(is, _*), _) if receiver.elem.isInstanceOf[ConstVectorElem[_]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "apply_by_collection" } =>
          Some((receiver, is)).asInstanceOf[Option[(Rep[ConstVector[T]], Coll[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstVector[T]], Coll[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object filterBy {
      def unapply(d: Def[_]): Option[(Rep[ConstVector[T]], Rep[T @uncheckedVariance => Boolean]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[ConstVectorElem[_]] && method.getName == "filterBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[ConstVector[T]], Rep[T @uncheckedVariance => Boolean]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstVector[T]], Rep[T @uncheckedVariance => Boolean]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object +^ {
      def unapply(d: Def[_]): Option[(Rep[ConstVector[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, n, _*), _) if receiver.elem.isInstanceOf[ConstVectorElem[_]] && method.getName == "$plus$up" =>
          Some((receiver, vector, n)).asInstanceOf[Option[(Rep[ConstVector[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstVector[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object -^ {
      def unapply(d: Def[_]): Option[(Rep[ConstVector[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, n, _*), _) if receiver.elem.isInstanceOf[ConstVectorElem[_]] && method.getName == "$minus$up" =>
          Some((receiver, vector, n)).asInstanceOf[Option[(Rep[ConstVector[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstVector[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object *^ {
      def unapply(d: Def[_]): Option[(Rep[ConstVector[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, n, _*), _) if receiver.elem.isInstanceOf[ConstVectorElem[_]] && method.getName == "$times$up" =>
          Some((receiver, vector, n)).asInstanceOf[Option[(Rep[ConstVector[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstVector[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object /^ {
      def unapply(d: Def[_]): Option[(Rep[ConstVector[T]], Vec[T], Fractional[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, f, _*), _) if receiver.elem.isInstanceOf[ConstVectorElem[_]] && method.getName == "$div$up" =>
          Some((receiver, vector, f)).asInstanceOf[Option[(Rep[ConstVector[T]], Vec[T], Fractional[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstVector[T]], Vec[T], Fractional[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object dot {
      def unapply(d: Def[_]): Option[(Rep[ConstVector[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, n, _*), _) if receiver.elem.isInstanceOf[ConstVectorElem[_]] && method.getName == "dot" =>
          Some((receiver, vector, n)).asInstanceOf[Option[(Rep[ConstVector[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstVector[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object * {
      def unapply(d: Def[_]): Option[(Rep[ConstVector[T]], Matr[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(matrix, n, _*), _) if receiver.elem.isInstanceOf[ConstVectorElem[_]] && method.getName == "$times" =>
          Some((receiver, matrix, n)).asInstanceOf[Option[(Rep[ConstVector[T]], Matr[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstVector[T]], Matr[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object pow_^ {
      def unapply(d: Def[_]): Option[(Rep[ConstVector[T]], DoubleRep, Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(order, n, _*), _) if receiver.elem.isInstanceOf[ConstVectorElem[_]] && method.getName == "pow_$up" =>
          Some((receiver, order, n)).asInstanceOf[Option[(Rep[ConstVector[T]], DoubleRep, Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstVector[T]], DoubleRep, Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object sum {
      def unapply(d: Def[_]): Option[(Rep[ConstVector[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(n, _*), _) if receiver.elem.isInstanceOf[ConstVectorElem[_]] && method.getName == "sum" =>
          Some((receiver, n)).asInstanceOf[Option[(Rep[ConstVector[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstVector[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduce {
      def unapply(d: Def[_]): Option[(Rep[ConstVector[T]], RepMonoid[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(m, _*), _) if receiver.elem.isInstanceOf[ConstVectorElem[_]] && method.getName == "reduce" =>
          Some((receiver, m)).asInstanceOf[Option[(Rep[ConstVector[T]], RepMonoid[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstVector[T]], RepMonoid[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object euclideanNorm {
      def unapply(d: Def[_]): Option[(Rep[ConstVector[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(n, _*), _) if receiver.elem.isInstanceOf[ConstVectorElem[_]] && method.getName == "euclideanNorm" =>
          Some((receiver, n)).asInstanceOf[Option[(Rep[ConstVector[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConstVector[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object ConstVectorCompanionMethods {
  }

  def mkConstVector[T]
    (const: Rep[T], length: IntRep)(implicit eT: Elem[T]): Rep[ConstVector[T]] =
    new ExpConstVector[T](const, length)
  def unmkConstVector[T](p: Rep[Vector[T]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ConstVectorElem[T] @unchecked =>
      Some((p.asRep[ConstVector[T]].const, p.asRep[ConstVector[T]].length))
    case _ =>
      None
  }

  case class ExpZeroVector[T]
      (override val length: IntRep)(implicit eT: Elem[T])
    extends AbsZeroVector[T](length)

  object ZeroVectorMethods {
    object items {
      def unapply(d: Def[_]): Option[Rep[ZeroVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ZeroVectorElem[_]] && method.getName == "items" =>
          Some(receiver).asInstanceOf[Option[Rep[ZeroVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[ZeroVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object nonZeroIndices {
      def unapply(d: Def[_]): Option[Rep[ZeroVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ZeroVectorElem[_]] && method.getName == "nonZeroIndices" =>
          Some(receiver).asInstanceOf[Option[Rep[ZeroVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[ZeroVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object nonZeroValues {
      def unapply(d: Def[_]): Option[Rep[ZeroVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ZeroVectorElem[_]] && method.getName == "nonZeroValues" =>
          Some(receiver).asInstanceOf[Option[Rep[ZeroVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[ZeroVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object nonZeroItems {
      def unapply(d: Def[_]): Option[Rep[ZeroVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ZeroVectorElem[_]] && method.getName == "nonZeroItems" =>
          Some(receiver).asInstanceOf[Option[Rep[ZeroVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[ZeroVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[ZeroVector[T]], IntRep) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[ZeroVectorElem[_]] && method.getName == "apply" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[ZeroVector[T]], IntRep) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ZeroVector[T]], IntRep) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_apply_by_collection {
      def unapply(d: Def[_]): Option[(Rep[ZeroVector[T]], Coll[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(is, _*), _) if receiver.elem.isInstanceOf[ZeroVectorElem[_]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "apply_by_collection" } =>
          Some((receiver, is)).asInstanceOf[Option[(Rep[ZeroVector[T]], Coll[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ZeroVector[T]], Coll[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object filterBy {
      def unapply(d: Def[_]): Option[(Rep[ZeroVector[T]], Rep[T @uncheckedVariance => Boolean]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[ZeroVectorElem[_]] && method.getName == "filterBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[ZeroVector[T]], Rep[T @uncheckedVariance => Boolean]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ZeroVector[T]], Rep[T @uncheckedVariance => Boolean]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object +^ {
      def unapply(d: Def[_]): Option[(Rep[ZeroVector[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, n, _*), _) if receiver.elem.isInstanceOf[ZeroVectorElem[_]] && method.getName == "$plus$up" =>
          Some((receiver, vector, n)).asInstanceOf[Option[(Rep[ZeroVector[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ZeroVector[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object -^ {
      def unapply(d: Def[_]): Option[(Rep[ZeroVector[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, n, _*), _) if receiver.elem.isInstanceOf[ZeroVectorElem[_]] && method.getName == "$minus$up" =>
          Some((receiver, vector, n)).asInstanceOf[Option[(Rep[ZeroVector[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ZeroVector[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object *^ {
      def unapply(d: Def[_]): Option[(Rep[ZeroVector[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, n, _*), _) if receiver.elem.isInstanceOf[ZeroVectorElem[_]] && method.getName == "$times$up" =>
          Some((receiver, vector, n)).asInstanceOf[Option[(Rep[ZeroVector[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ZeroVector[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object /^ {
      def unapply(d: Def[_]): Option[(Rep[ZeroVector[T]], Vec[T], Fractional[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, f, _*), _) if receiver.elem.isInstanceOf[ZeroVectorElem[_]] && method.getName == "$div$up" =>
          Some((receiver, vector, f)).asInstanceOf[Option[(Rep[ZeroVector[T]], Vec[T], Fractional[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ZeroVector[T]], Vec[T], Fractional[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object dot {
      def unapply(d: Def[_]): Option[(Rep[ZeroVector[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, n, _*), _) if receiver.elem.isInstanceOf[ZeroVectorElem[_]] && method.getName == "dot" =>
          Some((receiver, vector, n)).asInstanceOf[Option[(Rep[ZeroVector[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ZeroVector[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object * {
      def unapply(d: Def[_]): Option[(Rep[ZeroVector[T]], Matr[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(matrix, n, _*), _) if receiver.elem.isInstanceOf[ZeroVectorElem[_]] && method.getName == "$times" =>
          Some((receiver, matrix, n)).asInstanceOf[Option[(Rep[ZeroVector[T]], Matr[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ZeroVector[T]], Matr[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object pow_^ {
      def unapply(d: Def[_]): Option[(Rep[ZeroVector[T]], DoubleRep, Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(order, n, _*), _) if receiver.elem.isInstanceOf[ZeroVectorElem[_]] && method.getName == "pow_$up" =>
          Some((receiver, order, n)).asInstanceOf[Option[(Rep[ZeroVector[T]], DoubleRep, Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ZeroVector[T]], DoubleRep, Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object sum {
      def unapply(d: Def[_]): Option[(Rep[ZeroVector[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(n, _*), _) if receiver.elem.isInstanceOf[ZeroVectorElem[_]] && method.getName == "sum" =>
          Some((receiver, n)).asInstanceOf[Option[(Rep[ZeroVector[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ZeroVector[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduce {
      def unapply(d: Def[_]): Option[(Rep[ZeroVector[T]], RepMonoid[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(m, _*), _) if receiver.elem.isInstanceOf[ZeroVectorElem[_]] && method.getName == "reduce" =>
          Some((receiver, m)).asInstanceOf[Option[(Rep[ZeroVector[T]], RepMonoid[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ZeroVector[T]], RepMonoid[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object euclideanNorm {
      def unapply(d: Def[_]): Option[(Rep[ZeroVector[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(n, _*), _) if receiver.elem.isInstanceOf[ZeroVectorElem[_]] && method.getName == "euclideanNorm" =>
          Some((receiver, n)).asInstanceOf[Option[(Rep[ZeroVector[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ZeroVector[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkZeroVector[T]
    (length: IntRep)(implicit eT: Elem[T]): Rep[ZeroVector[T]] =
    new ExpZeroVector[T](length)
  def unmkZeroVector[T](p: Rep[Vector[T]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ZeroVectorElem[T] @unchecked =>
      Some((p.asRep[ZeroVector[T]].length))
    case _ =>
      None
  }

  case class ExpSparseVector[T]
      (override val nonZeroIndices: Coll[Int], override val nonZeroValues: Coll[T], override val length: IntRep)(implicit eT: Elem[T])
    extends AbsSparseVector[T](nonZeroIndices, nonZeroValues, length)

  object SparseVectorMethods {
    object items {
      def unapply(d: Def[_]): Option[Rep[SparseVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "items" =>
          Some(receiver).asInstanceOf[Option[Rep[SparseVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[SparseVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object nonZeroItems {
      def unapply(d: Def[_]): Option[Rep[SparseVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "nonZeroItems" =>
          Some(receiver).asInstanceOf[Option[Rep[SparseVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[SparseVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[SparseVector[T]], IntRep) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "apply" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[SparseVector[T]], IntRep) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector[T]], IntRep) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_apply_by_collection {
      def unapply(d: Def[_]): Option[(Rep[SparseVector[T]], Coll[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(is, _*), _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "apply_by_collection" } =>
          Some((receiver, is)).asInstanceOf[Option[(Rep[SparseVector[T]], Coll[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector[T]], Coll[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object filterBy {
      def unapply(d: Def[_]): Option[(Rep[SparseVector[T]], Rep[T @uncheckedVariance => Boolean]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "filterBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[SparseVector[T]], Rep[T @uncheckedVariance => Boolean]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector[T]], Rep[T @uncheckedVariance => Boolean]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object +^ {
      def unapply(d: Def[_]): Option[(Rep[SparseVector[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, n, _*), _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "$plus$up" =>
          Some((receiver, vector, n)).asInstanceOf[Option[(Rep[SparseVector[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object -^ {
      def unapply(d: Def[_]): Option[(Rep[SparseVector[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, n, _*), _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "$minus$up" =>
          Some((receiver, vector, n)).asInstanceOf[Option[(Rep[SparseVector[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object *^ {
      def unapply(d: Def[_]): Option[(Rep[SparseVector[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, n, _*), _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "$times$up" =>
          Some((receiver, vector, n)).asInstanceOf[Option[(Rep[SparseVector[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object /^ {
      def unapply(d: Def[_]): Option[(Rep[SparseVector[T]], Vec[T], Fractional[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, f, _*), _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "$div$up" =>
          Some((receiver, vector, f)).asInstanceOf[Option[(Rep[SparseVector[T]], Vec[T], Fractional[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector[T]], Vec[T], Fractional[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object dot {
      def unapply(d: Def[_]): Option[(Rep[SparseVector[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, n, _*), _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "dot" =>
          Some((receiver, vector, n)).asInstanceOf[Option[(Rep[SparseVector[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object * {
      def unapply(d: Def[_]): Option[(Rep[SparseVector[T]], Matr[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(matrix, n, _*), _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "$times" =>
          Some((receiver, matrix, n)).asInstanceOf[Option[(Rep[SparseVector[T]], Matr[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector[T]], Matr[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object pow_^ {
      def unapply(d: Def[_]): Option[(Rep[SparseVector[T]], DoubleRep, Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(order, n, _*), _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "pow_$up" =>
          Some((receiver, order, n)).asInstanceOf[Option[(Rep[SparseVector[T]], DoubleRep, Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector[T]], DoubleRep, Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object sum {
      def unapply(d: Def[_]): Option[(Rep[SparseVector[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(n, _*), _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "sum" =>
          Some((receiver, n)).asInstanceOf[Option[(Rep[SparseVector[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduce {
      def unapply(d: Def[_]): Option[(Rep[SparseVector[T]], RepMonoid[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(m, _*), _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "reduce" =>
          Some((receiver, m)).asInstanceOf[Option[(Rep[SparseVector[T]], RepMonoid[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector[T]], RepMonoid[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object euclideanNorm {
      def unapply(d: Def[_]): Option[(Rep[SparseVector[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(n, _*), _) if receiver.elem.isInstanceOf[SparseVectorElem[_]] && method.getName == "euclideanNorm" =>
          Some((receiver, n)).asInstanceOf[Option[(Rep[SparseVector[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVector[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object SparseVectorCompanionMethods {
  }

  def mkSparseVector[T]
    (nonZeroIndices: Coll[Int], nonZeroValues: Coll[T], length: IntRep)(implicit eT: Elem[T]): Rep[SparseVector[T]] =
    new ExpSparseVector[T](nonZeroIndices, nonZeroValues, length)
  def unmkSparseVector[T](p: Rep[Vector[T]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: SparseVectorElem[T] @unchecked =>
      Some((p.asRep[SparseVector[T]].nonZeroIndices, p.asRep[SparseVector[T]].nonZeroValues, p.asRep[SparseVector[T]].length))
    case _ =>
      None
  }

  case class ExpSparseVectorBoxed[T]
      (override val nonZeroItems: Coll[(Int, T)], override val length: IntRep)(implicit eT: Elem[T])
    extends AbsSparseVectorBoxed[T](nonZeroItems, length)

  object SparseVectorBoxedMethods {
    object items {
      def unapply(d: Def[_]): Option[Rep[SparseVectorBoxed[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SparseVectorBoxedElem[_]] && method.getName == "items" =>
          Some(receiver).asInstanceOf[Option[Rep[SparseVectorBoxed[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[SparseVectorBoxed[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object nonZeroIndices {
      def unapply(d: Def[_]): Option[Rep[SparseVectorBoxed[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SparseVectorBoxedElem[_]] && method.getName == "nonZeroIndices" =>
          Some(receiver).asInstanceOf[Option[Rep[SparseVectorBoxed[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[SparseVectorBoxed[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object nonZeroValues {
      def unapply(d: Def[_]): Option[Rep[SparseVectorBoxed[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SparseVectorBoxedElem[_]] && method.getName == "nonZeroValues" =>
          Some(receiver).asInstanceOf[Option[Rep[SparseVectorBoxed[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[SparseVectorBoxed[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[SparseVectorBoxed[T]], IntRep) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[SparseVectorBoxedElem[_]] && method.getName == "apply" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[SparseVectorBoxed[T]], IntRep) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVectorBoxed[T]], IntRep) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_apply_by_collection {
      def unapply(d: Def[_]): Option[(Rep[SparseVectorBoxed[T]], Coll[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(is, _*), _) if receiver.elem.isInstanceOf[SparseVectorBoxedElem[_]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "apply_by_collection" } =>
          Some((receiver, is)).asInstanceOf[Option[(Rep[SparseVectorBoxed[T]], Coll[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVectorBoxed[T]], Coll[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object filterBy {
      def unapply(d: Def[_]): Option[(Rep[SparseVectorBoxed[T]], Rep[T @uncheckedVariance => Boolean]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[SparseVectorBoxedElem[_]] && method.getName == "filterBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[SparseVectorBoxed[T]], Rep[T @uncheckedVariance => Boolean]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVectorBoxed[T]], Rep[T @uncheckedVariance => Boolean]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object +^ {
      def unapply(d: Def[_]): Option[(Rep[SparseVectorBoxed[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, n, _*), _) if receiver.elem.isInstanceOf[SparseVectorBoxedElem[_]] && method.getName == "$plus$up" =>
          Some((receiver, vector, n)).asInstanceOf[Option[(Rep[SparseVectorBoxed[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVectorBoxed[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object -^ {
      def unapply(d: Def[_]): Option[(Rep[SparseVectorBoxed[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, n, _*), _) if receiver.elem.isInstanceOf[SparseVectorBoxedElem[_]] && method.getName == "$minus$up" =>
          Some((receiver, vector, n)).asInstanceOf[Option[(Rep[SparseVectorBoxed[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVectorBoxed[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object *^ {
      def unapply(d: Def[_]): Option[(Rep[SparseVectorBoxed[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, n, _*), _) if receiver.elem.isInstanceOf[SparseVectorBoxedElem[_]] && method.getName == "$times$up" =>
          Some((receiver, vector, n)).asInstanceOf[Option[(Rep[SparseVectorBoxed[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVectorBoxed[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object /^ {
      def unapply(d: Def[_]): Option[(Rep[SparseVectorBoxed[T]], Vec[T], Fractional[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, f, _*), _) if receiver.elem.isInstanceOf[SparseVectorBoxedElem[_]] && method.getName == "$div$up" =>
          Some((receiver, vector, f)).asInstanceOf[Option[(Rep[SparseVectorBoxed[T]], Vec[T], Fractional[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVectorBoxed[T]], Vec[T], Fractional[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object dot {
      def unapply(d: Def[_]): Option[(Rep[SparseVectorBoxed[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, n, _*), _) if receiver.elem.isInstanceOf[SparseVectorBoxedElem[_]] && method.getName == "dot" =>
          Some((receiver, vector, n)).asInstanceOf[Option[(Rep[SparseVectorBoxed[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVectorBoxed[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object * {
      def unapply(d: Def[_]): Option[(Rep[SparseVectorBoxed[T]], Matr[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(matrix, n, _*), _) if receiver.elem.isInstanceOf[SparseVectorBoxedElem[_]] && method.getName == "$times" =>
          Some((receiver, matrix, n)).asInstanceOf[Option[(Rep[SparseVectorBoxed[T]], Matr[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVectorBoxed[T]], Matr[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object pow_^ {
      def unapply(d: Def[_]): Option[(Rep[SparseVectorBoxed[T]], DoubleRep, Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(order, n, _*), _) if receiver.elem.isInstanceOf[SparseVectorBoxedElem[_]] && method.getName == "pow_$up" =>
          Some((receiver, order, n)).asInstanceOf[Option[(Rep[SparseVectorBoxed[T]], DoubleRep, Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVectorBoxed[T]], DoubleRep, Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object sum {
      def unapply(d: Def[_]): Option[(Rep[SparseVectorBoxed[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(n, _*), _) if receiver.elem.isInstanceOf[SparseVectorBoxedElem[_]] && method.getName == "sum" =>
          Some((receiver, n)).asInstanceOf[Option[(Rep[SparseVectorBoxed[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVectorBoxed[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduce {
      def unapply(d: Def[_]): Option[(Rep[SparseVectorBoxed[T]], RepMonoid[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(m, _*), _) if receiver.elem.isInstanceOf[SparseVectorBoxedElem[_]] && method.getName == "reduce" =>
          Some((receiver, m)).asInstanceOf[Option[(Rep[SparseVectorBoxed[T]], RepMonoid[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVectorBoxed[T]], RepMonoid[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object euclideanNorm {
      def unapply(d: Def[_]): Option[(Rep[SparseVectorBoxed[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(n, _*), _) if receiver.elem.isInstanceOf[SparseVectorBoxedElem[_]] && method.getName == "euclideanNorm" =>
          Some((receiver, n)).asInstanceOf[Option[(Rep[SparseVectorBoxed[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SparseVectorBoxed[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object SparseVectorBoxedCompanionMethods {
  }

  def mkSparseVectorBoxed[T]
    (nonZeroItems: Coll[(Int, T)], length: IntRep)(implicit eT: Elem[T]): Rep[SparseVectorBoxed[T]] =
    new ExpSparseVectorBoxed[T](nonZeroItems, length)
  def unmkSparseVectorBoxed[T](p: Rep[Vector[T]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: SparseVectorBoxedElem[T] @unchecked =>
      Some((p.asRep[SparseVectorBoxed[T]].nonZeroItems, p.asRep[SparseVectorBoxed[T]].length))
    case _ =>
      None
  }

  case class ExpShiftVector[T]
      (override val nonZeroIndices: Coll[Int], override val nonZeroValues: Coll[T], override val offset: Rep[T], override val length: IntRep)(implicit eT: Elem[T])
    extends AbsShiftVector[T](nonZeroIndices, nonZeroValues, offset, length)

  object ShiftVectorMethods {
    object items {
      def unapply(d: Def[_]): Option[Rep[ShiftVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ShiftVectorElem[_]] && method.getName == "items" =>
          Some(receiver).asInstanceOf[Option[Rep[ShiftVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[ShiftVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object nonZeroItems {
      def unapply(d: Def[_]): Option[Rep[ShiftVector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ShiftVectorElem[_]] && method.getName == "nonZeroItems" =>
          Some(receiver).asInstanceOf[Option[Rep[ShiftVector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[ShiftVector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[ShiftVector[T]], IntRep) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[ShiftVectorElem[_]] && method.getName == "apply" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[ShiftVector[T]], IntRep) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ShiftVector[T]], IntRep) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_apply_by_collection {
      def unapply(d: Def[_]): Option[(Rep[ShiftVector[T]], Coll[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(is, _*), _) if receiver.elem.isInstanceOf[ShiftVectorElem[_]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "apply_by_collection" } =>
          Some((receiver, is)).asInstanceOf[Option[(Rep[ShiftVector[T]], Coll[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ShiftVector[T]], Coll[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object filterBy {
      def unapply(d: Def[_]): Option[(Rep[ShiftVector[T]], Rep[T @uncheckedVariance => Boolean]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[ShiftVectorElem[_]] && method.getName == "filterBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[ShiftVector[T]], Rep[T @uncheckedVariance => Boolean]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ShiftVector[T]], Rep[T @uncheckedVariance => Boolean]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object +^ {
      def unapply(d: Def[_]): Option[(Rep[ShiftVector[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, n, _*), _) if receiver.elem.isInstanceOf[ShiftVectorElem[_]] && method.getName == "$plus$up" =>
          Some((receiver, vector, n)).asInstanceOf[Option[(Rep[ShiftVector[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ShiftVector[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object -^ {
      def unapply(d: Def[_]): Option[(Rep[ShiftVector[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, n, _*), _) if receiver.elem.isInstanceOf[ShiftVectorElem[_]] && method.getName == "$minus$up" =>
          Some((receiver, vector, n)).asInstanceOf[Option[(Rep[ShiftVector[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ShiftVector[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object *^ {
      def unapply(d: Def[_]): Option[(Rep[ShiftVector[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, n, _*), _) if receiver.elem.isInstanceOf[ShiftVectorElem[_]] && method.getName == "$times$up" =>
          Some((receiver, vector, n)).asInstanceOf[Option[(Rep[ShiftVector[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ShiftVector[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object /^ {
      def unapply(d: Def[_]): Option[(Rep[ShiftVector[T]], Vec[T], Fractional[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, f, _*), _) if receiver.elem.isInstanceOf[ShiftVectorElem[_]] && method.getName == "$div$up" =>
          Some((receiver, vector, f)).asInstanceOf[Option[(Rep[ShiftVector[T]], Vec[T], Fractional[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ShiftVector[T]], Vec[T], Fractional[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object dot {
      def unapply(d: Def[_]): Option[(Rep[ShiftVector[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, n, _*), _) if receiver.elem.isInstanceOf[ShiftVectorElem[_]] && method.getName == "dot" =>
          Some((receiver, vector, n)).asInstanceOf[Option[(Rep[ShiftVector[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ShiftVector[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object * {
      def unapply(d: Def[_]): Option[(Rep[ShiftVector[T]], Matr[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(matrix, n, _*), _) if receiver.elem.isInstanceOf[ShiftVectorElem[_]] && method.getName == "$times" =>
          Some((receiver, matrix, n)).asInstanceOf[Option[(Rep[ShiftVector[T]], Matr[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ShiftVector[T]], Matr[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object pow_^ {
      def unapply(d: Def[_]): Option[(Rep[ShiftVector[T]], DoubleRep, Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(order, n, _*), _) if receiver.elem.isInstanceOf[ShiftVectorElem[_]] && method.getName == "pow_$up" =>
          Some((receiver, order, n)).asInstanceOf[Option[(Rep[ShiftVector[T]], DoubleRep, Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ShiftVector[T]], DoubleRep, Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object sum {
      def unapply(d: Def[_]): Option[(Rep[ShiftVector[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(n, _*), _) if receiver.elem.isInstanceOf[ShiftVectorElem[_]] && method.getName == "sum" =>
          Some((receiver, n)).asInstanceOf[Option[(Rep[ShiftVector[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ShiftVector[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduce {
      def unapply(d: Def[_]): Option[(Rep[ShiftVector[T]], RepMonoid[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(m, _*), _) if receiver.elem.isInstanceOf[ShiftVectorElem[_]] && method.getName == "reduce" =>
          Some((receiver, m)).asInstanceOf[Option[(Rep[ShiftVector[T]], RepMonoid[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ShiftVector[T]], RepMonoid[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object euclideanNorm {
      def unapply(d: Def[_]): Option[(Rep[ShiftVector[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(n, _*), _) if receiver.elem.isInstanceOf[ShiftVectorElem[_]] && method.getName == "euclideanNorm" =>
          Some((receiver, n)).asInstanceOf[Option[(Rep[ShiftVector[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ShiftVector[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkShiftVector[T]
    (nonZeroIndices: Coll[Int], nonZeroValues: Coll[T], offset: Rep[T], length: IntRep)(implicit eT: Elem[T]): Rep[ShiftVector[T]] =
    new ExpShiftVector[T](nonZeroIndices, nonZeroValues, offset, length)
  def unmkShiftVector[T](p: Rep[Vector[T]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ShiftVectorElem[T] @unchecked =>
      Some((p.asRep[ShiftVector[T]].nonZeroIndices, p.asRep[ShiftVector[T]].nonZeroValues, p.asRep[ShiftVector[T]].offset, p.asRep[ShiftVector[T]].length))
    case _ =>
      None
  }

  case class ExpShiftVectorBoxed[T]
      (override val nonZeroItems: Coll[(Int, T)], override val offset: Rep[T], override val length: IntRep)(implicit eT: Elem[T])
    extends AbsShiftVectorBoxed[T](nonZeroItems, offset, length)

  object ShiftVectorBoxedMethods {
    object items {
      def unapply(d: Def[_]): Option[Rep[ShiftVectorBoxed[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ShiftVectorBoxedElem[_]] && method.getName == "items" =>
          Some(receiver).asInstanceOf[Option[Rep[ShiftVectorBoxed[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[ShiftVectorBoxed[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object nonZeroIndices {
      def unapply(d: Def[_]): Option[Rep[ShiftVectorBoxed[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ShiftVectorBoxedElem[_]] && method.getName == "nonZeroIndices" =>
          Some(receiver).asInstanceOf[Option[Rep[ShiftVectorBoxed[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[ShiftVectorBoxed[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object nonZeroValues {
      def unapply(d: Def[_]): Option[Rep[ShiftVectorBoxed[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ShiftVectorBoxedElem[_]] && method.getName == "nonZeroValues" =>
          Some(receiver).asInstanceOf[Option[Rep[ShiftVectorBoxed[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[ShiftVectorBoxed[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[ShiftVectorBoxed[T]], IntRep) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[ShiftVectorBoxedElem[_]] && method.getName == "apply" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[ShiftVectorBoxed[T]], IntRep) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ShiftVectorBoxed[T]], IntRep) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_apply_by_collection {
      def unapply(d: Def[_]): Option[(Rep[ShiftVectorBoxed[T]], Coll[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(is, _*), _) if receiver.elem.isInstanceOf[ShiftVectorBoxedElem[_]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "apply_by_collection" } =>
          Some((receiver, is)).asInstanceOf[Option[(Rep[ShiftVectorBoxed[T]], Coll[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ShiftVectorBoxed[T]], Coll[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object filterBy {
      def unapply(d: Def[_]): Option[(Rep[ShiftVectorBoxed[T]], Rep[T @uncheckedVariance => Boolean]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[ShiftVectorBoxedElem[_]] && method.getName == "filterBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[ShiftVectorBoxed[T]], Rep[T @uncheckedVariance => Boolean]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ShiftVectorBoxed[T]], Rep[T @uncheckedVariance => Boolean]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object +^ {
      def unapply(d: Def[_]): Option[(Rep[ShiftVectorBoxed[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, n, _*), _) if receiver.elem.isInstanceOf[ShiftVectorBoxedElem[_]] && method.getName == "$plus$up" =>
          Some((receiver, vector, n)).asInstanceOf[Option[(Rep[ShiftVectorBoxed[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ShiftVectorBoxed[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object -^ {
      def unapply(d: Def[_]): Option[(Rep[ShiftVectorBoxed[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, n, _*), _) if receiver.elem.isInstanceOf[ShiftVectorBoxedElem[_]] && method.getName == "$minus$up" =>
          Some((receiver, vector, n)).asInstanceOf[Option[(Rep[ShiftVectorBoxed[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ShiftVectorBoxed[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object *^ {
      def unapply(d: Def[_]): Option[(Rep[ShiftVectorBoxed[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, n, _*), _) if receiver.elem.isInstanceOf[ShiftVectorBoxedElem[_]] && method.getName == "$times$up" =>
          Some((receiver, vector, n)).asInstanceOf[Option[(Rep[ShiftVectorBoxed[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ShiftVectorBoxed[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object /^ {
      def unapply(d: Def[_]): Option[(Rep[ShiftVectorBoxed[T]], Vec[T], Fractional[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, f, _*), _) if receiver.elem.isInstanceOf[ShiftVectorBoxedElem[_]] && method.getName == "$div$up" =>
          Some((receiver, vector, f)).asInstanceOf[Option[(Rep[ShiftVectorBoxed[T]], Vec[T], Fractional[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ShiftVectorBoxed[T]], Vec[T], Fractional[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object dot {
      def unapply(d: Def[_]): Option[(Rep[ShiftVectorBoxed[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, n, _*), _) if receiver.elem.isInstanceOf[ShiftVectorBoxedElem[_]] && method.getName == "dot" =>
          Some((receiver, vector, n)).asInstanceOf[Option[(Rep[ShiftVectorBoxed[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ShiftVectorBoxed[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object * {
      def unapply(d: Def[_]): Option[(Rep[ShiftVectorBoxed[T]], Matr[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(matrix, n, _*), _) if receiver.elem.isInstanceOf[ShiftVectorBoxedElem[_]] && method.getName == "$times" =>
          Some((receiver, matrix, n)).asInstanceOf[Option[(Rep[ShiftVectorBoxed[T]], Matr[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ShiftVectorBoxed[T]], Matr[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object pow_^ {
      def unapply(d: Def[_]): Option[(Rep[ShiftVectorBoxed[T]], DoubleRep, Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(order, n, _*), _) if receiver.elem.isInstanceOf[ShiftVectorBoxedElem[_]] && method.getName == "pow_$up" =>
          Some((receiver, order, n)).asInstanceOf[Option[(Rep[ShiftVectorBoxed[T]], DoubleRep, Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ShiftVectorBoxed[T]], DoubleRep, Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object sum {
      def unapply(d: Def[_]): Option[(Rep[ShiftVectorBoxed[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(n, _*), _) if receiver.elem.isInstanceOf[ShiftVectorBoxedElem[_]] && method.getName == "sum" =>
          Some((receiver, n)).asInstanceOf[Option[(Rep[ShiftVectorBoxed[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ShiftVectorBoxed[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduce {
      def unapply(d: Def[_]): Option[(Rep[ShiftVectorBoxed[T]], RepMonoid[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(m, _*), _) if receiver.elem.isInstanceOf[ShiftVectorBoxedElem[_]] && method.getName == "reduce" =>
          Some((receiver, m)).asInstanceOf[Option[(Rep[ShiftVectorBoxed[T]], RepMonoid[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ShiftVectorBoxed[T]], RepMonoid[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object euclideanNorm {
      def unapply(d: Def[_]): Option[(Rep[ShiftVectorBoxed[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(n, _*), _) if receiver.elem.isInstanceOf[ShiftVectorBoxedElem[_]] && method.getName == "euclideanNorm" =>
          Some((receiver, n)).asInstanceOf[Option[(Rep[ShiftVectorBoxed[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ShiftVectorBoxed[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkShiftVectorBoxed[T]
    (nonZeroItems: Coll[(Int, T)], offset: Rep[T], length: IntRep)(implicit eT: Elem[T]): Rep[ShiftVectorBoxed[T]] =
    new ExpShiftVectorBoxed[T](nonZeroItems, offset, length)
  def unmkShiftVectorBoxed[T](p: Rep[Vector[T]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ShiftVectorBoxedElem[T] @unchecked =>
      Some((p.asRep[ShiftVectorBoxed[T]].nonZeroItems, p.asRep[ShiftVectorBoxed[T]].offset, p.asRep[ShiftVectorBoxed[T]].length))
    case _ =>
      None
  }

  object VectorMethods {
    object length {
      def unapply(d: Def[_]): Option[Rep[Vector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[Vector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Vector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object items {
      def unapply(d: Def[_]): Option[Rep[Vector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "items" =>
          Some(receiver).asInstanceOf[Option[Rep[Vector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Vector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object nonZeroIndices {
      def unapply(d: Def[_]): Option[Rep[Vector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "nonZeroIndices" =>
          Some(receiver).asInstanceOf[Option[Rep[Vector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Vector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object nonZeroValues {
      def unapply(d: Def[_]): Option[Rep[Vector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "nonZeroValues" =>
          Some(receiver).asInstanceOf[Option[Rep[Vector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Vector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object nonZeroItems {
      def unapply(d: Def[_]): Option[Rep[Vector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "nonZeroItems" =>
          Some(receiver).asInstanceOf[Option[Rep[Vector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Vector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object zero {
      def unapply(d: Def[_]): Option[Rep[Vector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "zero" =>
          Some(receiver).asInstanceOf[Option[Rep[Vector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Vector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[Vector[T]], IntRep) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "apply" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[Vector[T]], IntRep) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vector[T]], IntRep) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_apply_by_collection {
      def unapply(d: Def[_]): Option[(Rep[Vector[T]], Coll[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(is, _*), _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "apply_by_collection" } =>
          Some((receiver, is)).asInstanceOf[Option[(Rep[Vector[T]], Coll[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vector[T]], Coll[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object filterBy {
      def unapply(d: Def[_]): Option[(Rep[Vector[T]], Rep[T @uncheckedVariance => Boolean]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "filterBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[Vector[T]], Rep[T @uncheckedVariance => Boolean]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vector[T]], Rep[T @uncheckedVariance => Boolean]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object +^ {
      def unapply(d: Def[_]): Option[(Rep[Vector[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, n, _*), _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "$plus$up" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, vector, n)).asInstanceOf[Option[(Rep[Vector[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vector[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object elementwise_sum_collection_+^ {
      def unapply(d: Def[_]): Option[(Rep[Vector[T]], Coll[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(coll, n, _*), _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "$plus$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "elementwise_sum_collection" } =>
          Some((receiver, coll, n)).asInstanceOf[Option[(Rep[Vector[T]], Coll[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vector[T]], Coll[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object elementwise_sum_value_+^ {
      def unapply(d: Def[_]): Option[(Rep[Vector[T]], Rep[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(value, n, _*), _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "$plus$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "elementwise_sum_value" } =>
          Some((receiver, value, n)).asInstanceOf[Option[(Rep[Vector[T]], Rep[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vector[T]], Rep[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object -^ {
      def unapply(d: Def[_]): Option[(Rep[Vector[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, n, _*), _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "$minus$up" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, vector, n)).asInstanceOf[Option[(Rep[Vector[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vector[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object elementwise_diff_collection_-^ {
      def unapply(d: Def[_]): Option[(Rep[Vector[T]], Coll[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(coll, n, _*), _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "$minus$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "elementwise_diff_collection" } =>
          Some((receiver, coll, n)).asInstanceOf[Option[(Rep[Vector[T]], Coll[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vector[T]], Coll[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object elementwise_diff_value_-^ {
      def unapply(d: Def[_]): Option[(Rep[Vector[T]], Rep[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(value, n, _*), _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "$minus$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "elementwise_diff_value" } =>
          Some((receiver, value, n)).asInstanceOf[Option[(Rep[Vector[T]], Rep[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vector[T]], Rep[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object *^ {
      def unapply(d: Def[_]): Option[(Rep[Vector[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, n, _*), _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "$times$up" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, vector, n)).asInstanceOf[Option[(Rep[Vector[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vector[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object elementwise_mult_collection_*^ {
      def unapply(d: Def[_]): Option[(Rep[Vector[T]], Coll[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(coll, n, _*), _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "$times$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "elementwise_mult_collection" } =>
          Some((receiver, coll, n)).asInstanceOf[Option[(Rep[Vector[T]], Coll[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vector[T]], Coll[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object elementwise_mult_value_*^ {
      def unapply(d: Def[_]): Option[(Rep[Vector[T]], Rep[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(value, n, _*), _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "$times$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "elementwise_mult_value" } =>
          Some((receiver, value, n)).asInstanceOf[Option[(Rep[Vector[T]], Rep[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vector[T]], Rep[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object /^ {
      def unapply(d: Def[_]): Option[(Rep[Vector[T]], Vec[T], Fractional[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, f, _*), _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "$div$up" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, vector, f)).asInstanceOf[Option[(Rep[Vector[T]], Vec[T], Fractional[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vector[T]], Vec[T], Fractional[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object elementwise_div_collection_/^ {
      def unapply(d: Def[_]): Option[(Rep[Vector[T]], Coll[T], Fractional[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(coll, f, _*), _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "$div$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "elementwise_div_collection" } =>
          Some((receiver, coll, f)).asInstanceOf[Option[(Rep[Vector[T]], Coll[T], Fractional[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vector[T]], Coll[T], Fractional[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object elementwise_div_value_/^ {
      def unapply(d: Def[_]): Option[(Rep[Vector[T]], Rep[T], Fractional[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(value, f, _*), _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "$div$up" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "elementwise_div_value" } =>
          Some((receiver, value, f)).asInstanceOf[Option[(Rep[Vector[T]], Rep[T], Fractional[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vector[T]], Rep[T], Fractional[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object * {
      def unapply(d: Def[_]): Option[(Rep[Vector[T]], Matr[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(matrix, n, _*), _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "$times" =>
          Some((receiver, matrix, n)).asInstanceOf[Option[(Rep[Vector[T]], Matr[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vector[T]], Matr[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object pow_^ {
      def unapply(d: Def[_]): Option[(Rep[Vector[T]], Rep[Double], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(order, n, _*), _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "pow_$up" =>
          Some((receiver, order, n)).asInstanceOf[Option[(Rep[Vector[T]], Rep[Double], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vector[T]], Rep[Double], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object euclideanNorm {
      def unapply(d: Def[_]): Option[(Rep[Vector[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(n, _*), _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "euclideanNorm" =>
          Some((receiver, n)).asInstanceOf[Option[(Rep[Vector[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vector[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object sum {
      def unapply(d: Def[_]): Option[(Rep[Vector[T]], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(n, _*), _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "sum" =>
          Some((receiver, n)).asInstanceOf[Option[(Rep[Vector[T]], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vector[T]], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduce {
      def unapply(d: Def[_]): Option[(Rep[Vector[T]], RepMonoid[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(m, _*), _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "reduce" =>
          Some((receiver, m)).asInstanceOf[Option[(Rep[Vector[T]], RepMonoid[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vector[T]], RepMonoid[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object dot {
      def unapply(d: Def[_]): Option[(Rep[Vector[T]], Vec[T], Numeric[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(vector, n, _*), _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "dot" =>
          Some((receiver, vector, n)).asInstanceOf[Option[(Rep[Vector[T]], Vec[T], Numeric[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vector[T]], Vec[T], Numeric[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object nonZeroesLength {
      def unapply(d: Def[_]): Option[Rep[Vector[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[VectorElem[_, _]] && method.getName == "nonZeroesLength" =>
          Some(receiver).asInstanceOf[Option[Rep[Vector[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Vector[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}

object Vectors_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAANVYTWwbRRidtWM7tkOalgYBIiIEQwWCOCBQQUGqUieFIJNE2VAhUxWNd8fOltnZZWcc2RwqThWCW8WVQyUkLr2gHjgU9QJIFQdOCCFx4sCpFFU9UHEA8c3sj9dJ1klMiIIPo/2Z+ebNe2+++dZXb6MM99CT3MAUs2mbCDytq+s5Lkr6AhOW6LzhmC1K5knjl89fujaV/vKrFDpSQ9l1zOc5raG8f7HQdqNrXZhVlMfMIFw4HhfosaqaoWw4lBJDWA4rW7bdErhOSblqcTFbRUN1x+y8hy4irYrGDIcZHhFEr1DMOeHB82EiEVnRfV7dd5bd7hysLFdRjq1izcOWAPgwx5jff5W4eoc5rGMLNBpAW3YlLOiTs2zX8UQ4RQ7CrTtmeDvEMDxAx6oX8AYuwxTNsi48izVhZNHFxru4SZagi+w+BIA5oY21jqvu01VU4MIEghZtl6onbRchBAo8r0BMd/mZjviZlvyUdOJZmFrvY/lyxXPaHeT/tDRCbRdCPLNDiDACWWBm6aNzxtv39KKdkoPbEkpOrTALgR5NcIOSAni8uXqZ3331yskUKtRQweJzdS48bIi45AFbRcyYIxTmiEDsNUGtqSS11Cxz0GeTJfKGY7uYQaSAyhHQiVqGJWRn+WwkUCeB+pxwSdhVa7tatN7JhPUq31QwpSu3Hnr2id8W3kqhVO8UeQipg/G9MKhA2bNAfkBAVrVHBNLWFMOyybe7ba7P5BENJ279bn47g86lIvKCuXanF4TI8J9+LP7w1KkUGq4pd5+huFkD/vgCJfayV3GYqKFhZ4N4/pvcBqbyalv9ciZp4BYVAatxOtJAh0CTifvQJZKrWeV5LSSg6Nt2yWGkdGal9If+3SdXpSs9NOK/8Tfm39bJv34ebQhlWIEyliA2D/kdqoDpd015wY+rOzY5OnXXOn/lY6HI1dq9+3q5fgG0nFXjHunDc5hfvrh0afzOZ+/cr/bFcN0SNnZLM3vYFaGJ/0PXo16WRitBnlVeea73ZXGeME58PyeQKdvx6J1qJkCd47GRlfgCJmLDYpM9qIWGUJ0ESpG1SFtp0h213Yp2ItofE0m6KWYeWK0ep7dP3UihzOso0wDb8yrK1J0WM0PK4TASpC1Oh8+0XsqBYuxhO6JY/SZRd72bEKuORa13TXvLG1sIRJsIzABmLsI4aTjw+kTePkSWEtYU6xHARSYgzCZAPpSSak/syVwVCXAgc8VGHpi5NqGdiI155RAILpuVHZTq4t5vdgo14jmHlxxwItR6EuMiMy1IxdsdGx56PDlNrHiWDWXnBnnx6+tv3rmxlFGVwLHgJDyLaYv4RWCQEbrZQZ5V2gzsQNCk32a7L0CoYu36XBvUCoNu2hEdMt1gR8J4fOhBbdsteA+fNUdCaybVM33qDpestVxKXrj+5/kPP3jNVUXMlpLUDyOby33QHrSTjsaVOe20ibk3Oz28ZfxBeWp75IfPWDvnPOWJZPn3MynB6pxGg5N/U5Hsxo5dKfa9AtDXrcYhrgB2kUlk+83g6eD/r+FYTMMBd25sadlts14aPgQHl3mHrJcd1H9Rq73c7RN0zAX4ILUFBw21GKZNUvdwkN49NJVwBunB1yas+uK9T5ee/v7ar6o2KsjvVvikZ9H/aPGaqJeLTHVuntMYWlBUfsUqpP8AF3HCeqIUAAA="
}
}

