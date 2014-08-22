
package scalan.linalgebra
package impl

import scalan._
import scalan.common.Default
import scalan.common.Lazy
import scalan.common.OverloadHack.Overloaded1
import scalan.arrays.PArrays
import scalan.arrays.PArraysDsl
import scalan.arrays.PArraysDslExp
import scalan.arrays.PArraysDslSeq
import scala.reflect.runtime.universe._
import scalan.common.Default


trait VectorsAbs extends Vectors
{ self: VectorsDsl =>

  // single proxy for each type family
  implicit def proxyVector[T:Elem](p: Rep[Vector[T]]): Vector[T] =
    proxyOps[Vector[T]](p)

  trait VectorElem[From,To] extends ViewElem[From, To]

  trait VectorCompanionElem extends CompanionElem[VectorCompanionAbs]
  implicit lazy val VectorCompanionElem: VectorCompanionElem = new VectorCompanionElem {
    lazy val tag = typeTag[VectorCompanionAbs]
    lazy val defaultRep = Default.defaultVal(Vector)
  }

  trait VectorCompanionAbs extends VectorCompanion
  def Vector: Rep[VectorCompanionAbs]
  implicit def proxyVectorCompanion(p: Rep[VectorCompanion]): VectorCompanion = {
    proxyOps[VectorCompanion](p, true)
  }


  // elem for concrete class
  class DenseVectorElem[T](implicit iso: Iso[DenseVectorData[T], DenseVector[T]]) extends VectorElem[DenseVectorData[T], DenseVector[T]]

  // state representation type
  type DenseVectorData[T] = PArray[T]

  // 3) Iso for concrete class
  class DenseVectorIso[T](implicit elem: Elem[T])
    extends Iso[DenseVectorData[T], DenseVector[T]] {
    override def from(p: Rep[DenseVector[T]]) =
      unmkDenseVector(p) match {
        case Some((coords)) => coords
        case None => !!!
      }
    override def to(p: Rep[PArray[T]]) = {
      val coords = p
      DenseVector(coords)
    }
    lazy val tag = {
      implicit val tagT = element[T].tag
      typeTag[DenseVector[T]]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[DenseVector[T]]](DenseVector(element[PArray[T]].defaultRepValue))
    lazy val eTo = new DenseVectorElem[T]()(this)
  }
  // 4) constructor and deconstructor
  trait DenseVectorCompanionAbs extends DenseVectorCompanion {

    def apply[T](coords: Rep[PArray[T]])(implicit elem: Elem[T]): Rep[DenseVector[T]] =
      mkDenseVector(coords)
    def unapply[T:Elem](p: Rep[DenseVector[T]]) = unmkDenseVector(p)
  }
  def DenseVector: Rep[DenseVectorCompanionAbs]
  implicit def proxyDenseVectorCompanion(p: Rep[DenseVectorCompanionAbs]): DenseVectorCompanionAbs = {
    proxyOps[DenseVectorCompanionAbs](p, true)
  }

  trait DenseVectorCompanionElem extends CompanionElem[DenseVectorCompanionAbs]
  implicit lazy val DenseVectorCompanionElem: DenseVectorCompanionElem = new DenseVectorCompanionElem {
    lazy val tag = typeTag[DenseVectorCompanionAbs]
    lazy val defaultRep = Default.defaultVal(DenseVector)
  }

  implicit def proxyDenseVector[T:Elem](p: Rep[DenseVector[T]]): DenseVector[T] = {
    proxyOps[DenseVector[T]](p)
  }

  implicit class ExtendedDenseVector[T](p: Rep[DenseVector[T]])(implicit elem: Elem[T]) {
    def toData: Rep[DenseVectorData[T]] = isoDenseVector(elem).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoDenseVector[T](implicit elem: Elem[T]): Iso[DenseVectorData[T], DenseVector[T]] =
    new DenseVectorIso[T]

  // 6) smart constructor and deconstructor
  def mkDenseVector[T](coords: Rep[PArray[T]])(implicit elem: Elem[T]): Rep[DenseVector[T]]
  def unmkDenseVector[T:Elem](p: Rep[DenseVector[T]]): Option[(Rep[PArray[T]])]


  // elem for concrete class
  class SparseVectorElem[T](implicit iso: Iso[SparseVectorData[T], SparseVector[T]]) extends VectorElem[SparseVectorData[T], SparseVector[T]]

  // state representation type
  type SparseVectorData[T] = (Array[Int], (PArray[T], Int))

  // 3) Iso for concrete class
  class SparseVectorIso[T](implicit elem: Elem[T])
    extends Iso[SparseVectorData[T], SparseVector[T]] {
    override def from(p: Rep[SparseVector[T]]) =
      unmkSparseVector(p) match {
        case Some((nonZeroIndices, nonZeroValues, length)) => Pair(nonZeroIndices, Pair(nonZeroValues, length))
        case None => !!!
      }
    override def to(p: Rep[(Array[Int], (PArray[T], Int))]) = {
      val Pair(nonZeroIndices, Pair(nonZeroValues, length)) = p
      SparseVector(nonZeroIndices, nonZeroValues, length)
    }
    lazy val tag = {
      implicit val tagT = element[T].tag
      typeTag[SparseVector[T]]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[SparseVector[T]]](SparseVector(element[Array[Int]].defaultRepValue, element[PArray[T]].defaultRepValue, 0))
    lazy val eTo = new SparseVectorElem[T]()(this)
  }
  // 4) constructor and deconstructor
  trait SparseVectorCompanionAbs extends SparseVectorCompanion {

    def apply[T](p: Rep[SparseVectorData[T]])(implicit elem: Elem[T]): Rep[SparseVector[T]] =
      isoSparseVector(elem).to(p)
    def apply[T](nonZeroIndices: Rep[Array[Int]], nonZeroValues: Rep[PArray[T]], length: Rep[Int])(implicit elem: Elem[T]): Rep[SparseVector[T]] =
      mkSparseVector(nonZeroIndices, nonZeroValues, length)
    def unapply[T:Elem](p: Rep[SparseVector[T]]) = unmkSparseVector(p)
  }
  def SparseVector: Rep[SparseVectorCompanionAbs]
  implicit def proxySparseVectorCompanion(p: Rep[SparseVectorCompanionAbs]): SparseVectorCompanionAbs = {
    proxyOps[SparseVectorCompanionAbs](p, true)
  }

  trait SparseVectorCompanionElem extends CompanionElem[SparseVectorCompanionAbs]
  implicit lazy val SparseVectorCompanionElem: SparseVectorCompanionElem = new SparseVectorCompanionElem {
    lazy val tag = typeTag[SparseVectorCompanionAbs]
    lazy val defaultRep = Default.defaultVal(SparseVector)
  }

  implicit def proxySparseVector[T:Elem](p: Rep[SparseVector[T]]): SparseVector[T] = {
    proxyOps[SparseVector[T]](p)
  }

  implicit class ExtendedSparseVector[T](p: Rep[SparseVector[T]])(implicit elem: Elem[T]) {
    def toData: Rep[SparseVectorData[T]] = isoSparseVector(elem).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoSparseVector[T](implicit elem: Elem[T]): Iso[SparseVectorData[T], SparseVector[T]] =
    new SparseVectorIso[T]

  // 6) smart constructor and deconstructor
  def mkSparseVector[T](nonZeroIndices: Rep[Array[Int]], nonZeroValues: Rep[PArray[T]], length: Rep[Int])(implicit elem: Elem[T]): Rep[SparseVector[T]]
  def unmkSparseVector[T:Elem](p: Rep[SparseVector[T]]): Option[(Rep[Array[Int]], Rep[PArray[T]], Rep[Int])]

}


trait VectorsSeq extends VectorsAbs { self: ScalanSeq with VectorsDsl =>
  lazy val Vector: Rep[VectorCompanionAbs] = new VectorCompanionAbs with UserTypeSeq[VectorCompanionAbs, VectorCompanionAbs] {
    lazy val selfType = element[VectorCompanionAbs]
  }

  case class SeqDenseVector[T]
      (override val coords: Rep[PArray[T]])
      (implicit override val elem: Elem[T])
    extends DenseVector[T](coords) with UserTypeSeq[Vector[T], DenseVector[T]] {
    lazy val selfType = element[DenseVector[T]].asInstanceOf[Elem[Vector[T]]]
  }
  lazy val DenseVector = new DenseVectorCompanionAbs with UserTypeSeq[DenseVectorCompanionAbs, DenseVectorCompanionAbs] {
    lazy val selfType = element[DenseVectorCompanionAbs]
  }

  def mkDenseVector[T]
      (coords: Rep[PArray[T]])(implicit elem: Elem[T]) =
      new SeqDenseVector[T](coords)
  def unmkDenseVector[T:Elem](p: Rep[DenseVector[T]]) =
    Some((p.coords))


  case class SeqSparseVector[T]
      (override val nonZeroIndices: Rep[Array[Int]], override val nonZeroValues: Rep[PArray[T]], override val length: Rep[Int])
      (implicit override val elem: Elem[T])
    extends SparseVector[T](nonZeroIndices, nonZeroValues, length) with UserTypeSeq[Vector[T], SparseVector[T]] {
    lazy val selfType = element[SparseVector[T]].asInstanceOf[Elem[Vector[T]]]
  }
  lazy val SparseVector = new SparseVectorCompanionAbs with UserTypeSeq[SparseVectorCompanionAbs, SparseVectorCompanionAbs] {
    lazy val selfType = element[SparseVectorCompanionAbs]
  }

  def mkSparseVector[T]
      (nonZeroIndices: Rep[Array[Int]], nonZeroValues: Rep[PArray[T]], length: Rep[Int])(implicit elem: Elem[T]) =
      new SeqSparseVector[T](nonZeroIndices, nonZeroValues, length)
  def unmkSparseVector[T:Elem](p: Rep[SparseVector[T]]) =
    Some((p.nonZeroIndices, p.nonZeroValues, p.length))

}


trait VectorsExp extends VectorsAbs { self: ScalanExp with VectorsDsl =>
  lazy val Vector: Rep[VectorCompanionAbs] = new VectorCompanionAbs with UserTypeDef[VectorCompanionAbs, VectorCompanionAbs] {
    lazy val selfType = element[VectorCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  case class ExpDenseVector[T]
      (override val coords: Rep[PArray[T]])
      (implicit override val elem: Elem[T])
    extends DenseVector[T](coords) with UserTypeDef[Vector[T], DenseVector[T]] {
    lazy val selfType = element[DenseVector[T]].asInstanceOf[Elem[Vector[T]]]
    override def mirror(t: Transformer) = ExpDenseVector[T](t(coords))
  }

  lazy val DenseVector: Rep[DenseVectorCompanionAbs] = new DenseVectorCompanionAbs with UserTypeDef[DenseVectorCompanionAbs, DenseVectorCompanionAbs] {
    lazy val selfType = element[DenseVectorCompanionAbs]
    override def mirror(t: Transformer) = this
  }


  def mkDenseVector[T]
    (coords: Rep[PArray[T]])(implicit elem: Elem[T]) =
    new ExpDenseVector[T](coords)
  def unmkDenseVector[T:Elem](p: Rep[DenseVector[T]]) =
    Some((p.coords))


  case class ExpSparseVector[T]
      (override val nonZeroIndices: Rep[Array[Int]], override val nonZeroValues: Rep[PArray[T]], override val length: Rep[Int])
      (implicit override val elem: Elem[T])
    extends SparseVector[T](nonZeroIndices, nonZeroValues, length) with UserTypeDef[Vector[T], SparseVector[T]] {
    lazy val selfType = element[SparseVector[T]].asInstanceOf[Elem[Vector[T]]]
    override def mirror(t: Transformer) = ExpSparseVector[T](t(nonZeroIndices), t(nonZeroValues), t(length))
  }

  lazy val SparseVector: Rep[SparseVectorCompanionAbs] = new SparseVectorCompanionAbs with UserTypeDef[SparseVectorCompanionAbs, SparseVectorCompanionAbs] {
    lazy val selfType = element[SparseVectorCompanionAbs]
    override def mirror(t: Transformer) = this
  }


  def mkSparseVector[T]
    (nonZeroIndices: Rep[Array[Int]], nonZeroValues: Rep[PArray[T]], length: Rep[Int])(implicit elem: Elem[T]) =
    new ExpSparseVector[T](nonZeroIndices, nonZeroValues, length)
  def unmkSparseVector[T:Elem](p: Rep[SparseVector[T]]) =
    Some((p.nonZeroIndices, p.nonZeroValues, p.length))

}
