
package scalan.linalgebra
package impl

import scalan._
import scalan.common.Default
import scala.reflect.runtime.universe._
import scalan.common.Default.defaultVal


trait MatricesAbs extends Matrices
{ self: MatricesDsl =>

  // single proxy for each type family
  implicit def proxyMatrix[T:Elem](p: Matr[T]): Matrix[T] = {
    proxyOps[Matrix[T]](p)
  }

  trait MatrixElem[From,To] extends ViewElem[From, To]

  trait MatrixCompanionElem extends CompanionElem[MatrixCompanionAbs]
  implicit lazy val MatrixCompanionElem: MatrixCompanionElem = new MatrixCompanionElem {
    lazy val tag = typeTag[MatrixCompanionAbs]
    lazy val defaultRep = defaultVal(Matrix)
  }

  trait MatrixCompanionAbs extends MatrixCompanionOps
  def Matrix: Rep[MatrixCompanionAbs]
  implicit def defaultOfMatrix[T:Elem]: Default[Rep[Matrix[T]]] = Matrix.defaultOf[T]
  implicit def proxyMatrixCompanion(p: Rep[MatrixCompanionOps]): MatrixCompanionOps = {
    proxyOps[MatrixCompanionOps](p, Some(true))
  }


  // elem for concrete class
  trait RowMajorMatrixElem[T] extends MatrixElem[RowMajorMatrixData[T], RowMajorMatrix[T]]

  // state representation type
  type RowMajorMatrixData[T] = PArray[DenseVector[T]]

  // 3) Iso for concrete class
  abstract class RowMajorMatrixIso[T](implicit elem: Elem[T])
    extends Iso[RowMajorMatrixData[T], RowMajorMatrix[T]] {
    override def from(p: Rep[RowMajorMatrix[T]]) =
      unmkRowMajorMatrix(p) match {
        case Some((rows)) => rows
        case None => !!!
      }
    override def to(p: Rep[PArray[DenseVector[T]]]) = {
      val rows = p
      RowMajorMatrix(rows)
    }
    lazy val tag = {
      implicit val tagT = element[T].tag
      typeTag[RowMajorMatrix[T]]
    }
    lazy val defaultRepTo = defaultVal[Rep[RowMajorMatrix[T]]](RowMajorMatrix(element[PArray[DenseVector[T]]].defaultRepValue))
  }
  // 4) constructor and deconstructor
  trait RowMajorMatrixCompanionAbs extends RowMajorMatrixCompanionOps {

    def apply[T]
          (rows: Rep[PArray[DenseVector[T]]])(implicit elem: Elem[T]): Rep[RowMajorMatrix[T]] =
      mkRowMajorMatrix(rows)
    def unapply[T:Elem](p: Rep[RowMajorMatrix[T]]) = unmkRowMajorMatrix(p)
  }

  def RowMajorMatrix: Rep[RowMajorMatrixCompanionAbs]
  implicit def proxyRowMajorMatrixCompanion(p: Rep[RowMajorMatrixCompanionAbs]): RowMajorMatrixCompanionAbs = {
    proxyOps[RowMajorMatrixCompanionAbs](p, Some(true))
  }

  trait RowMajorMatrixCompanionElem extends CompanionElem[RowMajorMatrixCompanionAbs]
  implicit lazy val RowMajorMatrixCompanionElem: RowMajorMatrixCompanionElem = new RowMajorMatrixCompanionElem {
    lazy val tag = typeTag[RowMajorMatrixCompanionAbs]
    lazy val defaultRep = defaultVal(RowMajorMatrix)
  }

  implicit def proxyRowMajorMatrix[T:Elem](p: Rep[RowMajorMatrix[T]]): RowMajorMatrixOps[T] = {
    proxyOps[RowMajorMatrixOps[T]](p)
  }

  implicit class ExtendedRowMajorMatrix[T](p: Rep[RowMajorMatrix[T]])(implicit elem: Elem[T]) {
    def toData: Rep[RowMajorMatrixData[T]] = isoRowMajorMatrix(elem).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoRowMajorMatrix[T](implicit elem: Elem[T]): Iso[RowMajorMatrixData[T], RowMajorMatrix[T]]

  // 6) smart constructor and deconstructor
  def mkRowMajorMatrix[T](rows: Rep[PArray[DenseVector[T]]])(implicit elem: Elem[T]): Rep[RowMajorMatrix[T]]
  def unmkRowMajorMatrix[T:Elem](p: Rep[RowMajorMatrix[T]]): Option[(Rep[PArray[DenseVector[T]]])]


  // elem for concrete class
  trait RowMajorFlatMatrixElem[T] extends MatrixElem[RowMajorFlatMatrixData[T], RowMajorFlatMatrix[T]]

  // state representation type
  type RowMajorFlatMatrixData[T] = (PArray[T], Int)

  // 3) Iso for concrete class
  abstract class RowMajorFlatMatrixIso[T](implicit elem: Elem[T])
    extends Iso[RowMajorFlatMatrixData[T], RowMajorFlatMatrix[T]] {
    override def from(p: Rep[RowMajorFlatMatrix[T]]) =
      unmkRowMajorFlatMatrix(p) match {
        case Some((rmValues, numColumns)) => Pair(rmValues, numColumns)
        case None => !!!
      }
    override def to(p: Rep[(PArray[T], Int)]) = {
      val Pair(rmValues, numColumns) = p
      RowMajorFlatMatrix(rmValues, numColumns)
    }
    lazy val tag = {
      implicit val tagT = element[T].tag
      typeTag[RowMajorFlatMatrix[T]]
    }
    lazy val defaultRepTo = defaultVal[Rep[RowMajorFlatMatrix[T]]](RowMajorFlatMatrix(element[PArray[T]].defaultRepValue, 0))
  }
  // 4) constructor and deconstructor
  trait RowMajorFlatMatrixCompanionAbs extends RowMajorFlatMatrixCompanionOps {

    def apply[T](p: Rep[RowMajorFlatMatrixData[T]])(implicit elem: Elem[T]): Rep[RowMajorFlatMatrix[T]] =
      isoRowMajorFlatMatrix(elem).to(p)
    def apply[T]
          (rmValues: Rep[PArray[T]], numColumns: Rep[Int])(implicit elem: Elem[T]): Rep[RowMajorFlatMatrix[T]] =
      mkRowMajorFlatMatrix(rmValues, numColumns)
    def unapply[T:Elem](p: Rep[RowMajorFlatMatrix[T]]) = unmkRowMajorFlatMatrix(p)
  }

  def RowMajorFlatMatrix: Rep[RowMajorFlatMatrixCompanionAbs]
  implicit def proxyRowMajorFlatMatrixCompanion(p: Rep[RowMajorFlatMatrixCompanionAbs]): RowMajorFlatMatrixCompanionAbs = {
    proxyOps[RowMajorFlatMatrixCompanionAbs](p, Some(true))
  }

  trait RowMajorFlatMatrixCompanionElem extends CompanionElem[RowMajorFlatMatrixCompanionAbs]
  implicit lazy val RowMajorFlatMatrixCompanionElem: RowMajorFlatMatrixCompanionElem = new RowMajorFlatMatrixCompanionElem {
    lazy val tag = typeTag[RowMajorFlatMatrixCompanionAbs]
    lazy val defaultRep = defaultVal(RowMajorFlatMatrix)
  }

  implicit def proxyRowMajorFlatMatrix[T:Elem](p: Rep[RowMajorFlatMatrix[T]]): RowMajorFlatMatrixOps[T] = {
    proxyOps[RowMajorFlatMatrixOps[T]](p)
  }

  implicit class ExtendedRowMajorFlatMatrix[T](p: Rep[RowMajorFlatMatrix[T]])(implicit elem: Elem[T]) {
    def toData: Rep[RowMajorFlatMatrixData[T]] = isoRowMajorFlatMatrix(elem).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoRowMajorFlatMatrix[T](implicit elem: Elem[T]): Iso[RowMajorFlatMatrixData[T], RowMajorFlatMatrix[T]]

  // 6) smart constructor and deconstructor
  def mkRowMajorFlatMatrix[T](rmValues: Rep[PArray[T]], numColumns: Rep[Int])(implicit elem: Elem[T]): Rep[RowMajorFlatMatrix[T]]
  def unmkRowMajorFlatMatrix[T:Elem](p: Rep[RowMajorFlatMatrix[T]]): Option[(Rep[PArray[T]], Rep[Int])]


  // elem for concrete class
  trait RowMajorSparseMatrixElem[T] extends MatrixElem[RowMajorSparseMatrixData[T], RowMajorSparseMatrix[T]]

  // state representation type
  type RowMajorSparseMatrixData[T] = PArray[SparseVector[T]]

  // 3) Iso for concrete class
  abstract class RowMajorSparseMatrixIso[T](implicit elem: Elem[T])
    extends Iso[RowMajorSparseMatrixData[T], RowMajorSparseMatrix[T]] {
    override def from(p: Rep[RowMajorSparseMatrix[T]]) =
      unmkRowMajorSparseMatrix(p) match {
        case Some((rows)) => rows
        case None => !!!
      }
    override def to(p: Rep[PArray[SparseVector[T]]]) = {
      val rows = p
      RowMajorSparseMatrix(rows)
    }
    lazy val tag = {
      implicit val tagT = element[T].tag
      typeTag[RowMajorSparseMatrix[T]]
    }
    lazy val defaultRepTo = defaultVal[Rep[RowMajorSparseMatrix[T]]](RowMajorSparseMatrix(element[PArray[SparseVector[T]]].defaultRepValue))
  }
  // 4) constructor and deconstructor
  trait RowMajorSparseMatrixCompanionAbs extends RowMajorSparseMatrixCompanionOps {

    def apply[T]
          (rows: Rep[PArray[SparseVector[T]]])(implicit elem: Elem[T]): Rep[RowMajorSparseMatrix[T]] =
      mkRowMajorSparseMatrix(rows)
    def unapply[T:Elem](p: Rep[RowMajorSparseMatrix[T]]) = unmkRowMajorSparseMatrix(p)
  }

  def RowMajorSparseMatrix: Rep[RowMajorSparseMatrixCompanionAbs]
  implicit def proxyRowMajorSparseMatrixCompanion(p: Rep[RowMajorSparseMatrixCompanionAbs]): RowMajorSparseMatrixCompanionAbs = {
    proxyOps[RowMajorSparseMatrixCompanionAbs](p, Some(true))
  }

  trait RowMajorSparseMatrixCompanionElem extends CompanionElem[RowMajorSparseMatrixCompanionAbs]
  implicit lazy val RowMajorSparseMatrixCompanionElem: RowMajorSparseMatrixCompanionElem = new RowMajorSparseMatrixCompanionElem {
    lazy val tag = typeTag[RowMajorSparseMatrixCompanionAbs]
    lazy val defaultRep = defaultVal(RowMajorSparseMatrix)
  }

  implicit def proxyRowMajorSparseMatrix[T:Elem](p: Rep[RowMajorSparseMatrix[T]]): RowMajorSparseMatrixOps[T] = {
    proxyOps[RowMajorSparseMatrixOps[T]](p)
  }

  implicit class ExtendedRowMajorSparseMatrix[T](p: Rep[RowMajorSparseMatrix[T]])(implicit elem: Elem[T]) {
    def toData: Rep[RowMajorSparseMatrixData[T]] = isoRowMajorSparseMatrix(elem).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoRowMajorSparseMatrix[T](implicit elem: Elem[T]): Iso[RowMajorSparseMatrixData[T], RowMajorSparseMatrix[T]]

  // 6) smart constructor and deconstructor
  def mkRowMajorSparseMatrix[T](rows: Rep[PArray[SparseVector[T]]])(implicit elem: Elem[T]): Rep[RowMajorSparseMatrix[T]]
  def unmkRowMajorSparseMatrix[T:Elem](p: Rep[RowMajorSparseMatrix[T]]): Option[(Rep[PArray[SparseVector[T]]])]

}


trait MatricesSeq extends MatricesAbs { self: ScalanSeq with MatricesDsl =>

  lazy val Matrix: Rep[MatrixCompanionAbs] = new MatrixCompanionAbs with UserTypeSeq[MatrixCompanionAbs, MatrixCompanionAbs] {
    lazy val selfType = element[MatrixCompanionAbs]
  }

  case class SeqRowMajorMatrix[T]
      (override val rows: Rep[PArray[DenseVector[T]]])
      (implicit override val elem: Elem[T])
    extends RowMajorMatrix[T](rows) with UserTypeSeq[Matrix[T], RowMajorMatrix[T]] {
    lazy val selfType = element[RowMajorMatrix[T]].asInstanceOf[Elem[Matrix[T]]]
  }

  lazy val RowMajorMatrix = new RowMajorMatrixCompanionAbs with UserTypeSeq[RowMajorMatrixCompanionAbs, RowMajorMatrixCompanionAbs] {
    lazy val selfType = element[RowMajorMatrixCompanionAbs]
  }



  implicit def isoRowMajorMatrix[T](implicit elem: Elem[T]):Iso[RowMajorMatrixData[T], RowMajorMatrix[T]] =
    new RowMajorMatrixIso[T] { i =>
      // should use i as iso reference
      lazy val eTo =
        new SeqViewElem[RowMajorMatrixData[T], RowMajorMatrix[T]]()(i) with RowMajorMatrixElem[T]
    }


  def mkRowMajorMatrix[T]
      (rows: Rep[PArray[DenseVector[T]]])(implicit elem: Elem[T]) =
      new SeqRowMajorMatrix[T](rows)
  def unmkRowMajorMatrix[T:Elem](p: Rep[RowMajorMatrix[T]]) =
    Some((p.rows))


  case class SeqRowMajorFlatMatrix[T]
      (override val rmValues: Rep[PArray[T]], override val numColumns: Rep[Int])
      (implicit override val elem: Elem[T])
    extends RowMajorFlatMatrix[T](rmValues, numColumns) with UserTypeSeq[Matrix[T], RowMajorFlatMatrix[T]] {
    lazy val selfType = element[RowMajorFlatMatrix[T]].asInstanceOf[Elem[Matrix[T]]]
  }

  lazy val RowMajorFlatMatrix = new RowMajorFlatMatrixCompanionAbs with UserTypeSeq[RowMajorFlatMatrixCompanionAbs, RowMajorFlatMatrixCompanionAbs] {
    lazy val selfType = element[RowMajorFlatMatrixCompanionAbs]
  }



  implicit def isoRowMajorFlatMatrix[T](implicit elem: Elem[T]):Iso[RowMajorFlatMatrixData[T], RowMajorFlatMatrix[T]] =
    new RowMajorFlatMatrixIso[T] { i =>
      // should use i as iso reference
      lazy val eTo =
        new SeqViewElem[RowMajorFlatMatrixData[T], RowMajorFlatMatrix[T]]()(i) with RowMajorFlatMatrixElem[T]
    }


  def mkRowMajorFlatMatrix[T]
      (rmValues: Rep[PArray[T]], numColumns: Rep[Int])(implicit elem: Elem[T]) =
      new SeqRowMajorFlatMatrix[T](rmValues, numColumns)
  def unmkRowMajorFlatMatrix[T:Elem](p: Rep[RowMajorFlatMatrix[T]]) =
    Some((p.rmValues, p.numColumns))


  case class SeqRowMajorSparseMatrix[T]
      (override val rows: Rep[PArray[SparseVector[T]]])
      (implicit override val elem: Elem[T])
    extends RowMajorSparseMatrix[T](rows) with UserTypeSeq[Matrix[T], RowMajorSparseMatrix[T]] {
    lazy val selfType = element[RowMajorSparseMatrix[T]].asInstanceOf[Elem[Matrix[T]]]
  }

  lazy val RowMajorSparseMatrix = new RowMajorSparseMatrixCompanionAbs with UserTypeSeq[RowMajorSparseMatrixCompanionAbs, RowMajorSparseMatrixCompanionAbs] {
    lazy val selfType = element[RowMajorSparseMatrixCompanionAbs]
  }



  implicit def isoRowMajorSparseMatrix[T](implicit elem: Elem[T]):Iso[RowMajorSparseMatrixData[T], RowMajorSparseMatrix[T]] =
    new RowMajorSparseMatrixIso[T] { i =>
      // should use i as iso reference
      lazy val eTo =
        new SeqViewElem[RowMajorSparseMatrixData[T], RowMajorSparseMatrix[T]]()(i) with RowMajorSparseMatrixElem[T]
    }


  def mkRowMajorSparseMatrix[T]
      (rows: Rep[PArray[SparseVector[T]]])(implicit elem: Elem[T]) =
      new SeqRowMajorSparseMatrix[T](rows)
  def unmkRowMajorSparseMatrix[T:Elem](p: Rep[RowMajorSparseMatrix[T]]) =
    Some((p.rows))

}


trait MatricesExp extends MatricesAbs with scalan.ProxyExp with scalan.ViewsExp { self: ScalanStaged with MatricesDsl =>

  lazy val Matrix: Rep[MatrixCompanionAbs] = new MatrixCompanionAbs with UserTypeExp[MatrixCompanionAbs, MatrixCompanionAbs] {
    lazy val selfType = element[MatrixCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  case class ExpRowMajorMatrix[T]
      (override val rows: Rep[PArray[DenseVector[T]]])
      (implicit override val elem: Elem[T])
    extends RowMajorMatrix[T](rows) with UserTypeExp[Matrix[T], RowMajorMatrix[T]] {
    lazy val selfType = element[RowMajorMatrix[T]].asInstanceOf[Elem[Matrix[T]]]
    override def mirror(t: Transformer) = ExpRowMajorMatrix[T](t(rows))
  }

  lazy val RowMajorMatrix: Rep[RowMajorMatrixCompanionAbs] = new RowMajorMatrixCompanionAbs with UserTypeExp[RowMajorMatrixCompanionAbs, RowMajorMatrixCompanionAbs] {
    lazy val selfType = element[RowMajorMatrixCompanionAbs]
    override def mirror(t: Transformer) = this
  }



  def mkRowMajorMatrix[T]
    (rows: Rep[PArray[DenseVector[T]]])(implicit elem: Elem[T]) =
    new ExpRowMajorMatrix[T](rows)
  def unmkRowMajorMatrix[T:Elem](p: Rep[RowMajorMatrix[T]]) =
    Some((p.rows))


  implicit def isoRowMajorMatrix[T](implicit elem: Elem[T]):Iso[RowMajorMatrixData[T], RowMajorMatrix[T]] =
    new RowMajorMatrixIso[T] { i =>
      // should use i as iso reference
      lazy val eTo =
        new StagedViewElem[RowMajorMatrixData[T], RowMajorMatrix[T]]()(i) with RowMajorMatrixElem[T]
    }


  case class ExpRowMajorFlatMatrix[T]
      (override val rmValues: Rep[PArray[T]], override val numColumns: Rep[Int])
      (implicit override val elem: Elem[T])
    extends RowMajorFlatMatrix[T](rmValues, numColumns) with UserTypeExp[Matrix[T], RowMajorFlatMatrix[T]] {
    lazy val selfType = element[RowMajorFlatMatrix[T]].asInstanceOf[Elem[Matrix[T]]]
    override def mirror(t: Transformer) = ExpRowMajorFlatMatrix[T](t(rmValues), t(numColumns))
  }

  lazy val RowMajorFlatMatrix: Rep[RowMajorFlatMatrixCompanionAbs] = new RowMajorFlatMatrixCompanionAbs with UserTypeExp[RowMajorFlatMatrixCompanionAbs, RowMajorFlatMatrixCompanionAbs] {
    lazy val selfType = element[RowMajorFlatMatrixCompanionAbs]
    override def mirror(t: Transformer) = this
  }



  def mkRowMajorFlatMatrix[T]
    (rmValues: Rep[PArray[T]], numColumns: Rep[Int])(implicit elem: Elem[T]) =
    new ExpRowMajorFlatMatrix[T](rmValues, numColumns)
  def unmkRowMajorFlatMatrix[T:Elem](p: Rep[RowMajorFlatMatrix[T]]) =
    Some((p.rmValues, p.numColumns))


  implicit def isoRowMajorFlatMatrix[T](implicit elem: Elem[T]):Iso[RowMajorFlatMatrixData[T], RowMajorFlatMatrix[T]] =
    new RowMajorFlatMatrixIso[T] { i =>
      // should use i as iso reference
      lazy val eTo =
        new StagedViewElem[RowMajorFlatMatrixData[T], RowMajorFlatMatrix[T]]()(i) with RowMajorFlatMatrixElem[T]
    }


  case class ExpRowMajorSparseMatrix[T]
      (override val rows: Rep[PArray[SparseVector[T]]])
      (implicit override val elem: Elem[T])
    extends RowMajorSparseMatrix[T](rows) with UserTypeExp[Matrix[T], RowMajorSparseMatrix[T]] {
    lazy val selfType = element[RowMajorSparseMatrix[T]].asInstanceOf[Elem[Matrix[T]]]
    override def mirror(t: Transformer) = ExpRowMajorSparseMatrix[T](t(rows))
  }

  lazy val RowMajorSparseMatrix: Rep[RowMajorSparseMatrixCompanionAbs] = new RowMajorSparseMatrixCompanionAbs with UserTypeExp[RowMajorSparseMatrixCompanionAbs, RowMajorSparseMatrixCompanionAbs] {
    lazy val selfType = element[RowMajorSparseMatrixCompanionAbs]
    override def mirror(t: Transformer) = this
  }



  def mkRowMajorSparseMatrix[T]
    (rows: Rep[PArray[SparseVector[T]]])(implicit elem: Elem[T]) =
    new ExpRowMajorSparseMatrix[T](rows)
  def unmkRowMajorSparseMatrix[T:Elem](p: Rep[RowMajorSparseMatrix[T]]) =
    Some((p.rows))


  implicit def isoRowMajorSparseMatrix[T](implicit elem: Elem[T]):Iso[RowMajorSparseMatrixData[T], RowMajorSparseMatrix[T]] =
    new RowMajorSparseMatrixIso[T] { i =>
      // should use i as iso reference
      lazy val eTo =
        new StagedViewElem[RowMajorSparseMatrixData[T], RowMajorSparseMatrix[T]]()(i) with RowMajorSparseMatrixElem[T]
    }

}
