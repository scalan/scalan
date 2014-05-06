package scalan.linalgebra

import scalan._
import scalan.common.Default

trait Matrices extends MatricesOps with Vectors { scalan: MatricesDsl =>
  type Matr[T] = Rep[Matrix[T]]
  
  trait Matrix[T] extends UserType[Matrix[T]] with MatrixOps[T] {}
  
  abstract class RowMajorMatrix[T](val rows: Rep[PArray[DenseVector[T]]])(implicit val elem: Elem[T]) extends Matrix[T] with RowMajorMatrixOps[T]
  
  abstract class RowMajorFlatMatrix[T](val rmValues: Rep[PArray[T]], val numColumns: Rep[Int])(implicit val elem: Elem[T]) extends Matrix[T] with RowMajorFlatMatrixOps[T]
  
  abstract class RowMajorSparseMatrix[T](val rows: Rep[PArray[SparseVector[T]]])(implicit val elem: Elem[T]) extends Matrix[T] with RowMajorSparseMatrixOps[T]
}