package scalan.linalgebra

import scalan._
import scalan.common.Default

trait Matrices extends MatricesOps with Vectors { scalan: MatricesDsl =>
  type Matr[T] = Rep[Matrix[T]]
  
  trait Matrix[T] extends UserType[Matrix[T]] with MatrixOps[T] {}
  
  abstract class RowMajorMatrix[T](val rmValues: Rep[PArray[T]], val numColumns: Rep[Int])(implicit val elem: Elem[T]) extends Matrix[T] with RowMajorMatrixOps[T]
  
  abstract class ColumnMajorMatrix[T](val cmValues: Rep[PArray[T]], val numRows: Rep[Int])(implicit val elem: Elem[T]) extends Matrix[T] with ColumnMajorMatrixOps[T]
  
  abstract class RowMajorSparseMatrix[T](val sparseRows: Rep[PArray[SparseVector[T]]])(implicit val elem: Elem[T]) extends Matrix[T] with RowMajorSparseMatrixOps[T]
  
  abstract class ColumnMajorSparseMatrix[T](val sparseColumns: Rep[PArray[SparseVector[T]]])(implicit val elem: Elem[T]) extends Matrix[T] with ColumnMajorSparseMatrixOps[T]
}