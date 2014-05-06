package scalan.linalgebra

import scalan.ScalanDsl
import scalan.ScalanSeqImplementation
import scalan.ScalanStaged
import scalan.common.Default
import scalan.ScalanSeq

trait MatricesOps { scalan: MatricesDsl =>
  trait MatrixOps[T] {
    def numColumns: Rep[Int]
    def numRows: Rep[Int]
    implicit def elem: Elem[T]
    def rows: PA[Vector[T]]
    def *(vector: Vec[T])(implicit n: Numeric[T], m: RepMonoid[T]): Vec[T]
  }

  trait MatrixCompanionOps extends TypeFamily1[Matrix] {
    def defaultOf[T: Elem] = RowMajorMatrix.defaultOf[T]
  }

  trait RowMajorMatrixOps[T] extends MatrixOps[T] {
    def rows: PA[DenseVector[T]]
    def numRows: Rep[Int] = rows.length
    def numColumns = rows(0).length
    
    def *(vector: Vec[T])(implicit n: Numeric[T], m: RepMonoid[T]) = DenseVector(rows.map { r => r.dot(vector) })
  }

  trait RowMajorMatrixCompanionOps extends ConcreteClass1[RowMajorMatrix] {
    def defaultOf[T: Elem] = Default.defaultVal(RowMajorMatrix(element[PArray[DenseVector[T]]].defaultRepValue))
  }

  trait RowMajorFlatMatrixOps[T] extends MatrixOps[T] {
    def rmValues: Rep[PArray[T]]
    def numRows: Rep[Int] = rmValues.length / numColumns
    
    def rows: PA[DenseVector[T]] = ???
    def *(vector: Vec[T])(implicit n: Numeric[T], m: RepMonoid[T]) = DenseVector(rows.map { r => r.dot(vector) })
  }

  trait RowMajorFlatMatrixCompanionOps extends ConcreteClass1[RowMajorFlatMatrix] {
    def defaultOf[T: Elem] = Default.defaultVal(RowMajorFlatMatrix(element[PArray[T]].defaultRepValue, intElement.defaultRepValue))
  }

//  trait ColumnMajorMatrixOps[T] extends MatrixOps[T] {
//    def cmValues: Rep[PArray[T]]
//    def numColumns: Rep[Int] = cmValues.length / numRows
//  }
//
//  trait ColumnMajorMatrixCompanionOps extends ConcreteClass1[ColumnMajorMatrix] {
//    def defaultOf[T: Elem] = Default.defaultVal(ColumnMajorMatrix(element[PArray[T]].defaultRepValue, intElement.defaultRepValue))
//  }

  trait RowMajorSparseMatrixOps[T] extends MatrixOps[T] {
    def rows: Rep[PArray[SparseVector[T]]]
    def numRows = rows.length
    def numColumns = rows(0).length
    def *(vector: Vec[T])(implicit n: Numeric[T], m: RepMonoid[T]) = DenseVector(rows.map { r => r.dot(vector) })
  }

  trait RowMajorSparseMatrixCompanionOps extends ConcreteClass1[RowMajorSparseMatrix] {
    def defaultOf[T: Elem] = Default.defaultVal(RowMajorSparseMatrix(element[PArray[SparseVector[T]]].defaultRepValue))
  }

//  trait ColumnMajorSparseMatrixOps[T] extends MatrixOps[T] {
//    def sparseColumns: Rep[PArray[SparseVector[T]]]
//    def numRows = sparseColumns(0).length
//    def numColumns = sparseColumns.length
//  }
//
//  trait ColumnMajorSparseMatrixCompanionOps extends ConcreteClass1[ColumnMajorSparseMatrix] {
//    def defaultOf[T: Elem] = Default.defaultVal(ColumnMajorSparseMatrix(element[PArray[SparseVector[T]]].defaultRepValue))
//  }
  
//  def matrixVectorMult[T: Elem: Numeric: RepMonoid](m: Matr[T], v: Vec[T]) = {
//    DenseVector(m.rows.map { r => r.dot(v) })
//  }
}

trait MatricesDsl extends ScalanDsl with MatricesAbs with MatricesOps with VectorsDsl {}

trait MatricesDslSeq extends MatricesDsl with MatricesSeq with VectorsDslSeq with ScalanSeq

trait MatricesDslExp extends MatricesDsl with MatricesExp with VectorsDslExp with ScalanStaged