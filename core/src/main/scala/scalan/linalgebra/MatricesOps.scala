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
    def columns: PA[Vector[T]] = ???
    def *(vector: Vec[T])(implicit n: Numeric[T], m: RepMonoid[T]): Vec[T]
    def *(mat: Matr[T])(implicit n: Numeric[T], m: RepMonoid[T], d: DummyImplicit): Matr[T] = {
      // FIXME probably won't work correctly, need a proper general solution
      implicit val eVec = element[DenseVector[T]].asElem[Vector[T]]
      val resColumns = mat.columns.map { col: Rep[Vector[T]] => this * col }
      companion.fromColumns(resColumns)
    }
    def companion: Rep[MatrixCompanionOps] = ???
  }

  trait MatrixCompanionOps extends TypeFamily1[Matrix] {
    def defaultOf[T: Elem] = RowMajorMatrix.defaultOf[T]
    def fromColumns[T](cols: PA[Vector[T]]): Matr[T] = ???
  }

  trait RowMajorMatrixOps[T] extends MatrixOps[T] {
    def rows: PA[DenseVector[T]]
    def numRows: Rep[Int] = rows.length
    def numColumns = rows(0).length
    // def companion = RowMajorMatrix
    
    def *(vector: Vec[T])(implicit n: Numeric[T], m: RepMonoid[T]) = DenseVector(rows.map { r => r.dot(vector) })
  }

  trait RowMajorMatrixCompanionOps extends ConcreteClass1[RowMajorMatrix] {
    def defaultOf[T: Elem] = Default.defaultVal(RowMajorMatrix(element[PArray[DenseVector[T]]].defaultRepValue))
    def fromColumns[T](cols: PA[Vector[T]]): Matr[T] = ???
  }

  trait RowMajorFlatMatrixOps[T] extends MatrixOps[T] {
    def rmValues: Rep[PArray[T]]
    def numRows: Rep[Int] = rmValues.length / numColumns
    
    def rows: PA[DenseVector[T]] = PArray(rmValues.arr.grouped(numColumns).map { row => DenseVector(PArray(row)) })
    def *(vector: Vec[T])(implicit n: Numeric[T], m: RepMonoid[T]) = DenseVector(rows.map { r => r.dot(vector) })
  }

  trait RowMajorFlatMatrixCompanionOps extends ConcreteClass1[RowMajorFlatMatrix] {
    def defaultOf[T: Elem] = Default.defaultVal(RowMajorFlatMatrix(element[PArray[T]].defaultRepValue, intElement.defaultRepValue))
    def fromColumns[T](cols: PA[Vector[T]]): Matr[T] = ???
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
    def fromColumns[T](cols: PA[Vector[T]]): Matr[T] = ???
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

trait MatricesDsl extends ScalanDsl with impl.MatricesAbs with MatricesOps with VectorsDsl {}

trait MatricesDslSeq extends MatricesDsl with impl.MatricesSeq with VectorsDslSeq with ScalanSeq

trait MatricesDslExp extends MatricesDsl with impl.MatricesExp with VectorsDslExp with ScalanStaged