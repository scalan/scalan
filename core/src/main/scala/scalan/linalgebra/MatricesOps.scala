package scalan.linalgebra

import scalan.ScalanDsl
import scalan.ScalanSeqImplementation
import scalan.ScalanStaged
import scalan.common.Default
import scalan.ScalanSeq

trait MatricesOps { scalan: MatricesDsl =>
  // FIXME probably won't work correctly, need a proper general solution
  implicit def eVec[T: Elem]: Elem[Vector[T]] = element[DenseVector[T]].asElem[Vector[T]]
      
  trait MatrixOps[T] {
    def numColumns: Rep[Int]
    def numRows: Rep[Int]
    implicit def elem: Elem[T]
    def rows: PA[Vector[T]]
    def columns: PA[Vector[T]]
    def *(vector: Vec[T])(implicit n: Numeric[T], m: RepMonoid[T]): Vec[T] =
      DenseVector(rows.map { r => r.dot(vector) })
    def *(mat: Matr[T])(implicit n: Numeric[T], m: RepMonoid[T], d: DummyImplicit): Matr[T] = {
      val resColumns = mat.columns.map { col: Rep[Vector[T]] => this * col }
      companion.fromColumns(resColumns)
    }
    def companion: Rep[MatrixCompanionOps]
  }

  trait MatrixCompanionOps extends TypeFamily1[Matrix] {
    def defaultOf[T: Elem]: Default[Rep[Matrix[T]]] =
      RowMajorMatrix.defaultOf[T]
    def fromColumns[T: Elem](cols: PA[Vector[T]]): Matr[T] =
      RowMajorMatrix.fromColumns(cols)
  }

  trait RowMajorMatrixOps[T] extends MatrixOps[T] {
    def companion = RowMajorMatrix
    def rows: PA[DenseVector[T]]
    def numRows: Rep[Int] = rows.length
    def numColumns = rows(0).length
    def columns =
      PArray(Array.tabulate(numColumns) { j => DenseVector(rows.map(_(j))) })
  }

  trait RowMajorMatrixCompanionOps extends ConcreteClass1[RowMajorMatrix] with MatrixCompanionOps {
    override def defaultOf[T: Elem] = Default.defaultVal(RowMajorMatrix(element[PArray[DenseVector[T]]].defaultRepValue))
    override def fromColumns[T: Elem](cols: PA[Vector[T]]): Matr[T] =
      RowMajorMatrix(PArray(Array.tabulate(cols(0).length) { i => DenseVector(cols.map(_(i))) }))
  }

  trait RowMajorFlatMatrixOps[T] extends MatrixOps[T] {
    def companion = RowMajorFlatMatrix
    def rmValues: Rep[PArray[T]]
    def numRows: Rep[Int] = rmValues.length / numColumns
    def columns =
      PArray(Array.tabulate(numColumns) { i =>
        DenseVector(PArray(rmValues.arr.stride(0, numRows, numColumns)))
      })
    
    def rows: PA[DenseVector[T]] = PArray(rmValues.arr.grouped(numColumns).map { row => DenseVector(PArray(row)) })
  }

  trait RowMajorFlatMatrixCompanionOps extends ConcreteClass1[RowMajorFlatMatrix] with MatrixCompanionOps {
    override def defaultOf[T: Elem] = Default.defaultVal(RowMajorFlatMatrix(element[PArray[T]].defaultRepValue, intElement.defaultRepValue))
    override def fromColumns[T: Elem](cols: PA[Vector[T]]): Matr[T] = {
      val numColumns = cols.length
      val numRows = cols(0).length
      val columnsArr: Arr[Array[T]] = cols.arr.map(col => col.coords.arr)
      val rmValues = Array.tabulate(numRows * numColumns) { i =>
        columnsArr(i / numColumns)(i % numColumns)
      }
      RowMajorFlatMatrix(PArray(rmValues), numColumns)
    }
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
    def companion = RowMajorSparseMatrix
    def rows: Rep[PArray[SparseVector[T]]]
    def columns = ???
    def numRows = rows.length
    def numColumns = rows(0).length
  }

  trait RowMajorSparseMatrixCompanionOps extends ConcreteClass1[RowMajorSparseMatrix] with MatrixCompanionOps {
    override def defaultOf[T: Elem] = Default.defaultVal(RowMajorSparseMatrix(element[PArray[SparseVector[T]]].defaultRepValue))
    override def fromColumns[T: Elem](cols: PA[Vector[T]]): Matr[T] = ???
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