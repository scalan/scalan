package scalan.linalgebra

import scalan._
import scalan.common.Default

trait Matrices extends Vectors { scalan: MatricesDsl =>
  type Matr[T] = Rep[Matrix[T]]

  trait Matrix[T] extends Reifiable[Matrix[T]] {
    def numColumns: Rep[Int]
    def numRows: Rep[Int]
    implicit def elem: Elem[T]
    def rows: PA[Vector[T]]
    def columns: PA[Vector[T]]
    def *(vector: Vec[T])(implicit n: Numeric[T]): Vec[T] =
      DenseVector(rows.map { r => r.dot(vector) })
    def *(mat: Matr[T])(implicit n: Numeric[T], d: DummyImplicit): Matr[T] = {
      val resColumns = mat.columns.map { col: Rep[Vector[T]] => this * col }
      companion.fromColumns(resColumns)
    }
    def companion: Rep[MatrixCompanion]
  }

  trait MatrixCompanion extends TypeFamily1[Matrix] {
    def defaultOf[T: Elem]: Default[Rep[Matrix[T]]] =
      RowMajorMatrix.defaultOf[T]
    def fromColumns[T: Elem](cols: PA[Vector[T]]): Matr[T] =
      RowMajorMatrix.fromColumns(cols)
  }

  abstract class RowMajorMatrix[T](val rows: Rep[PArray[DenseVector[T]]])(implicit val elem: Elem[T]) extends Matrix[T] {
    def companion = RowMajorMatrix
    def numRows: Rep[Int] = rows.length
    def numColumns = rows(0).length
    def columns =
      PArray(Array.tabulate(numColumns) { j => DenseVector(rows.map(_(j))) })
  }

  trait RowMajorMatrixCompanion extends ConcreteClass1[RowMajorMatrix] with MatrixCompanion {
    override def defaultOf[T: Elem] = Default.defaultVal(RowMajorMatrix(element[PArray[DenseVector[T]]].defaultRepValue))
    override def fromColumns[T: Elem](cols: PA[Vector[T]]): Matr[T] =
      RowMajorMatrix(PArray(Array.tabulate(cols(0).length) { i => DenseVector(cols.map(_(i))) }))
  }

  abstract class RowMajorFlatMatrix[T](val rmValues: Rep[PArray[T]], val numColumns: Rep[Int])(implicit val elem: Elem[T]) extends Matrix[T] {
    def companion = RowMajorFlatMatrix
    def numRows: Rep[Int] = rmValues.length / numColumns
    def columns =
      PArray(Array.tabulate(numColumns) { i =>
        DenseVector(PArray(rmValues.arr.stride(i, numRows, numColumns)))
      })

    def rows: PA[DenseVector[T]] = PArray(rmValues.arr.grouped(numColumns).map { row => DenseVector(PArray(row)) })
  }

  trait RowMajorFlatMatrixCompanion extends ConcreteClass1[RowMajorFlatMatrix] with MatrixCompanion {
    override def defaultOf[T: Elem] = Default.defaultVal(RowMajorFlatMatrix(element[PArray[T]].defaultRepValue, IntElement.defaultRepValue))
    override def fromColumns[T: Elem](cols: PA[Vector[T]]): Matr[T] = {
      val numColumns = cols.length
      val numRows = cols(0).length
      val columnsArr: Arr[Array[T]] = cols.arr.map(col => col.coords.arr)
      val rmValues = Array.tabulate(numRows * numColumns) { i =>
        columnsArr(i % numColumns)(i / numColumns)
      }
      RowMajorFlatMatrix(PArray(rmValues), numColumns)
    }
  }

  abstract class RowMajorSparseMatrix[T](val rows: Rep[PArray[SparseVector[T]]])(implicit val elem: Elem[T]) extends Matrix[T] {
    def companion = RowMajorSparseMatrix
    def columns = ???
    def numRows = rows.length
    def numColumns = rows(0).length
  }

  trait RowMajorSparseMatrixCompanion extends ConcreteClass1[RowMajorSparseMatrix] with MatrixCompanion {
    override def defaultOf[T: Elem] = Default.defaultVal(RowMajorSparseMatrix(element[PArray[SparseVector[T]]].defaultRepValue))
    override def fromColumns[T: Elem](cols: PA[Vector[T]]): Matr[T] = ???
  }

  //  trait ColumnMajorMatrixOps[T] extends MatrixOps[T] {
  //    def cmValues: Rep[PArray[T]]
  //    def numColumns: Rep[Int] = cmValues.length / numRows
  //  }
  //
  //  trait ColumnMajorMatrixCompanion extends ConcreteClass1[ColumnMajorMatrix] {
  //    def defaultOf[T: Elem] = Default.defaultVal(ColumnMajorMatrix(element[PArray[T]].defaultRepValue, IntElement.defaultRepValue))
  //  }

  //  trait ColumnMajorSparseMatrixOps[T] extends MatrixOps[T] {
  //    def sparseColumns: Rep[PArray[SparseVector[T]]]
  //    def numRows = sparseColumns(0).length
  //    def numColumns = sparseColumns.length
  //  }
  //
  //  trait ColumnMajorSparseMatrixCompanion extends ConcreteClass1[ColumnMajorSparseMatrix] {
  //    def defaultOf[T: Elem] = Default.defaultVal(ColumnMajorSparseMatrix(element[PArray[SparseVector[T]]].defaultRepValue))
  //  }

  //  def matrixVectorMult[T: Elem: Numeric](m: Matr[T], v: Vec[T]) = {
  //    DenseVector(m.rows.map { r => r.dot(v) })
  //  }
}

trait MatricesDsl extends ScalanDsl with impl.MatricesAbs with Matrices with VectorsDsl {}

trait MatricesDslSeq extends MatricesDsl with impl.MatricesSeq with VectorsDslSeq with ScalanSeq

trait MatricesDslExp extends MatricesDsl with impl.MatricesExp with VectorsDslExp with ScalanExp