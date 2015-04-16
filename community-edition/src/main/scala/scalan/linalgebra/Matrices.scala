package scalan.linalgebra

/**
  * Created by Victor Smirnov on 3/12/15.
  */

import scalan._
import scalan.common.OverloadHack.{Overloaded2, Overloaded1}
import scalan.common.Default

trait Matrices extends Vectors with Math { self: ScalanCommunityDsl =>

  type Matrix[T] = Rep[AbstractMatrix[T]]

  trait AbstractMatrix[T] extends Reifiable[AbstractMatrix[T]] {
    def numColumns: Rep[Int]
    def numRows: Rep[Int]
    implicit def elem: Elem[T]
    def rows: Rep[Collection[AbstractVector[T]]]
    def columns(implicit n: Numeric[T]): Rep[Collection[AbstractVector[T]]]
    def rmValues: Rep[Collection[T]]

    @OverloadId("rowsByVector")
    def apply(vector: Vector[Int])(implicit o: Overloaded2): Matrix[T] = apply(vector.items)
    @OverloadId("rows")
    def apply(iRows: Coll[Int])(implicit o: Overloaded1): Matrix[T]
    @OverloadId("row")
    def apply(row: Rep[Int]): Vector[T]
    def apply(row: Rep[Int], column: Rep[Int]): Rep[T]

    def transpose(implicit n: Numeric[T]): Matrix[T]
    def reduceByRows(implicit m: RepMonoid[T]): Vector[T] = {
      DenseVector(rows.map(row => row.nonZeroValues.reduce))
    }
    def reduceByColumns(implicit m: RepMonoid[T]): Vector[T]

    //@OverloadId("vector")
    def *(vector: Vector[T])(implicit n: Numeric[T]): Vector[T] =
      DenseVector(rows.map { r => r.dot(vector) })
    @OverloadId("matrix")
    def *(matrix: Rep[AbstractMatrix[T]])(implicit n: Numeric[T], o: Overloaded1): Rep[AbstractMatrix[T]]/* = {
      //val resColumns = matrix.columns.map { col: Rep[AbstractVector[T]] => this * col }
      //companion.fromColumns(resColumns)

      // TODO: we actually get Dense Matrix all the time, so it's in flat form always
      val mT = matrix.transpose
      RowMajorNestedMatrix(self.rows.flatMap(row => (mT * row).items), matrix.numColumns)
    }*/
    def +^^(other: Rep[AbstractMatrix[T]])(implicit n: Numeric[T]): Rep[AbstractMatrix[T]]
    def *^^(other: Rep[AbstractMatrix[T]])(implicit n: Numeric[T]): Rep[AbstractMatrix[T]]
    def average(implicit f: Fractional[T], m: RepMonoid[T]): DoubleRep

    def companion: Rep[AbstractMatrixCompanion]
  }

  abstract class RowMajorNestedMatrix[T](val rmValues: Rep[Collection[T]], val numColumns: Rep[Int])
                                        (implicit val elem: Elem[T]) extends AbstractMatrix[T] {

    def items = rmValues
    def companion = RowMajorNestedMatrix
    def numRows: Rep[Int] = rmValues.length /! numColumns
    def columns(implicit n: Numeric[T]): Rep[Collection[AbstractVector[T]]] = {
      Collection.indexRange(numColumns).map { i =>
        DenseVector(Collection(rmValues.arr.stride(i, numRows, numColumns)))}
    }
    def rows: Coll[DenseVector[T]] = Collection(rmValues.arr.grouped(numColumns).map { row => DenseVector(Collection(row)) })

    @OverloadId("rows")
    def apply(iRows: Coll[Int])(implicit o: Overloaded1): Matrix[T] = {
      RowMajorNestedMatrix(iRows.map(i => items.slice(numColumns * i, numColumns)).flatMap(v => v), numColumns)
    }
    @OverloadId("row")
    def apply(row: Rep[Int]): Vector[T] = DenseVector(rmValues.slice(row * numColumns, numColumns))
    def apply(row: Rep[Int], column: Rep[Int]): Rep[T] = items(toCellIndex(row, column))

    def fromCellIndex(iCell: Rep[Int]): Rep[(Int, Int)] = Pair(iCell /! numColumns, iCell % numColumns)
    def toCellIndex(iRow: Rep[Int], iCol: Rep[Int]): Rep[Int] = numColumns * iRow + iCol

    def blockCellIndices(top: Rep[Int], left: Rep[Int], height: Rep[Int], width: Rep[Int]): Coll[Int] = {
      for {
        i <- Collection.indexRange(height)
        j <- Collection.indexRange(width)
      } yield {
        toCellIndex(top + i, left + j)
      }
    }

    def transposeIndices(is: Coll[Int]): Coll[Int] = {
      for { i <- is } yield {
        val Pair(iRow, iCol) = fromCellIndex(i)
        val transM = RowMajorNestedMatrix(items, numRows)
        transM.toCellIndex(iCol, iRow)
      }
    }

    @OverloadId("block_size")
    def transpose(blockSize: Rep[Int])(implicit n: Numeric[T]): Matrix[T] =
      RowMajorNestedMatrix(columns.flatMap(col => col.items), numRows) //transposeNested(this, blockSize)
    def transpose(implicit n: Numeric[T]): Matrix[T] = transpose(10)

    def reduceByColumns(implicit m: RepMonoid[T]): Vector[T] = {
      val coll = Collection.indexRange(numColumns).map { column =>
        Collection.indexRange(numRows).map { row => this(row)(column) }.reduce
      }
      DenseVector(coll)
    }

    @OverloadId("matrix")
    def *(matrix: Rep[AbstractMatrix[T]])(implicit n: Numeric[T], o: Overloaded1): Rep[AbstractMatrix[T]] = {
      val resColumns = matrix.columns.map { col: Rep[AbstractVector[T]] => this * col }
      companion.fromColumns(resColumns)
      /*matrix match {
        case RowMajorNestedMatrixMatcher(_, numColumns1) =>
          val mT = matrix.transpose
          RowMajorNestedMatrix(self.flatMap(row => (mT * row).items), numColumns1)
        case RowMajorSparseMatrixMatcher(_, numColumns1) =>
          val mT = matrix.transpose
          // TODO: we actually get Dense Matrix all the time, so it's in flat form always
          //RowMajorSparseMatrix(self.map(row => mT * row), numColumns1)
          RowMajorNestedMatrix(self.flatMap(row => (mT * row).items), numColumns1)
        case _ =>
          other *^^ self
      }*/
    }

    def +^^(other: Rep[AbstractMatrix[T]])(implicit n: Numeric[T]): Rep[AbstractMatrix[T]] = {
      RowMajorNestedMatrix((rows zip other.rows).flatMap { case Pair(v1, v2) => (v1 +^ v2).items }, numColumns)
    }

    def *^^(other: Rep[AbstractMatrix[T]])(implicit n: Numeric[T]): Rep[AbstractMatrix[T]] = {
      other match {
        case RowMajorNestedMatrixMatcher(rmValues1, _) =>
          RowMajorNestedMatrix((rmValues zip rmValues1).map { case Pair(v1, v2) => v1 * v2 }, numColumns)
        case RowMajorSparseMatrixMatcher(rows1, _) =>
          RowMajorSparseMatrix((rows zip rows1).map { case Pair(v1, v2) => v1 *^ v2 }, numColumns)
        case _ =>
          other *^^ self
      }
    }

    def average(implicit f: Fractional[T], m: RepMonoid[T]): DoubleRep = {
      items.reduce.toDouble / items.length.toDouble
    }
  }

  abstract class RowMajorSparseMatrix[T](val rows: Rep[Collection[AbstractVector[T]]], val numColumns: Rep[Int])
                                        (implicit val elem: Elem[T]) extends AbstractMatrix[T] {
    def companion = RowMajorSparseMatrix
    def columns(implicit n: Numeric[T]): Rep[Collection[AbstractVector[T]]] = {
      Collection(SArray.tabulate(numColumns) { j => DenseVector(rows.map(_(j)))})
    }
    def numRows = rows.length
    def rmValues: Rep[Collection[T]] = rows.flatMap(row => row.items)

    @OverloadId("rows")
    def apply(iRows: Coll[Int])(implicit o: Overloaded1): Matrix[T] = {
      RowMajorSparseMatrix(iRows.map(i => rows(i)), numColumns)
    }
    @OverloadId("row")
    def apply(row: Rep[Int]): Vector[T] = rows(row)
    def apply(row: Rep[Int], column: Rep[Int]): Rep[T] = apply(row)(column)

    def transpose(implicit n: Numeric[T]): Rep[AbstractMatrix[T]] = transposeDirect(self)

    def reduceByColumns(implicit m: RepMonoid[T]): Vector[T] = {
      val coll = Collection.indexRange(numColumns).map { column =>
        Collection.indexRange(numRows).map { row => rows(row)(column) }.reduce
      }
      DenseVector(coll)
    }

    @OverloadId("matrix")
    def *(matrix: Rep[AbstractMatrix[T]])(implicit n: Numeric[T], o: Overloaded1): Rep[AbstractMatrix[T]] = {
      val mT = matrix.companion.fromRows(matrix.columns, matrix.numRows)
      RowMajorSparseMatrix(self.rows.map(row => mT * row), matrix.numColumns)
    }

    def +^^(other: Rep[AbstractMatrix[T]])(implicit n: Numeric[T]): Rep[AbstractMatrix[T]] = {
      other match {
        case RowMajorNestedMatrixMatcher(rmValues1, _) =>
          //RowMajorNestedMatrix((rmValues zip rmValues1).map { case Pair(v1, v2) => v1 + v2 }, numColumns)
          //(rows zip nm.rows).flatMap { case Pair(v1, v2) => (v1 +^ v2).items }, numColumns)
          other +^^ self
        case RowMajorSparseMatrixMatcher(rows1, _) =>
          RowMajorSparseMatrix((rows zip rows1).map { case Pair(v1, v2) => v1 +^ v2 }, numColumns)
        case _ =>
          other +^^ self
      }
    }

    def *^^(other: Rep[AbstractMatrix[T]])(implicit n: Numeric[T]): Rep[AbstractMatrix[T]] = {
      RowMajorSparseMatrix((rows zip other.rows).map { case Pair(v1, v2) => v1 *^ v2 }, numColumns)
    }

    def average(implicit f: Fractional[T], m: RepMonoid[T]): DoubleRep = {
      val items = rows.map(v => v.nonZeroValues).flatMap(v => v)
      items.reduce.toDouble / items.length.toDouble
    }
  }

  trait AbstractMatrixCompanion extends TypeFamily1[AbstractMatrix] {

    def fromColumns[T: Elem](cols: Rep[Collection[AbstractVector[T]]]): Rep[AbstractMatrix[T]] = {
      RowMajorNestedMatrix.fromColumns(cols)
    }
    def fromNColl[T](items: NColl[(Int, T)], numColumns: Rep[Int])
                    (implicit elem: Elem[T], o: Overloaded2): Matrix[T] = {
      RowMajorSparseMatrix(items.map { coll =>
        // FIXME: convertTo does not work
        //val collPair = coll.convertTo[PairCollection[Int, T]]
        //SparseVector(collPair.as, collPair.bs, numColumns)
        SparseVector(coll.as, coll.bs, numColumns)
      }, numColumns)
    }
    def fromRows[T: Elem](rows: Coll[AbstractVector[T]], length: IntRep): Matrix[T] = RowMajorSparseMatrix.fromRows(rows, length)
  }

  trait RowMajorNestedMatrixCompanion extends ConcreteClass1[RowMajorNestedMatrix] with AbstractMatrixCompanion {
    override def fromColumns[T: Elem](cols: Coll[AbstractVector[T]]): Matrix[T] = {
      val numColumns = cols.length
      val numRows = cols(0).length
      val columnsArr: Coll[Collection[T]] = cols.map(col => col.items)
      val rmValues = Collection.indexRange(numRows * numColumns).map { i =>
        columnsArr(i % numColumns)(i /! numColumns)
      }
      RowMajorNestedMatrix(rmValues, numColumns)
    }
    override def fromRows[T: Elem](rows: Coll[AbstractVector[T]], length: IntRep): Matrix[T] = {
      RowMajorNestedMatrix(rows.flatMap(v => v.convertTo[DenseVector[T]].items), length)
    }
  }

  trait RowMajorSparseMatrixCompanion extends ConcreteClass1[RowMajorSparseMatrix] with AbstractMatrixCompanion {
    override def fromColumns[T: Elem](cols: Coll[AbstractVector[T]]): Matrix[T] = ???
    override def fromRows[T: Elem](rows: Coll[AbstractVector[T]], length: IntRep): Matrix[T] = {
      RowMajorSparseMatrix(rows, length)
    }
  }
}

trait MatricesDsl extends impl.MatricesAbs with VectorsDsl { self: ScalanCommunityDsl => }

trait MatricesDslSeq extends impl.MatricesSeq with VectorsDslSeq { self: ScalanCommunityDslSeq => }

trait MatricesDslExp extends impl.MatricesExp with VectorsDslExp { self: ScalanCommunityDslExp => }
