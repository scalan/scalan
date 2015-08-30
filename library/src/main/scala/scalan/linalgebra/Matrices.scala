package scalan.linalgebra

/**
  * Created by Victor Smirnov on 3/12/15.
  */


import scalan._
import scalan.common.OverloadHack.{Overloaded2, Overloaded1}
import scala.annotation.unchecked.uncheckedVariance

trait Matrices extends Vectors with Math { self: ScalanCommunityDsl =>

  type Matrix[T] = Rep[AbstractMatrix[T]]

  trait AbstractMatrix[T] extends Reifiable[AbstractMatrix[T]] {
    def numColumns: Rep[Int]
    def numRows: Rep[Int]
    implicit def eT: Elem[T]
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

    //def mapBy[R: Elem](f: Rep[T => R @uncheckedVariance]): Matrix[R]
    def mapBy[R: Elem](f: Rep[AbstractVector[T] => AbstractVector[R] @uncheckedVariance]): Matrix[R]

    def transpose(implicit n: Numeric[T]): Matrix[T]
    def reduceByRows(implicit m: RepMonoid[T]): Vector[T] = {
      DenseVector(rows.map(row => row.nonZeroValues.reduce))
    }
    def reduceByColumns(implicit m: RepMonoid[T], n: Numeric[T]): Vector[T]

    def countNonZeroesByColumns(implicit n: Numeric[T]): Vector[Int] = {
      /*val zero = elem.defaultRepValue
      lazy val NonZeroesMonoid = RepMonoid[T]("NonZeroesMonoid", 0, false) {
        case (x1, x2) => (x1 !== zero).toInt + (x2 !== zero).toInt
      }*/
      val mT = transpose
      DenseVector(mT.rows.map(row => row.nonZeroIndices.length))
    }

    //@OverloadId("vector")
    def *(vector: Vector[T])(implicit n: Numeric[T]): Vector[T] = {
      DenseVector(rows.map { r => r.dot(vector) })
    }
    @OverloadId("matrix")
    def *(matrix: Matrix[T])(implicit n: Numeric[T], o: Overloaded1): Matrix[T]
    def +^^(other: Matrix[T])(implicit n: Numeric[T]): Matrix[T]
    @OverloadId("matrix")
    def *^^(other: Matrix[T])(implicit n: Numeric[T]): Matrix[T]
    def *^^(value: Rep[T])(implicit n: Numeric[T], o: Overloaded1): Matrix[T]
    def average(implicit f: Fractional[T], m: RepMonoid[T]): DoubleRep

    def companion: Rep[AbstractMatrixCompanion]
  }

  abstract class DenseFlatMatrix[T](val rmValues: Rep[Collection[T]], val numColumns: Rep[Int])
                                   (implicit val eT: Elem[T]) extends AbstractMatrix[T] {

    def items = rmValues
    def companion = DenseFlatMatrix
    def numRows: Rep[Int] = rmValues.length /! numColumns
    def columns(implicit n: Numeric[T]): Rep[Collection[AbstractVector[T]]] = {
      Collection.indexRange(numColumns).map { i =>
        DenseVector(Collection(rmValues.arr.stride(i, numRows, numColumns)))}
    }
    def rows: Coll[DenseVector[T]] = Collection(rmValues.arr.grouped(numColumns).map { row => DenseVector(Collection(row)) })

    @OverloadId("rows")
    def apply(iRows: Coll[Int])(implicit o: Overloaded1): Matrix[T] = {
      companion(iRows.map(i => items.slice(numColumns * i, numColumns)).flatMap(v => v), numColumns)
    }
    @OverloadId("row")
    def apply(row: Rep[Int]): Vector[T] = DenseVector(rmValues.slice(row * numColumns, numColumns))
    def apply(row: Rep[Int], column: Rep[Int]): Rep[T] = items(toCellIndex(row, column))

    //def mapBy[R: Elem](f: Rep[T => R @uncheckedVariance]): Matrix[R] = {
    //  DenseFlatMatrix()
    //}

    def mapBy[R: Elem](f: Rep[AbstractVector[T] => AbstractVector[R] @uncheckedVariance]): Matrix[R] = {
      DenseFlatMatrix.fromRows(rows.mapBy(f), numColumns)
    }

    def fromCellIndex(iCell: Rep[Int]): Rep[(Int, Int)] = Pair(iCell /! numColumns, iCell % numColumns)
    def toCellIndex(iRow: Rep[Int], iCol: Rep[Int]): Rep[Int] = numColumns * iRow + iCol

    @OverloadId("block_size")
    def transpose(blockSize: Rep[Int])(implicit n: Numeric[T]): Matrix[T] =
      companion(columns.flatMap(col => col.items), numRows)
    def transpose(implicit n: Numeric[T]): Matrix[T] = transpose(10)

    def reduceByColumns(implicit m: RepMonoid[T], n: Numeric[T]): Vector[T] = {
      val coll = Collection.indexRange(numColumns).map { column =>
        Collection.indexRange(numRows).map { row => this(row)(column) }.reduce
      }
      DenseVector(coll)
    }

    @OverloadId("matrix")
    def *(matrix: Matrix[T])(implicit n: Numeric[T], o: Overloaded1): Matrix[T] = {
      val mT = matrix.companion.fromRows(matrix.columns, matrix.numRows)
      val res = rows.flatMap { row => (mT * row).items}
      companion(res, matrix.numColumns)
    }

    def +^^(other: Matrix[T])(implicit n: Numeric[T]): Matrix[T] = {
      companion((rows zip other.rows).flatMap { case Pair(v1, v2) => (v1 +^ v2).items }, numColumns)
    }

    @OverloadId("matrix")
    def *^^(other: Matrix[T])(implicit n: Numeric[T]): Matrix[T] = {
      other match {
        case DenseFlatMatrixMatcher(rmValues1, _) =>
          companion((rmValues zip rmValues1).map { case Pair(v1, v2) => v1 * v2 }, numColumns)
        case CompoundMatrixMatcher(rows1, _) =>
          CompoundMatrix((rows zip rows1).map { case Pair(v1, v2) => v1 *^ v2 }, numColumns)
        case _ =>
          other *^^ self
      }
    }

    def *^^(value: Rep[T])(implicit n: Numeric[T], o: Overloaded1): Matrix[T] = {
      DenseFlatMatrix(rmValues.map(x => x * value), numColumns)
    }

    def average(implicit f: Fractional[T], m: RepMonoid[T]): DoubleRep = {
      items.reduce.toDouble / items.length.toDouble
    }
  }

  abstract class CompoundMatrix[T](val rows: Rep[Collection[AbstractVector[T]]], val numColumns: Rep[Int])
                                  (implicit val eT: Elem[T]) extends AbstractMatrix[T] {

    def companion = CompoundMatrix
    def columns(implicit n: Numeric[T]): Rep[Collection[AbstractVector[T]]] = {
      Collection(SArray.tabulate(numColumns) { j => DenseVector(rows.map(_(j)))})
    }
    def numRows = rows.length
    def rmValues: Rep[Collection[T]] = rows.flatMap(row => row.items)

    @OverloadId("rows")
    def apply(iRows: Coll[Int])(implicit o: Overloaded1): Matrix[T] = {
      companion(iRows.map(i => rows(i)), numColumns)
    }
    @OverloadId("row")
    def apply(row: Rep[Int]): Vector[T] = rows(row)
    def apply(row: Rep[Int], column: Rep[Int]): Rep[T] = apply(row)(column)

    def mapBy[R: Elem](f: Rep[AbstractVector[T] => AbstractVector[R] @uncheckedVariance]): Matrix[R] = {
      CompoundMatrix(rows.mapBy(f), numColumns)
    }

    def transpose(implicit n: Numeric[T]): Matrix[T] = transposeDirect(self)

    def reduceByColumns(implicit m: RepMonoid[T], n: Numeric[T]): Vector[T] = {
      val coll = Collection.indexRange(numColumns).map { column =>
        Collection.indexRange(numRows).map { row => rows(row)(column) }.reduce
      }
      DenseVector(coll)
//      val mT = transpose
//      mT.reduceByRows
      /*lazy val zeroVector = SparseVector(Collection(SArray.empty[Int]), Collection(SArray.empty[T]), length)
      lazy val VectorMonoid = RepMonoid[SparseVector[T]]("Vector", zeroVector, true) { (t1, t2) => t1 +^ t2 }
      rows.reduce(VectorMonoid)*/
    }

    @OverloadId("matrix")
    def *(matrix: Matrix[T])(implicit n: Numeric[T], o: Overloaded1): Matrix[T] = {
      //val mT = matrix.companion.fromRows(matrix.columns, matrix.numRows)
      //companion(self.rows.map(row => mT * row), matrix.numColumns)
      matrix match {
        case CompoundMatrixMatcher(rowsB, numColumnsB) =>
          val rowsNew = rows.map { vA =>
            val (is, vs) = (vA.nonZeroIndices, vA.nonZeroValues)
            val res = CompoundMatrix((vs zip rowsB(is)).map { case Pair(a, vB) => vB *^ a }, numColumnsB).reduceByColumns
            // TODO: find a proper way to carry over type of vector (Sparse or Dense)
            res//.convertTo(vA.element)
          }
          CompoundMatrix(rowsNew, numColumnsB)
        case DenseFlatMatrixMatcher(rmValuesB, numColumnsB) =>
          val rowsNew = rows.map { vA =>
            val itemsA = vA.items.flatMap(a => Collection.replicate(numColumnsB, a))
            DenseFlatMatrix((itemsA zip rmValuesB).map { case Pair(v1, v2) => v1 * v2 }, numColumnsB).reduceByColumns
          }
          CompoundMatrix(rowsNew, numColumnsB)
        case _ =>
          /*val rowsNew = rows.map { vA =>
            CompoundMatrix((vA.items zip matrix.rows).map { case Pair(a, vB) => vB *^ a }, matrix.numColumns).reduceByColumns
          }
          CompoundMatrix(rowsNew, numColumnsB)*/
          // TODO: remove on implementation of pattern-matching in staged evaluation
          val rowsB = matrix.rows
          val numColumnsB = matrix.numColumns
          val rowsNew = rows.map { vA =>
            val (is, vs) = (vA.nonZeroIndices, vA.nonZeroValues)
            val res = CompoundMatrix((vs zip rowsB(is)).map { case Pair(a, vB) => vB *^ a }, numColumnsB).reduceByColumns
            // TODO: find a proper way to carry over type of vector (Sparse or Dense)
            res//.convertTo(vA.element)
          }
          CompoundMatrix(rowsNew, numColumnsB)
      }
    }

    def +^^(other: Matrix[T])(implicit n: Numeric[T]): Matrix[T] = {
      other match {
        case DenseFlatMatrixMatcher(rmValues1, _) =>
          other +^^ self
        case CompoundMatrixMatcher(rows1, _) =>
          companion((rows zip rows1).map { case Pair(v1, v2) => v1 +^ v2 }, numColumns)
        case _ =>
          other +^^ self
      }
    }

    @OverloadId("matrix")
    def *^^(other: Matrix[T])(implicit n: Numeric[T]): Matrix[T] = {
      companion((rows zip other.rows).map { case Pair(v1, v2) => v1 *^ v2 }, numColumns)
    }

    def *^^(value: Rep[T])(implicit n: Numeric[T], o: Overloaded1): Matrix[T] = {
      CompoundMatrix(rows.map(row => row *^ value), numColumns)
    }

    def average(implicit f: Fractional[T], m: RepMonoid[T]): DoubleRep = {
      val items = rows.flatMap(v => v.nonZeroValues)
      items.reduce.toDouble / (numRows * numColumns).toDouble
    }
  }

  abstract class DiagonalMatrix[T](val diagonalValues: Rep[Collection[T]])
                                   (implicit val eT: Elem[T]) extends AbstractMatrix[T] {

    def numColumns: Rep[Int] = diagonalValues.length
    def numRows: Rep[Int] = numColumns
    def rmValues : Rep[Collection[T]] = SparseVector1(
      Collection.indexRange(numColumns).map { i : Rep[Int] =>  (i * i, diagonalValues(i))},
      numColumns * numColumns
    ).items
    def items = rmValues
    def companion = DiagonalMatrix

    def columns(implicit n: Numeric[T]): Rep[Collection[AbstractVector[T]]] = Collection.indexRange(numColumns).map { i =>
      SparseVector(Collection.replicate(i, 1), diagonalValues.slice(i, 1), numColumns)
    }
    def rows: Coll[AbstractVector[T]] = Collection.indexRange(numColumns).map { i =>
      SparseVector(Collection.replicate(i, 1), diagonalValues.slice(i, 1), numRows)
    }

    @OverloadId("rows")
    def apply(iRows: Coll[Int])(implicit o: Overloaded1): Matrix[T] = {
      companion(iRows.map(i => diagonalValues(i)))
    }
    @OverloadId("row")
    def apply(row: Rep[Int]): Vector[T] = SparseVector(Collection.replicate(row, 1), diagonalValues.slice(row, 1), numColumns)
    def apply(row: Rep[Int], column: Rep[Int]): Rep[T] = if (row == column) diagonalValues(row) else eT.defaultRepValue

    def mapBy[R: Elem](f: Rep[AbstractVector[T] => AbstractVector[R] @uncheckedVariance]): Matrix[R] = {
      DenseFlatMatrix.fromRows(rows.mapBy(f), numColumns)
    }

    def fromCellIndex(iCell: Rep[Int]): Rep[(Int, Int)] = Pair(iCell /! numColumns, iCell % numColumns)
    def toCellIndex(iRow: Rep[Int], iCol: Rep[Int]): Rep[Int] = numColumns * iRow + iCol

    def transpose(implicit n: Numeric[T]): Matrix[T] = self

    def reduceByColumns(implicit m: RepMonoid[T], n: Numeric[T]): Vector[T] = DenseVector(diagonalValues)

    @OverloadId("matrix")
    def *(matrix: Matrix[T])(implicit n: Numeric[T], o: Overloaded1): Matrix[T] = {
      matrix match {
        case DiagonalMatrixMatcher(diagonalValues1) =>
          DiagonalMatrix((DenseVector(diagonalValues) *^ DenseVector(diagonalValues1)).items)
        case _ =>
          DenseFlatMatrix(items, numColumns) * matrix
      }
    }

    def +^^(other: Matrix[T])(implicit n: Numeric[T]): Matrix[T] = {
      other match {
        case DiagonalMatrixMatcher(diagonalValues1) =>
          DiagonalMatrix((DenseVector(diagonalValues) +^ DenseVector(diagonalValues1)).items)
        case _ =>
          other +^^ DenseFlatMatrix(items, numColumns)
      }
    }

    @OverloadId("matrix")
    def *^^(other: Matrix[T])(implicit n: Numeric[T]): Matrix[T] = {
      other match {
        case DiagonalMatrixMatcher(diagonalValues1) =>
          DiagonalMatrix((DenseVector(diagonalValues) *^ DenseVector(diagonalValues1)).items)
        case _ =>
          other *^^ DenseFlatMatrix(items, numColumns)
      }
    }

    def *^^(value: Rep[T])(implicit n: Numeric[T], o: Overloaded1): Matrix[T] = {
      companion(diagonalValues.map(x => x * value))
    }

    def average(implicit f: Fractional[T], m: RepMonoid[T]): DoubleRep = {
      diagonalValues.reduce.toDouble / diagonalValues.length.toDouble
    }
  }

  trait AbstractMatrixCompanion extends TypeFamily1[AbstractMatrix] {

    def fromColumns[T: Elem](cols: Rep[Collection[AbstractVector[T]]]): Matrix[T] = {
      DenseFlatMatrix.fromColumns(cols)
    }
    def fromNColl[T](items: NColl[(Int, T)], numColumns: Rep[Int])
                    (implicit elem: Elem[T], o: Overloaded1): Matrix[T] = CompoundMatrix.fromNColl(items, numColumns)
    @OverloadId("dense")
    def fromNColl[T](items: NColl[T], numColumns: Rep[Int])
                    (implicit elem: Elem[T], o: Overloaded2): Matrix[T] = CompoundMatrix.fromNColl(items, numColumns)
    def fromRows[T: Elem](rows: Coll[AbstractVector[T]], length: IntRep): Matrix[T] = ??? //CompoundMatrix.fromRows(rows, length)
  }

  trait DenseFlatMatrixCompanion extends ConcreteClass1[DenseFlatMatrix] with AbstractMatrixCompanion {
    override def fromColumns[T: Elem](cols: Coll[AbstractVector[T]]): Matrix[T] = {
      val numColumns = cols.length
      val numRows = cols(0).length
      val columnsArr: Coll[Collection[T]] = cols.map(col => col.items)
      val rmValues = Collection.indexRange(numRows * numColumns).map { i =>
        columnsArr(i % numColumns)(i /! numColumns)
      }
      DenseFlatMatrix(rmValues, numColumns)
    }
    override def fromNColl[T](items: NColl[(Int, T)], numColumns: Rep[Int])
                             (implicit elem: Elem[T], o: Overloaded1): Matrix[T] = ???
    @OverloadId("dense")
    override def fromNColl[T](items: NColl[T], numColumns: Rep[Int])
                             (implicit elem: Elem[T], o: Overloaded2): Matrix[T] = {
      DenseFlatMatrix(items.flatMap { coll => coll }, numColumns)
    }
    override def fromRows[T: Elem](rows: Coll[AbstractVector[T]], length: IntRep): Matrix[T] = {
      DenseFlatMatrix(rows.flatMap(v => v.convertTo[DenseVector[T]].items), length)
    }
  }

  trait CompoundMatrixCompanion extends ConcreteClass1[CompoundMatrix] with AbstractMatrixCompanion {
    override def fromColumns[T: Elem](cols: Coll[AbstractVector[T]]): Matrix[T] = ???
    override def fromNColl[T](items: NColl[(Int, T)], numColumns: Rep[Int])
                             (implicit elem: Elem[T], o: Overloaded1): Matrix[T] = {
      CompoundMatrix(items.map { coll => SparseVector(coll.as, coll.bs, numColumns) }, numColumns)
    }
    @OverloadId("dense")
    override def fromNColl[T](items: NColl[T], numColumns: Rep[Int])
                             (implicit elem: Elem[T], o: Overloaded2): Matrix[T] = {
      CompoundMatrix(items.map { coll => DenseVector(coll) }, numColumns)
    }
    override def fromRows[T: Elem](rows: Coll[AbstractVector[T]], length: IntRep): Matrix[T] = {
      CompoundMatrix(rows, length)
    }
  }

  trait DiagonalMatrixCompanion extends ConcreteClass1[DiagonalMatrix] with AbstractMatrixCompanion {
    override def fromColumns[T: Elem](cols: Coll[AbstractVector[T]]): Matrix[T] = ???
    override def fromNColl[T](items: NColl[(Int, T)], numColumns: Rep[Int])
                             (implicit elem: Elem[T], o: Overloaded1): Matrix[T] = ???
    @OverloadId("dense")
    override def fromNColl[T](items: NColl[T], numColumns: Rep[Int])
                             (implicit elem: Elem[T], o: Overloaded2): Matrix[T] = ???
    override def fromRows[T: Elem](rows: Coll[AbstractVector[T]], length: IntRep): Matrix[T] = ???
  }

}

trait MatricesDsl extends impl.MatricesAbs with VectorsDsl { self: ScalanCommunityDsl =>

  type MatrixCompanion = Rep[AbstractMatrixCompanion]

  implicit class MatrixExtensions[T](matrix: Matrix[T]) {
    implicit def eItem: Elem[T] = matrix.selfType1.asInstanceOf[AbstractMatrixElem[T, _]].eT

    def map[R: Elem](f: Vector[T] => Vector[R]): Matrix[R] = matrix.mapBy(fun(f))

    //def filter(f: Rep[T] => Rep[Boolean]): Matrix[T] = matrix.filterBy(fun(f))

    //def flatMap[R: Elem](f: Rep[T] => Coll[R]): Matrix[R] = matrix.flatMapBy(fun(f))
  }
}

trait MatricesDslSeq extends impl.MatricesSeq with VectorsDslSeq { self: ScalanCommunityDslSeq => }

trait MatricesDslExp extends impl.MatricesExp with VectorsDslExp { self: ScalanCommunityDslExp => }
