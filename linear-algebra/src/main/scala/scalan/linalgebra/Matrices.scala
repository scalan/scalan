package scalan.linalgebra

import scalan._
import scalan.common.OverloadHack.{Overloaded1, Overloaded2, Overloaded3}
import scala.annotation.unchecked.uncheckedVariance

trait Matrices { self: LADsl =>

  type Matr[T] = Rep[Matrix[T]]

  trait Matrix[T] extends Def[Matrix[T]] {
    def numColumns: Rep[Int]
    def numRows: Rep[Int]
    implicit def eT: Elem[T]
    def rows: Rep[Collection[Vector[T]]]
    def toNColl: Rep[NestedCollection[T]] = CompoundCollection(rows.map(_.items))
    def columns: Rep[Collection[Vector[T]]]
    def rmValues: Rep[Collection[T]]

    @OverloadId("rowsByVector")
    def apply(vector: Vec[Int])(implicit o: Overloaded2): Matr[T] = apply(vector.items)
    @OverloadId("rows")
    def apply(iRows: Coll[Int])(implicit o: Overloaded1): Matr[T]
    @OverloadId("row")
    def apply(row: Rep[Int]): Vec[T]
    def apply(row: Rep[Int], column: Rep[Int]): Rep[T]

    //def mapBy[R: Elem](f: Rep[T => R @uncheckedVariance]): Matrix[R]
    def mapBy[R: Elem](f: Rep[Vector[T] => Vector[R] @uncheckedVariance]): Matr[R]

    def transpose(implicit n: Numeric[T]): Matr[T]
    def reduceByRows(implicit m: RepMonoid[T]): Vec[T] = {
      DenseVector(rows.map(row => row.nonZeroValues.reduce))
    }
    def reduceByColumns(implicit m: RepMonoid[T], n: Numeric[T]): Vec[T]

    def countNonZeroesByColumns(implicit n: Numeric[T]): Vec[Int] = {
      /*val zero = elem.defaultRepValue
      lazy val NonZeroesMonoid = RepMonoid[T]("NonZeroesMonoid", 0, false) {
        case (x1, x2) => (x1 !== zero).toInt + (x2 !== zero).toInt
      }*/
      val mT = transpose
      DenseVector(mT.rows.map(row => row.nonZeroIndices.length))
    }

    //@OverloadId("vector")
    def *(vector: Vec[T])(implicit n: Numeric[T]): Vec[T] = {
      DenseVector(rows.map { r => r.dot(vector) })
    }
    @OverloadId("matrix")
    def *(matrix: Matr[T])(implicit n: Numeric[T], o: Overloaded1): Matr[T]
    def +^^(other: Matr[T])(implicit n: Numeric[T]): Matr[T]
    @OverloadId("matrix")
    def *^^(other: Matr[T])(implicit n: Numeric[T]): Matr[T]
    def *^^(value: Rep[T])(implicit n: Numeric[T], o: Overloaded1): Matr[T]
    def average(implicit f: Fractional[T], m: RepMonoid[T]): DoubleRep

    def companion: Rep[MatrixCompanion]
  }

  abstract class DenseFlatMatrix[T](val rmValues: Rep[Collection[T]], val numColumns: Rep[Int])
                                   (implicit val eT: Elem[T]) extends Matrix[T] {

    def items = rmValues
    def companion = DenseFlatMatrix
    def numRows: Rep[Int] = rmValues.length /! numColumns
    def columns: Rep[Collection[Vector[T]]] = {
      Collection.indexRange(numColumns).map { i =>
        DenseVector(Collection(rmValues.arr.stride(i, numRows, numColumns)))}
    }
    def rows: Coll[DenseVector[T]] = Collection(rmValues.arr.grouped(numColumns).map { row => DenseVector(Collection(row)) })

    @OverloadId("rows")
    def apply(iRows: Coll[Int])(implicit o: Overloaded1): Matr[T] = {
      companion(iRows.map(i => items.slice(numColumns * i, numColumns)).flatMap(v => v), numColumns)
    }
    @OverloadId("row")
    def apply(row: Rep[Int]): Vec[T] = DenseVector(rmValues.slice(row * numColumns, numColumns))
    def apply(row: Rep[Int], column: Rep[Int]): Rep[T] = items(toCellIndex(row, column))

    //def mapBy[R: Elem](f: Rep[T => R @uncheckedVariance]): Matrix[R] = {
    //  DenseFlatMatrix()
    //}

    def mapBy[R: Elem](f: Rep[Vector[T] => Vector[R] @uncheckedVariance]): Matr[R] = {
      DenseFlatMatrix.fromRows(rows.mapBy(f), numColumns)
    }

    def fromCellIndex(iCell: Rep[Int]): Rep[(Int, Int)] = Pair(iCell /! numColumns, iCell % numColumns)
    def toCellIndex(iRow: Rep[Int], iCol: Rep[Int]): Rep[Int] = numColumns * iRow + iCol

    @OverloadId("block_size")
    def transpose(blockSize: Rep[Int])(implicit n: Numeric[T]): Matr[T] =
      companion(columns.flatMap(col => col.items), numRows)
    def transpose(implicit n: Numeric[T]): Matr[T] = transpose(10)

    def reduceByColumns(implicit m: RepMonoid[T], n: Numeric[T]): Vec[T] = {
      val coll = Collection.indexRange(numColumns).map { column =>
        Collection.indexRange(numRows).map { row => this(row)(column) }.reduce
      }
      DenseVector(coll)
    }

    @OverloadId("matrix")
    def *(matrix: Matr[T])(implicit n: Numeric[T], o: Overloaded1): Matr[T] = {
      val mT = matrix.transpose
      val res = rows.flatMap { row => (mT * row).items}
      companion(res, matrix.numColumns)
    }

    def +^^(other: Matr[T])(implicit n: Numeric[T]): Matr[T] = {
      companion((rows zip other.rows).flatMap { case Pair(v1, v2) => (v1 +^ v2).items }, numColumns)
    }

    @OverloadId("matrix")
    def *^^(other: Matr[T])(implicit n: Numeric[T]): Matr[T] = {
      other match {
        case DenseFlatMatrix(rmValues1, _) =>
          companion((rmValues zip rmValues1).map { case Pair(v1, v2) => v1 * v2 }, numColumns)
        case CompoundMatrix(rows1, _) =>
          CompoundMatrix((rows zip rows1).map { case Pair(v1, v2) => v1 *^ v2 }, numColumns)
        case _ =>
          other *^^ self
      }
    }

    def *^^(value: Rep[T])(implicit n: Numeric[T], o: Overloaded1): Matr[T] = {
      DenseFlatMatrix(rmValues.map(x => x * value), numColumns)
    }

    def average(implicit f: Fractional[T], m: RepMonoid[T]): DoubleRep = {
      items.reduce.toDouble / items.length.toDouble
    }
  }

  abstract class DenseMatrix[T](override val toNColl: Rep[NestedCollection[T]])
                               (implicit val eT: Elem[T]) extends Matrix[T] {
    def companion = DenseMatrix
    def rows = toNColl.map(DenseVector(_))
    def numColumns = rows(0).length
    def columns: Rep[Collection[Vector[T]]] = {
      Collection(SArray.tabulate(numColumns) { j => DenseVector(rows.map(_(j)))})
    }
    def numRows = rows.length
    def rmValues: Rep[Collection[T]] = toNColl.values

    @OverloadId("rows")
    def apply(iRows: Coll[Int])(implicit o: Overloaded1): Matr[T] = {
      companion(toNColl(iRows))
    }
    @OverloadId("row")
    def apply(row: Rep[Int]): Vec[T] = DenseVector(toNColl(row))
    def apply(row: Rep[Int], column: Rep[Int]): Rep[T] = apply(row)(column)

    def mapBy[R: Elem](f: Rep[Vector[T] => Vector[R] @uncheckedVariance]): Matr[R] = {
      DenseMatrix(CompoundCollection(toNColl.map(row => f(DenseVector(row)).items)))
    }

    def transpose(implicit n: Numeric[T]): Matr[T] = transposeDirect(self)

    def reduceByColumns(implicit m: RepMonoid[T], n: Numeric[T]): Vec[T] = {
      val coll = Collection.indexRange(numColumns).map { column =>
        Collection.indexRange(numRows).map { row => rows(row)(column) }.reduce
      }
      DenseVector(coll)
    }

    @OverloadId("matrix")
    def *(matrix: Matr[T])(implicit n: Numeric[T], o: Overloaded1): Matr[T] = {
      ???
//      matrix match {
//        case CompoundMatrix(rowsB, numColumnsB) =>
//          val rowsNew = rows.map { vA =>
//            val (is, vs) = (vA.nonZeroIndices, vA.nonZeroValues)
//            val res = CompoundMatrix((vs zip rowsB(is)).map { case Pair(a, vB) => vB *^ a }, numColumnsB).reduceByColumns
//            // TODO: find a proper way to carry over type of vector (Sparse or Dense)
//            res//.convertTo(vA.element)
//          }
//          CompoundMatrix(rowsNew, numColumnsB)
//        case DenseFlatMatrix(rmValuesB, numColumnsB) =>
//          val rowsNew = rows.map { vA =>
//            val itemsA = vA.items.flatMap(a => Collection.replicate(numColumnsB, a))
//            DenseFlatMatrix((itemsA zip rmValuesB).map { case Pair(v1, v2) => v1 * v2 }, numColumnsB).reduceByColumns
//          }
//          CompoundMatrix(rowsNew, numColumnsB)
//        case _ =>
//          // TODO: remove on implementation of pattern-matching in staged evaluation
//          val rowsB = matrix.rows
//          val numColumnsB = matrix.numColumns
//          val rowsNew = rows.map { vA =>
//            val (is, vs) = (vA.nonZeroIndices, vA.nonZeroValues)
//            val res = CompoundMatrix((vs zip rowsB(is)).map { case Pair(a, vB) => vB *^ a }, numColumnsB).reduceByColumns
//            // TODO: find a proper way to carry over type of vector (Sparse or Dense)
//            res
//          }
//          CompoundMatrix(rowsNew, numColumnsB)
//      }
    }

    def +^^(other: Matr[T])(implicit n: Numeric[T]): Matr[T] = {
      ???
//      other match {
//        case DenseFlatMatrix(rmValues1, _) =>
//          other +^^ self
//        case CompoundMatrix(rows1, _) =>
//          companion((rows zip rows1).map { case Pair(v1, v2) => (DenseVector(v1) +^ v2).items })
//        case _ =>
//          other +^^ self
//      }
    }

    @OverloadId("matrix")
    def *^^(other: Matr[T])(implicit n: Numeric[T]): Matr[T] = {
      ???
//      companion((rows zip other.rows).map { case Pair(v1, v2) => v1 *^ v2 })
    }

    def *^^(value: Rep[T])(implicit n: Numeric[T], o: Overloaded1): Matr[T] = {
      DenseMatrix(CompoundCollection(toNColl.map(row => (DenseVector(row) *^ value).items)))
    }

    def average(implicit f: Fractional[T], m: RepMonoid[T]): DoubleRep = {
      val items = toNColl.values
      items.reduce.toDouble / (numRows * numColumns).toDouble
    }
  }

  abstract class CompoundMatrix[T](val rows: Rep[Collection[Vector[T]]], val numColumns: Rep[Int])
                                  (implicit val eT: Elem[T]) extends Matrix[T] {

    def companion = CompoundMatrix
    def columns: Rep[Collection[Vector[T]]] = {
      Collection(SArray.tabulate(numColumns) { j => DenseVector(rows.map(_(j)))})
    }
    def numRows = rows.length
    def rmValues: Rep[Collection[T]] = rows.flatMap(row => row.items)

    @OverloadId("rows")
    def apply(iRows: Coll[Int])(implicit o: Overloaded1): Matr[T] = {
      companion(iRows.map(i => rows(i)), numColumns)
    }
    @OverloadId("row")
    def apply(row: Rep[Int]): Vec[T] = rows(row)
    def apply(row: Rep[Int], column: Rep[Int]): Rep[T] = apply(row)(column)

    def mapBy[R: Elem](f: Rep[Vector[T] => Vector[R] @uncheckedVariance]): Matr[R] = {
      CompoundMatrix(rows.mapBy(f), numColumns)
    }

    def transpose(implicit n: Numeric[T]): Matr[T] = transposeDirect(self)

    def reduceByColumns(implicit m: RepMonoid[T], n: Numeric[T]): Vec[T] = {
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
    def *(matrix: Matr[T])(implicit n: Numeric[T], o: Overloaded1): Matr[T] = {
      //val mT = matrix.companion.fromRows(matrix.columns, matrix.numRows)
      //companion(self.rows.map(row => mT * row), matrix.numColumns)
      matrix match {
        case CompoundMatrix(rowsB, numColumnsB) =>
          val rowsNew = rows.map { vA =>
            val (is, vs) = (vA.nonZeroIndices, vA.nonZeroValues)
            val res = CompoundMatrix((vs zip rowsB(is)).map { case Pair(a, vB) => vB *^ a }, numColumnsB).reduceByColumns
            // TODO: find a proper way to carry over type of vector (Sparse or Dense)
            res//.convertTo(vA.element)
          }
          CompoundMatrix(rowsNew, numColumnsB)
        case DenseFlatMatrix(rmValuesB, numColumnsB) =>
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

    def +^^(other: Matr[T])(implicit n: Numeric[T]): Matr[T] = {
      other match {
        case DenseFlatMatrix(rmValues1, _) =>
          other +^^ self
        case CompoundMatrix(rows1, _) =>
          companion((rows zip rows1).map { case Pair(v1, v2) => v1 +^ v2 }, numColumns)
        case _ =>
          other +^^ self
      }
    }

    @OverloadId("matrix")
    def *^^(other: Matr[T])(implicit n: Numeric[T]): Matr[T] = {
      companion((rows zip other.rows).map { case Pair(v1, v2) => v1 *^ v2 }, numColumns)
    }

    def *^^(value: Rep[T])(implicit n: Numeric[T], o: Overloaded1): Matr[T] = {
      CompoundMatrix(rows.map(row => row *^ value), numColumns)
    }

    def average(implicit f: Fractional[T], m: RepMonoid[T]): DoubleRep = {
      val items = rows.flatMap(v => v.nonZeroValues)
      items.reduce.toDouble / (numRows * numColumns).toDouble
    }
  }

  abstract class ConstMatrix[T](val item: Rep[T], val numColumns: Rep[Int], val numRows: Rep[Int])
                                   (implicit val eT: Elem[T]) extends Matrix[T] {

    def zeroValue = eT.defaultRepValue
    def companion = ConstMatrix
    def rmValues: Rep[Collection[T]] = Collection.replicate(numColumns*numRows, item)
    def items = rmValues
    def columns: Rep[Collection[Vector[T]]] = {
      Collection.indexRange(numColumns).map { i => ConstVector(item, numRows) }
    }
    def rows: Coll[ConstVector[T]] = {
      Collection.indexRange(numRows).map { i => ConstVector(item, numColumns) }
    }

    @OverloadId("rows")
    def apply(iRows: Coll[Int])(implicit o: Overloaded1): Matr[T] = {
      companion(item, numColumns, iRows.length)
    }
    @OverloadId("row")
    def apply(row: Rep[Int]): Vec[T] = ConstVector(item, numColumns)
    def apply(row: Rep[Int], column: Rep[Int]): Rep[T] = item

    def mapBy[R: Elem](f: Rep[Vector[T] => Vector[R] @uncheckedVariance]): Matr[R] = {
      DenseFlatMatrix.fromRows(rows.mapBy(f), numColumns)
    }

    def fromCellIndex(iCell: Rep[Int]): Rep[(Int, Int)] = Pair(iCell /! numColumns, iCell % numColumns)
    def toCellIndex(iRow: Rep[Int], iCol: Rep[Int]): Rep[Int] = numColumns * iRow + iCol

    def transpose(implicit n: Numeric[T]): Matr[T] = companion(item, numRows, numColumns)

    def reduceByColumns(implicit m: RepMonoid[T], n: Numeric[T]): Vec[T] = {
      val coll = Collection.indexRange(numColumns).map { column =>
        Collection.indexRange(numRows).map { row => item }.reduce
      }
      DenseVector(coll)
    }

    override def countNonZeroesByColumns(implicit n: Numeric[T]): Vec[Int] = {
      ConstVector(IF (item !== zeroValue) THEN numRows ELSE 0, numColumns)
    }

    override def *(vector: Vec[T])(implicit n: Numeric[T]): Vec[T] = {
      val dot = vector.reduce * item
      ConstVector(dot, numRows)
    }

    @OverloadId("matrix")
    def *(matrix: Matr[T])(implicit n: Numeric[T], o: Overloaded1): Matr[T] = {
      val mT = matrix.transpose
      val res = rows.flatMap { row => (mT * row).items}
      DenseFlatMatrix(res, matrix.numColumns)
    }

    def +^^(other: Matr[T])(implicit n: Numeric[T]): Matr[T] = {
      other match {
        case ConstMatrix(other_item, _, _) =>
          ConstMatrix(item + other_item, numColumns, numRows)
        case DenseFlatMatrix(rmValues1, _) =>
          DenseFlatMatrix(rmValues1.map(v => v + item), numColumns)
        case _ =>
          DenseFlatMatrix((rows zip other.rows).flatMap { case Pair(v1, v2) => (v1 +^ v2).items }, numColumns)
      }
    }

    @OverloadId("matrix")
    def *^^(other: Matr[T])(implicit n: Numeric[T]): Matr[T] = {
      other match {
        case ConstMatrix(other_item, _, _) =>
          ConstMatrix(item * other_item, numColumns, numRows)
        case DenseFlatMatrix(rmValues1, _) =>
          DenseFlatMatrix(rmValues1.map(v => v * item), numColumns)
        case CompoundMatrix(rows1, _) =>
          CompoundMatrix((rows zip rows1).map { case Pair(v1, v2) => v1 *^ v2 }, numColumns)
        case _ =>
          other *^^ self
      }
    }

    def *^^(value: Rep[T])(implicit n: Numeric[T], o: Overloaded1): Matr[T] = {
      companion(item * value, numColumns, numRows)
    }

    def average(implicit f: Fractional[T], m: RepMonoid[T]): DoubleRep = {
      item.toDouble
    }
  }

  abstract class DiagonalMatrix[T](val diagonalValues: Rep[Collection[T]])
                                  (implicit val eT: Elem[T]) extends Matrix[T] {

    def numColumns: Rep[Int] = diagonalValues.length
    def numRows: Rep[Int] = numColumns
    def rmValues : Rep[Collection[T]] = SparseVector1(
      Collection.indexRange(numColumns).map { i : Rep[Int] =>  (numColumns * i + i, diagonalValues(i))},
      numColumns * numColumns
    ).items
    def items = rmValues
    def companion = DiagonalMatrix

    def columns: Rep[Collection[Vector[T]]] = Collection.indexRange(numColumns).map { i =>
      SparseVector(Collection.replicate(1, i), diagonalValues.slice(i, 1), numColumns)
    }
    def rows: Coll[Vector[T]] = Collection.indexRange(numColumns).map { i =>
      SparseVector(Collection.replicate(1, i), diagonalValues.slice(i, 1), numRows)
    }

    @OverloadId("rows")
    def apply(iRows: Coll[Int])(implicit o: Overloaded1): Matr[T] = {
      companion(iRows.map(i => diagonalValues(i)))
    }
    @OverloadId("row")
    def apply(row: Rep[Int]): Vec[T] = SparseVector(Collection.replicate(1, row), diagonalValues.slice(row, 1), numColumns)
    def apply(row: Rep[Int], column: Rep[Int]): Rep[T] = IF (row === column) THEN diagonalValues(row) ELSE eT.defaultRepValue

    def mapBy[R: Elem](f: Rep[Vector[T] => Vector[R] @uncheckedVariance]): Matr[R] = {
      DenseFlatMatrix.fromRows(rows.mapBy(f), numColumns)
    }

    def fromCellIndex(iCell: Rep[Int]): Rep[(Int, Int)] = Pair(iCell /! numColumns, iCell % numColumns)
    def toCellIndex(iRow: Rep[Int], iCol: Rep[Int]): Rep[Int] = numColumns * iRow + iCol

    def transpose(implicit n: Numeric[T]): Matr[T] = self

    def reduceByColumns(implicit m: RepMonoid[T], n: Numeric[T]): Vec[T] = DenseVector(diagonalValues)

    @OverloadId("matrix")
    def *(matrix: Matr[T])(implicit n: Numeric[T], o: Overloaded1): Matr[T] = {
      matrix match {
        case DiagonalMatrix(diagonalValues1) =>
          DiagonalMatrix((DenseVector(diagonalValues) *^ DenseVector(diagonalValues1)).items)
        case _ =>
          DenseFlatMatrix(items, numColumns) * matrix
      }
    }

    def +^^(other: Matr[T])(implicit n: Numeric[T]): Matr[T] = {
      other match {
        case DiagonalMatrix(diagonalValues1) =>
          DiagonalMatrix((DenseVector(diagonalValues) +^ DenseVector(diagonalValues1)).items)
        case _ =>
          other +^^ DenseFlatMatrix(items, numColumns)
      }
    }

    @OverloadId("matrix")
    def *^^(other: Matr[T])(implicit n: Numeric[T]): Matr[T] = {
      other match {
        case DiagonalMatrix(diagonalValues1) =>
          DiagonalMatrix((DenseVector(diagonalValues) *^ DenseVector(diagonalValues1)).items)
        case _ =>
          other *^^ DenseFlatMatrix(items, numColumns)
      }
    }

    def *^^(value: Rep[T])(implicit n: Numeric[T], o: Overloaded1): Matr[T] = {
      companion(diagonalValues.map(x => x * value))
    }

    def average(implicit f: Fractional[T], m: RepMonoid[T]): DoubleRep = {
      diagonalValues.reduce.toDouble / diagonalValues.length.toDouble
    }
  }

  trait MatrixCompanion extends TypeFamily1[Matrix] {

    def fromColumns[T: Elem](cols: Rep[Collection[Vector[T]]]): Matr[T] = {
      DenseFlatMatrix.fromColumns(cols)
    }
    def fromNColl[T](items: NColl[(Int, T)], numColumns: Rep[Int])
                    (implicit elem: Elem[T], o: Overloaded1): Matr[T] = CompoundMatrix.fromNColl(items, numColumns)
    @OverloadId("dense")
    def fromNColl[T](items: NColl[T], numColumns: Rep[Int])
                    (implicit elem: Elem[T], o: Overloaded2): Matr[T] = CompoundMatrix.fromNColl(items, numColumns)
    def fromRows[T: Elem](rows: Coll[Vector[T]], length: IntRep): Matr[T] = ??? //CompoundMatrix.fromRows(rows, length)
  }

  trait DenseFlatMatrixCompanion extends ConcreteClass1[DenseFlatMatrix] with MatrixCompanion {
    override def fromColumns[T: Elem](cols: Coll[Vector[T]]): Matr[T] = {
      val numColumns = cols.length
      val numRows = cols(0).length
      val columnsArr: Coll[Collection[T]] = cols.map(col => col.items)
      val rmValues = Collection.indexRange(numRows * numColumns).map { i =>
        columnsArr(i % numColumns)(i /! numColumns)
      }
      DenseFlatMatrix(rmValues, numColumns)
    }
    override def fromNColl[T](items: NColl[(Int, T)], numColumns: Rep[Int])
                             (implicit elem: Elem[T], o: Overloaded1): Matr[T] = ???
    @OverloadId("dense")
    override def fromNColl[T](items: NColl[T], numColumns: Rep[Int])
                             (implicit elem: Elem[T], o: Overloaded2): Matr[T] = {
      DenseFlatMatrix(items.flatMap { coll => coll }, numColumns)
    }
    override def fromRows[T: Elem](rows: Coll[Vector[T]], length: IntRep): Matr[T] = {
      DenseFlatMatrix(rows.flatMap(v => v.convertTo[DenseVector[T]].items), length)
    }
  }

  trait DenseMatrixCompanion extends ConcreteClass1[DenseMatrix] with MatrixCompanion {
    override def fromColumns[T: Elem](cols: Coll[Vector[T]]): Matr[T] = {
      ???
    }

    override def fromNColl[T](items: NColl[(Int, T)], numColumns: Rep[Int])
                             (implicit elem: Elem[T], o: Overloaded1): Matr[T] = {
      DenseMatrix(CompoundCollection(items.map { coll => SparseVector(coll.as, coll.bs, numColumns).items }))
    }

    @OverloadId("dense1")
    override def fromNColl[T](items: NColl[T], numColumns: Rep[Int])
                             (implicit elem: Elem[T], o: Overloaded2): Matr[T] = {
      DenseMatrix(items)
    }

    override def fromRows[T: Elem](rows: Coll[Vector[T]], numCols: IntRep): Matr[T] = {
      DenseMatrix(CompoundCollection(rows.map { vec => vec.items }))
    }
  }

  trait CompoundMatrixCompanion extends ConcreteClass1[CompoundMatrix] with MatrixCompanion {
    override def fromColumns[T: Elem](cols: Coll[Vector[T]]): Matr[T] = {
      ???
    }

    override def fromNColl[T](items: NColl[(Int, T)], numColumns: Rep[Int])
                             (implicit elem: Elem[T], o: Overloaded1): Matr[T] = {
      CompoundMatrix(items.map { coll => SparseVector(coll.as, coll.bs, numColumns) }, numColumns)
    }

    @OverloadId("dense1")
    override def fromNColl[T](items: NColl[T], numColumns: Rep[Int])
                             (implicit elem: Elem[T], o: Overloaded2): Matr[T] = {
      CompoundMatrix(items.map { coll => DenseVector(coll) }, numColumns)
    }

    @OverloadId("dense2")
    def fromNColl[T](items: NColl[T])(implicit elem: Elem[T], o: Overloaded3): Matr[T] = {
      val numColumns = items(0).length
      fromNColl(items, numColumns)
    }

    @OverloadId("fromRows")
    def fromRows[T: Elem](rows: Coll[Vector[T]]): Matr[T] = {
      val numCols = rows(0).length
      CompoundMatrix(rows, numCols)
    }

    override def fromRows[T: Elem](rows: Coll[Vector[T]], numCols: IntRep): Matr[T] = {
      CompoundMatrix(rows, numCols)
    }
  }

  trait ConstMatrixCompanion extends ConcreteClass1[ConstMatrix] with MatrixCompanion {
    override def fromColumns[T: Elem](cols: Coll[Vector[T]]): Matr[T] = {
      val numColumns = cols.length
      val numRows = cols(0).length
      val item = cols(0)(0)
      ConstMatrix(item, numColumns, numRows)
    }
    override def fromNColl[T](items: NColl[(Int, T)], numColumns: Rep[Int])
                             (implicit elem: Elem[T], o: Overloaded1): Matr[T] = ???
    @OverloadId("dense")
    override def fromNColl[T](items: NColl[T], numColumns: Rep[Int])
                             (implicit elem: Elem[T], o: Overloaded2): Matr[T] = {
      val rmValues = items.flatMap { coll => coll }
      val numRows = rmValues.length /! numColumns
      val item = rmValues(0)
      ConstMatrix(item, numColumns, numRows)
    }
    override def fromRows[T: Elem](rows: Coll[Vector[T]], length: IntRep): Matr[T] = {
      val numRows = rows.length
      val numColumns = rows(0).length
      val item = rows(0)(0)
      ConstMatrix(item, numColumns, numRows)
    }
  }

  trait DiagonalMatrixCompanion extends ConcreteClass1[DiagonalMatrix] with MatrixCompanion {
    override def fromColumns[T: Elem](cols: Coll[Vector[T]]): Matr[T] = ???
    override def fromNColl[T](items: NColl[(Int, T)], numColumns: Rep[Int])
                             (implicit elem: Elem[T], o: Overloaded1): Matr[T] = ???
    @OverloadId("dense")
    override def fromNColl[T](items: NColl[T], numColumns: Rep[Int])
                             (implicit elem: Elem[T], o: Overloaded2): Matr[T] = ???
    override def fromRows[T: Elem](rows: Coll[Vector[T]], length: IntRep): Matr[T] = ???
  }
}

trait MatricesDsl extends impl.MatricesAbs { self: LADsl =>

//  type MatrCompanion = Rep[MatrixCompanion]

  implicit class MatrixExtensions[T](matrix: Matr[T]) {
    implicit def eItem: Elem[T] = matrix.selfType1.asInstanceOf[MatrixElem[T, _]].eT

    def map[R: Elem](f: Vec[T] => Vec[R]): Matr[R] = matrix.mapBy(fun(f))

    //def filter(f: Rep[T] => Rep[Boolean]): Matrix[T] = matrix.filterBy(fun(f))

    //def flatMap[R: Elem](f: Rep[T] => Coll[R]): Matrix[R] = matrix.flatMapBy(fun(f))
  }

  def transposeDirect[T](m: Matr[T])(implicit elem: Elem[T], n: Numeric[T]): Matr[T] = {
    val nestedItems = m.rows
    val newNestedItems =
      for (i <- Collection.indexRange(m.numColumns))
        yield {
          val items = Collection.indexRange(nestedItems.length).map(j => Pair(j, nestedItems(j)(i))).filter {
            p => p._2 !== n.zero//.nonZeroIndices.filter(l => l === i).length !== toRep(0)
          }
          /*val newRow: PairColl[Int, T] =
            indices zip indices.map { j =>
              val indices = nestedItems(j).nonZeroIndices
              val values = nestedItems(j).nonZeroValues
              val p = (indices zip Collection.indexRange(indices.length)).filter(l => l._1 === i)(toRep(0))
              values(p._2)
            }*/
          SparseVector(items.as, items.bs, m.numRows)
        }
    CompoundMatrix(newNestedItems, m.numRows)
  }
}

trait MatricesDslStd extends impl.MatricesStd { self: LADslStd => }

trait MatricesDslExp extends impl.MatricesExp { self: LADslExp => }
