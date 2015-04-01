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
    def columns: Rep[Collection[AbstractVector[T]]]
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
    def *(matrix: Rep[AbstractMatrix[T]])(implicit n: Numeric[T], o: Overloaded1): Rep[AbstractMatrix[T]] = {
      val resColumns = matrix.columns.map { col: Rep[AbstractVector[T]] => this * col }
      companion.fromColumns(resColumns)
    }
    def +^^(other: Rep[AbstractMatrix[T]])(implicit n: Numeric[T]): Rep[AbstractMatrix[T]]
    def *^^(other: Rep[AbstractMatrix[T]])(implicit n: Numeric[T]): Rep[AbstractMatrix[T]]
    def average(implicit f: Fractional[T], m: RepMonoid[T]): DoubleRep

    def companion: Rep[AbstractMatrixCompanion]
  }

  // TODO: we have similar classes RowMajorDirectMatrix and RowMajorSparseMatrix, 
  // should consider leaving only RowMajorDirectMatrix 
  // and making it invariant against AbstractVector implementations
  
  abstract class RowMajorDirectMatrix[T](val rows: Rep[Collection[AbstractVector[T]]])
                                        (implicit val elem: Elem[T])
    extends AbstractMatrix[T] {
    def companion = RowMajorDirectMatrix
    def numRows: Rep[Int] = rows.length
    def numColumns = rows(0).length
    def columns = Collection(SArray.tabulate(numColumns) { j => DenseVector(rows.map(_(j))) })
    def rmValues: Rep[Collection[T]] = ???

    @OverloadId("rows")
    def apply(iRows: Coll[Int])(implicit o: Overloaded1): Matrix[T] = {
      RowMajorDirectMatrix(iRows.map(i => rows(i)))
    }
    @OverloadId("row")
    def apply(row: Rep[Int]): Vector[T] = rows(row)
    def apply(row: Rep[Int], column: Rep[Int]): Rep[T] = apply(row)(column)

    def transpose(implicit n: Numeric[T]): Matrix[T] = ???

    def reduceByColumns(implicit m: RepMonoid[T]): Vector[T] = {
      val coll = Collection.indexRange(numColumns).map { column =>
        Collection.indexRange(numRows).map { row => rows(row)(column) }.reduce
      }
      DenseVector(coll)
    }

    def +^^(other: Rep[AbstractMatrix[T]])(implicit n: Numeric[T]): Rep[AbstractMatrix[T]] = ???

    def *^^(other: Rep[AbstractMatrix[T]])(implicit n: Numeric[T]): Rep[AbstractMatrix[T]] = ???

    def average(implicit f: Fractional[T], m: RepMonoid[T]): DoubleRep = {
      val flat = rows.map(v => v.items).flatMap(v => v)
      flat.reduce.toDouble / flat.length.toDouble
    }
  }

  abstract class RowMajorNestedMatrix[T](val rmValues: Rep[Collection[T]], val numColumns: Rep[Int])
                                        (implicit val elem: Elem[T])
    extends AbstractMatrix[T] {

    def items = rmValues
    def companion = RowMajorNestedMatrix
    def numRows: Rep[Int] = rmValues.length /! numColumns
    def columns: Rep[Collection[AbstractVector[T]]] = {
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

    def getTranspositionOfBlocks(blocks: Coll[((Int, Int), (Int, Int))]): Rep[IPairCollection[Int, Int]] = {
      val res = for { Pair(Pair(top, left), Pair(height, width)) <- blocks } yield {
        val bcis = blockCellIndices(top, left, height, width)
        val trans = transposeIndices(bcis)
        bcis zip trans
      }
      res.flatMap(c => c)
    }

    @OverloadId("block_size")
    def transpose(blockSize: Rep[Int])(implicit n: Numeric[T]): Matrix[T] = transposeNested(this, blockSize)/*{
      val n = (numRows - 1) /! blockSize + 1
      val m = (numColumns - 1) /! blockSize + 1
      val bHeight = numRows % blockSize
      val rWidth = numColumns % blockSize
      val lastHeight = IF(bHeight === 0) THEN blockSize ELSE bHeight
      val lastWidth = IF(rWidth === 0) THEN blockSize ELSE rWidth

      val blocks = for {
        i <- Collection.indexRange(n)
        j <- Collection.indexRange(m)
      } yield {
        val height: Rep[Int] = IF(i === n - 1) THEN lastHeight ELSE blockSize
        val width = IF(j === m - 1) THEN lastWidth ELSE blockSize
        Pair(Pair(i * blockSize, j * blockSize), Pair(height, width))
      }

      val is = getTranspositionOfBlocks(blocks)
      val transposedItems = items.updateMany(is.bs, items gather is.as)

      RowMajorFlatMatrix(transposedItems, numRows)
    }*/
    def transpose(implicit n: Numeric[T]): Matrix[T] = transpose(10)
    /*def reduceByRows(implicit m: RepMonoid[T]): Vector[T] = {
      DenseVector(rows.map(row => row.reduce))
    }*/
    def reduceByColumns(implicit m: RepMonoid[T]): Vector[T] = {
      val coll = Collection.indexRange(numColumns).map { column =>
        Collection.indexRange(numRows).map { row => this(row)(column) }.reduce
      }
      DenseVector(coll)
    }

    def +^^(other: Rep[AbstractMatrix[T]])(implicit n: Numeric[T]): Rep[AbstractMatrix[T]] = {
      RowMajorNestedMatrix((rows zip other.rows).flatMap { case Pair(v1, v2) => (v1 +^ v2).items }, numColumns)
      /*matchMatrix[T, AbstractMatrix[T]](other) {
        dm => RowMajorNestedMatrix((rows zip other.rows).flatMap { case Pair(v1, v2) => (v1 +^ v2).items }, numColumns)
      } {
        sm => RowMajorSparseMatrix((rows zip other.rows).map { case Pair(v1, v2) => v1 +^ v2 }, numColumns)
      }*/
    }

    def *^^(other: Rep[AbstractMatrix[T]])(implicit n: Numeric[T]): Rep[AbstractMatrix[T]] = {
      matchMatrix[T, AbstractMatrix[T]](other) {
        nm => RowMajorNestedMatrix((rows zip nm.rows).flatMap { case Pair(v1, v2) => (v1 *^ v2).items }, numColumns)
      } {
        sm => RowMajorSparseMatrix((rows zip sm.rows).map { case Pair(v1, v2) => v1 *^ v2 }, numColumns)
      }
    }

    def average(implicit f: Fractional[T], m: RepMonoid[T]): DoubleRep = {
      items.reduce.toDouble / items.length.toDouble
    }
  }

  abstract class RowMajorSparseMatrix[T](val rows: Rep[Collection[AbstractVector[T]]], val numColumns: Rep[Int])
                                        (implicit val elem: Elem[T])
    extends AbstractMatrix[T] {
    def companion = RowMajorSparseMatrix
    def columns: Rep[Collection[AbstractVector[T]]] = ???
    def numRows = rows.length
    //def numColumns = rows(0).length
    def rmValues: Rep[Collection[T]] = ???

    @OverloadId("rows")
    def apply(iRows: Coll[Int])(implicit o: Overloaded1): Matrix[T] = {
      RowMajorSparseMatrix(iRows.map(i => rows(i)), numColumns)
    }
    @OverloadId("row")
    def apply(row: Rep[Int]): Vector[T] = rows(row)
    def apply(row: Rep[Int], column: Rep[Int]): Rep[T] = apply(row)(column)

    def transpose(implicit n: Numeric[T]): Rep[AbstractMatrix[T]] = transposeDirect(this) /*{
      val nestedItems = sparseRows map {row => row.nonZeroItems}
      val newNestedItems = CompressedRowMatrix.transpose(nestedItems, numColumns)
      val newSparseRows = newNestedItems map { nonZeroItems => SparseVector(nonZeroItems, numRows)}
      CompressedRowMatrix(newSparseRows, numRows)
    }*/
    /*def reduceByRows(implicit m: RepMonoid[T]): Vector[T] = {
      DenseVector(rows.map(row => row.reduce))
    }*/
    def reduceByColumns(implicit m: RepMonoid[T]): Vector[T] = {
      val coll = Collection.indexRange(numColumns).map { column =>
        Collection.indexRange(numRows).map { row => rows(row)(column) }.reduce
      }
      DenseVector(coll)
    }

    def +^^(other: Rep[AbstractMatrix[T]])(implicit n: Numeric[T]): Rep[AbstractMatrix[T]] = {
      matchMatrix[T, AbstractMatrix[T]](other) {
        nm => RowMajorNestedMatrix((rows zip nm.rows).flatMap { case Pair(v1, v2) => (v1 +^ v2).items }, numColumns)
      } {
        sm => RowMajorSparseMatrix((rows zip sm.rows).map { case Pair(v1, v2) => v1 +^ v2 }, numColumns)
      }
    }

    def *^^(other: Rep[AbstractMatrix[T]])(implicit n: Numeric[T]): Rep[AbstractMatrix[T]] = {
      RowMajorSparseMatrix((rows zip other.rows).map { case Pair(v1, v2) => v1 *^ v2 }, numColumns)
      /*matchMatrix[T, AbstractMatrix[T]](other) {
        dm => RowMajorNestedMatrix((rows zip other.rows).flatMap { case Pair(v1, v2) => (v1 +^ v2).items }, numColumns)
      } {
        sm => RowMajorSparseMatrix((rows zip other.rows).map { case Pair(v1, v2) => v1 +^ v2 }, numColumns)
      }*/
    }

    def average(implicit f: Fractional[T], m: RepMonoid[T]): DoubleRep = {
      val items = rows.map(v => v.nonZeroValues).flatMap(v => v)
      items.reduce.toDouble / items.length.toDouble
    }
  }

  trait AbstractMatrixCompanion extends TypeFamily1[AbstractMatrix] {

    def defaultOf[T: Elem]: Default[Rep[AbstractMatrix[T]]] = RowMajorNestedMatrix.defaultOf[T]
    def fromColumns[T: Elem](cols: Rep[Collection[AbstractVector[T]]]): Rep[AbstractMatrix[T]] = {
      RowMajorNestedMatrix.fromColumns(cols)
    }
    def fromNColl[T](items: NColl[(Int, T)], numColumns: Rep[Int])
                    (implicit elem: Elem[T], o: Overloaded2): Matrix[T] = {
      RowMajorSparseMatrix(items.map { coll =>
        val collPair = coll.convertTo[PairCollection[Int, T]]
        SparseVector(collPair.as, collPair.bs, numColumns)
      }, numColumns)
    }
  }

  trait RowMajorDirectMatrixCompanion extends ConcreteClass1[RowMajorDirectMatrix] with AbstractMatrixCompanion {
    override def defaultOf[T: Elem] = Default.defaultVal(RowMajorDirectMatrix(element[Collection[DenseVector[T]]].defaultRepValue))
    override def fromColumns[T: Elem](cols: Coll[AbstractVector[T]]): Rep[AbstractMatrix[T]] =
      RowMajorDirectMatrix(Collection(SArray.tabulate(cols(0).length) { i => DenseVector(cols.map(_(i))) }))
  }

  trait RowMajorNestedMatrixCompanion extends ConcreteClass1[RowMajorNestedMatrix] with AbstractMatrixCompanion {

    override def defaultOf[T: Elem] = Default.defaultVal(RowMajorNestedMatrix(element[Collection[T]].defaultRepValue, IntElement.defaultRepValue))
    override def fromColumns[T: Elem](cols: Coll[AbstractVector[T]]): Matrix[T] = {
      val numColumns = cols.length
      val numRows = cols(0).length
      val columnsArr: Coll[Collection[T]] = cols.map(col => col.items)
      val rmValues = Collection.indexRange(numRows * numColumns).map { i =>
        columnsArr(i % numColumns)(i /! numColumns)
      }
      RowMajorNestedMatrix(rmValues, numColumns)
    }
    def fromRows[T: Elem](rows: Coll[AbstractVector[T]]): Matrix[T] = {
      RowMajorNestedMatrix(rows.flatMap(v => v.convertTo[DenseVector[T]].items), rows(0).length)
    }
  }

  trait RowMajorSparseMatrixCompanion extends ConcreteClass1[RowMajorSparseMatrix] with AbstractMatrixCompanion {

    override def defaultOf[T: Elem] = Default.defaultVal(RowMajorSparseMatrix(element[Collection[AbstractVector[T]]].defaultRepValue, IntElement.defaultRepValue))
    override def fromColumns[T: Elem](cols: Coll[AbstractVector[T]]): Matrix[T] = ???
  }
}

trait MatricesDsl extends impl.MatricesAbs with VectorsDsl { self: ScalanCommunityDsl =>

  def Matrix: Rep[AbstractMatrixCompanionAbs] = AbstractMatrix

  def matchMatrix[T, R](matrix: Matrix[T])(dense: Rep[RowMajorNestedMatrix[T]] => Rep[R])
                                          (sparse: Rep[RowMajorSparseMatrix[T]] => Rep[R]): Rep[R]
}

trait MatricesDslSeq extends impl.MatricesSeq with VectorsDslSeq { self: ScalanCommunityDslSeq =>

  def matchMatrix[T, R](matrix: Matrix[T])(dense: Rep[RowMajorNestedMatrix[T]] => Rep[R])
                                       (sparse: Rep[RowMajorSparseMatrix[T]] => Rep[R]): Rep[R] = {
    matrix match {
      case dm: RowMajorNestedMatrix[_] => dense(dm)
      case sm: RowMajorSparseMatrix[_] => sparse(sm)
    }
  }
}

trait MatricesDslExp extends impl.MatricesExp with VectorsDslExp { self: ScalanCommunityDslExp =>

  def matchMatrix[T, R](matrix: Matrix[T])(dense: Rep[RowMajorNestedMatrix[T]] => Rep[R])
                                          (sparse: Rep[RowMajorSparseMatrix[T]] => Rep[R]): Rep[R] = {
    matrix.elem.asInstanceOf[Elem[_]] match {
      case _: RowMajorNestedMatrixElem[_] => dense(matrix.asRep[RowMajorNestedMatrix[T]])
      case _: RowMajorSparseMatrixElem[_] => sparse(matrix.asRep[RowMajorSparseMatrix[T]])
    }
  }
}
