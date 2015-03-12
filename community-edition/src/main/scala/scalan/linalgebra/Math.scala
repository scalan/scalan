package scalan.linalgebra

/**
 * Created by Victor Smirnov on 3/12/15.
 */

import scalan.ScalanCommunityDsl

trait Math { self: ScalanCommunityDsl =>

  def transposeNested[T](m: Matrix[T], blockSize: Rep[Int]): Matrix[T] = {
    ???
  }
  def transposeDirect[T](m: Matrix[T]): Matrix[T] = ??? /*{
    val nestedItems = m.rows//.map { row => row.nonZeroItems }
    val newNestedItems = //CompressedRowMatrix.transpose(nestedItems, numColumns)
        for (i <- Collection.indexRange(m.numColumns))
        yield {
          val newRow: Coll[(Int, T)] =
            for (j <- Collection.indexRange(nestedItems.length) if nestedItems(j).nonZeroItems.as.arr.containsSorted(i))
            yield {
              val indices = nestedItems(j).nonZeroItems.as
              val values = nestedItems(j).nonZeroItems.bs
              val k = indices.binarySearch(i, 0, indices.length)
              val newElem = Pair(j, values(k))
              newElem
            }
          newRow
        }
    val newSparseRows = newNestedItems map { nonZeroItems => SparseVector(nonZeroItems, numRows)}
    RowMajorSparseMatrix(newSparseRows, m.numRows)
  }*/
}
