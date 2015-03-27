package scalan.linalgebra

/**
 * Created by Victor Smirnov on 3/12/15.
 */

import scalan.ScalanCommunityDsl

trait Math { self: ScalanCommunityDsl =>

  def transposeNested[T: Elem: Numeric](m: Matrix[T], blockSize: Rep[Int]): Matrix[T] = {
    m // TODO: faked because we need to implement everything
      // for default element Of AbstractMatrix that is RowMajorNestedMatrix
  }
//  def transposeDirect[T](m: Matrix[T]): Matrix[T] = {
//    val nestedItems = m.rows//.map { row => row.nonZeroItems }
//    val newNestedItems = //CompressedRowMatrix.transpose(nestedItems, numColumns)
//        for (i <- Collection.indexRange(m.numColumns))
//        yield {
//          val newRow: Coll[(Int, T)] =
//            for (j <- Collection.indexRange(nestedItems.length) if nestedItems(j).nonZeroIndices.filter(l => l === i).length !== toRep(0)
//                                                         /*if nestedItems(j).nonZeroIndices.arr.containsSorted(i)*/)
//            yield {
//              val indices = nestedItems(j).nonZeroIndices
//              val values = nestedItems(j).nonZeroValues
//              val k = indices.filter(l => l === i)(0)
//              val newElem = Pair(j, values(k))
//              newElem
//            }
//          newRow //.convertTo[PairCollection[Int, T]]
//        }
//    val newSparseRows = newNestedItems map { nonZeroItems => SparseVector(nonZeroItems, numRows)}
//    RowMajorSparseMatrix(newSparseRows, m.numRows)
//  }
  def transposeDirect[T: Elem: Numeric](m: Matrix[T]): Matrix[T] = {
    val nestedItems = m.rows
    val newNestedItems =
        for (i <- Collection.indexRange(m.numColumns))
        yield {
          val newRow: Coll[(Int, T)] =
            for (j <- Collection.indexRange(nestedItems.length)
                 if nestedItems(j).nonZeroIndices.filter(l => l === i).length !== toRep(0))
            yield {
              val indices = nestedItems(j).nonZeroIndices
              val values = nestedItems(j).nonZeroValues
              val Pair(r, k) = (indices zip Collection.indexRange(indices.length)).filter(l => l._1 === i)(0)
              val newElem = Pair(j, values(k))
              newElem
            }
          SparseVector(newRow, m.numRows)
        }
    RowMajorSparseMatrix(newNestedItems, m.numRows)
  }
}
