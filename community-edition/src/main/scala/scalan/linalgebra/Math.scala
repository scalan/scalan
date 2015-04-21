package scalan.linalgebra

/**
 * Created by Victor Smirnov on 3/12/15.
 */

import scalan.ScalanCommunityDsl

trait Math { self: ScalanCommunityDsl =>

  def transposeDirect[T: Elem: Numeric](m: Matrix[T]): Matrix[T] = {
    val nestedItems = m.rows
    val newNestedItems =
        for (i <- Collection.indexRange(m.numColumns))
        yield {
          val indices = Collection.indexRange(nestedItems.length).filter {
            j => nestedItems(j).nonZeroIndices.filter(l => l === i).length !== toRep(0)
          }
          val newRow: PairColl[Int, T] =
            indices.zip(indices.map { j =>
              val indices = nestedItems(j).nonZeroIndices
              val values = nestedItems(j).nonZeroValues
              val p = (indices zip Collection.indexRange(indices.length)).filter(l => l._1 === i)(0)
              values(p._2)
            })
          SparseVector(newRow, m.numRows)
        }
    CompoundMatrix(newNestedItems, m.numRows)
  }
}
