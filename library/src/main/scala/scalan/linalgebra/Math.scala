package scalan.linalgebra

/**
 * Created by Victor Smirnov on 3/12/15.
 */

import scalan.ScalanCommunityDsl

trait Math { self: ScalanCommunityDsl =>

  def transposeDirect[T](m: Matrix[T])(implicit elem: Elem[T], n: Numeric[T]): Matrix[T] = {
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
