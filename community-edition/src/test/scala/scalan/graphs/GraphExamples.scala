package scalan.graphs

import scalan.community.ScalanCommunityDsl
import scalan.primitives.PrimitiveExamples

/**
 * Created by afilippov on 2/17/15.
 */
trait GraphExamples extends ScalanCommunityDsl with GraphsDsl with PrimitiveExamples{
    //lazy val fromArray = fun { xs: Arr[Int] => Collection(xs) }
    //lazy val fromArrayOfPairs = fun { xs: Arr[(Int,Float)] => Collection(xs) }
    lazy val fromAndTo = fun { in: Rep[(NestedCollection[Int],NestedCollection[Double])] =>
      val links = in._1
      val edge_vals = in._2
      val vertex_vals = UnitCollection(links.length)
      val graph = AdjacencyGraph.fromAdjacencyList(vertex_vals, edge_vals, links)
      graph.vertexNum
    }
}
