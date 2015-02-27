package scalan.graphs

import scalan.ScalanCommunityDsl
import scalan.primitives.PrimitiveExamples

/**
 * Created by afilippov on 2/17/15.
 */
trait GraphExamples extends ScalanCommunityDsl with GraphsDsl with PrimitiveExamples{
    lazy val fromAndToAdj = fun { in: Rep[(NestedCollection[Int],NestedCollection[Double])] =>
      val links = in._1
      val edge_vals = in._2
      val vertex_vals = UnitCollection(links.length)
      val graph = AdjacencyGraph.fromAdjacencyList(vertex_vals, edge_vals, links)
      graph.vertexNum
    }
  lazy val fromAndToInc = fun { in: Rep[(Collection[Double],Int)] =>
    val incMatrix = in._1
    val vertexNum = in._2
    val vertex_vals = UnitCollection(vertexNum)
    val graph = IncidenceGraph.fromAdjacencyMatrix(vertex_vals, incMatrix, vertexNum)
    graph.vertexNum
  }

  val UNVISITED = -2
  val NO_PARENT= -1
  lazy val MinWeightMonoid = RepMonoid[(Double,(Int,Int))]("MinWeight", (Double.MaxValue,(Int.MaxValue, Int.MaxValue)), true) {
    (t1, t2) => IF (t1._1 < t2._1) {t1} ELSE t2
  }
  def MST_prime(g: Rep[Graph[Unit,Double]], startFront: Rep[Front], out: Coll[Int]): Coll[Int] = {

    def stopCondition(front: Rep[Front], unused: Any) = (g.outEdgesOf(front).length === 0)

    def step(front: Rep[Front], out: Coll[Int]) = {
      val outEdges = g.outEdgesOf(front)
      val minEdge = outEdges.map({ edge => Pair(edge.value, Pair(edge.fromId, edge.toId))}).reduce(MinWeightMonoid)
      val from = minEdge._2
      val to = minEdge._3
      (front.append(to), out.update(to, from))
    }

    from(startFront, out).until(stopCondition)(step)._2
  }

  lazy val mstFunAdj = fun { in: Rep[(NestedCollection[Int],NestedCollection[Double])] =>
    val links = in._1
    val edge_vals = in._2
    val vertex_vals = UnitCollection(links.length)
    val graph = AdjacencyGraph.fromAdjacencyList(vertex_vals, edge_vals, links)
    val out_in = Collection.replicate(graph.vertexNum, UNVISITED).update(0, NO_PARENT)
    val startFront:Front = Front.fromStartNode(0, graph.vertexNum)
    MST_prime(graph, startFront, out_in)
  }

  lazy val mstFunInc = fun { in: Rep[(Collection[Double], Int)] =>
    val incMatrix = in._1
    val vertexNum = in._2
    val vertex_vals = UnitCollection(vertexNum)
    val graph = IncidenceGraph.fromAdjacencyMatrix(vertex_vals, incMatrix, vertexNum)
    val out_in = Collection.replicate(graph.vertexNum, UNVISITED).update(0, NO_PARENT)
    val startFront:Front = Front.fromStartNode(0, graph.vertexNum)
    MST_prime(graph, startFront, out_in)
  }

  lazy val mstFun1Adj = fun { in: Rep[(Array[Int], (Array[Double], (Array[Int], Array[Int])))] =>
    val segments = Collection.fromArray(in._3) zip Collection.fromArray(in._4)
    val links = NestedCollection.createNestedCollection(Collection.fromArray(in._1), segments)
    val edge_vals = NestedCollection.createNestedCollection(Collection.fromArray(in._2), segments)

    val vertex_vals = UnitCollection(segments.length)
    val graph = AdjacencyGraph.fromAdjacencyList(vertex_vals, edge_vals, links)
    val out_in = Collection.replicate(graph.vertexNum, UNVISITED).update(0, NO_PARENT)
    val startFront = Front.fromStartNode(0, graph.vertexNum)
    val res = MST_prime(graph, startFront, out_in)
    res.arr
  }


  lazy val mstFun1Inc = fun { in: Rep[(Array[Double], Int)] =>
    val incMatrix = Collection.fromArray(in._1)
    val vertex_vals = UnitCollection(in._2)
    val graph = IncidenceGraph.fromAdjacencyMatrix(vertex_vals, incMatrix, in._2)
    val out_in = Collection.replicate(graph.vertexNum, UNVISITED).update(0, NO_PARENT)
    val startFront:Front = Front.fromStartNode(0, graph.vertexNum)
    val res = MST_prime(graph, startFront, out_in)
    res.arr
  }
}
