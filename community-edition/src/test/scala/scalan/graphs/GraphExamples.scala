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

  lazy val MinNumMonoid = RepMonoid[(Double,(Int,Int))]("MinNum", (Double.MaxValue,(Int.MaxValue, Int.MaxValue)), true) {
    (t1, t2) => IF (t1._1 < t2._1) {t1} ELSE t2
  }

  def minEdge(e1: Rep[Edge[Unit,Double]], e2: Rep[Edge[Unit,Double]]) = IF (e1.value < e2.value) {e1} ELSE {e2}
  lazy val MinEdgeMonoid = new RepMonoid[Edge[Unit, Double]]("MinEdge", Edge.MaxDoubleEdge, fun { p: Rep[(Edge[Unit,Double], Edge[Unit,Double])] =>
    IF (p._1.value < p._2.value) {p._1} ELSE {p._2} }, true)

  val UNVISITED = -2
  val NO_PARENT= -1

  def MST_prime(g: Rep[Graph[Unit,Double]], startVertex: Rep[Int], outInitial: Coll[Int]): Coll[Int] = {
    val visited = PBitSet.empty(g.vertexNum) add startVertex
    val startFront = Collection.singleton(startVertex)
    val out = outInitial.update(startVertex, NO_PARENT)
    val st = toRep(false)

    val result = from( startFront, visited, out, st).until((_, _, _, stop) => stop) { (front,visited, out, stop) =>
      val outEdges = g.outEdgesOf(front, visited)
      val ns = outEdges //.flatMap(i => i)
      val stop = (ns.length === 0) //isEmpty
      val res = IF  (stop) THEN {
        (front, visited, out)
      } ELSE {
        //val vals = ns.map({ edge => edge.value})
        //val froms = ns.map(edge => edge.fromId)
        //val tos = ns.map({ edge => edge.toId})
        //val minEdge = (vals zip (froms zip tos)).reduce(MinNumMonoid)
        val minEdge = ns.reduce(MinEdgeMonoid)

        val from: Rep[Int] = minEdge.fromId
        val to: Rep[Int] = minEdge.toId

        val newFront = front.append(to)
        val newVisited = visited add to
        val newOut = out.update(to, from)

        (newFront, newVisited, newOut)
      }
      (res._1, res._2, res._3, stop)
    }
    result._3
  }

  lazy val mstFunAdj = fun { in: Rep[(NestedCollection[Int],NestedCollection[Double])] =>
    val links = in._1
    val edge_vals = in._2
    val vertex_vals = UnitCollection(links.length)
    val graph = AdjacencyGraph.fromAdjacencyList(vertex_vals, edge_vals, links)
    val out_in = Collection.replicate(graph.vertexNum, UNVISITED)
    MST_prime(graph, 0, out_in)
  }

  lazy val mstFunInc = fun { in: Rep[(Collection[Double], Int)] =>
    val incMatrix = in._1
    val vertexNum = in._2
    val vertex_vals = UnitCollection(vertexNum)
    val graph = IncidenceGraph.fromAdjacencyMatrix(vertex_vals, incMatrix, vertexNum)
    val out_in = Collection.replicate(graph.vertexNum, UNVISITED)
    MST_prime(graph, 0, out_in)
  }

  lazy val mstFun1Adj = fun { in: Rep[(Array[Int], (Array[Double], (Array[Int], Array[Int])))] =>
    val segments = Collection.fromArray(in._3) zip Collection.fromArray(in._4)
    val links = NestedCollection.createNestedCollection(Collection.fromArray(in._1), segments)
    val edge_vals = NestedCollection.createNestedCollection(Collection.fromArray(in._2), segments)

    val vertex_vals = UnitCollection(segments.length)
    val graph = AdjacencyGraph.fromAdjacencyList(vertex_vals, edge_vals, links)
    val out_in = Collection.replicate(graph.vertexNum, UNVISITED)
    val res = MST_prime(graph, 0, out_in)
    res.arr
  }


  lazy val mstFun1Inc = fun { in: Rep[(Array[Double], Int)] =>
    val incMatrix = Collection.fromArray(in._1)
    val vertex_vals = UnitCollection(in._2)
    val graph = IncidenceGraph.fromAdjacencyMatrix(vertex_vals, incMatrix, in._2)
    val out_in = Collection.replicate(graph.vertexNum, UNVISITED)
    val res = MST_prime(graph, 0, out_in)
    res.arr
  }
}
