package scalan.graphs

import scalan.Scalan

/**
 * Created by afilippov on 2/19/15.
 */
trait MST_example extends Scalan{
  val NO_PARENT = -1
  val UNVISITED = -2
  type Ed = (Int, Int)
  type REdge = Rep[Ed]


  lazy val MinNumMonoid = RepMonoid[(Double,(Int,Int))]("MinNum", (Double.MaxValue,(Int.MaxValue, Int.MaxValue)), true) {
    (t1, t2) => IF (t1._1 < t2._1) {t1} ELSE t2
  }

  def MST_prime(links: Rep[Array[Int]], edge_vals: Rep[Array[Double]], offs: Rep[Array[Int]], lens: Rep[Array[Int]],
                startVertex: Rep[Int], outInitial: Rep[Array[Int]]) = {

    def outNeighboursOf(indices: Rep[Array[Int]]): Rep[Array[Array[Int]]] = {
      (offs zip lens)(indices).map { in =>
        links.slice(in._1, in._2)
      }
    }

    def fromId(edge: REdge): Rep[Int] = edge._1
    def outIndex(edge: REdge): Rep[Int] = edge._2
    def value(edge: REdge) = edge_vals(indexOfTarget(edge))
    def indexOfTarget(edge: REdge): Rep[Int] = offs(fromId(edge)) + outIndex(edge)
    def toId(edge: REdge): Rep[Int] = links(indexOfTarget(edge))


    def outEdgesOf(vs: Rep[Array[Int]], visited: Rep[Array[Boolean]]) : Rep[Array[Ed]] = {
      def predicate(edge: REdge): Rep[Boolean] = visited(toId(edge))

      val res = (vs zip outNeighboursOf(vs)).flatMap { in =>
        val Pair(v, ns) = in
        SArray.rangeFrom0(ns.length).map({ i => (v, i)}). filter { !predicate(_) }
      }
      res
    }

    val vertexNum = offs.length
    val visited = SArray.replicate(vertexNum,toRep(false)).update(startVertex, toRep(true))

    val startFront = SArray.replicate(1, startVertex)
    val out = outInitial.update(startVertex, NO_PARENT)
    val st = toRep(false)

    val result = from(startFront, visited, out, st).until((_, _, _, stop) => stop) { (front,visited, out, stop) =>
      val outEdges = outEdgesOf(front, visited)
      val stop = (outEdges.length === 0) //isEmpty
      val res = IF  (stop) THEN {
        (front, visited, out)
      } ELSE {
        val vals = outEdges.map({ edge => value(edge)})
        val froms = outEdges.map(edge =>fromId(edge))
        val tos = outEdges.map({ edge => toId(edge)})
        val minEdge = (vals zip (froms zip tos)).reduce(MinNumMonoid)

        val from = minEdge._2
        val to = minEdge._3

//        val newFront = front.append(to)
        val newFront = ArrayBuffer.empty[Int].++=(front).+=(to).toArray
        val newVisited = visited.update(to, toRep(true))
        val newOut = out.update(to, from)

        (newFront, newVisited, newOut)
      }
      (res._1, res._2, res._3, stop)
    }
    result._3
  }

  lazy val MST = fun { in: Rep[(Array[Int], (Array[Double], (Array[Int], Array[Int])))] =>
    val links = in._1
    val edge_vals = in._2
    val segOffsets = in._3
    val segLens = in._4

    MST_prime(links, edge_vals, segOffsets, segLens, 0, SArray.replicate(segOffsets.length, UNVISITED))
  }
}
