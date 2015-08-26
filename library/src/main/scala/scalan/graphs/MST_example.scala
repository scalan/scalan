package scalan.graphs

import scalan.Scalan
import scalan.collections.ListOps

/**
 * Created by afilippov on 2/19/15.
 */
trait MST_example extends Scalan with ListOps {
  val NO_PARENT = -1
  val UNVISITED = -2
  type Ed = (Int, Int)
  type REdge = Rep[Ed]


  lazy val MinNumMonoid = RepMonoid[(Double,(Int,Int))]("MinNum", (Double.MaxValue,(Int.MaxValue, Int.MaxValue)), true) {
    (t1, t2) => IF (t1._1 < t2._1) {t1} ELSE t2
  }

  def MST_prime_adjlist(links: Rep[Array[Int]], edge_vals: Rep[Array[Double]], offs: Rep[Array[Int]], lens: Rep[Array[Int]],
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

        val newFront = front :+ to
        val newVisited = visited.update(to, toRep(true))
        val newOut = out.update(to, from)

        (newFront, newVisited, newOut)
      }
      (res._1, res._2, res._3, stop)
    }
    result._3
  }

  def MSF_prime_adjlist(links: Rep[Array[Int]], edge_vals: Rep[Array[Double]], offs: Rep[Array[Int]], lens: Rep[Array[Int]]): Arr[Int] = {
    val startVertex = toRep(0);
    val vertexNum = offs.length
    val out = SArray.replicate(vertexNum, UNVISITED)
    val stop = toRep(false)

    val outIndexes = SArray.rangeFrom0(vertexNum)
    val result = from( startVertex, out, stop).until((_, _, stop) => stop ) { (start, out, stop) =>
      val newOut = MST_prime_adjlist(links, edge_vals, offs, lens, start, out)
      val remain = (outIndexes zip newOut).filter( x => x._2 === UNVISITED)
      val stop = (remain.length === 0)
      val newStart = IF (stop) THEN toRep(0) ELSE remain(0)._1
      (newStart, newOut, stop)

    }
    result._2
  }

  lazy val MST_adjlist = fun { in: Rep[(Array[Int], (Array[Double], (Array[Int], Array[Int])))] =>
    val links = in._1
    val edge_vals = in._2
    val segOffsets = in._3
    val segLens = in._4

    MST_prime_adjlist(links, edge_vals, segOffsets, segLens, 0, SArray.replicate(segOffsets.length, UNVISITED))
  }
  lazy val MSF_adjlist = fun { in: Rep[(Array[Int], (Array[Double], (Array[Int], Array[Int])))] =>
    val links = in._1
    val edge_vals = in._2
    val segOffsets = in._3
    val segLens = in._4

    MSF_prime_adjlist(links, edge_vals, segOffsets, segLens)
  }

  def MST_prime_adjmatrix(incMatrix: Rep[Array[Double]], vertexNum: Rep[Int],
                        startVertex: Rep[Int], outInitial: Rep[Array[Int]]) = {

    def rowIndexes = SArray.rangeFrom0(vertexNum)
    def vertexRow(v: Rep[Int]) : Arr[Double] = //incMatrix ->> (indexRange(vertexNum) +^ v*vertexNum)
      incMatrix.slice((v*vertexNum), vertexNum)

    def fromId(edge: REdge): Rep[Int] = edge._1
    def value(edge: REdge) = incMatrix(indexOfTarget(edge))
    def indexOfTarget(edge: REdge): Rep[Int] = fromId(edge)*vertexNum + toId(edge)
    def toId(edge: REdge): Rep[Int] = edge._2



    def outEdgesOf(vs: Rep[Array[Int]], visited: Rep[Array[Boolean]]) : Rep[Array[Ed]] = {
      def predicate(v: Rep[Int]): Rep[Boolean] = !visited(v)

      val res = vs.flatMap { v =>
        val row = vertexRow(v)
        (row zip rowIndexes).filter({i =>  (i._1 > toRep(0)) && predicate(i._2) }).map( i => (v,i._2))
      }
      res
    }

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

        val newFront = front :+ to
        val newVisited = visited.update(to, toRep(true))
        val newOut = out.update(to, from)

        (newFront, newVisited, newOut)
      }
      (res._1, res._2, res._3, stop)
    }
    result._3
  }

  def MSF_prime_adjmatrix(incMatrix: Rep[Array[Double]], vertexNum: Rep[Int]): Arr[Int] = {
    val startVertex = toRep(0);
    val out = SArray.replicate(vertexNum, UNVISITED)
    val stop = toRep(false)

    val outIndexes = SArray.rangeFrom0(vertexNum)
    val result = from( startVertex, out, stop).until((_, _, stop) => stop ) { (start, out, stop) =>
      val newOut = MST_prime_adjmatrix(incMatrix, vertexNum, start, out)
      val remain = (outIndexes zip newOut).filter( x => x._2 === UNVISITED)
      val stop = (remain.length === 0)
      val newStart = IF (stop) THEN toRep(0) ELSE remain(0)._1
      (newStart, newOut, stop)

    }
    result._2
  }
  lazy val MST_adjmatrix = fun { in: Rep[(Array[Double], Int)] =>
    val links = in._1
    val vertexNum = in._2

    MST_prime_adjmatrix(links, vertexNum, 0, SArray.replicate(vertexNum, UNVISITED))
  }
  lazy val MSF_adjmatrix = fun { in: Rep[(Array[Double], Int)] =>
    val links = in._1
    val vertexNum = in._2

    MSF_prime_adjmatrix(links, vertexNum)
  }

  def MST_prime_adjlistMap(links: Rep[Array[Int]], edge_vals: Rep[Array[Double]], offs: Rep[Array[Int]], lens: Rep[Array[Int]],
                        startVertex: Rep[Int], outInitial: Rep[Array[Int]]) = {

    def outNeighboursOf(index: Rep[Int]): Rep[Array[Int]] = {
      links.slice(offs(index), lens(index))
    }

    def fromId(edge: REdge): Rep[Int] = edge._1
    def outIndex(edge: REdge): Rep[Int] = edge._2
    def value(edge: REdge) = edge_vals(indexOfTarget(edge))
    def indexOfTarget(edge: REdge): Rep[Int] = offs(fromId(edge)) + outIndex(edge)
    def toId(edge: REdge): Rep[Int] = links(indexOfTarget(edge))


    def outEdgesOf(front: Rep[MMap[Int, Unit]]) : Rep[Array[Ed]] = {
      def predicate(edge: REdge): Rep[Boolean] = front.contains(toId(edge))

      val res = front.keys.flatMap { v =>
        val ns = outNeighboursOf(v)
        SArray.rangeFrom0(ns.length).map({ i => (v, i)})/*. filter { !predicate(_) }*/
      }
      res.filter{ !predicate(_)}
    }
    def stopCondition(unused1: Any, unused2: Any, stop: Rep[Boolean]) = stop

    val vertexNum = offs.length
    val empty = MMap.empty[Int,Unit]
    val startFront = (empty.update(startVertex, ()) | empty)

    val out = outInitial.update(startVertex, NO_PARENT)
    val st = toRep(false)

    val result = from(startFront, out, st).until(stopCondition) { (front, out, st) =>
      val outEdges = outEdgesOf(front)
      val stop = (outEdges.length === toRep(0))
      val res = IF  (stop) THEN {
        (front, out)
      } ELSE {
        val minEdge = outEdges.map({ edge => Pair(value(edge), Pair(fromId(edge), toId(edge)))}).reduce(MinNumMonoid)
        val from = minEdge._2
        val to = minEdge._3
        val newFront = (front.update(to, ()) | front)
        val newOut = out.update(to, from)
        (newFront, newOut)
      }
      (res._1, res._2, stop)
    }
    result._2
  }

  def MSF_prime_adjlistMap(links: Rep[Array[Int]], edge_vals: Rep[Array[Double]], offs: Rep[Array[Int]], lens: Rep[Array[Int]]): Arr[Int] = {
    val startVertex = toRep(0);
    val vertexNum = offs.length
    val out = SArray.replicate(vertexNum, UNVISITED)

    val outIndexes = SArray.rangeFrom0(vertexNum)
    val result = from( startVertex, out).until((start, _) => (start === toRep(-1)) ) { (start, out) =>
      val newOut = MST_prime_adjlistMap(links, edge_vals, offs, lens, start, out)
      val remain = (outIndexes zip newOut).filter( x => x._2 === UNVISITED)
      val newStart = IF (remain.length === 0) THEN toRep(-1) ELSE remain(0)._1
      (newStart, newOut)
    }
    result._2
  }

  lazy val MST_adjlistMap = fun { in: Rep[(Array[Int], (Array[Double], (Array[Int], Array[Int])))] =>
    val links = in._1
    val edge_vals = in._2
    val segOffsets = in._3
    val segLens = in._4

    MST_prime_adjlistMap(links, edge_vals, segOffsets, segLens, 0, SArray.replicate(segOffsets.length, UNVISITED))
  }
  lazy val MSF_adjlistMap = fun { in: Rep[(Array[Int], (Array[Double], (Array[Int], Array[Int])))] =>
    val links = in._1
    val edge_vals = in._2
    val segOffsets = in._3
    val segLens = in._4

    MSF_prime_adjlistMap(links, edge_vals, segOffsets, segLens)
  }

  def MST_prime_adjmatrixMap(incMatrix: Rep[Array[Double]], vertexNum: Rep[Int],
                             startVertex: Rep[Int], outInitial: Rep[Array[Int]]) = {

    def rowIndexes = SArray.rangeFrom0(vertexNum)
    def vertexRow(v: Rep[Int]) : Arr[Double] = incMatrix.slice((v*vertexNum), vertexNum)

    def fromId(edge: REdge): Rep[Int] = edge._1
    def value(edge: REdge) = incMatrix(indexOfTarget(edge))
    def indexOfTarget(edge: REdge): Rep[Int] = fromId(edge)*vertexNum + toId(edge)
    def toId(edge: REdge): Rep[Int] = edge._2

    def outEdgesOf(front: Rep[MMap[Int, Unit]]) : Rep[Array[Ed]] = {
      def predicate(v: Rep[Int]): Rep[Boolean] = !front.contains(v)

      val res = front.keys.flatMap { v =>
        val row = vertexRow(v)
        (row zip rowIndexes).filter({i =>  (i._1 > toRep(0.0)) && predicate(i._2) }).map( i => (v,i._2))
      }
      res
    }
    def stopCondition(unused1: Any, unused2: Any, stop: Rep[Boolean]) = stop

    val empty = MMap.empty[Int,Unit]
    val startFront = (empty.update(startVertex, ()) | empty)
    val out = outInitial.update(startVertex, NO_PARENT)
    val st = toRep(false)

    val result = from(startFront, out, st).until(stopCondition) { (front, out, st) =>
      val outEdges = outEdgesOf(front)
      val stop = (outEdges.length === toRep(0))
      val res = IF  (stop) THEN {
        (front, out)
      } ELSE {
        val minEdge = outEdges.map({ edge => Pair(value(edge), Pair(fromId(edge), toId(edge)))}).reduce(MinNumMonoid)
        val from = minEdge._2
        val to = minEdge._3
        val newFront = ((front.update(to, ()) | front))
        val newOut = out.update(to, from)
        (newFront, newOut)
      }
      (res._1, res._2, stop)
    }
    result._2
  }

  def MSF_prime_adjmatrixMap(incMatrix: Rep[Array[Double]], vertexNum: Rep[Int]): Arr[Int] = {
    val startVertex = toRep(0);
    val out = SArray.replicate(vertexNum, UNVISITED)

    val outIndexes = SArray.rangeFrom0(vertexNum)
    val result = from( startVertex, out).until((start, _) => (start === toRep(-1)) ) { (start, out) =>
      val newOut = MST_prime_adjmatrixMap(incMatrix, vertexNum, start, out)
      val remain = (outIndexes zip newOut).filter( x => x._2 === UNVISITED)
      val newStart = IF (remain.length === 0) THEN toRep(-1) ELSE remain(0)._1
      (newStart, newOut)
    }
    result._2
  }

  lazy val MST_adjmatrixMap = fun { in: Rep[(Array[Double], Int)] =>
    val links = in._1
    val vertexNum = in._2

    MST_prime_adjmatrixMap(links, vertexNum, 0, SArray.replicate(vertexNum, UNVISITED))
  }
  lazy val MSF_adjmatrixMap = fun { in: Rep[(Array[Double], Int)] =>
    val links = in._1
    val vertexNum = in._2

    MSF_prime_adjmatrixMap(links, vertexNum)
  }

  def MST_prime_adjlistList(links: Rep[Array[Int]], edge_vals: Rep[Array[Double]], offs: Rep[Array[Int]], lens: Rep[Array[Int]],
                        startVertex: Rep[Int], outInitial: Rep[Array[Int]]) = {

    def outNeighboursOf(index: Rep[Int]): Rep[Array[Int]] = {
      links.slice(offs(index),lens(index))
    }

    def fromId(edge: REdge): Rep[Int] = edge._1
    def outIndex(edge: REdge): Rep[Int] = edge._2
    def value(edge: REdge) = edge_vals(indexOfTarget(edge))
    def indexOfTarget(edge: REdge): Rep[Int] = offs(fromId(edge)) + outIndex(edge)
    def toId(edge: REdge): Rep[Int] = links(indexOfTarget(edge))


    def outEdgesOf(vs: Lst[Int], visited: Rep[Array[Boolean]]) : Arr[Ed] = {
      def predicate(edge: REdge): Rep[Boolean] = visited(toId(edge))

      val res = vs.toArray.flatMap { v =>
        val ns = outNeighboursOf(v)
        SArray.rangeFrom0(ns.length).map({ i => (v, i)}). filter { !predicate(_) }
      }
      res
    }

    val vertexNum = offs.length
    val visited = SArray.replicate(vertexNum,toRep(false)).update(startVertex, toRep(true))

    val startFront = startVertex :: SList.empty[Int]
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

        val newFront = to :: front
        val newVisited = visited.update(to, toRep(true))
        val newOut = out.update(to, from)

        (newFront, newVisited, newOut)
      }
      (res._1, res._2, res._3, stop)
    }
    result._3
  }

  def MSF_prime_adjlistList(links: Rep[Array[Int]], edge_vals: Rep[Array[Double]], offs: Rep[Array[Int]], lens: Rep[Array[Int]]): Arr[Int] = {
    val startVertex = toRep(0);
    val vertexNum = offs.length
    val out = SArray.replicate(vertexNum, UNVISITED)
    val stop = toRep(false)

    val outIndexes = SArray.rangeFrom0(vertexNum)
    val result = from( startVertex, out, stop).until((_, _, stop) => stop ) { (start, out, stop) =>
      val newOut = MST_prime_adjlistList(links, edge_vals, offs, lens, start, out)
      val remain = (outIndexes zip newOut).filter( x => x._2 === UNVISITED)
      val stop = (remain.length === 0)
      val newStart = IF (stop) THEN toRep(0) ELSE remain(0)._1
      (newStart, newOut, stop)

    }
    result._2
  }

  lazy val MST_adjlistList = fun { in: Rep[(Array[Int], (Array[Double], (Array[Int], Array[Int])))] =>
    val links = in._1
    val edge_vals = in._2
    val segOffsets = in._3
    val segLens = in._4

    MST_prime_adjlistList(links, edge_vals, segOffsets, segLens, 0, SArray.replicate(segOffsets.length, UNVISITED))
  }
  lazy val MSF_adjlistList = fun { in: Rep[(Array[Int], (Array[Double], (Array[Int], Array[Int])))] =>
    val links = in._1
    val edge_vals = in._2
    val segOffsets = in._3
    val segLens = in._4

    MSF_prime_adjlistList(links, edge_vals, segOffsets, segLens)
  }

  def MST_prime_adjmatrixList(incMatrix: Rep[Array[Double]], vertexNum: Rep[Int],
                          startVertex: Rep[Int], outInitial: Rep[Array[Int]]) = {

    def rowIndexes = SArray.rangeFrom0(vertexNum)
    def vertexRow(v: Rep[Int]) : Arr[Double] = //incMatrix ->> (indexRange(vertexNum) +^ v*vertexNum)
      incMatrix.slice((v*vertexNum), vertexNum)

    def fromId(edge: REdge): Rep[Int] = edge._1
    def value(edge: REdge) = incMatrix(indexOfTarget(edge))
    def indexOfTarget(edge: REdge): Rep[Int] = fromId(edge)*vertexNum + toId(edge)
    def toId(edge: REdge): Rep[Int] = edge._2



    def outEdgesOf(vs: Lst[Int], visited: Rep[Array[Boolean]]) : Arr[Ed] = {
      def predicate(v: Rep[Int]): Rep[Boolean] = !visited(v)

      val res = vs.toArray.flatMap { v =>
        val row = vertexRow(v)
        (row zip rowIndexes).filter({i =>  (i._1 > toRep(0)) && predicate(i._2) }).map( i => (v,i._2))
      }
      res
    }

    val visited = SArray.replicate(vertexNum,toRep(false)).update(startVertex, toRep(true))

    val startFront = startVertex :: SList.empty[Int]
    val out = outInitial.update(startVertex, NO_PARENT)
    val st = toRep(false)

    val result = from(startFront, visited, out, st).until((_, _, _, stop) => stop) { (front,visited, out, stop) =>
      val outEdges = outEdgesOf(front, visited)
      val stop = (outEdges.length === 0)
    val res = IF  (stop) THEN {
        (front, visited, out)
      } ELSE {
        val vals = outEdges.map({ edge => value(edge)})
        val froms = outEdges.map(edge =>fromId(edge))
        val tos = outEdges.map({ edge => toId(edge)})
        val minEdge = (vals zip (froms zip tos)).reduce(MinNumMonoid)

        val from = minEdge._2
        val to = minEdge._3

        val newFront = to :: front
        val newVisited = visited.update(to, toRep(true))
        val newOut = out.update(to, from)

        (newFront, newVisited, newOut)
      }
      (res._1, res._2, res._3, stop)
    }
    result._3
  }

  def MSF_prime_adjmatrixList(incMatrix: Rep[Array[Double]], vertexNum: Rep[Int]): Arr[Int] = {
    val startVertex = toRep(0);
    val out = SArray.replicate(vertexNum, UNVISITED)
    val stop = toRep(false)

    val outIndexes = SArray.rangeFrom0(vertexNum)
    val result = from( startVertex, out, stop).until((_, _, stop) => stop ) { (start, out, stop) =>
      val newOut = MST_prime_adjmatrixList(incMatrix, vertexNum, start, out)
      val remain = (outIndexes zip newOut).filter( x => x._2 === UNVISITED)
      val stop = (remain.length === 0)
      val newStart = IF (stop) THEN toRep(0) ELSE remain(0)._1
      (newStart, newOut, stop)

    }
    result._2
  }
  lazy val MST_adjmatrixList = fun { in: Rep[(Array[Double], Int)] =>
    val links = in._1
    val vertexNum = in._2

    MST_prime_adjmatrixList(links, vertexNum, 0, SArray.replicate(vertexNum, UNVISITED))
  }
  lazy val MSF_adjmatrixList = fun { in: Rep[(Array[Double], Int)] =>
    val links = in._1
    val vertexNum = in._2

    MSF_prime_adjmatrixList(links, vertexNum)
  }



}
