package scalan.graphs

import scalan.ScalanDsl
import scalan.collection.CollectionsDsl
import scalan.common.Default
import scalan.common.OverloadHack.Overloaded1
import scalan.community.ScalanCommunityDsl

/**
 * Created by afilippov on 2/17/15.
 */
trait Graphs extends ScalanCommunityDsl with CollectionsDsl { self: GraphsDsl =>
  type PG[V,E] = Rep[Graph[V,E]]
  type SimpleGraph = Graph[Unit, Unit]
  type AdjacencyList = Collection[Int] // adjacency list
  type AdjacencyMatrix = Collection[AdjacencyList]
  type IncidentMatrix = Collection[Boolean]
  type EdgeList = (Collection[Int], Collection[Int])

  trait Graph[V,E] {
    type Node = Rep[Vertex[V, E]]
    type EdgeType <: Edge[V, E]
    type REdge = Rep[EdgeType]

    implicit def eEdge: Elem[EdgeType]

    implicit def eV: Elem[V]

    implicit def eE: Elem[E]

    def makeEdgeFrom(v: Rep[Int], iE: Rep[Int]): REdge

    def makeEdgeFromTo(v1: Rep[Int], v2: Rep[Int]): REdge

    implicit protected def thisGraph: Rep[Graph[V, E]]

    def vertexValues: Coll[V]

    def edgeValues: NColl[E]

    def links: NColl[Int]

    def incMatrix: Coll[Boolean]

    def discardValues: Rep[SimpleGraph]

    def unzipValues: (Rep[SimpleGraph], Coll[V], NColl[E]) = (discardValues, vertexValues, edgeValues)

    //def zipValues[V1: Elem](vs: Coll[V1]): PG[V1, E]

    //def zipValues[V1: Elem, E1: Elem](vs: Coll[V1], ess: NColl[E1]): PG[V1, E1]

    def addEdges(el: Rep[EdgeList]): PG[V, E] = ???

    def inverted: PG[V, E] = ???

    def complement: PG[V, E] = ???

    def vertexNum: Rep[Int]

    def edgeNum: Rep[Int]

    def inDegrees: Coll[Int]

    def outDegrees: Coll[Int]

    def getNode(vId: Rep[Int]): Rep[Vertex[V, E]] = SVertex(vId, thisGraph)

    def getNodes(vIds: Coll[Int]): Coll[Vertex[V, E]] = ??? //mkView(vIds)

    def nodes: Coll[Vertex[V, E]]

    def edges: Coll[EdgeType]

    def inNeighbors(v: Rep[Int]): Coll[Int]

    def outNeighborsOf(v: Rep[Int]): Coll[Int]

    //def outNeighborsOf(vs: Coll[Int])(implicit o: Overloaded1): NColl[Int]

    def outNeighboursOf(vs: Coll[Int], excluding: Coll[Boolean]): NColl[Int] = ??? /*{
      def isExcluded(node: Rep[Int]) = excluding.contains(node)
      val res = outNeighborsOf(vs).map {
        _.filter(n => !isExcluded(n))
      }
      res
    }*/

    def outEdgesOf(vs: Coll[Int], excluding: Rep[Coll[Boolean]]): NColl[Edge[V, E]] = ??? /*{
      def isExcluded(node: Rep[Int]) = excluding.contains(node)
      val res = outEdges(vs, { ed => !isExcluded(ed.toId)})
      res
    }*/

    def outEdges(vs: Coll[Int], predicate: Rep[Edge[V, E]] => Rep[Boolean]): NColl[Edge[V, E]]

    def hasEdgeTo(fromId: Rep[Int], toId: Rep[Int]): Rep[Boolean]

    def commonNbrs(v1Id: Rep[Int], v2Id: Rep[Int]): Coll[Int]

    def commonNbrsNum(v1Id: Rep[Int], v2Id: Rep[Int]): Rep[Int]
  }

  implicit def defaultGraphElement[V: Elem, E: Elem]: Elem[Graph[V,E]] = {
    element[AdjacencyGraph[V,E]].asElem[Graph[V,E]]
  }

  trait GraphCompanion extends TypeFamily2[Graph] {
    def defaultOf[T: Elem, V:Elem] = AdjacencyGraph.defaultOf[T,V]
  }

  abstract class AdjacencyGraph[V, E](val vertexValues: Coll[V], val edgeValues: NColl[E], val links: NColl[Int])
  (implicit val eV: Elem[V], val eE: Elem[E]) extends Graph[V,E] {
    type EdgeType = AdjEdge[V, E]
    lazy val eEdge = element[EdgeType]
    def incMatrix = ???

    def vertexNum: Rep[Int] =  links.length
    def edgeNum: Rep[Int] = ??? //links.values.length
    def inDegrees: Coll[Int] = ??? /*{
      val zeros = replicate(vertexNum, 0)
      val ones = replicate(edgeNum, 1)
      zeros.permuteReduce(links.values, ones)
    }*/
    def outDegrees: Coll[Int] = ??? //links.segLengths

    def makeEdgeFrom(v: Rep[Int], iE: Rep[Int]): Rep[AdjEdge[V, E]] = AdjEdge(v, iE, thisGraph)
    def makeEdgeFromTo(v1: Rep[Int], v2: Rep[Int]): Rep[AdjEdge[V, E]] = ??? /* {
      val row = links(v1)
      val iE = ((row zip row.indexes).filter(s => (s._1 === v2)))(0)._2
      AdjEdge(v1, iE, thisGraph)
    } */

    def nodes: Coll[Vertex[V, E]] = ??? //getNodes(links.indexes)
    def edges: Coll[AdjEdge[V, E]] = ??? /*{
      val res = for {
        v: Rep[Int]@unchecked <- links.indexes
        iE <- links(v).indexes
      } yield makeEdgeFrom(v, iE)

      res
    }*/

    def outEdges(vs: Coll[Int], predicate: Rep[Edge[V, E]] => Rep[Boolean]): NColl[Edge[V, E]] = ??? /*{
      val res = (vs zip outNeighborsOf(vs)).map {
        case Pair(v, ns) =>
          ns.indexes.map({ i /*case Pair(i, _)*/ => makeEdgeFrom(v, i)}).
            filter {
            predicate(_)
          }
      }
      res.asInstanceOf[NColl[Edge[V, E]]]
    }*/
    def inNeighbors(v: Rep[Int]): Coll[Int] = ??? //inverted.links(v)
    def outNeighborsOf(v: Rep[Int]): Coll[Int] = ??? //links(v)
    //def outNeighborsOf(vs: Coll[Int])(implicit o: Overloaded1): NColl[Int] = links ->> vs
    def commonNbrs(v1Id: Rep[Int], v2Id: Rep[Int]): Coll[Int] = ??? /*{
      for (u <- outNeighborsOf(v1Id) if hasEdgeTo(u, v2Id)) yield u
    }*/
    def commonNbrsNum(v1Id: Rep[Int], v2Id: Rep[Int]): Rep[Int] = ??? /*{
      commonNbrs(v1Id, v2Id).length
    }*/

    def hasEdgeTo(fromId: Rep[Int], toId: Rep[Int]): Rep[Boolean] = ??? //outNeighborsOf(fromId).containsSorted(toId)

    def discardValues: Rep[SimpleGraph] = ??? /*{
      val sg = AdjacencyGraph(vertexValues.discardValues, edgeValues.discardNestedValues, links)
      sg
    }                                           */
  }

  trait AdjacencyGraphCompanion extends ConcreteClass2[AdjacencyGraph] {
    def defaultOf[V:Elem, E:Elem] = Default.defaultVal(AdjacencyGraph(element[Collection[V]].defaultRepValue,
                                                                      element[NestedCollection[E]].defaultRepValue,
                                                                      element[NestedCollection[Int]].defaultRepValue))
    def fromAdjacencyList[V:Elem, E:Elem](vertexValues: Coll[V], edgeValues: NColl[E], links: NColl[Int]) = {
      mkAdjacencyGraph(vertexValues, edgeValues, links)
    }
  }
}
