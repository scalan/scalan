package scalan.graphs

import scalan.common.Default
import scalan.community.ScalanCommunityDsl
import scalan.{ScalanSeq, ScalanExp, ScalanDsl}
import scalan.collection.{CollectionsDslExp, CollectionsDslSeq, CollectionsDsl}

trait Edges extends ScalanCommunityDsl with CollectionsDsl { self : GraphsDsl =>

  /**
   * Created by afilippov on 2/16/15.
   */
  trait Edge[V, E] {
    implicit def eV: Elem[V]

    implicit def eE: Elem[E]

    implicit def graph: PG[V, E] // ?

    //private def indexOfTarget = this.graph.edgeValues.segOffsets(fromId) + outIndex

    def outIndex: Rep[Int]

    def fromId: Rep[Int]

    def toId: Rep[Int]

    //= this.graph.links.values(indexOfTarget)
    def fromNode: Rep[Vertex[V, E]]

    //= Vertex(fromId, graph)
    def toNode: Rep[Vertex[V, E]]

    //= Vertex(toId, graph)
    def value: Rep[E] //= this.graph.edgeValues.values(indexOfTarget)
  }

  trait EdgeCompanion extends TypeFamily2[Edge] {
    def defaultOf[T: Elem, V:Elem] = AdjEdge.defaultOf[T,V]
    def MaxDoubleEdge =  AdjEdge(element[Int].defaultRepValue, element[Int].defaultRepValue,
      AdjacencyGraph(element[Collection[Unit]].defaultRepValue,
        NestedCollection(Collection.singleton(Double.MaxValue), Collection.singleton(Pair(0,1))),
        element[NestedCollection[Int]].defaultRepValue)
    )
  }
  abstract class AdjEdge[V, E](val fromId: Rep[Int], val outIndex: Rep[Int], val graph: PG[V, E])
                              (implicit val eV: Elem[V], val eE: Elem[E]) extends Edge[V, E] {
    private def indexOfTarget = graph.edgeValues.segOffsets(fromId) + outIndex

    def toId: Rep[Int] = graph.links.values(indexOfTarget)

    def fromNode: Rep[Vertex[V, E]] = SVertex(fromId, graph)

    def toNode: Rep[Vertex[V, E]] = SVertex(toId, graph)

    def value: Rep[E] = graph.edgeValues.values(indexOfTarget)
  }

  implicit def defaultEdgeElement[V: Elem, E: Elem]: Elem[Edge[V,E]] = {
    element[AdjEdge[V,E]].asElem[Edge[V,E]]
  }

  trait AdjEdgeCompanion extends ConcreteClass2[Edge] {
    def defaultOf[T: Elem, V:Elem] = Default.defaultVal(AdjEdge(element[Int].defaultRepValue, element[Int].defaultRepValue, element[Graph[T,V]].defaultRepValue))
  }

  abstract class IncEdge[V, E](val fromId: Rep[Int], val toId: Rep[Int], val graph: PG[V, E])
                              (implicit val eV: Elem[V], val eE: Elem[E]) extends Edge[V, E] {
    private def indexOfTarget = fromId*graph.vertexNum + toId
    //def toId: Rep[Int] = graph.links.values(indexOfTarget)
    def outIndex = ???

    def fromNode: Rep[Vertex[V, E]] = SVertex(fromId, graph)

    def toNode: Rep[Vertex[V, E]] = SVertex(toId, graph)

    def value: Rep[E] = graph.incMatrixWithVals(indexOfTarget)
  }
  trait IncEdgeCompanion extends ConcreteClass2[Edge] {
    def defaultOf[T: Elem, V:Elem] = Default.defaultVal(IncEdge(-1, -1, element[Graph[T,V]].defaultRepValue))
  }

}
