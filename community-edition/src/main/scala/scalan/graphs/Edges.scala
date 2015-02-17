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
  }
  abstract class AdjEdge[V, E](val fromId: Rep[Int], val outIndex: Rep[Int], val graph: PG[V, E])
                              (implicit val eV: Elem[V], val eE: Elem[E]) extends Edge[V, E] {
    private def indexOfTarget = ??? //graph.edgeValues.segOffsets(fromId) + outIndex

    def toId: Rep[Int] = graph.links.values(indexOfTarget)

    def fromNode: Rep[Vertex[V, E]] = SVertex(fromId, graph)

    def toNode: Rep[Vertex[V, E]] = SVertex(toId, graph)

    def value: Rep[E] = graph.edgeValues.values(indexOfTarget)
  }

  trait AdjEdgeCompanion extends ConcreteClass2[Edge] {
    def defaultOf[T: Elem, V:Elem] = Default.defaultVal(AdjEdge(-1, -1, element[Graph[T,V]].defaultRepValue))
  }

  abstract class IncEdge[V, E](val fromId: Rep[Int], val toId: Rep[Int], val graph: PG[V, E])
                              (implicit val eV: Elem[V], val eE: Elem[E]) extends Edge[V, E] {
    //private def indexOfTarget = graph.edgeValues.segOffsets(fromId) + outIndex
    //def toId: Rep[Int] = graph.links.values(indexOfTarget)
    def outIndex = ???

    def fromNode: Rep[Vertex[V, E]] = SVertex(fromId, graph)

    def toNode: Rep[Vertex[V, E]] = SVertex(toId, graph)

    def value: Rep[E] = ??? //graph.edgeValues.values(indexOfTarget)
  }
  trait IncEdgeCompanion extends ConcreteClass2[Edge] {
    def defaultOf[T: Elem, V:Elem] = Default.defaultVal(IncEdge(-1, -1, element[Graph[T,V]].defaultRepValue))
  }

}
