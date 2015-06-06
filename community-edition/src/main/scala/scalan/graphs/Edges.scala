package scalan.graphs

import scalan._
import scalan.collections.CollectionsDsl
import scalan.ScalanCommunityDsl
import scalan.Owner

trait Edges extends ScalanCommunityDsl with CollectionsDsl { self : GraphsDsl =>

  /**
   * Created by afilippov on 2/16/15.
   */
  trait Edge[V, E]  extends Reifiable[Edge[V,E]]{
    implicit def eV: Elem[V]

    implicit def eE: Elem[E]

    @Owner
    implicit def graph: Rep[Graph[V, E]]

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
    def MaxDoubleEdge = AdjEdge(element[Int].defaultRepValue, element[Int].defaultRepValue,
      AdjacencyGraph(element[Collection[Unit]].defaultRepValue,
        NestedCollectionFlat(Collection.singleton(Double.MaxValue), PairCollectionSOA(Collection.singleton(0), Collection.singleton(1))),
        element[NestedCollectionFlat[Int]].defaultRepValue)
    )
  }
  abstract class AdjEdge[V, E](val fromId: Rep[Int], val outIndex: Rep[Int], val graph: Rep[Graph[V, E]])
                              (implicit val eV: Elem[V], val eE: Elem[E]) extends Edge[V, E] {
    private def indexOfTarget = graph.edgeValues.segOffsets(fromId) + outIndex

    def toId: Rep[Int] = graph.links.values(indexOfTarget)

    def fromNode: Rep[Vertex[V, E]] = SVertex(fromId, graph)

    def toNode: Rep[Vertex[V, E]] = SVertex(toId, graph)

    def value: Rep[E] = graph.edgeValues.values(indexOfTarget)
  }

  trait AdjEdgeCompanion extends ConcreteClass2[Edge]

  abstract class IncEdge[V, E](val fromId: Rep[Int], val toId: Rep[Int], val graph: Rep[Graph[V, E]])
                              (implicit val eV: Elem[V], val eE: Elem[E]) extends Edge[V, E] {
    private def indexOfTarget = fromId*graph.vertexNum + toId
    //def toId: Rep[Int] = graph.links.values(indexOfTarget)
    def outIndex: Rep[Int] = ???

    def fromNode: Rep[Vertex[V, E]] = SVertex(fromId, graph)

    def toNode: Rep[Vertex[V, E]] = SVertex(toId, graph)

    def value: Rep[E] = graph.incMatrixWithVals(indexOfTarget)
  }
  trait IncEdgeCompanion extends ConcreteClass2[Edge]

}

trait EdgesDsl extends impl.EdgesAbs { self: GraphsDsl => }
trait EdgesDslSeq extends impl.EdgesSeq { self: GraphsDslSeq => }
trait EdgesDslExp extends impl.EdgesExp { self: GraphsDslExp => }
