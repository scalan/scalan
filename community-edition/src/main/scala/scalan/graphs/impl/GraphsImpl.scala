package scalan.graphs
package impl

import scala.annotation.unchecked.uncheckedVariance
import scalan._
import scalan.{ScalanSeq, ScalanExp, Scalan}
import scalan.collection.{CollectionsDslExp, CollectionsDslSeq, CollectionsDsl}
import scalan.common.Default
import scalan.common.OverloadHack.Overloaded1
import scala.reflect.runtime.universe._
import scala.reflect._
import scalan.common.Default

// Abs -----------------------------------
trait GraphsAbs extends Scalan with Graphs {
  self: GraphsDsl =>
  // single proxy for each type family
  implicit def proxyGraph[V, E](p: Rep[Graph[V, E]]): Graph[V, E] = {
    implicit val tag = weakTypeTag[Graph[V, E]]
    proxyOps[Graph[V, E]](p)(TagImplicits.typeTagToClassTag[Graph[V, E]])
  }

  abstract class GraphElem[V, E, From, To <: Graph[V, E]](iso: Iso[From, To])(implicit eV: Elem[V], eE: Elem[E])
    extends ViewElem[From, To](iso) {
    override def convert(x: Rep[Reifiable[_]]) = convertGraph(x.asRep[Graph[V, E]])
    def convertGraph(x : Rep[Graph[V, E]]): Rep[To]
  }

  trait GraphCompanionElem extends CompanionElem[GraphCompanionAbs]
  implicit lazy val GraphCompanionElem: GraphCompanionElem = new GraphCompanionElem {
    lazy val tag = weakTypeTag[GraphCompanionAbs]
    protected def getDefaultRep = Graph
  }

  abstract class GraphCompanionAbs extends CompanionBase[GraphCompanionAbs] with GraphCompanion {
    override def toString = "Graph"
  }
  def Graph: Rep[GraphCompanionAbs]
  implicit def proxyGraphCompanion(p: Rep[GraphCompanion]): GraphCompanion = {
    proxyOps[GraphCompanion](p)
  }

  // elem for concrete class
  class AdjacencyGraphElem[V, E](iso: Iso[AdjacencyGraphData[V, E], AdjacencyGraph[V, E]])(implicit val eV: Elem[V], val eE: Elem[E])
    extends GraphElem[V, E, AdjacencyGraphData[V, E], AdjacencyGraph[V, E]](iso) {
    def convertGraph(x: Rep[Graph[V, E]]) = AdjacencyGraph(x.vertexValues, x.edgeValues, x.links)
  }

  // state representation type
  type AdjacencyGraphData[V, E] = (Collection[V], (NestedCollection[E], NestedCollection[Int]))

  // 3) Iso for concrete class
  class AdjacencyGraphIso[V, E](implicit eV: Elem[V], eE: Elem[E])
    extends Iso[AdjacencyGraphData[V, E], AdjacencyGraph[V, E]] {
    override def from(p: Rep[AdjacencyGraph[V, E]]) =
      unmkAdjacencyGraph(p) match {
        case Some((vertexValues, edgeValues, links)) => Pair(vertexValues, Pair(edgeValues, links))
        case None => !!!
      }
    override def to(p: Rep[(Collection[V], (NestedCollection[E], NestedCollection[Int]))]) = {
      val Pair(vertexValues, Pair(edgeValues, links)) = p
      AdjacencyGraph(vertexValues, edgeValues, links)
    }
    lazy val tag = {
      weakTypeTag[AdjacencyGraph[V, E]]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[AdjacencyGraph[V, E]]](AdjacencyGraph(element[Collection[V]].defaultRepValue, element[NestedCollection[E]].defaultRepValue, element[NestedCollection[Int]].defaultRepValue))
    lazy val eTo = new AdjacencyGraphElem[V, E](this)
  }
  // 4) constructor and deconstructor
  abstract class AdjacencyGraphCompanionAbs extends CompanionBase[AdjacencyGraphCompanionAbs] with AdjacencyGraphCompanion {
    override def toString = "AdjacencyGraph"
    def apply[V, E](p: Rep[AdjacencyGraphData[V, E]])(implicit eV: Elem[V], eE: Elem[E]): Rep[AdjacencyGraph[V, E]] =
      isoAdjacencyGraph(eV, eE).to(p)
    def apply[V, E](vertexValues: Coll[V], edgeValues: NColl[E], links: NColl[Int])(implicit eV: Elem[V], eE: Elem[E]): Rep[AdjacencyGraph[V, E]] =
      mkAdjacencyGraph(vertexValues, edgeValues, links)
    def unapply[V:Elem, E:Elem](p: Rep[AdjacencyGraph[V, E]]) = unmkAdjacencyGraph(p)
  }
  def AdjacencyGraph: Rep[AdjacencyGraphCompanionAbs]
  implicit def proxyAdjacencyGraphCompanion(p: Rep[AdjacencyGraphCompanionAbs]): AdjacencyGraphCompanionAbs = {
    proxyOps[AdjacencyGraphCompanionAbs](p)
  }

  class AdjacencyGraphCompanionElem extends CompanionElem[AdjacencyGraphCompanionAbs] {
    lazy val tag = weakTypeTag[AdjacencyGraphCompanionAbs]
    protected def getDefaultRep = AdjacencyGraph
  }
  implicit lazy val AdjacencyGraphCompanionElem: AdjacencyGraphCompanionElem = new AdjacencyGraphCompanionElem

  implicit def proxyAdjacencyGraph[V, E](p: Rep[AdjacencyGraph[V, E]]): AdjacencyGraph[V, E] =
    proxyOps[AdjacencyGraph[V, E]](p)

  implicit class ExtendedAdjacencyGraph[V, E](p: Rep[AdjacencyGraph[V, E]])(implicit eV: Elem[V], eE: Elem[E]) {
    def toData: Rep[AdjacencyGraphData[V, E]] = isoAdjacencyGraph(eV, eE).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoAdjacencyGraph[V, E](implicit eV: Elem[V], eE: Elem[E]): Iso[AdjacencyGraphData[V, E], AdjacencyGraph[V, E]] =
    new AdjacencyGraphIso[V, E]

  // 6) smart constructor and deconstructor
  def mkAdjacencyGraph[V, E](vertexValues: Coll[V], edgeValues: NColl[E], links: NColl[Int])(implicit eV: Elem[V], eE: Elem[E]): Rep[AdjacencyGraph[V, E]]
  def unmkAdjacencyGraph[V:Elem, E:Elem](p: Rep[AdjacencyGraph[V, E]]): Option[(Rep[Collection[V]], Rep[NestedCollection[E]], Rep[NestedCollection[Int]])]

  // elem for concrete class
  class IncidenceGraphElem[V, E](iso: Iso[IncidenceGraphData[V, E], IncidenceGraph[V, E]])(implicit val eV: Elem[V], val eE: Elem[E])
    extends GraphElem[V, E, IncidenceGraphData[V, E], IncidenceGraph[V, E]](iso) {
    def convertGraph(x: Rep[Graph[V, E]]) = IncidenceGraph(x.vertexValues, x.incMatrixWithVals, x.vertexNum)
  }

  // state representation type
  type IncidenceGraphData[V, E] = (Collection[V], (Collection[E], Int))

  // 3) Iso for concrete class
  class IncidenceGraphIso[V, E](implicit eV: Elem[V], eE: Elem[E])
    extends Iso[IncidenceGraphData[V, E], IncidenceGraph[V, E]] {
    override def from(p: Rep[IncidenceGraph[V, E]]) =
      unmkIncidenceGraph(p) match {
        case Some((vertexValues, incMatrixWithVals, vertexNum)) => Pair(vertexValues, Pair(incMatrixWithVals, vertexNum))
        case None => !!!
      }
    override def to(p: Rep[(Collection[V], (Collection[E], Int))]) = {
      val Pair(vertexValues, Pair(incMatrixWithVals, vertexNum)) = p
      IncidenceGraph(vertexValues, incMatrixWithVals, vertexNum)
    }
    lazy val tag = {
      weakTypeTag[IncidenceGraph[V, E]]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[IncidenceGraph[V, E]]](IncidenceGraph(element[Collection[V]].defaultRepValue, element[Collection[E]].defaultRepValue, 0))
    lazy val eTo = new IncidenceGraphElem[V, E](this)
  }
  // 4) constructor and deconstructor
  abstract class IncidenceGraphCompanionAbs extends CompanionBase[IncidenceGraphCompanionAbs] with IncidenceGraphCompanion {
    override def toString = "IncidenceGraph"
    def apply[V, E](p: Rep[IncidenceGraphData[V, E]])(implicit eV: Elem[V], eE: Elem[E]): Rep[IncidenceGraph[V, E]] =
      isoIncidenceGraph(eV, eE).to(p)
    def apply[V, E](vertexValues: Coll[V], incMatrixWithVals: Coll[E], vertexNum: Rep[Int])(implicit eV: Elem[V], eE: Elem[E]): Rep[IncidenceGraph[V, E]] =
      mkIncidenceGraph(vertexValues, incMatrixWithVals, vertexNum)
    def unapply[V:Elem, E:Elem](p: Rep[IncidenceGraph[V, E]]) = unmkIncidenceGraph(p)
  }
  def IncidenceGraph: Rep[IncidenceGraphCompanionAbs]
  implicit def proxyIncidenceGraphCompanion(p: Rep[IncidenceGraphCompanionAbs]): IncidenceGraphCompanionAbs = {
    proxyOps[IncidenceGraphCompanionAbs](p)
  }

  class IncidenceGraphCompanionElem extends CompanionElem[IncidenceGraphCompanionAbs] {
    lazy val tag = weakTypeTag[IncidenceGraphCompanionAbs]
    protected def getDefaultRep = IncidenceGraph
  }
  implicit lazy val IncidenceGraphCompanionElem: IncidenceGraphCompanionElem = new IncidenceGraphCompanionElem

  implicit def proxyIncidenceGraph[V, E](p: Rep[IncidenceGraph[V, E]]): IncidenceGraph[V, E] =
    proxyOps[IncidenceGraph[V, E]](p)

  implicit class ExtendedIncidenceGraph[V, E](p: Rep[IncidenceGraph[V, E]])(implicit eV: Elem[V], eE: Elem[E]) {
    def toData: Rep[IncidenceGraphData[V, E]] = isoIncidenceGraph(eV, eE).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoIncidenceGraph[V, E](implicit eV: Elem[V], eE: Elem[E]): Iso[IncidenceGraphData[V, E], IncidenceGraph[V, E]] =
    new IncidenceGraphIso[V, E]

  // 6) smart constructor and deconstructor
  def mkIncidenceGraph[V, E](vertexValues: Coll[V], incMatrixWithVals: Coll[E], vertexNum: Rep[Int])(implicit eV: Elem[V], eE: Elem[E]): Rep[IncidenceGraph[V, E]]
  def unmkIncidenceGraph[V:Elem, E:Elem](p: Rep[IncidenceGraph[V, E]]): Option[(Rep[Collection[V]], Rep[Collection[E]], Rep[Int])]
}

// Seq -----------------------------------
trait GraphsSeq extends GraphsDsl with ScalanSeq {
  self: GraphsDslSeq =>
  lazy val Graph: Rep[GraphCompanionAbs] = new GraphCompanionAbs with UserTypeSeq[GraphCompanionAbs, GraphCompanionAbs] {
    lazy val selfType = element[GraphCompanionAbs]
  }

  case class SeqAdjacencyGraph[V, E]
      (override val vertexValues: Coll[V], override val edgeValues: NColl[E], override val links: NColl[Int])
      (implicit eV: Elem[V], eE: Elem[E])
    extends AdjacencyGraph[V, E](vertexValues, edgeValues, links)
        with UserTypeSeq[Graph[V,E], AdjacencyGraph[V, E]] {
    lazy val selfType = element[AdjacencyGraph[V, E]].asInstanceOf[Elem[Graph[V,E]]]
  }
  lazy val AdjacencyGraph = new AdjacencyGraphCompanionAbs with UserTypeSeq[AdjacencyGraphCompanionAbs, AdjacencyGraphCompanionAbs] {
    lazy val selfType = element[AdjacencyGraphCompanionAbs]
  }

  def mkAdjacencyGraph[V, E]
      (vertexValues: Coll[V], edgeValues: NColl[E], links: NColl[Int])(implicit eV: Elem[V], eE: Elem[E]): Rep[AdjacencyGraph[V, E]] =
      new SeqAdjacencyGraph[V, E](vertexValues, edgeValues, links)
  def unmkAdjacencyGraph[V:Elem, E:Elem](p: Rep[AdjacencyGraph[V, E]]) =
    Some((p.vertexValues, p.edgeValues, p.links))

  case class SeqIncidenceGraph[V, E]
      (override val vertexValues: Coll[V], override val incMatrixWithVals: Coll[E], override val vertexNum: Rep[Int])
      (implicit eV: Elem[V], eE: Elem[E])
    extends IncidenceGraph[V, E](vertexValues, incMatrixWithVals, vertexNum)
        with UserTypeSeq[Graph[V,E], IncidenceGraph[V, E]] {
    lazy val selfType = element[IncidenceGraph[V, E]].asInstanceOf[Elem[Graph[V,E]]]
  }
  lazy val IncidenceGraph = new IncidenceGraphCompanionAbs with UserTypeSeq[IncidenceGraphCompanionAbs, IncidenceGraphCompanionAbs] {
    lazy val selfType = element[IncidenceGraphCompanionAbs]
  }

  def mkIncidenceGraph[V, E]
      (vertexValues: Coll[V], incMatrixWithVals: Coll[E], vertexNum: Rep[Int])(implicit eV: Elem[V], eE: Elem[E]): Rep[IncidenceGraph[V, E]] =
      new SeqIncidenceGraph[V, E](vertexValues, incMatrixWithVals, vertexNum)
  def unmkIncidenceGraph[V:Elem, E:Elem](p: Rep[IncidenceGraph[V, E]]) =
    Some((p.vertexValues, p.incMatrixWithVals, p.vertexNum))
}

// Exp -----------------------------------
trait GraphsExp extends GraphsDsl with ScalanExp {
  self: GraphsDslExp =>
  lazy val Graph: Rep[GraphCompanionAbs] = new GraphCompanionAbs with UserTypeDef[GraphCompanionAbs, GraphCompanionAbs] {
    lazy val selfType = element[GraphCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  case class ExpAdjacencyGraph[V, E]
      (override val vertexValues: Coll[V], override val edgeValues: NColl[E], override val links: NColl[Int])
      (implicit eV: Elem[V], eE: Elem[E])
    extends AdjacencyGraph[V, E](vertexValues, edgeValues, links) with UserTypeDef[Graph[V,E], AdjacencyGraph[V, E]] {
    lazy val selfType = element[AdjacencyGraph[V, E]].asInstanceOf[Elem[Graph[V,E]]]
    override def mirror(t: Transformer) = ExpAdjacencyGraph[V, E](t(vertexValues), t(edgeValues), t(links))
  }

  lazy val AdjacencyGraph: Rep[AdjacencyGraphCompanionAbs] = new AdjacencyGraphCompanionAbs with UserTypeDef[AdjacencyGraphCompanionAbs, AdjacencyGraphCompanionAbs] {
    lazy val selfType = element[AdjacencyGraphCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object AdjacencyGraphMethods {
    object incMatrix {
      def unapply(d: Def[_]): Option[Rep[AdjacencyGraph[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AdjacencyGraphElem[_, _]] && method.getName == "incMatrix" =>
          Some(receiver).asInstanceOf[Option[Rep[AdjacencyGraph[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[AdjacencyGraph[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object incMatrixWithVals {
      def unapply(d: Def[_]): Option[Rep[AdjacencyGraph[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AdjacencyGraphElem[_, _]] && method.getName == "incMatrixWithVals" =>
          Some(receiver).asInstanceOf[Option[Rep[AdjacencyGraph[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[AdjacencyGraph[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object vertexNum {
      def unapply(d: Def[_]): Option[Rep[AdjacencyGraph[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AdjacencyGraphElem[_, _]] && method.getName == "vertexNum" =>
          Some(receiver).asInstanceOf[Option[Rep[AdjacencyGraph[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[AdjacencyGraph[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object edgeNum {
      def unapply(d: Def[_]): Option[Rep[AdjacencyGraph[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AdjacencyGraphElem[_, _]] && method.getName == "edgeNum" =>
          Some(receiver).asInstanceOf[Option[Rep[AdjacencyGraph[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[AdjacencyGraph[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object inDegrees {
      def unapply(d: Def[_]): Option[Rep[AdjacencyGraph[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AdjacencyGraphElem[_, _]] && method.getName == "inDegrees" =>
          Some(receiver).asInstanceOf[Option[Rep[AdjacencyGraph[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[AdjacencyGraph[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object outDegrees {
      def unapply(d: Def[_]): Option[Rep[AdjacencyGraph[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AdjacencyGraphElem[_, _]] && method.getName == "outDegrees" =>
          Some(receiver).asInstanceOf[Option[Rep[AdjacencyGraph[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[AdjacencyGraph[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object makeEdgeFrom {
      def unapply(d: Def[_]): Option[(Rep[AdjacencyGraph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(v, iE, _*), _) if receiver.elem.isInstanceOf[AdjacencyGraphElem[_, _]] && method.getName == "makeEdgeFrom" =>
          Some((receiver, v, iE)).asInstanceOf[Option[(Rep[AdjacencyGraph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AdjacencyGraph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object makeEdgeFromTo {
      def unapply(d: Def[_]): Option[(Rep[AdjacencyGraph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(v1, v2, _*), _) if receiver.elem.isInstanceOf[AdjacencyGraphElem[_, _]] && method.getName == "makeEdgeFromTo" =>
          Some((receiver, v1, v2)).asInstanceOf[Option[(Rep[AdjacencyGraph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AdjacencyGraph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object nodes {
      def unapply(d: Def[_]): Option[Rep[AdjacencyGraph[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AdjacencyGraphElem[_, _]] && method.getName == "nodes" =>
          Some(receiver).asInstanceOf[Option[Rep[AdjacencyGraph[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[AdjacencyGraph[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object edges {
      def unapply(d: Def[_]): Option[Rep[AdjacencyGraph[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AdjacencyGraphElem[_, _]] && method.getName == "edges" =>
          Some(receiver).asInstanceOf[Option[Rep[AdjacencyGraph[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[AdjacencyGraph[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `outEdges`: Method has function arguments predicate

    object inNeighbors {
      def unapply(d: Def[_]): Option[(Rep[AdjacencyGraph[V, E]], Rep[Int]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(v, _*), _) if receiver.elem.isInstanceOf[AdjacencyGraphElem[_, _]] && method.getName == "inNeighbors" =>
          Some((receiver, v)).asInstanceOf[Option[(Rep[AdjacencyGraph[V, E]], Rep[Int]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AdjacencyGraph[V, E]], Rep[Int]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object outNeighborsOf_1 {
      def unapply(d: Def[_]): Option[(Rep[AdjacencyGraph[V, E]], Rep[Int]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(v, _*), _) if receiver.elem.isInstanceOf[AdjacencyGraphElem[_, _]] && method.getName == "outNeighborsOf" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "1" } =>
          Some((receiver, v)).asInstanceOf[Option[(Rep[AdjacencyGraph[V, E]], Rep[Int]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AdjacencyGraph[V, E]], Rep[Int]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object outNeighborsOf_2 {
      def unapply(d: Def[_]): Option[(Rep[AdjacencyGraph[V, E]], Coll[Int]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(vs, _*), _) if receiver.elem.isInstanceOf[AdjacencyGraphElem[_, _]] && method.getName == "outNeighborsOf" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "2" } =>
          Some((receiver, vs)).asInstanceOf[Option[(Rep[AdjacencyGraph[V, E]], Coll[Int]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AdjacencyGraph[V, E]], Coll[Int]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object commonNbrs {
      def unapply(d: Def[_]): Option[(Rep[AdjacencyGraph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(v1Id, v2Id, _*), _) if receiver.elem.isInstanceOf[AdjacencyGraphElem[_, _]] && method.getName == "commonNbrs" =>
          Some((receiver, v1Id, v2Id)).asInstanceOf[Option[(Rep[AdjacencyGraph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AdjacencyGraph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object commonNbrsNum {
      def unapply(d: Def[_]): Option[(Rep[AdjacencyGraph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(v1Id, v2Id, _*), _) if receiver.elem.isInstanceOf[AdjacencyGraphElem[_, _]] && method.getName == "commonNbrsNum" =>
          Some((receiver, v1Id, v2Id)).asInstanceOf[Option[(Rep[AdjacencyGraph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AdjacencyGraph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object hasEdgeTo {
      def unapply(d: Def[_]): Option[(Rep[AdjacencyGraph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(fromId, toId, _*), _) if receiver.elem.isInstanceOf[AdjacencyGraphElem[_, _]] && method.getName == "hasEdgeTo" =>
          Some((receiver, fromId, toId)).asInstanceOf[Option[(Rep[AdjacencyGraph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AdjacencyGraph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object discardValues {
      def unapply(d: Def[_]): Option[Rep[AdjacencyGraph[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AdjacencyGraphElem[_, _]] && method.getName == "discardValues" =>
          Some(receiver).asInstanceOf[Option[Rep[AdjacencyGraph[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[AdjacencyGraph[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object AdjacencyGraphCompanionMethods {
    object defaultOf {
      def unapply(d: Def[_]): Option[Unit forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AdjacencyGraphCompanionElem] && method.getName == "defaultOf" =>
          Some(()).asInstanceOf[Option[Unit forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Unit forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromAdjacencyList {
      def unapply(d: Def[_]): Option[(Coll[V], NColl[E], NColl[Int]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(vertexValues, edgeValues, links, _*), _) if receiver.elem.isInstanceOf[AdjacencyGraphCompanionElem] && method.getName == "fromAdjacencyList" =>
          Some((vertexValues, edgeValues, links)).asInstanceOf[Option[(Coll[V], NColl[E], NColl[Int]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Coll[V], NColl[E], NColl[Int]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkAdjacencyGraph[V, E]
    (vertexValues: Coll[V], edgeValues: NColl[E], links: NColl[Int])(implicit eV: Elem[V], eE: Elem[E]): Rep[AdjacencyGraph[V, E]] =
    new ExpAdjacencyGraph[V, E](vertexValues, edgeValues, links)
  def unmkAdjacencyGraph[V:Elem, E:Elem](p: Rep[AdjacencyGraph[V, E]]) =
    Some((p.vertexValues, p.edgeValues, p.links))

  case class ExpIncidenceGraph[V, E]
      (override val vertexValues: Coll[V], override val incMatrixWithVals: Coll[E], override val vertexNum: Rep[Int])
      (implicit eV: Elem[V], eE: Elem[E])
    extends IncidenceGraph[V, E](vertexValues, incMatrixWithVals, vertexNum) with UserTypeDef[Graph[V,E], IncidenceGraph[V, E]] {
    lazy val selfType = element[IncidenceGraph[V, E]].asInstanceOf[Elem[Graph[V,E]]]
    override def mirror(t: Transformer) = ExpIncidenceGraph[V, E](t(vertexValues), t(incMatrixWithVals), t(vertexNum))
  }

  lazy val IncidenceGraph: Rep[IncidenceGraphCompanionAbs] = new IncidenceGraphCompanionAbs with UserTypeDef[IncidenceGraphCompanionAbs, IncidenceGraphCompanionAbs] {
    lazy val selfType = element[IncidenceGraphCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object IncidenceGraphMethods {
    object incMatrix {
      def unapply(d: Def[_]): Option[Rep[IncidenceGraph[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[IncidenceGraphElem[_, _]] && method.getName == "incMatrix" =>
          Some(receiver).asInstanceOf[Option[Rep[IncidenceGraph[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[IncidenceGraph[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object edgeNum {
      def unapply(d: Def[_]): Option[Rep[IncidenceGraph[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[IncidenceGraphElem[_, _]] && method.getName == "edgeNum" =>
          Some(receiver).asInstanceOf[Option[Rep[IncidenceGraph[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[IncidenceGraph[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object inDegrees {
      def unapply(d: Def[_]): Option[Rep[IncidenceGraph[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[IncidenceGraphElem[_, _]] && method.getName == "inDegrees" =>
          Some(receiver).asInstanceOf[Option[Rep[IncidenceGraph[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[IncidenceGraph[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object outDegrees {
      def unapply(d: Def[_]): Option[Rep[IncidenceGraph[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[IncidenceGraphElem[_, _]] && method.getName == "outDegrees" =>
          Some(receiver).asInstanceOf[Option[Rep[IncidenceGraph[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[IncidenceGraph[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object makeEdgeFrom {
      def unapply(d: Def[_]): Option[(Rep[IncidenceGraph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(v, iE, _*), _) if receiver.elem.isInstanceOf[IncidenceGraphElem[_, _]] && method.getName == "makeEdgeFrom" =>
          Some((receiver, v, iE)).asInstanceOf[Option[(Rep[IncidenceGraph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[IncidenceGraph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object makeEdgeFromTo {
      def unapply(d: Def[_]): Option[(Rep[IncidenceGraph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(v1, v2, _*), _) if receiver.elem.isInstanceOf[IncidenceGraphElem[_, _]] && method.getName == "makeEdgeFromTo" =>
          Some((receiver, v1, v2)).asInstanceOf[Option[(Rep[IncidenceGraph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[IncidenceGraph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object nodes {
      def unapply(d: Def[_]): Option[Rep[IncidenceGraph[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[IncidenceGraphElem[_, _]] && method.getName == "nodes" =>
          Some(receiver).asInstanceOf[Option[Rep[IncidenceGraph[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[IncidenceGraph[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object edges {
      def unapply(d: Def[_]): Option[Rep[IncidenceGraph[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[IncidenceGraphElem[_, _]] && method.getName == "edges" =>
          Some(receiver).asInstanceOf[Option[Rep[IncidenceGraph[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[IncidenceGraph[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object rowIndexes {
      def unapply(d: Def[_]): Option[Rep[IncidenceGraph[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[IncidenceGraphElem[_, _]] && method.getName == "rowIndexes" =>
          Some(receiver).asInstanceOf[Option[Rep[IncidenceGraph[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[IncidenceGraph[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object vertexRow {
      def unapply(d: Def[_]): Option[(Rep[IncidenceGraph[V, E]], Rep[Int]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(v, _*), _) if receiver.elem.isInstanceOf[IncidenceGraphElem[_, _]] && method.getName == "vertexRow" =>
          Some((receiver, v)).asInstanceOf[Option[(Rep[IncidenceGraph[V, E]], Rep[Int]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[IncidenceGraph[V, E]], Rep[Int]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object vertexNonZeroRow {
      def unapply(d: Def[_]): Option[(Rep[IncidenceGraph[V, E]], Rep[Int]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(v, _*), _) if receiver.elem.isInstanceOf[IncidenceGraphElem[_, _]] && method.getName == "vertexNonZeroRow" =>
          Some((receiver, v)).asInstanceOf[Option[(Rep[IncidenceGraph[V, E]], Rep[Int]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[IncidenceGraph[V, E]], Rep[Int]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object links {
      def unapply(d: Def[_]): Option[Rep[IncidenceGraph[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[IncidenceGraphElem[_, _]] && method.getName == "links" =>
          Some(receiver).asInstanceOf[Option[Rep[IncidenceGraph[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[IncidenceGraph[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object edgeValues {
      def unapply(d: Def[_]): Option[Rep[IncidenceGraph[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[IncidenceGraphElem[_, _]] && method.getName == "edgeValues" =>
          Some(receiver).asInstanceOf[Option[Rep[IncidenceGraph[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[IncidenceGraph[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `outEdges`: Method has function arguments predicate

    object inNeighbors {
      def unapply(d: Def[_]): Option[(Rep[IncidenceGraph[V, E]], Rep[Int]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(v, _*), _) if receiver.elem.isInstanceOf[IncidenceGraphElem[_, _]] && method.getName == "inNeighbors" =>
          Some((receiver, v)).asInstanceOf[Option[(Rep[IncidenceGraph[V, E]], Rep[Int]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[IncidenceGraph[V, E]], Rep[Int]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object outNeighborsOf_1 {
      def unapply(d: Def[_]): Option[(Rep[IncidenceGraph[V, E]], Rep[Int]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(v, _*), _) if receiver.elem.isInstanceOf[IncidenceGraphElem[_, _]] && method.getName == "outNeighborsOf" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "1" } =>
          Some((receiver, v)).asInstanceOf[Option[(Rep[IncidenceGraph[V, E]], Rep[Int]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[IncidenceGraph[V, E]], Rep[Int]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object outNeighborsOf_2 {
      def unapply(d: Def[_]): Option[(Rep[IncidenceGraph[V, E]], Coll[Int]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(vs, _*), _) if receiver.elem.isInstanceOf[IncidenceGraphElem[_, _]] && method.getName == "outNeighborsOf" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "2" } =>
          Some((receiver, vs)).asInstanceOf[Option[(Rep[IncidenceGraph[V, E]], Coll[Int]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[IncidenceGraph[V, E]], Coll[Int]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object outEdgesOf1 {
      def unapply(d: Def[_]): Option[(Rep[IncidenceGraph[V, E]], Rep[Int]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(v, _*), _) if receiver.elem.isInstanceOf[IncidenceGraphElem[_, _]] && method.getName == "outEdgesOf1" =>
          Some((receiver, v)).asInstanceOf[Option[(Rep[IncidenceGraph[V, E]], Rep[Int]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[IncidenceGraph[V, E]], Rep[Int]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object commonNbrs {
      def unapply(d: Def[_]): Option[(Rep[IncidenceGraph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(v1Id, v2Id, _*), _) if receiver.elem.isInstanceOf[IncidenceGraphElem[_, _]] && method.getName == "commonNbrs" =>
          Some((receiver, v1Id, v2Id)).asInstanceOf[Option[(Rep[IncidenceGraph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[IncidenceGraph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object commonNbrsNum {
      def unapply(d: Def[_]): Option[(Rep[IncidenceGraph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(v1Id, v2Id, _*), _) if receiver.elem.isInstanceOf[IncidenceGraphElem[_, _]] && method.getName == "commonNbrsNum" =>
          Some((receiver, v1Id, v2Id)).asInstanceOf[Option[(Rep[IncidenceGraph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[IncidenceGraph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object hasEdgeTo {
      def unapply(d: Def[_]): Option[(Rep[IncidenceGraph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(fromId, toId, _*), _) if receiver.elem.isInstanceOf[IncidenceGraphElem[_, _]] && method.getName == "hasEdgeTo" =>
          Some((receiver, fromId, toId)).asInstanceOf[Option[(Rep[IncidenceGraph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[IncidenceGraph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object discardValues {
      def unapply(d: Def[_]): Option[Rep[IncidenceGraph[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[IncidenceGraphElem[_, _]] && method.getName == "discardValues" =>
          Some(receiver).asInstanceOf[Option[Rep[IncidenceGraph[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[IncidenceGraph[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object IncidenceGraphCompanionMethods {
    object defaultOf {
      def unapply(d: Def[_]): Option[Unit forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[IncidenceGraphCompanionElem] && method.getName == "defaultOf" =>
          Some(()).asInstanceOf[Option[Unit forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Unit forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromAdjacencyMatrix {
      def unapply(d: Def[_]): Option[(Coll[V], Coll[E], Rep[Int]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(vertexValues, incMatrixWithVals, vertexNum, _*), _) if receiver.elem.isInstanceOf[IncidenceGraphCompanionElem] && method.getName == "fromAdjacencyMatrix" =>
          Some((vertexValues, incMatrixWithVals, vertexNum)).asInstanceOf[Option[(Coll[V], Coll[E], Rep[Int]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Coll[V], Coll[E], Rep[Int]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkIncidenceGraph[V, E]
    (vertexValues: Coll[V], incMatrixWithVals: Coll[E], vertexNum: Rep[Int])(implicit eV: Elem[V], eE: Elem[E]): Rep[IncidenceGraph[V, E]] =
    new ExpIncidenceGraph[V, E](vertexValues, incMatrixWithVals, vertexNum)
  def unmkIncidenceGraph[V:Elem, E:Elem](p: Rep[IncidenceGraph[V, E]]) =
    Some((p.vertexValues, p.incMatrixWithVals, p.vertexNum))

  object GraphMethods {
    // WARNING: Cannot generate matcher for method `makeEdgeFrom`: Method's return type REdge is not a Rep

    // WARNING: Cannot generate matcher for method `makeEdgeFromTo`: Method's return type REdge is not a Rep

    object thisGraph {
      def unapply(d: Def[_]): Option[Rep[Graph[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[GraphElem[_, _, _, _]] && method.getName == "thisGraph" =>
          Some(receiver).asInstanceOf[Option[Rep[Graph[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Graph[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object vertexValues {
      def unapply(d: Def[_]): Option[Rep[Graph[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[GraphElem[_, _, _, _]] && method.getName == "vertexValues" =>
          Some(receiver).asInstanceOf[Option[Rep[Graph[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Graph[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object edgeValues {
      def unapply(d: Def[_]): Option[Rep[Graph[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[GraphElem[_, _, _, _]] && method.getName == "edgeValues" =>
          Some(receiver).asInstanceOf[Option[Rep[Graph[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Graph[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object links {
      def unapply(d: Def[_]): Option[Rep[Graph[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[GraphElem[_, _, _, _]] && method.getName == "links" =>
          Some(receiver).asInstanceOf[Option[Rep[Graph[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Graph[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object incMatrix {
      def unapply(d: Def[_]): Option[Rep[Graph[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[GraphElem[_, _, _, _]] && method.getName == "incMatrix" =>
          Some(receiver).asInstanceOf[Option[Rep[Graph[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Graph[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object incMatrixWithVals {
      def unapply(d: Def[_]): Option[Rep[Graph[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[GraphElem[_, _, _, _]] && method.getName == "incMatrixWithVals" =>
          Some(receiver).asInstanceOf[Option[Rep[Graph[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Graph[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object discardValues {
      def unapply(d: Def[_]): Option[Rep[Graph[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[GraphElem[_, _, _, _]] && method.getName == "discardValues" =>
          Some(receiver).asInstanceOf[Option[Rep[Graph[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Graph[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `unzipValues`: Method's return type (Rep[SimpleGraph],Coll[V],NColl[E]) is not a Rep

    object addEdges {
      def unapply(d: Def[_]): Option[(Rep[Graph[V, E]], Rep[EdgeList]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(el, _*), _) if receiver.elem.isInstanceOf[GraphElem[_, _, _, _]] && method.getName == "addEdges" =>
          Some((receiver, el)).asInstanceOf[Option[(Rep[Graph[V, E]], Rep[EdgeList]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Graph[V, E]], Rep[EdgeList]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object inverted {
      def unapply(d: Def[_]): Option[Rep[Graph[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[GraphElem[_, _, _, _]] && method.getName == "inverted" =>
          Some(receiver).asInstanceOf[Option[Rep[Graph[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Graph[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object complement {
      def unapply(d: Def[_]): Option[Rep[Graph[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[GraphElem[_, _, _, _]] && method.getName == "complement" =>
          Some(receiver).asInstanceOf[Option[Rep[Graph[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Graph[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object vertexNum {
      def unapply(d: Def[_]): Option[Rep[Graph[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[GraphElem[_, _, _, _]] && method.getName == "vertexNum" =>
          Some(receiver).asInstanceOf[Option[Rep[Graph[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Graph[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object edgeNum {
      def unapply(d: Def[_]): Option[Rep[Graph[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[GraphElem[_, _, _, _]] && method.getName == "edgeNum" =>
          Some(receiver).asInstanceOf[Option[Rep[Graph[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Graph[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object inDegrees {
      def unapply(d: Def[_]): Option[Rep[Graph[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[GraphElem[_, _, _, _]] && method.getName == "inDegrees" =>
          Some(receiver).asInstanceOf[Option[Rep[Graph[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Graph[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object outDegrees {
      def unapply(d: Def[_]): Option[Rep[Graph[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[GraphElem[_, _, _, _]] && method.getName == "outDegrees" =>
          Some(receiver).asInstanceOf[Option[Rep[Graph[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Graph[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object getNode {
      def unapply(d: Def[_]): Option[(Rep[Graph[V, E]], Rep[Int]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(vId, _*), _) if receiver.elem.isInstanceOf[GraphElem[_, _, _, _]] && method.getName == "getNode" =>
          Some((receiver, vId)).asInstanceOf[Option[(Rep[Graph[V, E]], Rep[Int]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Graph[V, E]], Rep[Int]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object getNodes {
      def unapply(d: Def[_]): Option[(Rep[Graph[V, E]], Coll[Int]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(vIds, _*), _) if receiver.elem.isInstanceOf[GraphElem[_, _, _, _]] && method.getName == "getNodes" =>
          Some((receiver, vIds)).asInstanceOf[Option[(Rep[Graph[V, E]], Coll[Int]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Graph[V, E]], Coll[Int]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object nodes {
      def unapply(d: Def[_]): Option[Rep[Graph[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[GraphElem[_, _, _, _]] && method.getName == "nodes" =>
          Some(receiver).asInstanceOf[Option[Rep[Graph[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Graph[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object edges {
      def unapply(d: Def[_]): Option[Rep[Graph[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[GraphElem[_, _, _, _]] && method.getName == "edges" =>
          Some(receiver).asInstanceOf[Option[Rep[Graph[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Graph[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object inNeighbors {
      def unapply(d: Def[_]): Option[(Rep[Graph[V, E]], Rep[Int]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(v, _*), _) if receiver.elem.isInstanceOf[GraphElem[_, _, _, _]] && method.getName == "inNeighbors" =>
          Some((receiver, v)).asInstanceOf[Option[(Rep[Graph[V, E]], Rep[Int]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Graph[V, E]], Rep[Int]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object outNeighborsOf_1 {
      def unapply(d: Def[_]): Option[(Rep[Graph[V, E]], Rep[Int]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(v, _*), _) if receiver.elem.isInstanceOf[GraphElem[_, _, _, _]] && method.getName == "outNeighborsOf" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "1" } =>
          Some((receiver, v)).asInstanceOf[Option[(Rep[Graph[V, E]], Rep[Int]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Graph[V, E]], Rep[Int]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object outNeighborsOf_2 {
      def unapply(d: Def[_]): Option[(Rep[Graph[V, E]], Coll[Int]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(vs, _*), _) if receiver.elem.isInstanceOf[GraphElem[_, _, _, _]] && method.getName == "outNeighborsOf" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "2" } =>
          Some((receiver, vs)).asInstanceOf[Option[(Rep[Graph[V, E]], Coll[Int]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Graph[V, E]], Coll[Int]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object outEdgesOf_1 {
      def unapply(d: Def[_]): Option[(Rep[Graph[V, E]], Coll[Int], Rep[PBitSet]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(vs, excluding, _*), _) if receiver.elem.isInstanceOf[GraphElem[_, _, _, _]] && method.getName == "outEdgesOf" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "1" } =>
          Some((receiver, vs, excluding)).asInstanceOf[Option[(Rep[Graph[V, E]], Coll[Int], Rep[PBitSet]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Graph[V, E]], Coll[Int], Rep[PBitSet]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object outEdgesOf_2 {
      def unapply(d: Def[_]): Option[(Rep[Graph[V, E]], Rep[Front]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(fr, _*), _) if receiver.elem.isInstanceOf[GraphElem[_, _, _, _]] && method.getName == "outEdgesOf" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "2" } =>
          Some((receiver, fr)).asInstanceOf[Option[(Rep[Graph[V, E]], Rep[Front]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Graph[V, E]], Rep[Front]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `outEdges`: Method has function arguments predicate

    object hasEdgeTo {
      def unapply(d: Def[_]): Option[(Rep[Graph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(fromId, toId, _*), _) if receiver.elem.isInstanceOf[GraphElem[_, _, _, _]] && method.getName == "hasEdgeTo" =>
          Some((receiver, fromId, toId)).asInstanceOf[Option[(Rep[Graph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Graph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object commonNbrs {
      def unapply(d: Def[_]): Option[(Rep[Graph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(v1Id, v2Id, _*), _) if receiver.elem.isInstanceOf[GraphElem[_, _, _, _]] && method.getName == "commonNbrs" =>
          Some((receiver, v1Id, v2Id)).asInstanceOf[Option[(Rep[Graph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Graph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object commonNbrsNum {
      def unapply(d: Def[_]): Option[(Rep[Graph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(v1Id, v2Id, _*), _) if receiver.elem.isInstanceOf[GraphElem[_, _, _, _]] && method.getName == "commonNbrsNum" =>
          Some((receiver, v1Id, v2Id)).asInstanceOf[Option[(Rep[Graph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Graph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object GraphCompanionMethods {
    object defaultOf {
      def unapply(d: Def[_]): Option[Unit forSome {type T; type V}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[GraphCompanionElem] && method.getName == "defaultOf" =>
          Some(()).asInstanceOf[Option[Unit forSome {type T; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Unit forSome {type T; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}
