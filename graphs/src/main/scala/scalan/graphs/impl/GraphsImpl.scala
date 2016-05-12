package scalan.graphs

import scalan._
import scalan.collections.{CollectionsDslExp, CollectionsDslStd, CollectionsDsl}
import scalan.{ScalanStd, ScalanExp, Scalan}
import scalan.common.OverloadHack.Overloaded1
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait GraphsAbs extends scalan.ScalanDsl with Graphs {
  self: GraphsDsl =>

  // single proxy for each type family
  implicit def proxyGraph[V, E](p: Rep[Graph[V, E]]): Graph[V, E] = {
    proxyOps[Graph[V, E]](p)(scala.reflect.classTag[Graph[V, E]])
  }

  // familyElem
  class GraphElem[V, E, To <: Graph[V, E]](implicit _eV: Elem[V], _eE: Elem[E])
    extends EntityElem[To] {
    def eV = _eV
    def eE = _eE
    lazy val parent: Option[Elem[_]] = None
    lazy val typeArgs = scala.collection.immutable.ListMap[String, TypeDesc]("V" -> eV, "E" -> eE)
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagV = eV.tag
      implicit val tagE = eE.tag
      weakTypeTag[Graph[V, E]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[Graph[V, E]] => convertGraph(x) }
      tryConvert(element[Graph[V, E]], this, x, conv)
    }

    def convertGraph(x: Rep[Graph[V, E]]): Rep[To] = {
      x.selfType1 match {
        case _: GraphElem[_, _, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have GraphElem[_, _, _], but got $e", x)
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def graphElement[V, E](implicit eV: Elem[V], eE: Elem[E]): Elem[Graph[V, E]] =
    cachedElem[GraphElem[V, E, Graph[V, E]]](eV, eE)

  implicit case object GraphCompanionElem extends CompanionElem[GraphCompanionAbs] {
    lazy val tag = weakTypeTag[GraphCompanionAbs]
    protected def getDefaultRep = Graph
  }

  abstract class GraphCompanionAbs extends CompanionDef[GraphCompanionAbs] with GraphCompanion {
    def selfType = GraphCompanionElem
    override def toString = "Graph"
  }
  def Graph: Rep[GraphCompanionAbs]
  implicit def proxyGraphCompanionAbs(p: Rep[GraphCompanionAbs]): GraphCompanionAbs =
    proxyOps[GraphCompanionAbs](p)

  abstract class AbsAdjacencyGraph[V, E]
      (vertexValues: Coll[V], edgeValues: NColl[E], links: NColl[Int])(implicit eV: Elem[V], eE: Elem[E])
    extends AdjacencyGraph[V, E](vertexValues, edgeValues, links) with Def[AdjacencyGraph[V, E]] {
    lazy val selfType = element[AdjacencyGraph[V, E]]
  }
  // elem for concrete class
  class AdjacencyGraphElem[V, E](val iso: Iso[AdjacencyGraphData[V, E], AdjacencyGraph[V, E]])(implicit override val eV: Elem[V], override val eE: Elem[E])
    extends GraphElem[V, E, AdjacencyGraph[V, E]]
    with ConcreteElem[AdjacencyGraphData[V, E], AdjacencyGraph[V, E]] {
    override lazy val parent: Option[Elem[_]] = Some(graphElement(element[V], element[E]))
    override lazy val typeArgs = scala.collection.immutable.ListMap[String, TypeDesc]("V" -> eV, "E" -> eE)

    override def convertGraph(x: Rep[Graph[V, E]]) = AdjacencyGraph(x.vertexValues, x.edgeValues, x.links)
    override def getDefaultRep = AdjacencyGraph(element[Collection[V]].defaultRepValue, element[NestedCollection[E]].defaultRepValue, element[NestedCollection[Int]].defaultRepValue)
    override lazy val tag = {
      implicit val tagV = eV.tag
      implicit val tagE = eE.tag
      weakTypeTag[AdjacencyGraph[V, E]]
    }
  }

  // state representation type
  type AdjacencyGraphData[V, E] = (Collection[V], (NestedCollection[E], NestedCollection[Int]))

  // 3) Iso for concrete class
  class AdjacencyGraphIso[V, E](implicit eV: Elem[V], eE: Elem[E])
    extends EntityIso[AdjacencyGraphData[V, E], AdjacencyGraph[V, E]] with Def[AdjacencyGraphIso[V, E]] {
    override def from(p: Rep[AdjacencyGraph[V, E]]) =
      (p.vertexValues, p.edgeValues, p.links)
    override def to(p: Rep[(Collection[V], (NestedCollection[E], NestedCollection[Int]))]) = {
      val Pair(vertexValues, Pair(edgeValues, links)) = p
      AdjacencyGraph(vertexValues, edgeValues, links)
    }
    lazy val eFrom = pairElement(element[Collection[V]], pairElement(element[NestedCollection[E]], element[NestedCollection[Int]]))
    lazy val eTo = new AdjacencyGraphElem[V, E](self)
    lazy val selfType = new AdjacencyGraphIsoElem[V, E](eV, eE)
    def productArity = 2
    def productElement(n: Int) = (eV, eE).productElement(n)
  }
  case class AdjacencyGraphIsoElem[V, E](eV: Elem[V], eE: Elem[E]) extends Elem[AdjacencyGraphIso[V, E]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new AdjacencyGraphIso[V, E]()(eV, eE))
    lazy val tag = {
      implicit val tagV = eV.tag
      implicit val tagE = eE.tag
      weakTypeTag[AdjacencyGraphIso[V, E]]
    }
  }
  // 4) constructor and deconstructor
  class AdjacencyGraphCompanionAbs extends CompanionDef[AdjacencyGraphCompanionAbs] with AdjacencyGraphCompanion {
    def selfType = AdjacencyGraphCompanionElem
    override def toString = "AdjacencyGraph"
    @scalan.OverloadId("fromData")
    def apply[V, E](p: Rep[AdjacencyGraphData[V, E]])(implicit eV: Elem[V], eE: Elem[E]): Rep[AdjacencyGraph[V, E]] =
      isoAdjacencyGraph(eV, eE).to(p)
    @scalan.OverloadId("fromFields")
    def apply[V, E](vertexValues: Coll[V], edgeValues: NColl[E], links: NColl[Int])(implicit eV: Elem[V], eE: Elem[E]): Rep[AdjacencyGraph[V, E]] =
      mkAdjacencyGraph(vertexValues, edgeValues, links)

    def unapply[V, E](p: Rep[Graph[V, E]]) = unmkAdjacencyGraph(p)
  }
  lazy val AdjacencyGraphRep: Rep[AdjacencyGraphCompanionAbs] = new AdjacencyGraphCompanionAbs
  lazy val AdjacencyGraph: AdjacencyGraphCompanionAbs = proxyAdjacencyGraphCompanion(AdjacencyGraphRep)
  implicit def proxyAdjacencyGraphCompanion(p: Rep[AdjacencyGraphCompanionAbs]): AdjacencyGraphCompanionAbs = {
    proxyOps[AdjacencyGraphCompanionAbs](p)
  }

  implicit case object AdjacencyGraphCompanionElem extends CompanionElem[AdjacencyGraphCompanionAbs] {
    lazy val tag = weakTypeTag[AdjacencyGraphCompanionAbs]
    protected def getDefaultRep = AdjacencyGraph
  }

  implicit def proxyAdjacencyGraph[V, E](p: Rep[AdjacencyGraph[V, E]]): AdjacencyGraph[V, E] =
    proxyOps[AdjacencyGraph[V, E]](p)

  implicit class ExtendedAdjacencyGraph[V, E](p: Rep[AdjacencyGraph[V, E]])(implicit eV: Elem[V], eE: Elem[E]) {
    def toData: Rep[AdjacencyGraphData[V, E]] = isoAdjacencyGraph(eV, eE).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoAdjacencyGraph[V, E](implicit eV: Elem[V], eE: Elem[E]): Iso[AdjacencyGraphData[V, E], AdjacencyGraph[V, E]] =
    reifyObject(new AdjacencyGraphIso[V, E]()(eV, eE))

  // 6) smart constructor and deconstructor
  def mkAdjacencyGraph[V, E](vertexValues: Coll[V], edgeValues: NColl[E], links: NColl[Int])(implicit eV: Elem[V], eE: Elem[E]): Rep[AdjacencyGraph[V, E]]
  def unmkAdjacencyGraph[V, E](p: Rep[Graph[V, E]]): Option[(Rep[Collection[V]], Rep[NestedCollection[E]], Rep[NestedCollection[Int]])]

  abstract class AbsIncidenceGraph[V, E]
      (vertexValues: Coll[V], incMatrixWithVals: Coll[E], vertexNum: Rep[Int])(implicit eV: Elem[V], eE: Elem[E])
    extends IncidenceGraph[V, E](vertexValues, incMatrixWithVals, vertexNum) with Def[IncidenceGraph[V, E]] {
    lazy val selfType = element[IncidenceGraph[V, E]]
  }
  // elem for concrete class
  class IncidenceGraphElem[V, E](val iso: Iso[IncidenceGraphData[V, E], IncidenceGraph[V, E]])(implicit override val eV: Elem[V], override val eE: Elem[E])
    extends GraphElem[V, E, IncidenceGraph[V, E]]
    with ConcreteElem[IncidenceGraphData[V, E], IncidenceGraph[V, E]] {
    override lazy val parent: Option[Elem[_]] = Some(graphElement(element[V], element[E]))
    override lazy val typeArgs = scala.collection.immutable.ListMap[String, TypeDesc]("V" -> eV, "E" -> eE)

    override def convertGraph(x: Rep[Graph[V, E]]) = IncidenceGraph(x.vertexValues, x.incMatrixWithVals, x.vertexNum)
    override def getDefaultRep = IncidenceGraph(element[Collection[V]].defaultRepValue, element[Collection[E]].defaultRepValue, 0)
    override lazy val tag = {
      implicit val tagV = eV.tag
      implicit val tagE = eE.tag
      weakTypeTag[IncidenceGraph[V, E]]
    }
  }

  // state representation type
  type IncidenceGraphData[V, E] = (Collection[V], (Collection[E], Int))

  // 3) Iso for concrete class
  class IncidenceGraphIso[V, E](implicit eV: Elem[V], eE: Elem[E])
    extends EntityIso[IncidenceGraphData[V, E], IncidenceGraph[V, E]] with Def[IncidenceGraphIso[V, E]] {
    override def from(p: Rep[IncidenceGraph[V, E]]) =
      (p.vertexValues, p.incMatrixWithVals, p.vertexNum)
    override def to(p: Rep[(Collection[V], (Collection[E], Int))]) = {
      val Pair(vertexValues, Pair(incMatrixWithVals, vertexNum)) = p
      IncidenceGraph(vertexValues, incMatrixWithVals, vertexNum)
    }
    lazy val eFrom = pairElement(element[Collection[V]], pairElement(element[Collection[E]], element[Int]))
    lazy val eTo = new IncidenceGraphElem[V, E](self)
    lazy val selfType = new IncidenceGraphIsoElem[V, E](eV, eE)
    def productArity = 2
    def productElement(n: Int) = (eV, eE).productElement(n)
  }
  case class IncidenceGraphIsoElem[V, E](eV: Elem[V], eE: Elem[E]) extends Elem[IncidenceGraphIso[V, E]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new IncidenceGraphIso[V, E]()(eV, eE))
    lazy val tag = {
      implicit val tagV = eV.tag
      implicit val tagE = eE.tag
      weakTypeTag[IncidenceGraphIso[V, E]]
    }
  }
  // 4) constructor and deconstructor
  class IncidenceGraphCompanionAbs extends CompanionDef[IncidenceGraphCompanionAbs] with IncidenceGraphCompanion {
    def selfType = IncidenceGraphCompanionElem
    override def toString = "IncidenceGraph"
    @scalan.OverloadId("fromData")
    def apply[V, E](p: Rep[IncidenceGraphData[V, E]])(implicit eV: Elem[V], eE: Elem[E]): Rep[IncidenceGraph[V, E]] =
      isoIncidenceGraph(eV, eE).to(p)
    @scalan.OverloadId("fromFields")
    def apply[V, E](vertexValues: Coll[V], incMatrixWithVals: Coll[E], vertexNum: Rep[Int])(implicit eV: Elem[V], eE: Elem[E]): Rep[IncidenceGraph[V, E]] =
      mkIncidenceGraph(vertexValues, incMatrixWithVals, vertexNum)

    def unapply[V, E](p: Rep[Graph[V, E]]) = unmkIncidenceGraph(p)
  }
  lazy val IncidenceGraphRep: Rep[IncidenceGraphCompanionAbs] = new IncidenceGraphCompanionAbs
  lazy val IncidenceGraph: IncidenceGraphCompanionAbs = proxyIncidenceGraphCompanion(IncidenceGraphRep)
  implicit def proxyIncidenceGraphCompanion(p: Rep[IncidenceGraphCompanionAbs]): IncidenceGraphCompanionAbs = {
    proxyOps[IncidenceGraphCompanionAbs](p)
  }

  implicit case object IncidenceGraphCompanionElem extends CompanionElem[IncidenceGraphCompanionAbs] {
    lazy val tag = weakTypeTag[IncidenceGraphCompanionAbs]
    protected def getDefaultRep = IncidenceGraph
  }

  implicit def proxyIncidenceGraph[V, E](p: Rep[IncidenceGraph[V, E]]): IncidenceGraph[V, E] =
    proxyOps[IncidenceGraph[V, E]](p)

  implicit class ExtendedIncidenceGraph[V, E](p: Rep[IncidenceGraph[V, E]])(implicit eV: Elem[V], eE: Elem[E]) {
    def toData: Rep[IncidenceGraphData[V, E]] = isoIncidenceGraph(eV, eE).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoIncidenceGraph[V, E](implicit eV: Elem[V], eE: Elem[E]): Iso[IncidenceGraphData[V, E], IncidenceGraph[V, E]] =
    reifyObject(new IncidenceGraphIso[V, E]()(eV, eE))

  // 6) smart constructor and deconstructor
  def mkIncidenceGraph[V, E](vertexValues: Coll[V], incMatrixWithVals: Coll[E], vertexNum: Rep[Int])(implicit eV: Elem[V], eE: Elem[E]): Rep[IncidenceGraph[V, E]]
  def unmkIncidenceGraph[V, E](p: Rep[Graph[V, E]]): Option[(Rep[Collection[V]], Rep[Collection[E]], Rep[Int])]

  registerModule(Graphs_Module)
}

// Std -----------------------------------
trait GraphsStd extends scalan.ScalanDslStd with GraphsDsl {
  self: GraphsDslStd =>
  lazy val Graph: Rep[GraphCompanionAbs] = new GraphCompanionAbs {
  }

  case class StdAdjacencyGraph[V, E]
      (override val vertexValues: Coll[V], override val edgeValues: NColl[E], override val links: NColl[Int])(implicit eV: Elem[V], eE: Elem[E])
    extends AbsAdjacencyGraph[V, E](vertexValues, edgeValues, links) {
  }

  def mkAdjacencyGraph[V, E]
    (vertexValues: Coll[V], edgeValues: NColl[E], links: NColl[Int])(implicit eV: Elem[V], eE: Elem[E]): Rep[AdjacencyGraph[V, E]] =
    new StdAdjacencyGraph[V, E](vertexValues, edgeValues, links)
  def unmkAdjacencyGraph[V, E](p: Rep[Graph[V, E]]) = p match {
    case p: AdjacencyGraph[V, E] @unchecked =>
      Some((p.vertexValues, p.edgeValues, p.links))
    case _ => None
  }

  case class StdIncidenceGraph[V, E]
      (override val vertexValues: Coll[V], override val incMatrixWithVals: Coll[E], override val vertexNum: Rep[Int])(implicit eV: Elem[V], eE: Elem[E])
    extends AbsIncidenceGraph[V, E](vertexValues, incMatrixWithVals, vertexNum) {
  }

  def mkIncidenceGraph[V, E]
    (vertexValues: Coll[V], incMatrixWithVals: Coll[E], vertexNum: Rep[Int])(implicit eV: Elem[V], eE: Elem[E]): Rep[IncidenceGraph[V, E]] =
    new StdIncidenceGraph[V, E](vertexValues, incMatrixWithVals, vertexNum)
  def unmkIncidenceGraph[V, E](p: Rep[Graph[V, E]]) = p match {
    case p: IncidenceGraph[V, E] @unchecked =>
      Some((p.vertexValues, p.incMatrixWithVals, p.vertexNum))
    case _ => None
  }
}

// Exp -----------------------------------
trait GraphsExp extends scalan.ScalanDslExp with GraphsDsl {
  self: GraphsDslExp =>
  lazy val Graph: Rep[GraphCompanionAbs] = new GraphCompanionAbs {
  }

  case class ExpAdjacencyGraph[V, E]
      (override val vertexValues: Coll[V], override val edgeValues: NColl[E], override val links: NColl[Int])(implicit eV: Elem[V], eE: Elem[E])
    extends AbsAdjacencyGraph[V, E](vertexValues, edgeValues, links)

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

    object outEdges {
      def unapply(d: Def[_]): Option[(Rep[AdjacencyGraph[V, E]], Coll[Int], Rep[Edge[V, E] => Boolean]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(vs, predicate, _*), _) if receiver.elem.isInstanceOf[AdjacencyGraphElem[_, _]] && method.getName == "outEdges" =>
          Some((receiver, vs, predicate)).asInstanceOf[Option[(Rep[AdjacencyGraph[V, E]], Coll[Int], Rep[Edge[V, E] => Boolean]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AdjacencyGraph[V, E]], Coll[Int], Rep[Edge[V, E] => Boolean]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

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
    object fromAdjacencyList {
      def unapply(d: Def[_]): Option[(Coll[V], NColl[E], NColl[Int]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(vertexValues, edgeValues, links, _*), _) if receiver.elem == AdjacencyGraphCompanionElem && method.getName == "fromAdjacencyList" =>
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
  def unmkAdjacencyGraph[V, E](p: Rep[Graph[V, E]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: AdjacencyGraphElem[V, E] @unchecked =>
      Some((p.asRep[AdjacencyGraph[V, E]].vertexValues, p.asRep[AdjacencyGraph[V, E]].edgeValues, p.asRep[AdjacencyGraph[V, E]].links))
    case _ =>
      None
  }

  case class ExpIncidenceGraph[V, E]
      (override val vertexValues: Coll[V], override val incMatrixWithVals: Coll[E], override val vertexNum: Rep[Int])(implicit eV: Elem[V], eE: Elem[E])
    extends AbsIncidenceGraph[V, E](vertexValues, incMatrixWithVals, vertexNum)

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

    object outEdges {
      def unapply(d: Def[_]): Option[(Rep[IncidenceGraph[V, E]], Coll[Int], Rep[Edge[V, E] => Boolean]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(vs, predicate, _*), _) if receiver.elem.isInstanceOf[IncidenceGraphElem[_, _]] && method.getName == "outEdges" =>
          Some((receiver, vs, predicate)).asInstanceOf[Option[(Rep[IncidenceGraph[V, E]], Coll[Int], Rep[Edge[V, E] => Boolean]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[IncidenceGraph[V, E]], Coll[Int], Rep[Edge[V, E] => Boolean]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

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
    object fromAdjacencyMatrix {
      def unapply(d: Def[_]): Option[(Coll[V], Coll[E], Rep[Int]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(vertexValues, incMatrixWithVals, vertexNum, _*), _) if receiver.elem == IncidenceGraphCompanionElem && method.getName == "fromAdjacencyMatrix" =>
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
  def unmkIncidenceGraph[V, E](p: Rep[Graph[V, E]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: IncidenceGraphElem[V, E] @unchecked =>
      Some((p.asRep[IncidenceGraph[V, E]].vertexValues, p.asRep[IncidenceGraph[V, E]].incMatrixWithVals, p.asRep[IncidenceGraph[V, E]].vertexNum))
    case _ =>
      None
  }

  object GraphMethods {
    object makeEdgeFrom {
      def unapply(d: Def[_]): Option[(Rep[Graph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(v, iE, _*), _) if receiver.elem.isInstanceOf[GraphElem[_, _, _]] && method.getName == "makeEdgeFrom" =>
          Some((receiver, v, iE)).asInstanceOf[Option[(Rep[Graph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Graph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object makeEdgeFromTo {
      def unapply(d: Def[_]): Option[(Rep[Graph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(v1, v2, _*), _) if receiver.elem.isInstanceOf[GraphElem[_, _, _]] && method.getName == "makeEdgeFromTo" =>
          Some((receiver, v1, v2)).asInstanceOf[Option[(Rep[Graph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Graph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object thisGraph {
      def unapply(d: Def[_]): Option[Rep[Graph[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[GraphElem[_, _, _]] && method.getName == "thisGraph" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[GraphElem[_, _, _]] && method.getName == "vertexValues" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[GraphElem[_, _, _]] && method.getName == "edgeValues" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[GraphElem[_, _, _]] && method.getName == "links" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[GraphElem[_, _, _]] && method.getName == "incMatrix" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[GraphElem[_, _, _]] && method.getName == "incMatrixWithVals" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[GraphElem[_, _, _]] && method.getName == "discardValues" =>
          Some(receiver).asInstanceOf[Option[Rep[Graph[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Graph[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object addEdges {
      def unapply(d: Def[_]): Option[(Rep[Graph[V, E]], Rep[EdgeList]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(el, _*), _) if receiver.elem.isInstanceOf[GraphElem[_, _, _]] && method.getName == "addEdges" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[GraphElem[_, _, _]] && method.getName == "inverted" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[GraphElem[_, _, _]] && method.getName == "complement" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[GraphElem[_, _, _]] && method.getName == "vertexNum" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[GraphElem[_, _, _]] && method.getName == "edgeNum" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[GraphElem[_, _, _]] && method.getName == "inDegrees" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[GraphElem[_, _, _]] && method.getName == "outDegrees" =>
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
        case MethodCall(receiver, method, Seq(vId, _*), _) if receiver.elem.isInstanceOf[GraphElem[_, _, _]] && method.getName == "getNode" =>
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
        case MethodCall(receiver, method, Seq(vIds, _*), _) if receiver.elem.isInstanceOf[GraphElem[_, _, _]] && method.getName == "getNodes" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[GraphElem[_, _, _]] && method.getName == "nodes" =>
          Some(receiver).asInstanceOf[Option[Rep[Graph[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Graph[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object simpleNodes {
      def unapply(d: Def[_]): Option[Rep[Graph[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[GraphElem[_, _, _]] && method.getName == "simpleNodes" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[GraphElem[_, _, _]] && method.getName == "edges" =>
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
        case MethodCall(receiver, method, Seq(v, _*), _) if receiver.elem.isInstanceOf[GraphElem[_, _, _]] && method.getName == "inNeighbors" =>
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
        case MethodCall(receiver, method, Seq(v, _*), _) if receiver.elem.isInstanceOf[GraphElem[_, _, _]] && method.getName == "outNeighborsOf" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "1" } =>
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
        case MethodCall(receiver, method, Seq(vs, _*), _) if receiver.elem.isInstanceOf[GraphElem[_, _, _]] && method.getName == "outNeighborsOf" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "2" } =>
          Some((receiver, vs)).asInstanceOf[Option[(Rep[Graph[V, E]], Coll[Int]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Graph[V, E]], Coll[Int]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object outEdgesOf_1 {
      def unapply(d: Def[_]): Option[(Rep[Graph[V, E]], Coll[Int], Rep[BitSet]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(vs, excluding, _*), _) if receiver.elem.isInstanceOf[GraphElem[_, _, _]] && method.getName == "outEdgesOf" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "1" } =>
          Some((receiver, vs, excluding)).asInstanceOf[Option[(Rep[Graph[V, E]], Coll[Int], Rep[BitSet]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Graph[V, E]], Coll[Int], Rep[BitSet]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object outEdgesOf_2 {
      def unapply(d: Def[_]): Option[(Rep[Graph[V, E]], Rep[Front]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(fr, _*), _) if receiver.elem.isInstanceOf[GraphElem[_, _, _]] && method.getName == "outEdgesOf" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "2" } =>
          Some((receiver, fr)).asInstanceOf[Option[(Rep[Graph[V, E]], Rep[Front]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Graph[V, E]], Rep[Front]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object outEdges {
      def unapply(d: Def[_]): Option[(Rep[Graph[V, E]], Coll[Int], Rep[Edge[V, E] => Boolean]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(vs, predicate, _*), _) if receiver.elem.isInstanceOf[GraphElem[_, _, _]] && method.getName == "outEdges" =>
          Some((receiver, vs, predicate)).asInstanceOf[Option[(Rep[Graph[V, E]], Coll[Int], Rep[Edge[V, E] => Boolean]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Graph[V, E]], Coll[Int], Rep[Edge[V, E] => Boolean]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object hasEdgeTo {
      def unapply(d: Def[_]): Option[(Rep[Graph[V, E]], Rep[Int], Rep[Int]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(fromId, toId, _*), _) if receiver.elem.isInstanceOf[GraphElem[_, _, _]] && method.getName == "hasEdgeTo" =>
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
        case MethodCall(receiver, method, Seq(v1Id, v2Id, _*), _) if receiver.elem.isInstanceOf[GraphElem[_, _, _]] && method.getName == "commonNbrs" =>
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
        case MethodCall(receiver, method, Seq(v1Id, v2Id, _*), _) if receiver.elem.isInstanceOf[GraphElem[_, _, _]] && method.getName == "commonNbrsNum" =>
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
  }
}

object Graphs_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAANVXS2wbRRieteM4tkNIy6NQCRKCeYMdJaIVilAUHKdK5ThRtkmQqUDj3bGz6e7ssjuObA5F4tADcEIIiQOHSiAuvSAOHCpVSICEEEICIYTEmVMpqnqgJxD/zD68m+w6BNoDexjtvP7H93//PzOXrqGMY6NHHQXrmJYMwnBJFv8LDivKVco01lsx1Y5OFknr+6c+N/NvvF1PofEGGt7GzqKjN1DO/al2reBfZmoN5TBViMNM22HooZrQUFZMXScK00xa1gyjw3BTJ+Wa5rC5GhpqmmrvVXQeSTU0rphUsQkjckXHjkMcb3yEcIu0oJ8T/d6q1ddBy9yLcsiLMzbWGJgPOsbd9evEknvUpD2DoTHPtFWLmwVrCqRrgQ/LhqULNekaymqGZdrM15oFDdum6neHKIYBdLS2g3dxGbS2yzKzNdrmwiysnMNtUoclfPkQ+OAQvXWmZxFPeMFhakRf10IIQVRmhGGlPmalALMSx6woE1vDuvYa5pNrttntIfeT0gh1LRDx9AEifAmkStXim2eVl27KBSPFN3e5KVlh0DAImkhgiAgPYPv1+jvOjVMXT6ZQvoHymrPQdJiNFRamgQdXAVNqMmFzgCC22xDBqaQICi0LsGYPTXKKaViYgiQPy1EIlK4pGuOL+dioF54E7LPMIv5SqWtJgb+TCf4KLlWwrq9dvf+ZR36rvphCqaiKHIiUIRlsXyhDmVM2trY92by9kyFpsw8w71ZFlze5br/NDjAlAOWxq7+rX02js6kASk/zP4seiMg4P/9U+PGJ+RQaaQiyL+m43QA0napOjFW7YlLWQCPmLrHdmewu1vlfbDSzKmnhjs48jMPgpAEchiYTM9UiHLk5kQGSD0DBJXHdpKS4tFb8Q/7m3UucozYadWfc1P1LO/nnL2MtJujL0CgYy0h3E+sd4vgwD1UgE2ID4WIuBo8HynkzwVCeqG0SFZSpx0iqHigpo2v0XLwQGz2cxDmLrNmaAXVvlzz7xeWN61fqGUG7ox7QwjS35Hg49zHnUEjTDKWXKYtjV96FUDYNcmTqhvbyxbeY4JHUjVa01eYOVJA5se/BAZTyi+0nFy7cc/3DV+4SBWGkqTEDW8XpQ5QDP3tvY7qjaPzGKt6hI9JiZs/kgrqDFUKVnkjmhGzl7bFgzo06BOBYdHMl7MZEaGdI5XFpD3VSZDNgMc/KA1m8X0B1kID9BSjW6YmArg8k0xUwvne9drd+bf5KCmVOo0wLaoVTQ5mm2aGqHzw44yFB2Qv+mBQNHgQL29gIgiW+SdTHLFpEN2IX7EvJghT1+7/U5n0x25vut7AGHdGosoIhx7tbGtsGiQPlHVyJcq5p9Y7hb0rDrcgVw5vZeHdPiPa5Q2XOMlU0FUhE/k3mRDf/fzJnv9MToW3xZD0Um0MGD8cGJQ1V+FZxPSFKAyhR4GV3CRua3puJs+JwNBhLir4VlndbEObt+33pJ6DylRIq3yJRdGwTlV/liQFPDfdUm31vfuv0fVsb4lwdVcUidya4CsU/jFawNSeu8Y8PuMbDomLVsOCZBj+zXz7/w+vffvyRuAP1YWRo2A09Q3d4trdFP3BpKsEl2TtAgUvnb35Qf/K7T38VV488P4rhQkaDR1H4yhENeM7VDW+cELiQSvxwDjHoM95c/hsJcX9Fkg4AAA=="
}
}

