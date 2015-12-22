package scalan.graphs

import scalan._
import scalan.collections.{CollectionsDslExp, CollectionsDslSeq, CollectionsDsl}
import scalan.{ScalanSeq, ScalanExp, Scalan}
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
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("V" -> Left(eV), "E" -> Left(eE))
    }
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
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("V" -> Left(eV), "E" -> Left(eE))
    }

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
    def apply[V, E](p: Rep[AdjacencyGraphData[V, E]])(implicit eV: Elem[V], eE: Elem[E]): Rep[AdjacencyGraph[V, E]] =
      isoAdjacencyGraph(eV, eE).to(p)
    def apply[V, E](vertexValues: Coll[V], edgeValues: NColl[E], links: NColl[Int])(implicit eV: Elem[V], eE: Elem[E]): Rep[AdjacencyGraph[V, E]] =
      mkAdjacencyGraph(vertexValues, edgeValues, links)
  }
  object AdjacencyGraphMatcher {
    def unapply[V, E](p: Rep[Graph[V, E]]) = unmkAdjacencyGraph(p)
  }
  lazy val AdjacencyGraph: Rep[AdjacencyGraphCompanionAbs] = new AdjacencyGraphCompanionAbs
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
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("V" -> Left(eV), "E" -> Left(eE))
    }

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
    def apply[V, E](p: Rep[IncidenceGraphData[V, E]])(implicit eV: Elem[V], eE: Elem[E]): Rep[IncidenceGraph[V, E]] =
      isoIncidenceGraph(eV, eE).to(p)
    def apply[V, E](vertexValues: Coll[V], incMatrixWithVals: Coll[E], vertexNum: Rep[Int])(implicit eV: Elem[V], eE: Elem[E]): Rep[IncidenceGraph[V, E]] =
      mkIncidenceGraph(vertexValues, incMatrixWithVals, vertexNum)
  }
  object IncidenceGraphMatcher {
    def unapply[V, E](p: Rep[Graph[V, E]]) = unmkIncidenceGraph(p)
  }
  lazy val IncidenceGraph: Rep[IncidenceGraphCompanionAbs] = new IncidenceGraphCompanionAbs
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

// Seq -----------------------------------
trait GraphsSeq extends scalan.ScalanDslSeq with GraphsDsl {
  self: GraphsDslSeq =>
  lazy val Graph: Rep[GraphCompanionAbs] = new GraphCompanionAbs {
  }

  case class SeqAdjacencyGraph[V, E]
      (override val vertexValues: Coll[V], override val edgeValues: NColl[E], override val links: NColl[Int])(implicit eV: Elem[V], eE: Elem[E])
    extends AbsAdjacencyGraph[V, E](vertexValues, edgeValues, links) {
  }

  def mkAdjacencyGraph[V, E]
    (vertexValues: Coll[V], edgeValues: NColl[E], links: NColl[Int])(implicit eV: Elem[V], eE: Elem[E]): Rep[AdjacencyGraph[V, E]] =
    new SeqAdjacencyGraph[V, E](vertexValues, edgeValues, links)
  def unmkAdjacencyGraph[V, E](p: Rep[Graph[V, E]]) = p match {
    case p: AdjacencyGraph[V, E] @unchecked =>
      Some((p.vertexValues, p.edgeValues, p.links))
    case _ => None
  }

  case class SeqIncidenceGraph[V, E]
      (override val vertexValues: Coll[V], override val incMatrixWithVals: Coll[E], override val vertexNum: Rep[Int])(implicit eV: Elem[V], eE: Elem[E])
    extends AbsIncidenceGraph[V, E](vertexValues, incMatrixWithVals, vertexNum) {
  }

  def mkIncidenceGraph[V, E]
    (vertexValues: Coll[V], incMatrixWithVals: Coll[E], vertexNum: Rep[Int])(implicit eV: Elem[V], eE: Elem[E]): Rep[IncidenceGraph[V, E]] =
    new SeqIncidenceGraph[V, E](vertexValues, incMatrixWithVals, vertexNum)
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
  val dump = "H4sIAAAAAAAAANVXS2wbRRieteM4tkP64FGIBAnBBYHADkFQoRyq4DpVkOtE2TZFpgKN12Nn0tnZze44sjn02APcEBcOSFTigtQL4oSQKiSEhHrghBASZ06lqOqBnkD8M/vwOt51CLQHfBjtzM78j+/7/n/W12+jjOugZ10DM8xLJhG4pKvnFVcU9SoXVPTPWa0uI2dI+3Hrm09f/nz2qxQ60kCT29g947IGynkP1Z4dPutkt4ZymBvEFZbjCvR0TXkoGxZjxBDU4mVqml2Bm4yUa9QVyzU00bRa/V10BWk1dNSwuOEQQfQKw65LXH99isiIaDjPqXl/3R744GWZRTmSxXkHUwHhg4+j3v5NYut9bvG+KdCMH9q6LcOCPVlq2pYjAhdZMLdttYLpBMewgI7XdvAeLoOLTlkXDuUdOFmwsXEZd0gdtsjtExCwS1j7fN9W83QN5V2yCwCtmTZTKz0bIQQMLKkgSgN8SiE+JYlPUScOxYy+h+XLDcfq9ZH309II9Www8eIBJgILpMpbxfcvGW/f0wtmSh7uyVCyKsNJMDSXoAZFBeD4/eaH7t2z106lUL6B8tRdabrCwYaIUu6jVcCcW0LFHAKInQ6wtZDElvKyAnv2SSJnWKaNOVjyoZwGnhg1qJCb5dq0z04C9Flhk2Cr1rO1MN/5hHyVbiqYsY1bT7x08rfqWymUGnaRA5M6CN8JjAqUOetge9u3LccjAmlbA4DltKqmcsj1BmN2TCghKM/d+r313SK6lAqh9D3/M/bARMb9+afCj8+fTqGphtL6KsOdBqDpVhkx152KxUUDTVl7xPHeZPcwk0+xbGZbpI27TPgYR8FJAzgCzSdWpU0kcsuqArQAgIIn4rrFSXF1o/iHfvOj61KjDpr23nhl+hc99ecvM22h5CvQNAQrSG8Lsy5xA5gnKlAJsUR4mKvF2dC5HOYEypNWhwwbytRjLFUPtJRhlF+ON+KgZ5I0Z5MNh5rQ4/bIq99+feHOjXpGye64D7QKzes4Ps4DzCUU2qJA6TUu4tSV9yDULZMcW7hL37n2gVA60nrDDW29uQMdZFmde2qMpILG+sXVq4/e+ezdh1VDmGpSYWK7uHiIdhBU7wMsdzTM30zFv2BUWSzte7nS2sEG4UZfFXNCtcrxRPjOYx0IODF8uBJNYy5yMuJyVtsnnRTZClUsq/JAFY8aqI4zMNqAYpOeC+X6ZLJcAePHNmuPsNunb6RQ5k2UaUOvcGso07S6vBWQB/c5FKh4I1jThskDsrCDzZAs9ZtHA8yGm+iF2A0jJVnQhvP+L715hLP95X4fe9Axyo1zGGq8d5GKbbA41t7BnSjnhVbvmsGhNHwBeWbk8Ep8uq+p8fVDVc4aN2gLRET+TeUMH/7/VM5o0nORY/FiPZSaIwFPxpKShi58v7SewNIYSRRk213FJmX9pbgoDieDmST27ai9B4KwHD8e7PE3TnpQCvSQ3wU7au4D4qCFhOao+xcScHPl3if1F3748ld1lefl1QYfODz8jxG9wocBzHm+4S9DJFiQprzsVKB/A44V2GHCDQAA"
}
}

