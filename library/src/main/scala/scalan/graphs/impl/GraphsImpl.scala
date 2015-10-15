package scalan.graphs

import scalan._
import scalan.collections.{CollectionsDslExp, CollectionsDslSeq, CollectionsDsl}
import scalan.{ScalanSeq, ScalanExp, Scalan}
import scalan.common.OverloadHack.Overloaded1
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait GraphsAbs extends Graphs with scalan.Scalan {
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
    lazy val entityDef: STraitOrClassDef = {
      val module = getModules("Graphs")
      module.entities.find(_.name == "Graph").get
    }
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("V" -> Left(eV), "E" -> Left(eE))
    }
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagV = eV.tag
      implicit val tagE = eE.tag
      weakTypeTag[Graph[V, E]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Reifiable[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[Graph[V, E]] => convertGraph(x) }
      tryConvert(element[Graph[V, E]], this, x, conv)
    }

    def convertGraph(x: Rep[Graph[V, E]]): Rep[To] = {
      x.selfType1 match {
        case _: GraphElem[_, _, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have GraphElem[_, _, _], but got $e")
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

  abstract class GraphCompanionAbs extends CompanionBase[GraphCompanionAbs] with GraphCompanion {
    override def toString = "Graph"
  }
  def Graph: Rep[GraphCompanionAbs]
  implicit def proxyGraphCompanion(p: Rep[GraphCompanion]): GraphCompanion =
    proxyOps[GraphCompanion](p)

  // elem for concrete class
  class AdjacencyGraphElem[V, E](val iso: Iso[AdjacencyGraphData[V, E], AdjacencyGraph[V, E]])(implicit eV: Elem[V], eE: Elem[E])
    extends GraphElem[V, E, AdjacencyGraph[V, E]]
    with ConcreteElem[AdjacencyGraphData[V, E], AdjacencyGraph[V, E]] {
    override lazy val parent: Option[Elem[_]] = Some(graphElement(element[V], element[E]))
    override lazy val entityDef = {
      val module = getModules("Graphs")
      module.concreteSClasses.find(_.name == "AdjacencyGraph").get
    }
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
    extends Iso[AdjacencyGraphData[V, E], AdjacencyGraph[V, E]]()(pairElement(implicitly[Elem[Collection[V]]], pairElement(implicitly[Elem[NestedCollection[E]]], implicitly[Elem[NestedCollection[Int]]]))) {
    override def from(p: Rep[AdjacencyGraph[V, E]]) =
      (p.vertexValues, p.edgeValues, p.links)
    override def to(p: Rep[(Collection[V], (NestedCollection[E], NestedCollection[Int]))]) = {
      val Pair(vertexValues, Pair(edgeValues, links)) = p
      AdjacencyGraph(vertexValues, edgeValues, links)
    }
    lazy val eTo = new AdjacencyGraphElem[V, E](this)
  }
  // 4) constructor and deconstructor
  abstract class AdjacencyGraphCompanionAbs extends CompanionBase[AdjacencyGraphCompanionAbs] with AdjacencyGraphCompanion {
    override def toString = "AdjacencyGraph"
    def apply[V, E](p: Rep[AdjacencyGraphData[V, E]])(implicit eV: Elem[V], eE: Elem[E]): Rep[AdjacencyGraph[V, E]] =
      isoAdjacencyGraph(eV, eE).to(p)
    def apply[V, E](vertexValues: Coll[V], edgeValues: NColl[E], links: NColl[Int])(implicit eV: Elem[V], eE: Elem[E]): Rep[AdjacencyGraph[V, E]] =
      mkAdjacencyGraph(vertexValues, edgeValues, links)
  }
  object AdjacencyGraphMatcher {
    def unapply[V, E](p: Rep[Graph[V, E]]) = unmkAdjacencyGraph(p)
  }
  def AdjacencyGraph: Rep[AdjacencyGraphCompanionAbs]
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
    cachedIso[AdjacencyGraphIso[V, E]](eV, eE)

  // 6) smart constructor and deconstructor
  def mkAdjacencyGraph[V, E](vertexValues: Coll[V], edgeValues: NColl[E], links: NColl[Int])(implicit eV: Elem[V], eE: Elem[E]): Rep[AdjacencyGraph[V, E]]
  def unmkAdjacencyGraph[V, E](p: Rep[Graph[V, E]]): Option[(Rep[Collection[V]], Rep[NestedCollection[E]], Rep[NestedCollection[Int]])]

  // elem for concrete class
  class IncidenceGraphElem[V, E](val iso: Iso[IncidenceGraphData[V, E], IncidenceGraph[V, E]])(implicit eV: Elem[V], eE: Elem[E])
    extends GraphElem[V, E, IncidenceGraph[V, E]]
    with ConcreteElem[IncidenceGraphData[V, E], IncidenceGraph[V, E]] {
    override lazy val parent: Option[Elem[_]] = Some(graphElement(element[V], element[E]))
    override lazy val entityDef = {
      val module = getModules("Graphs")
      module.concreteSClasses.find(_.name == "IncidenceGraph").get
    }
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
    extends Iso[IncidenceGraphData[V, E], IncidenceGraph[V, E]]()(pairElement(implicitly[Elem[Collection[V]]], pairElement(implicitly[Elem[Collection[E]]], implicitly[Elem[Int]]))) {
    override def from(p: Rep[IncidenceGraph[V, E]]) =
      (p.vertexValues, p.incMatrixWithVals, p.vertexNum)
    override def to(p: Rep[(Collection[V], (Collection[E], Int))]) = {
      val Pair(vertexValues, Pair(incMatrixWithVals, vertexNum)) = p
      IncidenceGraph(vertexValues, incMatrixWithVals, vertexNum)
    }
    lazy val eTo = new IncidenceGraphElem[V, E](this)
  }
  // 4) constructor and deconstructor
  abstract class IncidenceGraphCompanionAbs extends CompanionBase[IncidenceGraphCompanionAbs] with IncidenceGraphCompanion {
    override def toString = "IncidenceGraph"
    def apply[V, E](p: Rep[IncidenceGraphData[V, E]])(implicit eV: Elem[V], eE: Elem[E]): Rep[IncidenceGraph[V, E]] =
      isoIncidenceGraph(eV, eE).to(p)
    def apply[V, E](vertexValues: Coll[V], incMatrixWithVals: Coll[E], vertexNum: Rep[Int])(implicit eV: Elem[V], eE: Elem[E]): Rep[IncidenceGraph[V, E]] =
      mkIncidenceGraph(vertexValues, incMatrixWithVals, vertexNum)
  }
  object IncidenceGraphMatcher {
    def unapply[V, E](p: Rep[Graph[V, E]]) = unmkIncidenceGraph(p)
  }
  def IncidenceGraph: Rep[IncidenceGraphCompanionAbs]
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
    cachedIso[IncidenceGraphIso[V, E]](eV, eE)

  // 6) smart constructor and deconstructor
  def mkIncidenceGraph[V, E](vertexValues: Coll[V], incMatrixWithVals: Coll[E], vertexNum: Rep[Int])(implicit eV: Elem[V], eE: Elem[E]): Rep[IncidenceGraph[V, E]]
  def unmkIncidenceGraph[V, E](p: Rep[Graph[V, E]]): Option[(Rep[Collection[V]], Rep[Collection[E]], Rep[Int])]

  registerModule(scalan.meta.ScalanCodegen.loadModule(Graphs_Module.dump))
}

// Seq -----------------------------------
trait GraphsSeq extends GraphsDsl with scalan.ScalanSeq {
  self: GraphsDslSeq =>
  lazy val Graph: Rep[GraphCompanionAbs] = new GraphCompanionAbs with UserTypeSeq[GraphCompanionAbs] {
    lazy val selfType = element[GraphCompanionAbs]
  }

  case class SeqAdjacencyGraph[V, E]
      (override val vertexValues: Coll[V], override val edgeValues: NColl[E], override val links: NColl[Int])
      (implicit eV: Elem[V], eE: Elem[E])
    extends AdjacencyGraph[V, E](vertexValues, edgeValues, links)
        with UserTypeSeq[AdjacencyGraph[V, E]] {
    lazy val selfType = element[AdjacencyGraph[V, E]]
  }
  lazy val AdjacencyGraph = new AdjacencyGraphCompanionAbs with UserTypeSeq[AdjacencyGraphCompanionAbs] {
    lazy val selfType = element[AdjacencyGraphCompanionAbs]
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
      (override val vertexValues: Coll[V], override val incMatrixWithVals: Coll[E], override val vertexNum: Rep[Int])
      (implicit eV: Elem[V], eE: Elem[E])
    extends IncidenceGraph[V, E](vertexValues, incMatrixWithVals, vertexNum)
        with UserTypeSeq[IncidenceGraph[V, E]] {
    lazy val selfType = element[IncidenceGraph[V, E]]
  }
  lazy val IncidenceGraph = new IncidenceGraphCompanionAbs with UserTypeSeq[IncidenceGraphCompanionAbs] {
    lazy val selfType = element[IncidenceGraphCompanionAbs]
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
trait GraphsExp extends GraphsDsl with scalan.ScalanExp {
  self: GraphsDslExp =>
  lazy val Graph: Rep[GraphCompanionAbs] = new GraphCompanionAbs with UserTypeDef[GraphCompanionAbs] {
    lazy val selfType = element[GraphCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  case class ExpAdjacencyGraph[V, E]
      (override val vertexValues: Coll[V], override val edgeValues: NColl[E], override val links: NColl[Int])
      (implicit eV: Elem[V], eE: Elem[E])
    extends AdjacencyGraph[V, E](vertexValues, edgeValues, links) with UserTypeDef[AdjacencyGraph[V, E]] {
    lazy val selfType = element[AdjacencyGraph[V, E]]
    override def mirror(t: Transformer) = ExpAdjacencyGraph[V, E](t(vertexValues), t(edgeValues), t(links))
  }

  lazy val AdjacencyGraph: Rep[AdjacencyGraphCompanionAbs] = new AdjacencyGraphCompanionAbs with UserTypeDef[AdjacencyGraphCompanionAbs] {
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
      (override val vertexValues: Coll[V], override val incMatrixWithVals: Coll[E], override val vertexNum: Rep[Int])
      (implicit eV: Elem[V], eE: Elem[E])
    extends IncidenceGraph[V, E](vertexValues, incMatrixWithVals, vertexNum) with UserTypeDef[IncidenceGraph[V, E]] {
    lazy val selfType = element[IncidenceGraph[V, E]]
    override def mirror(t: Transformer) = ExpIncidenceGraph[V, E](t(vertexValues), t(incMatrixWithVals), t(vertexNum))
  }

  lazy val IncidenceGraph: Rep[IncidenceGraphCompanionAbs] = new IncidenceGraphCompanionAbs with UserTypeDef[IncidenceGraphCompanionAbs] {
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

object Graphs_Module {
  val packageName = "scalan.graphs"
  val name = "Graphs"
  val dump = "H4sIAAAAAAAAANVXTWwbRRSe3cRxbIf0R6hVK4FDcEEgiKMgqFAOVeo6VZDjRNkQkKmQxuuxM+ns7GZnHNkceuAIN4TECaHee+OChNQLQkIcOCFA4sypLUIVtCcQb2Z/vE7WSSraAz6MdmbfvPfm+773Zn3rHsoIH70gbMwwn3OIxHOWfl4SsmRVuaSyv+q2uoxcIe0Pz3xlr/LLwkQnGmhiG4srgjVQLnio9rz42SK7NZTD3CZCur6Q6LmajlC2XcaILanLy9RxuhI3GSnXqJCLNTTedFv9XXQDGTV00na57RNJrArDQhARrk8SlRGN5zk97695gxi8rE5RTpxi08dUQvoQ42Rgv0E8q89d3nckmg5TW/NUWmCTpY7n+jIKkQV3224rmo5zDAvodG0H7+EyhOiULelT3oGdBQ/b13GH1MFEmY9DwoKw9mbf0/OxGsoLsgsArTge0ys9DyEEDCzoJOYG+MzF+MwpfEoW8Slm9AOsXq77bq+Pgp8xhlDPAxevHOEi8kCqvFX66Jr93kOr4Jhqc0+lktUnnABHxRFq0FQAjt9tfCLuX7150UT5BspTsdQU0se2TFIeolXAnLtS5xwDiP0OsDU7ii0dZQls9kkiZ7uOhzl4CqGcAp4YtalUxmptKmRnBPRZ6ZHI1Oh5RnzemRHn1bqpYMbW75x79cLd6rsmModD5MClBcL3I6cSZa762NsOfavxhETG1gBgNa3qqRpyvcGYPSSVGJQX7/ze+nYeXTNjKMPIx2MPXGTELz8VfnzpkokmG1rrywx3GoCmqDLirPkVl8sGmnT3iB+8ye5hpp5S2cy2SBt3mQwxToIzBuBINDOyKj2ikFvUFWBEABQCEdddTkrL66UH1vef3lIa9dFU8CYo03/oxb9/nW5LLV+JpiBZSXpbmHWJiGAer0AlpBIRYK4Xz8fB1VCUKE9aHTLsKFNP8VQ90lOGUX493YmPnh+lOY+s+9SBHrdHXv/m67f/uF3PaNmdDoHWqQUdJ8R5gLmCwpiXaGyFyzR15QMILdchp2bv0/dvfiy1jozecENba+5AB1nU+549RFJRY/2rMW/+ee7nL0yUA+U0qXSwV5o/Zjt4giWOhjmbroSXii6FhX0vl1o72Cbc7usCTq/QAb2A9NnhHZVk7sVBwZ9NxDlv7NOISbZiuaryO1KuBx1UD3NwsNOknrQY6/KZ0boEYM9s1J5m9y7dNlHmLZRpQ1MQNZRpul3eihiDixsqUV6O1oxhxoAh7GMnZkj/ZtAAs+FuuZlqcKD2Csbwuf9LEz7A2f66fozN5hTl9iqGYu69Q+U2eDzU39EtJxekVu860aYx+NQJ3KjhtfTjvqHHNx+pXFa4TVsgInLschne8f8pl4MnLSa2pSv0kSScSHgilYncBqFtqj7kHpfMkwQdIoGC6q3L2KGsv5AW+hi0T49i20s6eSKIqvGzgU1oOBGAJtFTYavr6HmIgo9mR3RAK7xq4L678fDz+ss/fPmbvpjz6tKCzxUe/2NIXsj7iAxiwx+ARLIgRXWN6UT/BbXHWAiQDQAA"
}
}

