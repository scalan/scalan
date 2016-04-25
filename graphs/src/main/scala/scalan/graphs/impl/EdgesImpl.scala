package scalan.graphs

import scalan.collections.CollectionsDsl
import scalan.Owner
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait EdgesAbs extends scalan.ScalanDsl with Edges {
  self: GraphsDsl =>

  // single proxy for each type family
  implicit def proxyEdge[V, E](p: Rep[Edge[V, E]]): Edge[V, E] = {
    proxyOps[Edge[V, E]](p)(scala.reflect.classTag[Edge[V, E]])
  }

  // familyElem
  class EdgeElem[V, E, To <: Edge[V, E]](implicit _eV: Elem[V], _eE: Elem[E])
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
      weakTypeTag[Edge[V, E]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[Edge[V, E]] => convertEdge(x) }
      tryConvert(element[Edge[V, E]], this, x, conv)
    }

    def convertEdge(x: Rep[Edge[V, E]]): Rep[To] = {
      x.selfType1 match {
        case _: EdgeElem[_, _, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have EdgeElem[_, _, _], but got $e", x)
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def edgeElement[V, E](implicit eV: Elem[V], eE: Elem[E]): Elem[Edge[V, E]] =
    cachedElem[EdgeElem[V, E, Edge[V, E]]](eV, eE)

  implicit case object EdgeCompanionElem extends CompanionElem[EdgeCompanionAbs] {
    lazy val tag = weakTypeTag[EdgeCompanionAbs]
    protected def getDefaultRep = Edge
  }

  abstract class EdgeCompanionAbs extends CompanionDef[EdgeCompanionAbs] with EdgeCompanion {
    def selfType = EdgeCompanionElem
    override def toString = "Edge"
  }
  def Edge: Rep[EdgeCompanionAbs]
  implicit def proxyEdgeCompanionAbs(p: Rep[EdgeCompanionAbs]): EdgeCompanionAbs =
    proxyOps[EdgeCompanionAbs](p)

  abstract class AbsAdjEdge[V, E]
      (fromId: Rep[Int], outIndex: Rep[Int], graph: Rep[Graph[V, E]])(implicit eV: Elem[V], eE: Elem[E])
    extends AdjEdge[V, E](fromId, outIndex, graph) with Def[AdjEdge[V, E]] {
    lazy val selfType = element[AdjEdge[V, E]]
  }
  // elem for concrete class
  class AdjEdgeElem[V, E](val iso: Iso[AdjEdgeData[V, E], AdjEdge[V, E]])(implicit override val eV: Elem[V], override val eE: Elem[E])
    extends EdgeElem[V, E, AdjEdge[V, E]]
    with ConcreteElem[AdjEdgeData[V, E], AdjEdge[V, E]] {
    override lazy val parent: Option[Elem[_]] = Some(edgeElement(element[V], element[E]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("V" -> Left(eV), "E" -> Left(eE))
    }

    override def convertEdge(x: Rep[Edge[V, E]]) = AdjEdge(x.fromId, x.outIndex, x.graph)
    override def getDefaultRep = AdjEdge(0, 0, element[Graph[V, E]].defaultRepValue)
    override lazy val tag = {
      implicit val tagV = eV.tag
      implicit val tagE = eE.tag
      weakTypeTag[AdjEdge[V, E]]
    }
  }

  // state representation type
  type AdjEdgeData[V, E] = (Int, (Int, Graph[V, E]))

  // 3) Iso for concrete class
  class AdjEdgeIso[V, E](implicit eV: Elem[V], eE: Elem[E])
    extends EntityIso[AdjEdgeData[V, E], AdjEdge[V, E]] with Def[AdjEdgeIso[V, E]] {
    override def from(p: Rep[AdjEdge[V, E]]) =
      (p.fromId, p.outIndex, p.graph)
    override def to(p: Rep[(Int, (Int, Graph[V, E]))]) = {
      val Pair(fromId, Pair(outIndex, graph)) = p
      AdjEdge(fromId, outIndex, graph)
    }
    lazy val eFrom = pairElement(element[Int], pairElement(element[Int], element[Graph[V, E]]))
    lazy val eTo = new AdjEdgeElem[V, E](self)
    lazy val selfType = new AdjEdgeIsoElem[V, E](eV, eE)
    def productArity = 2
    def productElement(n: Int) = (eV, eE).productElement(n)
  }
  case class AdjEdgeIsoElem[V, E](eV: Elem[V], eE: Elem[E]) extends Elem[AdjEdgeIso[V, E]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new AdjEdgeIso[V, E]()(eV, eE))
    lazy val tag = {
      implicit val tagV = eV.tag
      implicit val tagE = eE.tag
      weakTypeTag[AdjEdgeIso[V, E]]
    }
  }
  // 4) constructor and deconstructor
  class AdjEdgeCompanionAbs extends CompanionDef[AdjEdgeCompanionAbs] with AdjEdgeCompanion {
    def selfType = AdjEdgeCompanionElem
    override def toString = "AdjEdge"
    @scalan.OverloadId("fromData")
    def apply[V, E](p: Rep[AdjEdgeData[V, E]])(implicit eV: Elem[V], eE: Elem[E]): Rep[AdjEdge[V, E]] =
      isoAdjEdge(eV, eE).to(p)
    @scalan.OverloadId("fromFields")
    def apply[V, E](fromId: Rep[Int], outIndex: Rep[Int], graph: Rep[Graph[V, E]])(implicit eV: Elem[V], eE: Elem[E]): Rep[AdjEdge[V, E]] =
      mkAdjEdge(fromId, outIndex, graph)

    def unapply[V, E](p: Rep[Edge[V, E]]) = unmkAdjEdge(p)
  }
  lazy val AdjEdgeRep: Rep[AdjEdgeCompanionAbs] = new AdjEdgeCompanionAbs
  lazy val AdjEdge: AdjEdgeCompanionAbs = proxyAdjEdgeCompanion(AdjEdgeRep)
  implicit def proxyAdjEdgeCompanion(p: Rep[AdjEdgeCompanionAbs]): AdjEdgeCompanionAbs = {
    proxyOps[AdjEdgeCompanionAbs](p)
  }

  implicit case object AdjEdgeCompanionElem extends CompanionElem[AdjEdgeCompanionAbs] {
    lazy val tag = weakTypeTag[AdjEdgeCompanionAbs]
    protected def getDefaultRep = AdjEdge
  }

  implicit def proxyAdjEdge[V, E](p: Rep[AdjEdge[V, E]]): AdjEdge[V, E] =
    proxyOps[AdjEdge[V, E]](p)

  implicit class ExtendedAdjEdge[V, E](p: Rep[AdjEdge[V, E]])(implicit eV: Elem[V], eE: Elem[E]) {
    def toData: Rep[AdjEdgeData[V, E]] = isoAdjEdge(eV, eE).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoAdjEdge[V, E](implicit eV: Elem[V], eE: Elem[E]): Iso[AdjEdgeData[V, E], AdjEdge[V, E]] =
    reifyObject(new AdjEdgeIso[V, E]()(eV, eE))

  // 6) smart constructor and deconstructor
  def mkAdjEdge[V, E](fromId: Rep[Int], outIndex: Rep[Int], graph: Rep[Graph[V, E]])(implicit eV: Elem[V], eE: Elem[E]): Rep[AdjEdge[V, E]]
  def unmkAdjEdge[V, E](p: Rep[Edge[V, E]]): Option[(Rep[Int], Rep[Int], Rep[Graph[V, E]])]

  abstract class AbsIncEdge[V, E]
      (fromId: Rep[Int], toId: Rep[Int], graph: Rep[Graph[V, E]])(implicit eV: Elem[V], eE: Elem[E])
    extends IncEdge[V, E](fromId, toId, graph) with Def[IncEdge[V, E]] {
    lazy val selfType = element[IncEdge[V, E]]
  }
  // elem for concrete class
  class IncEdgeElem[V, E](val iso: Iso[IncEdgeData[V, E], IncEdge[V, E]])(implicit override val eV: Elem[V], override val eE: Elem[E])
    extends EdgeElem[V, E, IncEdge[V, E]]
    with ConcreteElem[IncEdgeData[V, E], IncEdge[V, E]] {
    override lazy val parent: Option[Elem[_]] = Some(edgeElement(element[V], element[E]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("V" -> Left(eV), "E" -> Left(eE))
    }

    override def convertEdge(x: Rep[Edge[V, E]]) = IncEdge(x.fromId, x.toId, x.graph)
    override def getDefaultRep = IncEdge(0, 0, element[Graph[V, E]].defaultRepValue)
    override lazy val tag = {
      implicit val tagV = eV.tag
      implicit val tagE = eE.tag
      weakTypeTag[IncEdge[V, E]]
    }
  }

  // state representation type
  type IncEdgeData[V, E] = (Int, (Int, Graph[V, E]))

  // 3) Iso for concrete class
  class IncEdgeIso[V, E](implicit eV: Elem[V], eE: Elem[E])
    extends EntityIso[IncEdgeData[V, E], IncEdge[V, E]] with Def[IncEdgeIso[V, E]] {
    override def from(p: Rep[IncEdge[V, E]]) =
      (p.fromId, p.toId, p.graph)
    override def to(p: Rep[(Int, (Int, Graph[V, E]))]) = {
      val Pair(fromId, Pair(toId, graph)) = p
      IncEdge(fromId, toId, graph)
    }
    lazy val eFrom = pairElement(element[Int], pairElement(element[Int], element[Graph[V, E]]))
    lazy val eTo = new IncEdgeElem[V, E](self)
    lazy val selfType = new IncEdgeIsoElem[V, E](eV, eE)
    def productArity = 2
    def productElement(n: Int) = (eV, eE).productElement(n)
  }
  case class IncEdgeIsoElem[V, E](eV: Elem[V], eE: Elem[E]) extends Elem[IncEdgeIso[V, E]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new IncEdgeIso[V, E]()(eV, eE))
    lazy val tag = {
      implicit val tagV = eV.tag
      implicit val tagE = eE.tag
      weakTypeTag[IncEdgeIso[V, E]]
    }
  }
  // 4) constructor and deconstructor
  class IncEdgeCompanionAbs extends CompanionDef[IncEdgeCompanionAbs] with IncEdgeCompanion {
    def selfType = IncEdgeCompanionElem
    override def toString = "IncEdge"
    @scalan.OverloadId("fromData")
    def apply[V, E](p: Rep[IncEdgeData[V, E]])(implicit eV: Elem[V], eE: Elem[E]): Rep[IncEdge[V, E]] =
      isoIncEdge(eV, eE).to(p)
    @scalan.OverloadId("fromFields")
    def apply[V, E](fromId: Rep[Int], toId: Rep[Int], graph: Rep[Graph[V, E]])(implicit eV: Elem[V], eE: Elem[E]): Rep[IncEdge[V, E]] =
      mkIncEdge(fromId, toId, graph)

    def unapply[V, E](p: Rep[Edge[V, E]]) = unmkIncEdge(p)
  }
  lazy val IncEdgeRep: Rep[IncEdgeCompanionAbs] = new IncEdgeCompanionAbs
  lazy val IncEdge: IncEdgeCompanionAbs = proxyIncEdgeCompanion(IncEdgeRep)
  implicit def proxyIncEdgeCompanion(p: Rep[IncEdgeCompanionAbs]): IncEdgeCompanionAbs = {
    proxyOps[IncEdgeCompanionAbs](p)
  }

  implicit case object IncEdgeCompanionElem extends CompanionElem[IncEdgeCompanionAbs] {
    lazy val tag = weakTypeTag[IncEdgeCompanionAbs]
    protected def getDefaultRep = IncEdge
  }

  implicit def proxyIncEdge[V, E](p: Rep[IncEdge[V, E]]): IncEdge[V, E] =
    proxyOps[IncEdge[V, E]](p)

  implicit class ExtendedIncEdge[V, E](p: Rep[IncEdge[V, E]])(implicit eV: Elem[V], eE: Elem[E]) {
    def toData: Rep[IncEdgeData[V, E]] = isoIncEdge(eV, eE).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoIncEdge[V, E](implicit eV: Elem[V], eE: Elem[E]): Iso[IncEdgeData[V, E], IncEdge[V, E]] =
    reifyObject(new IncEdgeIso[V, E]()(eV, eE))

  // 6) smart constructor and deconstructor
  def mkIncEdge[V, E](fromId: Rep[Int], toId: Rep[Int], graph: Rep[Graph[V, E]])(implicit eV: Elem[V], eE: Elem[E]): Rep[IncEdge[V, E]]
  def unmkIncEdge[V, E](p: Rep[Edge[V, E]]): Option[(Rep[Int], Rep[Int], Rep[Graph[V, E]])]

  registerModule(Edges_Module)
}

// Std -----------------------------------
trait EdgesStd extends scalan.ScalanDslStd with EdgesDsl {
  self: GraphsDslStd =>
  lazy val Edge: Rep[EdgeCompanionAbs] = new EdgeCompanionAbs {
  }

  case class StdAdjEdge[V, E]
      (override val fromId: Rep[Int], override val outIndex: Rep[Int], override val graph: Rep[Graph[V, E]])(implicit eV: Elem[V], eE: Elem[E])
    extends AbsAdjEdge[V, E](fromId, outIndex, graph) {
  }

  def mkAdjEdge[V, E]
    (fromId: Rep[Int], outIndex: Rep[Int], graph: Rep[Graph[V, E]])(implicit eV: Elem[V], eE: Elem[E]): Rep[AdjEdge[V, E]] =
    new StdAdjEdge[V, E](fromId, outIndex, graph)
  def unmkAdjEdge[V, E](p: Rep[Edge[V, E]]) = p match {
    case p: AdjEdge[V, E] @unchecked =>
      Some((p.fromId, p.outIndex, p.graph))
    case _ => None
  }

  case class StdIncEdge[V, E]
      (override val fromId: Rep[Int], override val toId: Rep[Int], override val graph: Rep[Graph[V, E]])(implicit eV: Elem[V], eE: Elem[E])
    extends AbsIncEdge[V, E](fromId, toId, graph) {
  }

  def mkIncEdge[V, E]
    (fromId: Rep[Int], toId: Rep[Int], graph: Rep[Graph[V, E]])(implicit eV: Elem[V], eE: Elem[E]): Rep[IncEdge[V, E]] =
    new StdIncEdge[V, E](fromId, toId, graph)
  def unmkIncEdge[V, E](p: Rep[Edge[V, E]]) = p match {
    case p: IncEdge[V, E] @unchecked =>
      Some((p.fromId, p.toId, p.graph))
    case _ => None
  }
}

// Exp -----------------------------------
trait EdgesExp extends scalan.ScalanDslExp with EdgesDsl {
  self: GraphsDslExp =>
  lazy val Edge: Rep[EdgeCompanionAbs] = new EdgeCompanionAbs {
  }

  case class ExpAdjEdge[V, E]
      (override val fromId: Rep[Int], override val outIndex: Rep[Int], override val graph: Rep[Graph[V, E]])(implicit eV: Elem[V], eE: Elem[E])
    extends AbsAdjEdge[V, E](fromId, outIndex, graph)

  object AdjEdgeMethods {
    object indexOfTarget {
      def unapply(d: Def[_]): Option[Rep[AdjEdge[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AdjEdgeElem[_, _]] && method.getName == "indexOfTarget" =>
          Some(receiver).asInstanceOf[Option[Rep[AdjEdge[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[AdjEdge[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object toId {
      def unapply(d: Def[_]): Option[Rep[AdjEdge[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AdjEdgeElem[_, _]] && method.getName == "toId" =>
          Some(receiver).asInstanceOf[Option[Rep[AdjEdge[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[AdjEdge[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromNode {
      def unapply(d: Def[_]): Option[Rep[AdjEdge[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AdjEdgeElem[_, _]] && method.getName == "fromNode" =>
          Some(receiver).asInstanceOf[Option[Rep[AdjEdge[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[AdjEdge[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object toNode {
      def unapply(d: Def[_]): Option[Rep[AdjEdge[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AdjEdgeElem[_, _]] && method.getName == "toNode" =>
          Some(receiver).asInstanceOf[Option[Rep[AdjEdge[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[AdjEdge[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object value {
      def unapply(d: Def[_]): Option[Rep[AdjEdge[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AdjEdgeElem[_, _]] && method.getName == "value" =>
          Some(receiver).asInstanceOf[Option[Rep[AdjEdge[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[AdjEdge[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object AdjEdgeCompanionMethods {
  }

  def mkAdjEdge[V, E]
    (fromId: Rep[Int], outIndex: Rep[Int], graph: Rep[Graph[V, E]])(implicit eV: Elem[V], eE: Elem[E]): Rep[AdjEdge[V, E]] =
    new ExpAdjEdge[V, E](fromId, outIndex, graph)
  def unmkAdjEdge[V, E](p: Rep[Edge[V, E]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: AdjEdgeElem[V, E] @unchecked =>
      Some((p.asRep[AdjEdge[V, E]].fromId, p.asRep[AdjEdge[V, E]].outIndex, p.asRep[AdjEdge[V, E]].graph))
    case _ =>
      None
  }

  case class ExpIncEdge[V, E]
      (override val fromId: Rep[Int], override val toId: Rep[Int], override val graph: Rep[Graph[V, E]])(implicit eV: Elem[V], eE: Elem[E])
    extends AbsIncEdge[V, E](fromId, toId, graph)

  object IncEdgeMethods {
    object indexOfTarget {
      def unapply(d: Def[_]): Option[Rep[IncEdge[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[IncEdgeElem[_, _]] && method.getName == "indexOfTarget" =>
          Some(receiver).asInstanceOf[Option[Rep[IncEdge[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[IncEdge[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object outIndex {
      def unapply(d: Def[_]): Option[Rep[IncEdge[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[IncEdgeElem[_, _]] && method.getName == "outIndex" =>
          Some(receiver).asInstanceOf[Option[Rep[IncEdge[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[IncEdge[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromNode {
      def unapply(d: Def[_]): Option[Rep[IncEdge[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[IncEdgeElem[_, _]] && method.getName == "fromNode" =>
          Some(receiver).asInstanceOf[Option[Rep[IncEdge[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[IncEdge[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object toNode {
      def unapply(d: Def[_]): Option[Rep[IncEdge[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[IncEdgeElem[_, _]] && method.getName == "toNode" =>
          Some(receiver).asInstanceOf[Option[Rep[IncEdge[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[IncEdge[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object value {
      def unapply(d: Def[_]): Option[Rep[IncEdge[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[IncEdgeElem[_, _]] && method.getName == "value" =>
          Some(receiver).asInstanceOf[Option[Rep[IncEdge[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[IncEdge[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object IncEdgeCompanionMethods {
  }

  def mkIncEdge[V, E]
    (fromId: Rep[Int], toId: Rep[Int], graph: Rep[Graph[V, E]])(implicit eV: Elem[V], eE: Elem[E]): Rep[IncEdge[V, E]] =
    new ExpIncEdge[V, E](fromId, toId, graph)
  def unmkIncEdge[V, E](p: Rep[Edge[V, E]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: IncEdgeElem[V, E] @unchecked =>
      Some((p.asRep[IncEdge[V, E]].fromId, p.asRep[IncEdge[V, E]].toId, p.asRep[IncEdge[V, E]].graph))
    case _ =>
      None
  }

  object EdgeMethods {
    object graph {
      def unapply(d: Def[_]): Option[Rep[Edge[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[EdgeElem[_, _, _]] && method.getName == "graph" =>
          Some(receiver).asInstanceOf[Option[Rep[Edge[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Edge[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object outIndex {
      def unapply(d: Def[_]): Option[Rep[Edge[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[EdgeElem[_, _, _]] && method.getName == "outIndex" =>
          Some(receiver).asInstanceOf[Option[Rep[Edge[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Edge[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromId {
      def unapply(d: Def[_]): Option[Rep[Edge[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[EdgeElem[_, _, _]] && method.getName == "fromId" =>
          Some(receiver).asInstanceOf[Option[Rep[Edge[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Edge[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object toId {
      def unapply(d: Def[_]): Option[Rep[Edge[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[EdgeElem[_, _, _]] && method.getName == "toId" =>
          Some(receiver).asInstanceOf[Option[Rep[Edge[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Edge[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromNode {
      def unapply(d: Def[_]): Option[Rep[Edge[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[EdgeElem[_, _, _]] && method.getName == "fromNode" =>
          Some(receiver).asInstanceOf[Option[Rep[Edge[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Edge[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object toNode {
      def unapply(d: Def[_]): Option[Rep[Edge[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[EdgeElem[_, _, _]] && method.getName == "toNode" =>
          Some(receiver).asInstanceOf[Option[Rep[Edge[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Edge[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object value {
      def unapply(d: Def[_]): Option[Rep[Edge[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[EdgeElem[_, _, _]] && method.getName == "value" =>
          Some(receiver).asInstanceOf[Option[Rep[Edge[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Edge[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object EdgeCompanionMethods {
    object MaxDoubleEdge {
      def unapply(d: Def[_]): Option[Unit] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem == EdgeCompanionElem && method.getName == "MaxDoubleEdge" =>
          Some(()).asInstanceOf[Option[Unit]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Unit] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}

object Edges_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAANVXTWwbRRSe9U9sxyEN5U8UQdLgAkVgR0WoQgZVwXUqV24SZUtUmQo03h07G2Znl91xtOZQbjnADXFFqAKEkCokxBGpF0BCCHFACCHBlVMpqnJoTyDezP54nXhdUsiBPYx2Zt689+b73nszc+U6yroOeszVMMWsbBKOy6r8X3R5Sa0zbvD+OUvvUXKadIo3q0d/3a41UuhQC01sYPe0S1uo4P/UPTv6V7neRAXMNOJyy3E5OtqUFiqaRSnRuGGximGaPY7blFSahsurTZRpW3r/dXQJKU00o1lMcwgnao1i1yVuMJ4nwiMj6hdkv79iD2ywithFJbaL8w42OLgPNmZ8+TViq31msb7J0XTg2oot3AKZnGHalsNDEzlQt2HpYTfDMAygw81NvIUrYKJbUbljsC6sLNpYew13yTKICPEMOOwS2jnft2U/DSIu1wGghmlTqTHt2QghoOCE9KI8AKgcAVQWAJVU4hiYGm9gMbnqWF4f+Z+SRsizQcVTt1ERaiB1ppfeuqi9fEstmimx2BOu5KRDE6BoNiEcJBcA5Ddr77g7Zy6fTKHJFpo03MW2yx2s8TjnAVxFzJjFpc8RgtjpAl3zSXRJK4sgsysmCppl2piBpgDLKSCKGprBhbAYmwroScA+x20SiiqerUT7nUvYrwycGqZ09dqDTx/7vX4hhVLDJgqgUoXId0KlHGXqepcEqkV7iCNlfYCv6NZlVzQFb9DmxngSYfL4tT/0rxfQxVSEZGD4n5EHKrLuzz8Vfzx+KoXyLRnrSxR3WwCmW6fEXHFqFuMtlLe2iOPP5LYwFX8jyczppIN7lAcQx7FJAzYczSVmpU0EcFWZAEoIQNGP4WWLkdLSaumm+u27V0SIOmjKn/HT9C/j5J+/THe4jF6OJjqOZTb0EOA05HeEx6NJ5Npk1TFMqCZb5Nkvv3jpxtXlrOT3cLCldUx7xM/tYEeD3QmjygJYajDuMyjtHYm2IppZDjD2eIPpxNvrmmiOjVub7TrY3hixp2Akeyaa32ekDeJt0gdVtUxy9/yO8crlt7mMLMUbLnEr7U0oKVW57pExQRaW2s+2t++78eGr98gKkW8b3MR2aWEf9SFM5wPMfzQM3XQtOHJkopwYnpRJnZC1on0gmvPZg/CYWdQ3xapa3OvZ2JKYhSPKLu5TZH1gGtJyJMvx4NmroD5Owd644CgXOCw1ROnzcHL6AJb3rzXvpddPXU2h7FmU7UCVcJso27Z6TA9JgpOcE4+/GI4pwyQBKdjBZkSK/ObQAKzhoL4wUqC+G4+iMoK/OyvKe7janaeJxee2GZ7h1h2tO/jKINrnZPv8QadJg2n/rzQJHI6nSXJk7it0Y55OjAQ/DaX1PwrsBFrGMF8UpXQJmwbt/3va70rg3I5pOxB0Rfv+QCYQzEoYwa2g2vkJFoDhoPmEIqgGBwzQcunWe8tPfv/5b/IKMSmOKrjCsOgVEb86DINX8JMVHgUxXwEFcXhFDhxPcoDr4ilBTDDlH6I7Zz8pPPRp+wN5jOfFa4No1I3uYaNfYOewXZVPiCfGPCFAqFQ3bXgPws8zX73ww5vfffyRvID9DTQE86NGDgAA"
}
}

trait EdgesDsl extends impl.EdgesAbs {self: GraphsDsl =>}
trait EdgesDslStd extends impl.EdgesStd {self: GraphsDslStd =>}
trait EdgesDslExp extends impl.EdgesExp {self: GraphsDslExp =>}
