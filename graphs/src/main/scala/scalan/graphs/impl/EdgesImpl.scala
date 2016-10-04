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
    lazy val typeArgs = TypeArgs("V" -> eV, "E" -> eE)
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
    override lazy val typeArgs = TypeArgs("V" -> eV, "E" -> eE)

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
    def productElement(n: Int) = n match {
      case 0 => eV
      case 1 => eE
    }
  }
  case class AdjEdgeIsoElem[V, E](eV: Elem[V], eE: Elem[E]) extends Elem[AdjEdgeIso[V, E]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new AdjEdgeIso[V, E]()(eV, eE))
    lazy val tag = {
      implicit val tagV = eV.tag
      implicit val tagE = eE.tag
      weakTypeTag[AdjEdgeIso[V, E]]
    }
    lazy val typeArgs = TypeArgs("V" -> eV, "E" -> eE)
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
    override lazy val typeArgs = TypeArgs("V" -> eV, "E" -> eE)

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
    def productElement(n: Int) = n match {
      case 0 => eV
      case 1 => eE
    }
  }
  case class IncEdgeIsoElem[V, E](eV: Elem[V], eE: Elem[E]) extends Elem[IncEdgeIso[V, E]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new IncEdgeIso[V, E]()(eV, eE))
    lazy val tag = {
      implicit val tagV = eV.tag
      implicit val tagE = eE.tag
      weakTypeTag[IncEdgeIso[V, E]]
    }
    lazy val typeArgs = TypeArgs("V" -> eV, "E" -> eE)
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
  }
}

object Edges_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAANVXX2gcRRifvT+5XC7GGJFoQRPjRUX0LrRILVEkJpeackmObpuWa4nM7c5dNu7OjrtzYc+H+lZQ30QUBJGK4ktRxDdFoVRBRHzw1WefqlL6YEFQ/Gb2z+0lt1dTzYP7MOz8+/78ft/3zczlX1HWddDDroZNTEsW4bikyv8FlxfVCuUG76zaetskS6S5+fz7f5yzXp1MofE6GtrC7pJr1lHe/6l4LPpXuV5FeUw14nLbcTl6sCo1lDXbNInGDZuWDctqc9wwSblquHy+ijINW++8hC4gpYrGNZtqDuFEXTSx6xI3GB8mwiIj6udlv7POujpoWXhRjnlxysEGB/NBx7i//iRhaofatGNxNBaYts6EWbCmQDwGPqxYzJRq0lWUMyxmOzzUmgMNW7YedjMUwwCaqG7jHVwGra2yyh2DtoQwhrUXcYuswRKxPAM+uMRsnuowEggvuFzv0ecxhBCwclgaVupiVoowKwnMiipxDGwaL2MxWXNsr4P8T0kj5DEQ8fgtRIQSSIXqxdfOa+duqgUrJTZ7wpScNGgIBE0lRIikB7D99uQb7o3jl46m0EgdjRjuQsPlDtZ4PAwCuAqYUptLmyMEsdMCBmeSGJRaFmDNrjDJa7bFMAVJAZajQJRpaAYXi8XYaEBPAvY5zki4VPGYEvk7neCvjKVFbJq1a/c9MftL5WwKpXpV5EGkCsnghEI5ylT0FglEi/ZOjpSNLr6iW5Fd0eS9bpsbYEmEySPXftO/mUPnUxGSgeJ/Rh6ImHjqnc9nSe3jFBquy1hfNnFL0iigWiKuVkfD9g5x/PHcDjbFX18qczpp4rbJA4DjyKQBGY6mE9OUEQHbvAx/JXS/4Efwmk1JcblW/F397s3LIkAdNOrP+Hn7l3H0z5/GmlzGLkdDTce2VvQQ3jQkfITGQ0nUMlJzDAvKyw558uoXp69/tZaV7E4ELm1gs038zA486nonlCpzoGmFcp8/qe9Q5IpopjjA2OYrVCfeXtNEMztob7blYLbVx6dgJHs8mt9nnHWjbcQHVbUtctfMDWPz0utcxpXi9Ra49cY2FJR5ue+BASEW1t5PL1685/oHL9wt68Nww+AWZsW5fVSHMJkPMPtRL3Rji8EZJNPkcO+kTOmEnBXtZDTnswfhMb6gb4tdi3Grp2JbYhoOKbu4T5GNrmqTWH1ZjgfPXgGVQQL2xgVHucBgKSFKn/uT0wewpJPq6tufTG2mUPYEyjahSrhVlG3YbaqHJMHRzonHnwvHlF6SgBTsYCsiRX7TqAtWb1Cf7bugshuPgtKHv9sryXu42p2nicXnlhme4fZt7Tv4yiDaY7J9+qDTZIVq/680CQyOp0lyZO4rdGOWDvUFPw2l9T8K7ARaBjBfEKV0GVuG2fn3tN+RwDmLSTsQdEX7Xlf6MShxpYQSt0Q0EztEF1d1YsFTwj+mjrz17JkT9545LQ/KUV0u8mei207/h88qZvPymv7ogGs6LCpWLAbPMPg58vUzP77y/UcfymtOF0TIcEk7wBiY7heEyKOZBI/U4ECEMLpw8921x3747Gd55RkRRytcuWj05olfdXrJzvvFBZ4wMWyBNXHYxqLnS9Fc+Rv+bTYNcQ4AAA=="
}
}

trait EdgesDsl extends impl.EdgesAbs {self: GraphsDsl =>}
trait EdgesDslStd extends impl.EdgesStd {self: GraphsDslStd =>}
trait EdgesDslExp extends impl.EdgesExp {self: GraphsDslExp =>}
