package scalan.graphs

import scalan.collections.CollectionsDsl
import scalan.ScalanCommunityDsl
import scalan.Owner
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}

package impl {
// Abs -----------------------------------
trait EdgesAbs extends Edges with scalan.Scalan {
  self: GraphsDsl =>

  // single proxy for each type family
  implicit def proxyEdge[V, E](p: Rep[Edge[V, E]]): Edge[V, E] = {
    proxyOps[Edge[V, E]](p)(scala.reflect.classTag[Edge[V, E]])
  }

  // familyElem
  class EdgeElem[V, E, To <: Edge[V, E]](implicit val eV: Elem[V], val eE: Elem[E])
    extends EntityElem[To] {
    val parent: Option[Elem[_]] = None
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagV = eV.tag
      implicit val tagE = eE.tag
      weakTypeTag[Edge[V, E]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Reifiable[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[Edge[V, E]] => convertEdge(x) }
      tryConvert(element[Edge[V, E]], this, x, conv)
    }

    def convertEdge(x : Rep[Edge[V, E]]): Rep[To] = {
      assert(x.selfType1 match { case _: EdgeElem[_, _, _] => true; case _ => false })
      x.asRep[To]
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def edgeElement[V, E](implicit eV: Elem[V], eE: Elem[E]): Elem[Edge[V, E]] =
    new EdgeElem[V, E, Edge[V, E]]

  implicit case object EdgeCompanionElem extends CompanionElem[EdgeCompanionAbs] {
    lazy val tag = weakTypeTag[EdgeCompanionAbs]
    protected def getDefaultRep = Edge
  }

  abstract class EdgeCompanionAbs extends CompanionBase[EdgeCompanionAbs] with EdgeCompanion {
    override def toString = "Edge"
  }
  def Edge: Rep[EdgeCompanionAbs]
  implicit def proxyEdgeCompanion(p: Rep[EdgeCompanion]): EdgeCompanion =
    proxyOps[EdgeCompanion](p)

  // elem for concrete class
  class AdjEdgeElem[V, E](val iso: Iso[AdjEdgeData[V, E], AdjEdge[V, E]])(implicit eV: Elem[V], eE: Elem[E])
    extends EdgeElem[V, E, AdjEdge[V, E]]
    with ConcreteElem[AdjEdgeData[V, E], AdjEdge[V, E]] {
    override val parent: Option[Elem[_]] = Some(edgeElement(element[V], element[E]))

    override def convertEdge(x: Rep[Edge[V, E]]) = AdjEdge(x.fromId, x.outIndex, x.graph)
    override def getDefaultRep = super[ConcreteElem].getDefaultRep
    override lazy val tag = {
      implicit val tagV = eV.tag
      implicit val tagE = eE.tag
      weakTypeTag[AdjEdge[V, E]]
    }
  }

  // state representation type
  type AdjEdgeData[V, E] = (Int, (Int, Graph[V,E]))

  // 3) Iso for concrete class
  class AdjEdgeIso[V, E](implicit eV: Elem[V], eE: Elem[E])
    extends Iso[AdjEdgeData[V, E], AdjEdge[V, E]]()(pairElement(implicitly[Elem[Int]], pairElement(implicitly[Elem[Int]], implicitly[Elem[Graph[V,E]]]))) {
    override def from(p: Rep[AdjEdge[V, E]]) =
      (p.fromId, p.outIndex, p.graph)
    override def to(p: Rep[(Int, (Int, Graph[V,E]))]) = {
      val Pair(fromId, Pair(outIndex, graph)) = p
      AdjEdge(fromId, outIndex, graph)
    }
    lazy val defaultRepTo: Rep[AdjEdge[V, E]] = AdjEdge(0, 0, element[Graph[V,E]].defaultRepValue)
    lazy val eTo = new AdjEdgeElem[V, E](this)
  }
  // 4) constructor and deconstructor
  abstract class AdjEdgeCompanionAbs extends CompanionBase[AdjEdgeCompanionAbs] with AdjEdgeCompanion {
    override def toString = "AdjEdge"
    def apply[V, E](p: Rep[AdjEdgeData[V, E]])(implicit eV: Elem[V], eE: Elem[E]): Rep[AdjEdge[V, E]] =
      isoAdjEdge(eV, eE).to(p)
    def apply[V, E](fromId: Rep[Int], outIndex: Rep[Int], graph: Rep[Graph[V,E]])(implicit eV: Elem[V], eE: Elem[E]): Rep[AdjEdge[V, E]] =
      mkAdjEdge(fromId, outIndex, graph)
  }
  object AdjEdgeMatcher {
    def unapply[V, E](p: Rep[Edge[V, E]]) = unmkAdjEdge(p)
  }
  def AdjEdge: Rep[AdjEdgeCompanionAbs]
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
    new AdjEdgeIso[V, E]

  // 6) smart constructor and deconstructor
  def mkAdjEdge[V, E](fromId: Rep[Int], outIndex: Rep[Int], graph: Rep[Graph[V,E]])(implicit eV: Elem[V], eE: Elem[E]): Rep[AdjEdge[V, E]]
  def unmkAdjEdge[V, E](p: Rep[Edge[V, E]]): Option[(Rep[Int], Rep[Int], Rep[Graph[V,E]])]

  // elem for concrete class
  class IncEdgeElem[V, E](val iso: Iso[IncEdgeData[V, E], IncEdge[V, E]])(implicit eV: Elem[V], eE: Elem[E])
    extends EdgeElem[V, E, IncEdge[V, E]]
    with ConcreteElem[IncEdgeData[V, E], IncEdge[V, E]] {
    override val parent: Option[Elem[_]] = Some(edgeElement(element[V], element[E]))

    override def convertEdge(x: Rep[Edge[V, E]]) = IncEdge(x.fromId, x.toId, x.graph)
    override def getDefaultRep = super[ConcreteElem].getDefaultRep
    override lazy val tag = {
      implicit val tagV = eV.tag
      implicit val tagE = eE.tag
      weakTypeTag[IncEdge[V, E]]
    }
  }

  // state representation type
  type IncEdgeData[V, E] = (Int, (Int, Graph[V,E]))

  // 3) Iso for concrete class
  class IncEdgeIso[V, E](implicit eV: Elem[V], eE: Elem[E])
    extends Iso[IncEdgeData[V, E], IncEdge[V, E]]()(pairElement(implicitly[Elem[Int]], pairElement(implicitly[Elem[Int]], implicitly[Elem[Graph[V,E]]]))) {
    override def from(p: Rep[IncEdge[V, E]]) =
      (p.fromId, p.toId, p.graph)
    override def to(p: Rep[(Int, (Int, Graph[V,E]))]) = {
      val Pair(fromId, Pair(toId, graph)) = p
      IncEdge(fromId, toId, graph)
    }
    lazy val defaultRepTo: Rep[IncEdge[V, E]] = IncEdge(0, 0, element[Graph[V,E]].defaultRepValue)
    lazy val eTo = new IncEdgeElem[V, E](this)
  }
  // 4) constructor and deconstructor
  abstract class IncEdgeCompanionAbs extends CompanionBase[IncEdgeCompanionAbs] with IncEdgeCompanion {
    override def toString = "IncEdge"
    def apply[V, E](p: Rep[IncEdgeData[V, E]])(implicit eV: Elem[V], eE: Elem[E]): Rep[IncEdge[V, E]] =
      isoIncEdge(eV, eE).to(p)
    def apply[V, E](fromId: Rep[Int], toId: Rep[Int], graph: Rep[Graph[V,E]])(implicit eV: Elem[V], eE: Elem[E]): Rep[IncEdge[V, E]] =
      mkIncEdge(fromId, toId, graph)
  }
  object IncEdgeMatcher {
    def unapply[V, E](p: Rep[Edge[V, E]]) = unmkIncEdge(p)
  }
  def IncEdge: Rep[IncEdgeCompanionAbs]
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
    new IncEdgeIso[V, E]

  // 6) smart constructor and deconstructor
  def mkIncEdge[V, E](fromId: Rep[Int], toId: Rep[Int], graph: Rep[Graph[V,E]])(implicit eV: Elem[V], eE: Elem[E]): Rep[IncEdge[V, E]]
  def unmkIncEdge[V, E](p: Rep[Edge[V, E]]): Option[(Rep[Int], Rep[Int], Rep[Graph[V,E]])]

  registerModule(scalan.meta.ScalanCodegen.loadModule(Edges_Module.dump))
}

// Seq -----------------------------------
trait EdgesSeq extends EdgesDsl with scalan.ScalanSeq {
  self: GraphsDslSeq =>
  lazy val Edge: Rep[EdgeCompanionAbs] = new EdgeCompanionAbs with UserTypeSeq[EdgeCompanionAbs] {
    lazy val selfType = element[EdgeCompanionAbs]
  }

  case class SeqAdjEdge[V, E]
      (override val fromId: Rep[Int], override val outIndex: Rep[Int], override val graph: Rep[Graph[V,E]])
      (implicit eV: Elem[V], eE: Elem[E])
    extends AdjEdge[V, E](fromId, outIndex, graph)
        with UserTypeSeq[AdjEdge[V, E]] {
    lazy val selfType = element[AdjEdge[V, E]]
  }
  lazy val AdjEdge = new AdjEdgeCompanionAbs with UserTypeSeq[AdjEdgeCompanionAbs] {
    lazy val selfType = element[AdjEdgeCompanionAbs]
  }

  def mkAdjEdge[V, E]
      (fromId: Rep[Int], outIndex: Rep[Int], graph: Rep[Graph[V,E]])(implicit eV: Elem[V], eE: Elem[E]): Rep[AdjEdge[V, E]] =
      new SeqAdjEdge[V, E](fromId, outIndex, graph)
  def unmkAdjEdge[V, E](p: Rep[Edge[V, E]]) = p match {
    case p: AdjEdge[V, E] @unchecked =>
      Some((p.fromId, p.outIndex, p.graph))
    case _ => None
  }

  case class SeqIncEdge[V, E]
      (override val fromId: Rep[Int], override val toId: Rep[Int], override val graph: Rep[Graph[V,E]])
      (implicit eV: Elem[V], eE: Elem[E])
    extends IncEdge[V, E](fromId, toId, graph)
        with UserTypeSeq[IncEdge[V, E]] {
    lazy val selfType = element[IncEdge[V, E]]
  }
  lazy val IncEdge = new IncEdgeCompanionAbs with UserTypeSeq[IncEdgeCompanionAbs] {
    lazy val selfType = element[IncEdgeCompanionAbs]
  }

  def mkIncEdge[V, E]
      (fromId: Rep[Int], toId: Rep[Int], graph: Rep[Graph[V,E]])(implicit eV: Elem[V], eE: Elem[E]): Rep[IncEdge[V, E]] =
      new SeqIncEdge[V, E](fromId, toId, graph)
  def unmkIncEdge[V, E](p: Rep[Edge[V, E]]) = p match {
    case p: IncEdge[V, E] @unchecked =>
      Some((p.fromId, p.toId, p.graph))
    case _ => None
  }
}

// Exp -----------------------------------
trait EdgesExp extends EdgesDsl with scalan.ScalanExp {
  self: GraphsDslExp =>
  lazy val Edge: Rep[EdgeCompanionAbs] = new EdgeCompanionAbs with UserTypeDef[EdgeCompanionAbs] {
    lazy val selfType = element[EdgeCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  case class ExpAdjEdge[V, E]
      (override val fromId: Rep[Int], override val outIndex: Rep[Int], override val graph: Rep[Graph[V,E]])
      (implicit eV: Elem[V], eE: Elem[E])
    extends AdjEdge[V, E](fromId, outIndex, graph) with UserTypeDef[AdjEdge[V, E]] {
    lazy val selfType = element[AdjEdge[V, E]]
    override def mirror(t: Transformer) = ExpAdjEdge[V, E](t(fromId), t(outIndex), t(graph))
  }

  lazy val AdjEdge: Rep[AdjEdgeCompanionAbs] = new AdjEdgeCompanionAbs with UserTypeDef[AdjEdgeCompanionAbs] {
    lazy val selfType = element[AdjEdgeCompanionAbs]
    override def mirror(t: Transformer) = this
  }

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
    (fromId: Rep[Int], outIndex: Rep[Int], graph: Rep[Graph[V,E]])(implicit eV: Elem[V], eE: Elem[E]): Rep[AdjEdge[V, E]] =
    new ExpAdjEdge[V, E](fromId, outIndex, graph)
  def unmkAdjEdge[V, E](p: Rep[Edge[V, E]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: AdjEdgeElem[V, E] @unchecked =>
      Some((p.asRep[AdjEdge[V, E]].fromId, p.asRep[AdjEdge[V, E]].outIndex, p.asRep[AdjEdge[V, E]].graph))
    case _ =>
      None
  }

  case class ExpIncEdge[V, E]
      (override val fromId: Rep[Int], override val toId: Rep[Int], override val graph: Rep[Graph[V,E]])
      (implicit eV: Elem[V], eE: Elem[E])
    extends IncEdge[V, E](fromId, toId, graph) with UserTypeDef[IncEdge[V, E]] {
    lazy val selfType = element[IncEdge[V, E]]
    override def mirror(t: Transformer) = ExpIncEdge[V, E](t(fromId), t(toId), t(graph))
  }

  lazy val IncEdge: Rep[IncEdgeCompanionAbs] = new IncEdgeCompanionAbs with UserTypeDef[IncEdgeCompanionAbs] {
    lazy val selfType = element[IncEdgeCompanionAbs]
    override def mirror(t: Transformer) = this
  }

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
    (fromId: Rep[Int], toId: Rep[Int], graph: Rep[Graph[V,E]])(implicit eV: Elem[V], eE: Elem[E]): Rep[IncEdge[V, E]] =
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

object Edges_Module {
  val packageName = "scalan.graphs"
  val name = "Edges"
  val dump = "H4sIAAAAAAAAANVWS2wbRRie3dhxbIc09IJaCVKCWwQCO6qECgoSCo5TLJkkypaA3AppvDt2JszObmbH0ZpD74C4VNxQhXrg1htHJC4ICXHghCgSZ04FBBXQE4iZ2acf64aoObCH0c7Mv//j+/5vZm//AvIeAxc8ExJIqzbisGqo9zWPV4wG5ZgPXnesPkHrqLu+f+dF9unehzo41Qaze9Bb90gbFIOXhu/G7wY6aIEipCbyuMM8Dp5sqQg10yEEmRw7tIZtu89hh6BaC3t8tQVyHccaHIDrQGuBRdOhJkMcGXUCPQ954fockhnheF5U88GWm8SgNVlFLVXFFQYxF+mLGIuB/Q5yjQF16MDmYCFMbcuVaQmbArZdh/EoREG423OsaJqjUCyA0619eAhrIkSvZnCGaU98WXah+Q7soU1hIs1zImEPke6VgavmMy1Q8tCBAKhpu0St+C4AQDBwUSVRTfCpxvhUJT4VAzEMCX4Xys1t5vgDEDzaDAC+K1w89wAXkQfUoFbl/Wvm1ftG2dblx75MpaAqnBWOljK6QVEhcPx654Z37/KtSzootUEJe2sdjzNo8jTlIVplSKnDVc4xgJD1BFvLWWypKGvCZqQliqZju5AKTyGU84Ingk3MpbFcmw/ZyYC+wF0UmWq+q8X1nsuoV/VNHRKyfffM8+d/brylA304RFG4NETjs8gpB7mG1UOhazme4kDbTfCV04aayqHoJ2NhSiYxJk/f/dX6agVc02Mkw8BHI0+4yHs/fF/+7plXdDDXVq2+QWCvLcD0GgTZW6zuUN4Gc84hYsFO4RAS+TaRzIKFurBPeAhxGpsZgQ0H5zJF6SIJ3KoSgBYBUA56eNOhqLKxXfnL+Oaj27JFGZgPdgKV/oMv/f3jQper7uVgtsscu2lFAM8Iecd4PJVFrou2GbbFYXKIXvjy8zd++2Izr/g9HZa0C0kfBdIOK0qqk0G1FRGpSXnAoIp3Ni5FDktcwNjnTWohfzw1OZyf9m2+x6C7N6GmcCV/Od7/j52W9FspANVwbPTo8j389q0PuOoszR8+4bY6++JIWVXfPTGlyaKT9s/2iv7HmTuf6KAoeqmDuQ3dysoRz4cT1DwYhmuhHt4yShwXhzeVkCcrNaFJ9MHimrUvTevpVJcSHh5LuT2rjZCso90kntDfRDrTXTLuoDHNwXgDcFAIE1YeYp08nq0TAeCNqxdeY79//J4uQc53nD61IkbETc2Rz1+N1rRhRgQDkEE7YiABZ7hb3xzaaIzWXdYmkHO8U3aMk1HhZZ4mD5RsjjvH+u7kpS7Hl9T48olooEnN/5cGwoTTGhhvwyP1Zyqz2YkIF3cQ7mL5G/aQejjNwBRmy/Ic3IA2JoNj0vpIBqduysVDQU+ONxOb0DCvABJphEdToJKwYgaWM04sI7wCxD10/f7NzWe//ewndbGX5GUifixo/GufvtBHOAsUJ/7UU7mKquX1ovL8F59Ct1M5DQAA"
}
}

trait EdgesDsl extends impl.EdgesAbs {self: GraphsDsl =>}
trait EdgesDslSeq extends impl.EdgesSeq {self: GraphsDslSeq =>}
trait EdgesDslExp extends impl.EdgesExp {self: GraphsDslExp =>}
