package scalan.graphs

import scalan.collections.CollectionsDsl
import scalan.{Scalan, ScalanExp, ScalanSeq}
import scalan.ScalanCommunityDsl
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait VerticesAbs extends Vertices with scalan.Scalan {
  self: GraphsDsl =>

  // single proxy for each type family
  implicit def proxyVertex[V, E](p: Rep[Vertex[V, E]]): Vertex[V, E] = {
    proxyOps[Vertex[V, E]](p)(scala.reflect.classTag[Vertex[V, E]])
  }

  // familyElem
  class VertexElem[V, E, To <: Vertex[V, E]](implicit _eV: Elem[V], _eE: Elem[E])
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
      weakTypeTag[Vertex[V, E]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[Vertex[V, E]] => convertVertex(x) }
      tryConvert(element[Vertex[V, E]], this, x, conv)
    }

    def convertVertex(x: Rep[Vertex[V, E]]): Rep[To] = {
      x.selfType1 match {
        case _: VertexElem[_, _, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have VertexElem[_, _, _], but got $e")
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def vertexElement[V, E](implicit eV: Elem[V], eE: Elem[E]): Elem[Vertex[V, E]] =
    cachedElem[VertexElem[V, E, Vertex[V, E]]](eV, eE)

  implicit case object VertexCompanionElem extends CompanionElem[VertexCompanionAbs] {
    lazy val tag = weakTypeTag[VertexCompanionAbs]
    protected def getDefaultRep = Vertex
  }

  abstract class VertexCompanionAbs extends CompanionDef[VertexCompanionAbs] with VertexCompanion {
    def selfType = VertexCompanionElem
    override def toString = "Vertex"
  }
  def Vertex: Rep[VertexCompanionAbs]
  implicit def proxyVertexCompanion(p: Rep[VertexCompanion]): VertexCompanion =
    proxyOps[VertexCompanion](p)

  abstract class AbsSVertex[V, E]
      (id: Rep[Int], graph: PG[V, E])(implicit eV: Elem[V], eE: Elem[E])
    extends SVertex[V, E](id, graph) with Def[SVertex[V, E]] {
    lazy val selfType = element[SVertex[V, E]]
  }
  // elem for concrete class
  class SVertexElem[V, E](val iso: Iso[SVertexData[V, E], SVertex[V, E]])(implicit eV: Elem[V], eE: Elem[E])
    extends VertexElem[V, E, SVertex[V, E]]
    with ConcreteElem[SVertexData[V, E], SVertex[V, E]] {
    override lazy val parent: Option[Elem[_]] = Some(vertexElement(element[V], element[E]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("V" -> Left(eV), "E" -> Left(eE))
    }

    override def convertVertex(x: Rep[Vertex[V, E]]) = SVertex(x.id, x.graph)
    override def getDefaultRep = SVertex(0, element[Graph[V, E]].defaultRepValue)
    override lazy val tag = {
      implicit val tagV = eV.tag
      implicit val tagE = eE.tag
      weakTypeTag[SVertex[V, E]]
    }
  }

  // state representation type
  type SVertexData[V, E] = (Int, Graph[V, E])

  // 3) Iso for concrete class
  class SVertexIso[V, E](implicit eV: Elem[V], eE: Elem[E])
    extends Iso[SVertexData[V, E], SVertex[V, E]]()(pairElement(implicitly[Elem[Int]], implicitly[Elem[Graph[V, E]]])) {
    override def from(p: Rep[SVertex[V, E]]) =
      (p.id, p.graph)
    override def to(p: Rep[(Int, Graph[V, E])]) = {
      val Pair(id, graph) = p
      SVertex(id, graph)
    }
    lazy val eTo = new SVertexElem[V, E](this)
  }
  // 4) constructor and deconstructor
  class SVertexCompanionAbs extends CompanionDef[SVertexCompanionAbs] with SVertexCompanion {
    def selfType = SVertexCompanionElem
    override def toString = "SVertex"
    def apply[V, E](p: Rep[SVertexData[V, E]])(implicit eV: Elem[V], eE: Elem[E]): Rep[SVertex[V, E]] =
      isoSVertex(eV, eE).to(p)
    def apply[V, E](id: Rep[Int], graph: PG[V, E])(implicit eV: Elem[V], eE: Elem[E]): Rep[SVertex[V, E]] =
      mkSVertex(id, graph)
  }
  object SVertexMatcher {
    def unapply[V, E](p: Rep[Vertex[V, E]]) = unmkSVertex(p)
  }
  lazy val SVertex: Rep[SVertexCompanionAbs] = new SVertexCompanionAbs
  implicit def proxySVertexCompanion(p: Rep[SVertexCompanionAbs]): SVertexCompanionAbs = {
    proxyOps[SVertexCompanionAbs](p)
  }

  implicit case object SVertexCompanionElem extends CompanionElem[SVertexCompanionAbs] {
    lazy val tag = weakTypeTag[SVertexCompanionAbs]
    protected def getDefaultRep = SVertex
  }

  implicit def proxySVertex[V, E](p: Rep[SVertex[V, E]]): SVertex[V, E] =
    proxyOps[SVertex[V, E]](p)

  implicit class ExtendedSVertex[V, E](p: Rep[SVertex[V, E]])(implicit eV: Elem[V], eE: Elem[E]) {
    def toData: Rep[SVertexData[V, E]] = isoSVertex(eV, eE).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoSVertex[V, E](implicit eV: Elem[V], eE: Elem[E]): Iso[SVertexData[V, E], SVertex[V, E]] =
    cachedIso[SVertexIso[V, E]](eV, eE)

  // 6) smart constructor and deconstructor
  def mkSVertex[V, E](id: Rep[Int], graph: PG[V, E])(implicit eV: Elem[V], eE: Elem[E]): Rep[SVertex[V, E]]
  def unmkSVertex[V, E](p: Rep[Vertex[V, E]]): Option[(Rep[Int], Rep[Graph[V, E]])]

  registerModule(Vertices_Module)
}

// Seq -----------------------------------
trait VerticesSeq extends VerticesDsl with scalan.ScalanSeq {
  self: GraphsDslSeq =>
  lazy val Vertex: Rep[VertexCompanionAbs] = new VertexCompanionAbs {
  }

  case class SeqSVertex[V, E]
      (override val id: Rep[Int], override val graph: PG[V, E])(implicit eV: Elem[V], eE: Elem[E])
    extends AbsSVertex[V, E](id, graph) {
  }

  def mkSVertex[V, E]
    (id: Rep[Int], graph: PG[V, E])(implicit eV: Elem[V], eE: Elem[E]): Rep[SVertex[V, E]] =
    new SeqSVertex[V, E](id, graph)
  def unmkSVertex[V, E](p: Rep[Vertex[V, E]]) = p match {
    case p: SVertex[V, E] @unchecked =>
      Some((p.id, p.graph))
    case _ => None
  }
}

// Exp -----------------------------------
trait VerticesExp extends VerticesDsl with scalan.ScalanExp {
  self: GraphsDslExp =>
  lazy val Vertex: Rep[VertexCompanionAbs] = new VertexCompanionAbs {
  }

  case class ExpSVertex[V, E]
      (override val id: Rep[Int], override val graph: PG[V, E])(implicit eV: Elem[V], eE: Elem[E])
    extends AbsSVertex[V, E](id, graph)

  object SVertexMethods {
  }

  object SVertexCompanionMethods {
  }

  def mkSVertex[V, E]
    (id: Rep[Int], graph: PG[V, E])(implicit eV: Elem[V], eE: Elem[E]): Rep[SVertex[V, E]] =
    new ExpSVertex[V, E](id, graph)
  def unmkSVertex[V, E](p: Rep[Vertex[V, E]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: SVertexElem[V, E] @unchecked =>
      Some((p.asRep[SVertex[V, E]].id, p.asRep[SVertex[V, E]].graph))
    case _ =>
      None
  }

  object VertexMethods {
    object graph {
      def unapply(d: Def[_]): Option[Rep[Vertex[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[VertexElem[_, _, _]] && method.getName == "graph" =>
          Some(receiver).asInstanceOf[Option[Rep[Vertex[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Vertex[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object id {
      def unapply(d: Def[_]): Option[Rep[Vertex[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[VertexElem[_, _, _]] && method.getName == "id" =>
          Some(receiver).asInstanceOf[Option[Rep[Vertex[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Vertex[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object value {
      def unapply(d: Def[_]): Option[Rep[Vertex[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[VertexElem[_, _, _]] && method.getName == "value" =>
          Some(receiver).asInstanceOf[Option[Rep[Vertex[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Vertex[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object outNbrs {
      def unapply(d: Def[_]): Option[Rep[Vertex[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[VertexElem[_, _, _]] && method.getName == "outNbrs" =>
          Some(receiver).asInstanceOf[Option[Rep[Vertex[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Vertex[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object outEdges {
      def unapply(d: Def[_]): Option[Rep[Vertex[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[VertexElem[_, _, _]] && method.getName == "outEdges" =>
          Some(receiver).asInstanceOf[Option[Rep[Vertex[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Vertex[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object hasEdgeTo {
      def unapply(d: Def[_]): Option[(Rep[Vertex[V, E]], Rep[Vertex[V, E]]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(v, _*), _) if receiver.elem.isInstanceOf[VertexElem[_, _, _]] && method.getName == "hasEdgeTo" =>
          Some((receiver, v)).asInstanceOf[Option[(Rep[Vertex[V, E]], Rep[Vertex[V, E]]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vertex[V, E]], Rep[Vertex[V, E]]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object numOutNbrs {
      def unapply(d: Def[_]): Option[Rep[Vertex[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[VertexElem[_, _, _]] && method.getName == "numOutNbrs" =>
          Some(receiver).asInstanceOf[Option[Rep[Vertex[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Vertex[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object numInNbrs {
      def unapply(d: Def[_]): Option[Rep[Vertex[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[VertexElem[_, _, _]] && method.getName == "numInNbrs" =>
          Some(receiver).asInstanceOf[Option[Rep[Vertex[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Vertex[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object commonNbrs {
      def unapply(d: Def[_]): Option[(Rep[Vertex[V, E]], Rep[Vertex[V, E]]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(v, _*), _) if receiver.elem.isInstanceOf[VertexElem[_, _, _]] && method.getName == "commonNbrs" =>
          Some((receiver, v)).asInstanceOf[Option[(Rep[Vertex[V, E]], Rep[Vertex[V, E]]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vertex[V, E]], Rep[Vertex[V, E]]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object commonNbrsNum {
      def unapply(d: Def[_]): Option[(Rep[Vertex[V, E]], Rep[Vertex[V, E]]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(v, _*), _) if receiver.elem.isInstanceOf[VertexElem[_, _, _]] && method.getName == "commonNbrsNum" =>
          Some((receiver, v)).asInstanceOf[Option[(Rep[Vertex[V, E]], Rep[Vertex[V, E]]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vertex[V, E]], Rep[Vertex[V, E]]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object VertexCompanionMethods {
  }
}

object Vertices_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAALVWzW8bRRR/u47j2A5JqBDQSOAQDAgEdhqBesihCq4TFZnYyrYRMhXSeD12psx+ZHcc2Rz6B8ANcUXQI1JvnBBShYSQEAdOCJA4cypFqIL2VMSb2Q/vJnaIitjDaD7evo/f7/dm9+YdyPoePO+bhBO7YlFBKoaab/qibNRtwcToTac74PQi7T3pfPXpuc+Wv9BhsQ2z+8S/6PM25INJfejGc4MeNCBPbJP6wvF8Ac80VISq6XBOTcEcu8osayBIh9Nqg/liowEzHac7OoDroDVgyXRs06OCGjVOfJ/64f4clRmxeJ1X61HTHcewq7KKaqKKyx5hAtPHGEuB/S51jZHt2CNLwEKYWtOVaaFNjlmu44koRA7d7TvdaDljE9yAM41r5JBUMUS/agiP2X18s+gS813SpztoIs1nMGGf8t7lkavWmQYUfHqAAF2yXK52hi4AIAPrKonKGJ9KjE9F4lM2qMcIZ+8RedjynOEIgkfLAAxddPHyv7iIPNC63S2/f9V8+75RtHT58lCmklMVzqKj0hQ1KCoQx293P/Tvbt84r0OhDQXmb3Z84RFTJCkP0SoS23aEyjkGkHh9ZGt1GlsqyibaHJFE3nQsl9joKYRyHnnizGRCGsu9+ZCdKdDnhEsjU23oanG9K1PqVbqpEc5bt8++8txv9bd00NMh8ujSQOF7kVMBs3vUE3QYOpfjogBtb4ywXNbVUg754XjMnZBLjMoLt3/vfrMGV/UYyzD06ehDF1n/5x+LP7x4QYe5thL7Fif9NsLp1zm1ml7NsUUb5pxD6gUnuUPC5Wwinbku7ZEBFyHISXQyiI6Alalt6VIJ3YZqAS0CoBioeMexaXmrVb5nfPfRTSlSD+aDk6BP/2bnH/yy0BNKvwJ01o3AzWBzx1g8O41al7Y8ZuFVckhf+/rLK3/c2skqds+E5ewRPqBBY4fVjCuTAbU1jHTJFgF7Kt5yXIYcSgKyfY+4+1Feemv7YUVRCCo3HIs+unqXvXPjA6Ho14bpi6jZuYadv6Hee/oEJUQX4l/tNf3Psz99okMeCe8wYRG3vHbKNv4fWxPSQC3Uwo+BUvB6+jBnBA03GbwxHcjZUmhaS2ZbGpPwRMLzsnaETJ3uRSFnZJ9M5DKphuMO6ic5OM5+urZSrOmnpmsaMXx8t/EYv3Phlg7ZNyDbw7b1G5DtOAO7G5GD31Z0Kl6P9rQ0OUgG8YgVk6GeFRiDlZZuc6LB8XIS9b56hOAMajG98x9u0STzyvTcxJhFKb8tYjE+Wp8Y+xSCWpyqJzflppQIMBmuh8BTjlfGNqHhnIzKZPfCI6FOglsohMKD1SnyMcKWRC6u3/9456XvP/9V3YgF2dx4G9vxH1HyJkxDl99WsfAHJ5Eu6l22u0r1H+uRXW1wCgAA"
}
}

trait VerticesDsl extends impl.VerticesAbs {self: GraphsDsl =>}
trait VerticesDslSeq extends impl.VerticesSeq {self: GraphsDslSeq =>}
trait VerticesDslExp extends impl.VerticesExp {self: GraphsDslExp =>}
