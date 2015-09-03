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
  class VertexElem[V, E, To <: Vertex[V, E]](implicit val eV: Elem[V], val eE: Elem[E])
    extends EntityElem[To] {
    lazy val parent: Option[Elem[_]] = None
    lazy val entityDef: STraitOrClassDef = {
      val module = getModules("Vertices")
      module.entities.find(_.name == "Vertex").get
    }
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("V" -> Left(eV), "E" -> Left(eE))
    }
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagV = eV.tag
      implicit val tagE = eE.tag
      weakTypeTag[Vertex[V, E]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Reifiable[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[Vertex[V, E]] => convertVertex(x) }
      tryConvert(element[Vertex[V, E]], this, x, conv)
    }

    def convertVertex(x : Rep[Vertex[V, E]]): Rep[To] = {
      assert(x.selfType1 match { case _: VertexElem[_, _, _] => true; case _ => false })
      x.asRep[To]
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def vertexElement[V, E](implicit eV: Elem[V], eE: Elem[E]): Elem[Vertex[V, E]] =
    cachedElem[VertexElem[V, E, Vertex[V, E]]](eV, eE)

  implicit case object VertexCompanionElem extends CompanionElem[VertexCompanionAbs] {
    lazy val tag = weakTypeTag[VertexCompanionAbs]
    protected def getDefaultRep = Vertex
  }

  abstract class VertexCompanionAbs extends CompanionBase[VertexCompanionAbs] with VertexCompanion {
    override def toString = "Vertex"
  }
  def Vertex: Rep[VertexCompanionAbs]
  implicit def proxyVertexCompanion(p: Rep[VertexCompanion]): VertexCompanion =
    proxyOps[VertexCompanion](p)

  // elem for concrete class
  class SVertexElem[V, E](val iso: Iso[SVertexData[V, E], SVertex[V, E]])(implicit eV: Elem[V], eE: Elem[E])
    extends VertexElem[V, E, SVertex[V, E]]
    with ConcreteElem[SVertexData[V, E], SVertex[V, E]] {
    override lazy val parent: Option[Elem[_]] = Some(vertexElement(element[V], element[E]))
    override lazy val entityDef = {
      val module = getModules("Vertices")
      module.concreteSClasses.find(_.name == "SVertex").get
    }
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("V" -> Left(eV), "E" -> Left(eE))
    }

    override def convertVertex(x: Rep[Vertex[V, E]]) = SVertex(x.id, x.graph)
    override def getDefaultRep = super[ConcreteElem].getDefaultRep
    override lazy val tag = {
      implicit val tagV = eV.tag
      implicit val tagE = eE.tag
      weakTypeTag[SVertex[V, E]]
    }
  }

  // state representation type
  type SVertexData[V, E] = (Int, Graph[V,E])

  // 3) Iso for concrete class
  class SVertexIso[V, E](implicit eV: Elem[V], eE: Elem[E])
    extends Iso[SVertexData[V, E], SVertex[V, E]]()(pairElement(implicitly[Elem[Int]], implicitly[Elem[Graph[V,E]]])) {
    override def from(p: Rep[SVertex[V, E]]) =
      (p.id, p.graph)
    override def to(p: Rep[(Int, Graph[V,E])]) = {
      val Pair(id, graph) = p
      SVertex(id, graph)
    }
    lazy val defaultRepTo: Rep[SVertex[V, E]] = SVertex(0, element[Graph[V,E]].defaultRepValue)
    lazy val eTo = new SVertexElem[V, E](this)
  }
  // 4) constructor and deconstructor
  abstract class SVertexCompanionAbs extends CompanionBase[SVertexCompanionAbs] with SVertexCompanion {
    override def toString = "SVertex"
    def apply[V, E](p: Rep[SVertexData[V, E]])(implicit eV: Elem[V], eE: Elem[E]): Rep[SVertex[V, E]] =
      isoSVertex(eV, eE).to(p)
    def apply[V, E](id: Rep[Int], graph: PG[V,E])(implicit eV: Elem[V], eE: Elem[E]): Rep[SVertex[V, E]] =
      mkSVertex(id, graph)
  }
  object SVertexMatcher {
    def unapply[V, E](p: Rep[Vertex[V, E]]) = unmkSVertex(p)
  }
  def SVertex: Rep[SVertexCompanionAbs]
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
  def mkSVertex[V, E](id: Rep[Int], graph: PG[V,E])(implicit eV: Elem[V], eE: Elem[E]): Rep[SVertex[V, E]]
  def unmkSVertex[V, E](p: Rep[Vertex[V, E]]): Option[(Rep[Int], Rep[Graph[V,E]])]

  registerModule(scalan.meta.ScalanCodegen.loadModule(Vertices_Module.dump))
}

// Seq -----------------------------------
trait VerticesSeq extends VerticesDsl with scalan.ScalanSeq {
  self: GraphsDslSeq =>
  lazy val Vertex: Rep[VertexCompanionAbs] = new VertexCompanionAbs with UserTypeSeq[VertexCompanionAbs] {
    lazy val selfType = element[VertexCompanionAbs]
  }

  case class SeqSVertex[V, E]
      (override val id: Rep[Int], override val graph: PG[V,E])
      (implicit eV: Elem[V], eE: Elem[E])
    extends SVertex[V, E](id, graph)
        with UserTypeSeq[SVertex[V, E]] {
    lazy val selfType = element[SVertex[V, E]]
  }
  lazy val SVertex = new SVertexCompanionAbs with UserTypeSeq[SVertexCompanionAbs] {
    lazy val selfType = element[SVertexCompanionAbs]
  }

  def mkSVertex[V, E]
      (id: Rep[Int], graph: PG[V,E])(implicit eV: Elem[V], eE: Elem[E]): Rep[SVertex[V, E]] =
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
  lazy val Vertex: Rep[VertexCompanionAbs] = new VertexCompanionAbs with UserTypeDef[VertexCompanionAbs] {
    lazy val selfType = element[VertexCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  case class ExpSVertex[V, E]
      (override val id: Rep[Int], override val graph: PG[V,E])
      (implicit eV: Elem[V], eE: Elem[E])
    extends SVertex[V, E](id, graph) with UserTypeDef[SVertex[V, E]] {
    lazy val selfType = element[SVertex[V, E]]
    override def mirror(t: Transformer) = ExpSVertex[V, E](t(id), t(graph))
  }

  lazy val SVertex: Rep[SVertexCompanionAbs] = new SVertexCompanionAbs with UserTypeDef[SVertexCompanionAbs] {
    lazy val selfType = element[SVertexCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object SVertexMethods {
  }

  object SVertexCompanionMethods {
  }

  def mkSVertex[V, E]
    (id: Rep[Int], graph: PG[V,E])(implicit eV: Elem[V], eE: Elem[E]): Rep[SVertex[V, E]] =
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
      def unapply(d: Def[_]): Option[(Rep[Vertex[V, E]], Rep[Vertex[V,E]]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(v, _*), _) if receiver.elem.isInstanceOf[VertexElem[_, _, _]] && method.getName == "hasEdgeTo" =>
          Some((receiver, v)).asInstanceOf[Option[(Rep[Vertex[V, E]], Rep[Vertex[V,E]]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vertex[V, E]], Rep[Vertex[V,E]]) forSome {type V; type E}] = exp match {
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
      def unapply(d: Def[_]): Option[(Rep[Vertex[V, E]], Rep[Vertex[V,E]]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(v, _*), _) if receiver.elem.isInstanceOf[VertexElem[_, _, _]] && method.getName == "commonNbrs" =>
          Some((receiver, v)).asInstanceOf[Option[(Rep[Vertex[V, E]], Rep[Vertex[V,E]]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vertex[V, E]], Rep[Vertex[V,E]]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object commonNbrsNum {
      def unapply(d: Def[_]): Option[(Rep[Vertex[V, E]], Rep[Vertex[V,E]]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(v, _*), _) if receiver.elem.isInstanceOf[VertexElem[_, _, _]] && method.getName == "commonNbrsNum" =>
          Some((receiver, v)).asInstanceOf[Option[(Rep[Vertex[V, E]], Rep[Vertex[V,E]]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vertex[V, E]], Rep[Vertex[V,E]]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object VertexCompanionMethods {
  }
}

object Vertices_Module {
  val packageName = "scalan.graphs"
  val name = "Vertices"
  val dump = "H4sIAAAAAAAAALVWzW8bRRR/u4nj2A5JqISqVgKHYEAgsEME6iGHKrhOVOTGVraNkKkqjddjZ8rs7GZmHNkc+gfADSFxQqj3Shw4InFBSIgDJ74kLlzgUkCoAnoqYmb2w+vEDlERexjtzL59H7/f773dO79CRnB4RriIIlb2sERlx9xvCllyakwSObzid/oUX8LdBz+Wv/m5e/ExG5ZaMLePxCVBW5ALb2qDILl38EEdcoi5WEifCwlP1k2EiutTil1JfFYhnteXqE1xpU6E3KjDbNvvDA/gFlh1WHZ95nIssVOlSAgsovN5rDMiyT5n9sNGMIrBKrqKSqqKqxwRqdJXMZZD+10cOEPms6EnYTFKrRHotJRNlniBz2UcIqvc7fudeDvLkDqAM/Wb6BBVVIhexZGcsJ56sxAg903UwzvKRJvPqoQFpt2rw8DsZ+qQF/hAAXTZC6g5GQQAoBhYN0mUR/iUE3zKGp+SgzlBlLyF9MMm9wdDCC9rBmAQKBcv/IuL2AOusU7p7evuG/edgmfrlwc6laypcE45Kk5Rg6FC4fjF7rvi3vbtCzbkW5AnYrMtJEeuTFMeoVVAjPnS5JwAiHhPsbU6jS0TZVPZHJFEzvW9ADHlKYJyQfFEiUukNtZnCxE7U6DPygDHptYgsJJ6V6bUa3RTRZQ275578elfaq/bYI+HyCmXjhI+j51KmNvDXOJB5FyvSxKsvRHCelszW73kBqM1e0IuCSrP3v2t8/kaXLcTLKPQp6NPuciI778tfP3cRRvmW0bsWxT1WgpOUaPYa/Cqz2QL5v1DzMMn2UNE9d1EOrMd3EV9KiOQ0+jMKHQkrExtywBr6DZMC1gxAIVQxTs+w6WtZukv58v37miRclgIn4R9+je58OCHxa40+pVgk04M7oxq7gSLp6ZRG+AmJ54aJYf4lc8+ufb7pzsZw+6ZqJw9RPs4bOyomlFlOqC1piJdZjJkz8Q7n5Shl6KETI+jYD/Oy25uP6wo8mHlju/hR1fvkRu335GGfmswPoga7Zuq8zfMe0+coIR4IP7ZWrP/OPfdhzbkFOFtIj0UlNZO2cb/Y2vCOFCL1ehjYBS8Pv4w64QNNxm8ER2Ks+XItJrOtjgi4WzK83nrCJk23otDzuo+mchlWg3HHdROcnCc/fHaiommH5+uaYUhO+tcef+j4g0bMq9BpqvaVtQh0/b7rBOTo76tyql8NT6zxslRZCCOvIQMc63ACKxx6TYmGhwvJ1Xvy0cIzu1i0iX6QzV+/h9maZp/Y/rSxMgFLcIt5BE6XJ8Y+xSyWpqqqmDMTTEVYDJoD4GqXq+NbCLDeR2V6B6GRyK1hLMogoLD6hQROVFjqulw6/4HO89/9fFPZi7mdYurmcyS/6L0PDxC57aJpX5zUukq1eumN6n+A2Cm3FN2CgAA"
}
}

trait VerticesDsl extends impl.VerticesAbs {self: GraphsDsl =>}
trait VerticesDslSeq extends impl.VerticesSeq {self: GraphsDslSeq =>}
trait VerticesDslExp extends impl.VerticesExp {self: GraphsDslExp =>}
