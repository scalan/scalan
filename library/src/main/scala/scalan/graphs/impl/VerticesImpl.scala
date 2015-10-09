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
  val dump = "H4sIAAAAAAAAALVWTWwbRRR+u4njX5LQS9VK4BAMCAR2iEA95ICC60RFbmxl26gyVaXxeuxMmZ3dzI4jL4fegRtC6gmh3itx4IjEBSEhDpwQIHHmVECoKlRComJm9sfrxA5REXsY7cy+fT/f9723e/dXyPgcnvdtRBGrOligqqXvN31RsRpMEBFcdntDii/iPi+VcsHtv94zYakDC/vIv+jTDuTDm8bIS+4tfNCEPGI29oXLfQHPNHWEmu1Sim1BXFYjjjMUqEtxrUl8sdGE+a7bCw7gFhhNWLZdZnMssFWnyPexH53nsMqIJPu83gctbxyD1VQVtVQVVzgiQqYvYyyH9rvYswLmssARsBil1vJUWtImSxzP5SIOkZXu9t1evJ1nSB7AmeZNdIhqMsSgZglO2EC+WfSQ/Q4a4B1posznZcI+pv0rgaf3c00o+PhAAnTJ8ag+GXkAIBlY10lUx/hUE3yqCp+KhTlBlLyL1MM2d0cBhJcxBzDypIuX/8VF7AE3WK/y/nX77YdW0THVyyOVSlZXuCAdlWeoQVMhcfx690P//vadCyYUOlAg/mbXFxzZIk15hFYRMeYKnXMCIOIDydbqLLZ0lE1pc0QSedt1PMSkpwjKkuSJEpsIZazOShE7M6DPCg/HpsbIM5J6V2bUq3VTR5S275175blfGtdMMCdD5KVLSwqfx04FLOxhLvAocq7WJQHG3hhhtW3orVryo/GaPSGXBJUX7v3W+2oNrpsJllHo09EnXWT8H78vfvfiGybkOlrsWxQNOhJOv0Gx0+J1l4kO5NxDzMMn2UNE1d1UOrM93EdDKiKQ0+jMSXQErMxsSw8r6DZ0CxgxAMVQxTsuw5WtduVP65uP7iqRciiFT8I+fUQu/P3TYl9o/QowSS8Gd042d4LFs7Oo9XCbE0eOkkP8+pefX/39i52MZvdMVM4eokMcNnZUzbgyFdBYk5EuMRGyp+OdT8pQS1lAZsCRtx/nZba3H1cUhbByy3Xwk6v3yY07HwhNvzGaHESt7k3Z+Rv6vadPUEI8EP/orJkPzv3wiQl5SXiXCAd5lbVTtvH/2JowCdRiPfoYaAWvTz7MWmHDTQdvTIfkbDkyraezLY9JOJvyfN44QqaJ9+KQ86pPpnKZVsNxB42THBxnf7K2cqLpp2ZrWmLIzlqXb39avmFC5i3I9GXb+k3IdN0h68XkyG+rdCrejM+MSXIkGYgjJyFDXyswBmtSuq2pBsfLSdX72hGC87uY9In6UE2e/4dZmuZfm746NXJRiXALOYQG61Njn0JWSzNV5U24KacCTAftMVBV69WxTWSYU1GJ6mF4IlJLOIsiKDiszhCRFTWmnA63Hn6889K3n/2s52JBtbicySz5L0rPwyN0butY8jcnla5UvWp6neo/t3nCYXYKAAA="
}
}

trait VerticesDsl extends impl.VerticesAbs {self: GraphsDsl =>}
trait VerticesDslSeq extends impl.VerticesSeq {self: GraphsDslSeq =>}
trait VerticesDslExp extends impl.VerticesExp {self: GraphsDslExp =>}
