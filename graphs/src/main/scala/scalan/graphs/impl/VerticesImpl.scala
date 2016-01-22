package scalan.graphs

import scalan.collections.CollectionsDsl
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait VerticesAbs extends scalan.ScalanDsl with Vertices {
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
        case e => !!!(s"Expected $x to have VertexElem[_, _, _], but got $e", x)
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
  implicit def proxyVertexCompanionAbs(p: Rep[VertexCompanionAbs]): VertexCompanionAbs =
    proxyOps[VertexCompanionAbs](p)

  abstract class AbsSVertex[V, E]
      (id: Rep[Int], graph: PG[V, E])(implicit eV: Elem[V], eE: Elem[E])
    extends SVertex[V, E](id, graph) with Def[SVertex[V, E]] {
    lazy val selfType = element[SVertex[V, E]]
  }
  // elem for concrete class
  class SVertexElem[V, E](val iso: Iso[SVertexData[V, E], SVertex[V, E]])(implicit override val eV: Elem[V], override val eE: Elem[E])
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
    extends EntityIso[SVertexData[V, E], SVertex[V, E]] with Def[SVertexIso[V, E]] {
    override def from(p: Rep[SVertex[V, E]]) =
      (p.id, p.graph)
    override def to(p: Rep[(Int, Graph[V, E])]) = {
      val Pair(id, graph) = p
      SVertex(id, graph)
    }
    lazy val eFrom = pairElement(element[Int], element[Graph[V, E]])
    lazy val eTo = new SVertexElem[V, E](self)
    lazy val selfType = new SVertexIsoElem[V, E](eV, eE)
    def productArity = 2
    def productElement(n: Int) = (eV, eE).productElement(n)
  }
  case class SVertexIsoElem[V, E](eV: Elem[V], eE: Elem[E]) extends Elem[SVertexIso[V, E]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new SVertexIso[V, E]()(eV, eE))
    lazy val tag = {
      implicit val tagV = eV.tag
      implicit val tagE = eE.tag
      weakTypeTag[SVertexIso[V, E]]
    }
  }
  // 4) constructor and deconstructor
  class SVertexCompanionAbs extends CompanionDef[SVertexCompanionAbs] with SVertexCompanion {
    def selfType = SVertexCompanionElem
    override def toString = "SVertex"
    def apply[V, E](p: Rep[SVertexData[V, E]])(implicit eV: Elem[V], eE: Elem[E]): Rep[SVertex[V, E]] =
      isoSVertex(eV, eE).to(p)
    def apply[V, E](id: Rep[Int], graph: PG[V, E])(implicit eV: Elem[V], eE: Elem[E]): Rep[SVertex[V, E]] =
      mkSVertex(id, graph)

    def unapply[V, E](p: Rep[Vertex[V, E]]) = unmkSVertex(p)
  }
  lazy val SVertexRep: Rep[SVertexCompanionAbs] = new SVertexCompanionAbs
  lazy val SVertex: SVertexCompanionAbs = proxySVertexCompanion(SVertexRep)
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
    reifyObject(new SVertexIso[V, E]()(eV, eE))

  // 6) smart constructor and deconstructor
  def mkSVertex[V, E](id: Rep[Int], graph: PG[V, E])(implicit eV: Elem[V], eE: Elem[E]): Rep[SVertex[V, E]]
  def unmkSVertex[V, E](p: Rep[Vertex[V, E]]): Option[(Rep[Int], Rep[Graph[V, E]])]

  registerModule(Vertices_Module)
}

// Std -----------------------------------
trait VerticesStd extends scalan.ScalanDslStd with VerticesDsl {
  self: GraphsDslStd =>
  lazy val Vertex: Rep[VertexCompanionAbs] = new VertexCompanionAbs {
  }

  case class StdSVertex[V, E]
      (override val id: Rep[Int], override val graph: PG[V, E])(implicit eV: Elem[V], eE: Elem[E])
    extends AbsSVertex[V, E](id, graph) {
  }

  def mkSVertex[V, E]
    (id: Rep[Int], graph: PG[V, E])(implicit eV: Elem[V], eE: Elem[E]): Rep[SVertex[V, E]] =
    new StdSVertex[V, E](id, graph)
  def unmkSVertex[V, E](p: Rep[Vertex[V, E]]) = p match {
    case p: SVertex[V, E] @unchecked =>
      Some((p.id, p.graph))
    case _ => None
  }
}

// Exp -----------------------------------
trait VerticesExp extends scalan.ScalanDslExp with VerticesDsl {
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
  val dump = "H4sIAAAAAAAAALVWS2wbRRj+dx3Hr5CE8hKVwCEYEAjsEIGKlEMVXCcqMomVLRYyFWi8O3amzD6YHUc2hx57gBviyqESEpdeUA8cQL0gJMSBE0JInDhwKkVVD+2piJnZh3cTb6BF7GG0M/PP//i+75/dKzcg7zN41jcRRU7dxhzVDfW+6fOa0XI44ZM3XWtE8Rk8+O2L166u5r76RoelHszvI/+MT3tQCl5aYy9+N7jVhhJyTOxzl/kcnmqrCA3TpRSbnLhOg9j2iKM+xY028flGG+b6rjX5AC6C1oZl03VMhjk2mhT5PvbD9SKWGZF4XlLzya43jeE0ZBWNRBXnGCJcpC9iLAf2e9gzJo7rTGwOi2Fqu55MS9gUiO25jEchCsLdvmtF0zkHiQU40b6ADlBDhBg2DM6IMxQnKx4y30dDvCNMpPmcSNjHdHBu4ql5rg1ln1sCoLO2R9XK2AMAwcC6SqI+xace41OX+NQMzAii5EMkNzvMHU8geLQcwNgTLl78BxeRB9xyrNpH58137hgVW5eHxzKVgqpwXjiqZqhBUSFw/H7vE//W9uVTOpR7UCb+Zt/nDJk8SXmIVgU5jstVzjGAiA0FW6tZbKkom8LmkCRKpmt7yBGeQigXBE+UmIRLY7m2ELKTAX2Bezgy1caeFte7klGv0k0TUdq5/vhLz/zRelsHPR2iJFwaQvgscsphvosZx+PQuRyXOGjdKcJy2lJTOZTG07FwTC4xKs9d/9P6bg3O6zGWYeh/R59wkfd/+bny0/OndSj2lNi3KBr2BJx+i2J7lzVdh/eg6B5gFuwUDhCVbzPpLFh4gEaUhyAn0ckJdDisZLalhyV0G6oFtAiASqDiHdfBta1O7bbxw6dXpEgZLAQ7QZ/+RU7d/XVxwJV+OejEisDNieaOsXg6i1oPdxixxVVygF/99uu3bl7bySt2T4TldBEd4aCxw2qmlcmA2pqIdNbhAXsq3sm4DDlUOeSHDHn7UV56Z/t+RVEOKjdcGz+4eou8e/ljrujXxumLaLd/QXT+hjr35DFKiC7ELy9deuTm5+89pBq52CfcRl5t7R7aOOq6/7FNIQ3aYjP8MCg1r6c3C0bQfBlAyvGxeC9gSVC5HJ5qJhOvJo4kgpzUDnGs424UfU62z0yKkyI56qB1nIOjokiXWY2l/kS21AWcj+61H6Y3Tl/TIf8G5Aeim/025PvuyLEinsQnVzjlr0drWponwQtiyI55Uc8KTMFKK7oz0+BoOYl6XznEdU5INL3yHy7XDBGo+cszw1ekKLeQTehkfWYa9yazpUyVeSmP1USs2SDeB8py7E5tQsOijEpkq8MDoXqCKytEhcFqhqiMsGcFQxfvfLbzwo9Xf1fXZ1l2v7i6nfj3KXltplEsbatY4m8oka7oAnkfqFT/BkQzo4OdCgAA"
}
}

trait VerticesDsl extends impl.VerticesAbs {self: GraphsDsl =>}
trait VerticesDslStd extends impl.VerticesStd {self: GraphsDslStd =>}
trait VerticesDslExp extends impl.VerticesExp {self: GraphsDslExp =>}
