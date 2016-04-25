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
    @scalan.OverloadId("fromData")
    def apply[V, E](p: Rep[SVertexData[V, E]])(implicit eV: Elem[V], eE: Elem[E]): Rep[SVertex[V, E]] =
      isoSVertex(eV, eE).to(p)
    @scalan.OverloadId("fromFields")
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
  val dump = "H4sIAAAAAAAAALVWS2wbRRieXcexHYcklJcoAqfBPIrADgHUQ4Sq4DpRKjexssVCpgKNd8fOltkHO+PI5lBuOcANceVQAUJIFRLiiNQLVEIIcagqhARXTqWoyqE9gfhn9uHdxBtoEXsY7ez++z++7/v/nUs3UJZ56EmmY4rtikU4rmjyfoXxsla3ucmHZxyjT8kp0i3eWj72625tXUWzbTS5jdkpRtuo4N/UB250r3GjgQrY1gnjjsc4OtaQEaq6QynRuenYVdOy+hx3KKk2TMaXG2ii4xjDt9EFpDTQnO7Yukc40WoUM0ZY8DxPREZmtC/I/XDTHcWwq6KKaqyKsx42OaQPMeZ8+y3iakPbsYcWRzNBapuuSAtscqblOh4PQ+TA3bZjhNsJG8MDdKRxHu/gKoToVTXumXYPviy6WH8L98gGmAjzCUiYEdo9O3TlPgMmjBsA0LrlUukxM3ARQkDBksyiMgKoEgFUEQCVNeKZmJrvYPGy6TmDIfIvJYPQwAUXz/6Di9ADqdtG+b1z+uu3taKlio8HIpWcTGgSHJVS5CC5ACC/2/qA7a1dPKGiqTaaMtlKh3EP6zzOeQBXEdu2w2XOEYLY6wFdC2l0ySgrYLNPEwXdsVxsg6cAy2kgipq6yYWxeDYd0JOCfY67JDRVBq4S1TufUq8UTg1T2rz+8HNP/F5/TUVqMkQBXGqgfC90ytFki3icDALnYp3lSGmNEBbbutyKpTAYrblDcolQeer6H8aVRXROjbAMQv87+sBFlv38U/Ha8ZMqyrel2lcp7rUBTlanxNr0ao7N2yjv7BDPf5PbwVTcjaUzZ5Au7lMegBxHJwPocDSf2pcuEdAtyxZQQgCKvoo3HJuUV5vlW9r3H14SIvXQtP/Gb9S/zBN//jLT5VK/HKmmEYKbge6OsHg8jVqXND3TglmyQ1765utXb17eyEp2jwTltDDtE7+zg2pGlYmAyiJEWre5z56MdzQqQywljrI9D7vbYV5qc+1uRTHlV645Frl3Yc984+L7XNKvDJKTaLNzHjp/WX732CFKCCfil7u7D9z85M37ZCPnOya3sFtevIM2Drvuf2xTlARtphb8GaSal5Ivc5rffClAivWh6J3PElA5F3xViydein0SC3JU2cexSlph9AnRPmMpjovkoIP6YQ4OiiJZZimS+qPpUgc4H9xq3E9vnLysouxplO1CN7MGynacvm2EPME/F5zyV8JnSpIn4AV72Ip4kdc8GoGVVHRzrMHBcmL1vriP6wxINPnkPwzXFBHI/fNjwxeFKFexZdLh0tg07kxms6kqcxMeS7FY40G8C5TF2hrZBIZ5EdUUrY7uCdTjj6wAFQ8tpIhKC3oWGLpw+6ONZ3786jc5PqdE98PotqPzU3xsJlEsrMlYcByKpQtdIOZBlMDxtAS4IQ5RxIJQ/lzaO/154ZEvOh/LyZgX5yyiUxb9f8afPc9gd1kenp4+5PAERuW65cJJGG5e+Pblq+/+8Nmn8sfzN+msZv5ACwAA"
}
}

trait VerticesDsl extends impl.VerticesAbs {self: GraphsDsl =>}
trait VerticesDslStd extends impl.VerticesStd {self: GraphsDslStd =>}
trait VerticesDslExp extends impl.VerticesExp {self: GraphsDslExp =>}
