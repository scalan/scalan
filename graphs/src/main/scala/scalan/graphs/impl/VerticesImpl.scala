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
    lazy val typeArgs = TypeArgs("V" -> eV, "E" -> eE)
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
    override lazy val typeArgs = TypeArgs("V" -> eV, "E" -> eE)

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
    def productElement(n: Int) = n match {
      case 0 => eV
      case 1 => eE
    }
  }
  case class SVertexIsoElem[V, E](eV: Elem[V], eE: Elem[E]) extends Elem[SVertexIso[V, E]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new SVertexIso[V, E]()(eV, eE))
    lazy val tag = {
      implicit val tagV = eV.tag
      implicit val tagE = eE.tag
      weakTypeTag[SVertexIso[V, E]]
    }
    lazy val typeArgs = TypeArgs("V" -> eV, "E" -> eE)
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
  val dump = "H4sIAAAAAAAAALVWT2gcVRh/O5vN7mZjGiMSLejGuLUiupumSoUgEpNNbNkkS6fdyrZE3s683Ux9M/OceRtmPdRbQb2JKAgeKoqXooi3Cl5UEBEPXj17qkrpwUJB8Xtv/uxMshNtxTk85v2Z78/v9/u+eVd/QznXQY+5GqbYqpqE46oq35ddXlHrFjf4YMPW+5Ssku72ix/ePm++Maug6TYa38HuqkvbqOi/1D0Wvatcb6AitjTicttxOXqkIT3UNJtSonHDtmqGafY57lBSaxguX2qgsY6tD15Fl1CmgaY129Icwom6QrHrEjdYLxARkRHNi3I+2GJDH1ZNZFGLZXHGwQaH8MHHtH/+NGHqwLKtgcnRVBDaFhNhwZkS8RjkcNJkVLrJNlDeMJnt8NBrHjzs2Ho4HbMwLKCZxkW8i2vgtVdTuWNYPWGMYe0V3CObcEQcH4McXEK7ZwaMBMZLLtcT/jyGEAJWFmVg1SFm1QizqsCsohLHwNR4DYvNpmN7A+Q/mSxCHgMTT/6DidACqVt65c0L2vlbaslUxMeeCCUvAxoHQ+UUhUh6ANvvTr/t3ly/ckJBE200YbjLHZc7WONxGQRwlbBl2VzGHCGInR4wOJ/GoPSyDGf2yKSo2SbDFlgKsJwEoqihGVwcFmuTAT0p2Oc5I+HRjMcyUb5zKflKLa1gSpvXH3zqyK/1lxSkJF0UwaQKxeCERjkabxGHEy8wLsZDHGVaQ4TFtC6nYih6wzF/QCwRKkev/65/u4AuKBGWget/Rx+YmHn2/WtHSPNTBRXaUu1rFPckkQKsVeJqbVSwd4njr+d3MRVvI8nM66SL+5QHEMexyQI2HM2lFiojArglWQCZMP2Sr+FN2yKVtWblD/X7d64KiTpo0t/xK/cv48SfP091uVQvR4qhh9BmodwjJB5NI5aRpmOY0Fx2yTNff3n2xlebOcntTJBOC9M+8es6yGaYmXCYWQBPJy3ucyf9HY7SEEOZo1zPwWwnjEtprt+tJCb8zFXbJPfO3zS2r7zFJfkZL9mHtjoXoe6X5HcPH6CDsEV+fvny/Tc+evk+WcaFjsFNzCoLd1DEYc39j0WKkqBNrQS/CqnlxeRmXvVLLwVIMc5Gez5LQOV08NVKPPBy7JOYk8OZPRwrpBV6H6tTYo6kOC6S/QbqBxnYL4pkmuVI6g+lSx3gtGbVjfc+K28rKHcK5bpQzW4D5Tp239JDnuAnDEb5C+FaJskT8IIdbEa8yGcODcFKKro58sD+dGL5Pr2H6yxINLnyH1prigjk/NhI9yUhyjVsGnSwODKMO5PZoVSVsYTFcszXaBDvAmUxtobWj4Feqil6WSUaxQ7RxQ2FmHCD8sv++LvPnzv1wLmzsvFM6vKQvxO1+NH3vQ3MluTt5PEDbidwqFI3Gdw+4eX4N8/99PoPn3wse/sQSI4KAiVDtCZ0TxC932KjpOZTklKDHgOKunTrg80nfvziF9nuJ0S3gl+NFd324m0+yXpxXfqCy1sMXqha0b9iMpK/BO9vDrFeQmsLAAA="
}
}

trait VerticesDsl extends impl.VerticesAbs {self: GraphsDsl =>}
trait VerticesDslStd extends impl.VerticesStd {self: GraphsDslStd =>}
trait VerticesDslExp extends impl.VerticesExp {self: GraphsDslExp =>}
