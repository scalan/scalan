package scalan.primitives

import scalan.common.Utils
import scalan.compilation.{GraphVizConfig, GraphVizExport}
import scalan.staged.Expressions
import scalan._
import scalan.common.OverloadHack._
import scala.reflect.{Manifest}
import scala.reflect.runtime._
import universe._
import scalan.compilation.Compiler

/**
 The code is taken from LMS and is used in Scalan with the same semantics
 in order to easily translate operations to the equivalents via LmsBridge.
 Their usage in Scalan is limited to be consistent with functional semantics of Scalan.
 Don't expect everything possible in LMS to be also possible in Scalan in the same way.
 There are changes in the code:
 - Sym -> Exp
 - Manifest -> Elem
 - infix -> implicit class
 - no SourceContext, withPos
 - mirroring implemented in Scalan way (though consistent with LMS)
 */

trait StructTags extends Base { self: Scalan =>
  abstract class StructTag
  case class SimpleTag(name: String) extends StructTag
  //  case class NestClassTag[C[_],T](elem: StructTag[T]) extends StructTag[C[T]]
  //  case class AnonTag[T](fields: RefinedManifest[T]) extends StructTag[T]
  //  case class MapTag[T]() extends StructTag[T]
}

trait Structs extends StructTags { self: Scalan =>

  case class StructElem[T](fields: Seq[(String, Elem[Any])]) extends Element[T] {
    override def isEntityType = fields.exists(_._2.isEntityType)
    lazy val tag = typeTag[Product].asInstanceOf[WeakTypeTag[T]]
    protected def getDefaultRep = {
      val res = struct(fields.map { case (fn,fe) => (fn, fe.defaultRepValue) }: _*)
      res.asRep[T]
    }
    def get(fieldName: String): Option[Elem[Any]] = fields.find(_._1 == fieldName).map(_._2)
    override def canEqual(other: Any) = other.isInstanceOf[StructElem[_]]
    override def toString = s"${getClass.getSimpleName}{${fields.map { case (fn,fe) => s"$fn: $fe"}.mkString(";")}}"
    def fieldElems: Seq[Elem[Any]] = fields.map(_._2)
  }

  /**
   * Get tuple field name by index
   */
  def tupleFN(fieldIndex: Int) = s"_$fieldIndex"

  def structElement(fields: Seq[(String, Elem[Any])]): StructElem[_] =
    cachedElem[StructElem[_]](fields)

  def structElement(fields: Seq[Elem[_]])(implicit o: Overloaded1): StructElem[_] =
    cachedElem[StructElem[_]](fields.zipWithIndex.map { case (f, i) => tupleFN(i + 1) -> f })

  def structElem2[A:Elem, B:Elem]: StructElem[_] =
    structElement(Seq(element[A], element[B]))

  class PairStructIso[S, A, B](implicit eA: Elem[A], eB: Elem[B])
    extends Iso[S, (A,B)]()(structElem2[A,B].asElem[S]) {
    override def from(p: Rep[(A,B)]) =
      structFromPair(p).asRep[S]
    override def to(struct: Rep[S]) = {
      (field(struct, 1).asRep[A], field(struct, 2).asRep[B])
    }
    lazy val eTo = element[(A,B)]
  }

  def pairStructIso[S, A, B](implicit eA: Elem[A], eB: Elem[B]): Iso[S, (A,B)] =
    cachedIso[PairStructIso[S,A,B]](eA, eB)

  implicit class StructOps(s: Rep[_]) {
    def apply(iField: Int): Rep[_] = field(s, iField)
    def apply(fieldName: String): Rep[_] = field(s, fieldName)
  }

  def struct(fields: (String, Rep[Any])*): Rep[_]
  def struct(tag: StructTag, fields: Seq[(String, Rep[Any])]): Rep[_]
  def field(struct: Rep[Any], field: String): Rep[_]
  def field(struct: Rep[Any], fieldIndex: Int): Rep[_] = field(struct, tupleFN(fieldIndex))
  def fields(struct: Rep[Any], fields: Seq[String]): Rep[_]
  def structFromPair[A,B](p: Rep[(A,B)]): Rep[_] = struct(tupleFN(1) -> p._1, tupleFN(2) -> p._2)
  def tuple(items: Rep[Any]*): Rep[_] = struct(items.zipWithIndex.map { case (item, i) => tupleFN(i + 1) -> item }: _*)

  case class Link(field: String, nestedField: String, nestedElem: Elem[_], flatIndex: Int)

  class FlatteningIso[T](val eTo: StructElem[T], val flatIsos: Map[String, Iso[Any,Any]], links: Seq[Link])
      extends Iso[Any,T]()(structElement(links.map(_.nestedElem)).asElem[Any]) {

    val groups = links.groupBy(_.field)

    def to(x: Rep[Any]) = {
      val items = eTo.fields.map { case (fn, fe) =>
        val g = groups(fn)
        flatIsos.get(fn) match {
          case Some(iso) =>
            val projectedStruct = struct(g.map(link => (link.nestedField -> x(link.flatIndex))): _*)
            val s = iso.to(projectedStruct)
            (fn -> s)
          case None =>
            assert(g.length == 1, s"Many fields $g can't relate to the single field $fn without iso")
            (fn -> x(g(0).flatIndex))
        }
      }
      struct(items: _*).asRep[T]
    }

    def from(y: Rep[T]) = {
      val items = eTo.fields.flatMap { case (fn, fe) =>
        val g = groups(fn)
        flatIsos.get(fn) match {
          case Some(iso) =>
            val nestedStruct = iso.from(y(fn))
            g.map(link =>
              tupleFN(link.flatIndex) -> nestedStruct(link.nestedField))
          case None =>
            List(tupleFN(g(0).flatIndex) -> y(fn))
        }
      }
      struct(items: _*).asRep[T]
    }
  }

  /**
   * Flattens top level tree of structs in [[eTo]]. Types other than structs are considered as leaves.
   * @param eTo descriptor of struct type
   * @return an isomorphism [[i]] in which [[eTo]] is given by param and [[eFrom]] is flattened [[eTo]] preserving
   *         related order of the components
   */
  def flatteningIso[T](eTo: StructElem[T]): Iso[_,T] = {
    val flatIsos = eTo.fields.collect {
      case (fn, fe: StructElem[_]) => (fn, flatteningIso(fe).asInstanceOf[Iso[Any,Any]])
      }.toMap

    if (flatIsos.isEmpty) return identityIso(eTo)

    // relate resulting field types by original field name
    val fromFields = eTo.fields.flatMap {
      case (fn, fe) =>
        flatIsos.get(fn) match {
          case Some(iso) =>
            iso.eFrom match {
              case flatElem: StructElem[_] =>
                flatElem.fields.map { case (nestedName, nestedE) => (fn, nestedName -> nestedE) }
              case _ => !!!(s"StructElem is expected as eFrom of flattened Iso $iso")
            }
          case None => List((fn, "" -> fe))
        }
    }

    val links = fromFields.zipWithIndex.map {
      case ((fn, (nestedN, nestedE)), i) => Link(fn, nestedN, nestedE, i + 1)
    }

    val res = new FlatteningIso(eTo, flatIsos, links)
    res.asInstanceOf[Iso[_,T]]
  }
}

trait StructsSeq extends Structs { self: ScalanSeq =>
  def struct(fields: (String, Rep[Any])*): Rep[_] = ???
  def struct(tag: StructTag, fields: Seq[(String, Rep[Any])]): Rep[_] = ???
  def field(struct: Rep[Any], field: String): Rep[_] = ???
  def fields(struct: Rep[Any], fields: Seq[String]): Rep[_] = ???
}

trait StructsExp extends Expressions with Structs with StructTags with EffectsExp with ViewsExp
    with Utils with GraphVizExport { self: ScalanExp =>


  abstract class AbstractStruct[T] extends Def[T] {
    def tag: StructTag
    def fields: Seq[(String, Rep[Any])]
    lazy val selfType = structElement(fields).asElem[T]
  }

  abstract class AbstractField[T] extends Def[T] {
    def struct: Rep[Any]
    def field: String
    lazy val selfType = Struct.getFieldElem(struct, field).asElem[T]
  }

  object Struct {
    def getFieldElem(struct: Rep[Any], fn: String): Elem[Any] = struct.elem match {
      case se: StructElem[_] =>
        se.get(fn).get
      case _ => !!!(s"Symbol with StructElem expected but found ${struct.elem}", struct)
    }

    def unapply[T](d: Def[T]) = unapplyStruct(d)
  }

  def unapplyStruct[T](d: Def[T]): Option[(StructTag, Seq[(String, Rep[Any])])] = d match {
    case s: AbstractStruct[T] => Some((s.tag, s.fields))
    case _ => None
  }

  object Field {
    def unapply[T](d: Def[T]) = unapplyField(d)
  }

  def unapplyField[T](d: Def[T]): Option[(Rep[Any], String)] = d match {
    case f: AbstractField[T] => Some((f.struct, f.field))
    case _ => None
  }

  case class SimpleStruct[T](tag: StructTag, fields: Seq[(String, Rep[Any])]) extends AbstractStruct[T]
  case class FieldApply[T](struct: Rep[Any], field: String) extends AbstractField[T]
  case class ProjectionStruct[T](struct: Rep[Any], outFields: Seq[String]) extends AbstractStruct[T] {
    val tag = SimpleTag("struct")
    val fields = outFields.map(fn => (fn, field(struct, fn).asRep[Any]))
  }

  def struct(fields: (String, Rep[Any])*): Rep[_] = struct(SimpleTag("struct"), fields)
  def struct(tag: StructTag, fields: (String, Rep[Any])*)(implicit o: Overloaded2): Rep[_] = struct(tag, fields)
  def struct(tag: StructTag, fields: Seq[(String, Rep[Any])]): Rep[_] = reifyObject(SimpleStruct(tag, fields))
  def field(struct: Rep[Any], field: String): Rep[_] = FieldApply[Any](struct, field)
  def fields(struct: Rep[Any], fields: Seq[String]): Rep[_] = ProjectionStruct[Any](struct, fields)

  def structElement(items: Seq[(String, Rep[Any])])(implicit o1: Overloaded1): Elem[_] = {
    val e = StructElem(items.map { case (f, s) => (f, s.elem) })
    e
  }

  override def syms(e: Any): List[Exp[Any]] = e match {
    case s: ProjectionStruct[_] => syms(s.struct)
    case s: AbstractStruct[_] => s.fields.flatMap(e => this.syms(e._2)).toList
    case _ => super.syms(e)
  }

  override def symsFreq(e: Any): List[(Exp[Any], Double)] = e match {
    case s: ProjectionStruct[_] => symsFreq(s.struct)
    case s: AbstractStruct[_] => s.fields.flatMap(e => symsFreq(e._2)).toList
    case _ => super.symsFreq(e)
  }

  override def effectSyms(e: Any): List[Exp[Any]] = e match {
    case s: ProjectionStruct[_] => effectSyms(s.struct)
    case s: AbstractStruct[_] => s.fields.flatMap(e => effectSyms(e._2)).toList
    case _ => super.effectSyms(e)
  }

  override def readSyms(e: Any): List[Exp[Any]] = e match {
    case s: AbstractStruct[_] => Nil //struct creation doesn't de-reference any of its inputs
    case _ => super.readSyms(e)
  }

  override def aliasSyms(e: Any): List[Exp[Any]] = e match {
    case SimpleStruct(tag,fields) => Nil
    case FieldApply(s,x) => Nil
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Exp[Any]] = e match {
    case SimpleStruct(tag,fields) => fields.collect { case (k, v: Sym[_]) => v }.toList
    case FieldApply(s,x) => Nil
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Exp[Any]] = e match {
    case SimpleStruct(tag,fields) => Nil
    case FieldApply(s,x) => syms(s)
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Exp[Any]] = e match {
    case SimpleStruct(tag,fields) => Nil
    case FieldApply(s,x) => Nil
    case _ => super.copySyms(e)
  }

  override protected def formatDef(d: Def[_])(implicit config: GraphVizConfig): String = d match {
    case SimpleStruct(tag, fields) =>
      val name = tag match { case SimpleTag(_) => "" case _ => ""}
      s"$name{${fields.map { case (fn, s) => s"$fn:$s" }.mkString(";")}}"

    case ProjectionStruct(struct, outs) => s"$struct.${outs.mkString("[",",", "]")}"
    case FieldApply(struct, fn) => s"$struct.[$fn]"
    case _ => super.formatDef(d)
  }

  case class ViewStruct[A, B](source: Exp[A])(val iso: Iso[A, B])
    extends View[A, B] {
    override def toString = s"ViewStruct[${iso.eTo.name}]($source)"
    override def equals(other: Any) = other match {
      case v: ViewStruct[_, _] => source == v.source && iso.eTo == v.iso.eTo
      case _ => false
    }
  }

  override def unapplyViews[T](s: Exp[T]): Option[Unpacked[T]] = (s match {
    case Def(view: ViewStruct[a, b]) =>
      Some((view.source, view.iso))
    case _ =>
      super.unapplyViews(s)
  }).asInstanceOf[Option[Unpacked[T]]]

}

trait StructsCompiler[ScalanCake <: ScalanCtxExp with StructsExp] extends Compiler[ScalanCake] {
  import scalan._

  object StructsRewriter extends Rewriter {
    def apply[T](x: Exp[T]): Exp[T] = (x match {
      case Def(Tup(a, b)) => struct(tupleFN(1) -> a, tupleFN(2) -> b)
      case _ => x
    }).asRep[T]
  }

  override def graphPasses(compilerConfig: CompilerConfig) =
    super.graphPasses(compilerConfig) :+
      constantPass(StructsPass(DefaultMirror, StructsRewriter))

  case class StructsPass(mirror: Mirror[MapTransformer], rewriter: Rewriter) extends GraphPass {
    def name = "structs"

    def apply(graph: PGraph): PGraph = {
      graph.transform(mirror, rewriter, MapTransformer.Empty)
    }
  }

}


