package scalan.primitives

import scalan.common.Utils
import scalan.compilation.{GraphVizConfig, GraphVizExport}
import scalan.staged.Expressions
import scalan.{ScalanExp, ScalanSeq, Scalan}
import scalan.common.OverloadHack._
import scala.reflect.{Manifest}
import scala.reflect.runtime._
import universe._

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

trait StructTags {
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
  }

  def structElement(fields: Seq[(String, Elem[Any])]): StructElem[_] =
    cachedElem[StructElem[_]](fields)

  def struct(fields: (String, Rep[Any])*): Rep[_]
  def struct(tag: StructTag, fields: Seq[(String, Rep[Any])]): Rep[_]
  def field(struct: Rep[Any], field: String): Rep[_]
  def fields(struct: Rep[Any], fields: Seq[String]): Rep[_]
}

trait StructsSeq extends Structs { self: ScalanSeq =>
  def struct(fields: (String, Rep[Any])*): Rep[_] = ???
  def struct(tag: StructTag, fields: Seq[(String, Rep[Any])]): Rep[_] = ???
  def field(struct: Rep[Any], field: String): Rep[_] = ???
  def fields(struct: Rep[Any], fields: Seq[String]): Rep[_] = ???
}

trait StructsExp extends Expressions with Structs with StructTags with EffectsExp
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
    case FieldApply(struct, fn) => s"$struct.$fn"
    case _ => super.formatDef(d)
  }

}


