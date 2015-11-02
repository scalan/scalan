package scalan.primitives

import scalan.common.Utils
import scalan.compilation.GraphVizExport
import scalan.staged.Expressions
import scalan.{ScalanExp, ScalanSeq, Scalan}
import scalan.common.OverloadHack._
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
  abstract class StructTag[+T]
  case class SimpleTag[T](name: String) extends StructTag[T]
  //  case class NestClassTag[C[_],T](elem: StructTag[T]) extends StructTag[C[T]]
  //  case class AnonTag[T](fields: RefinedManifest[T]) extends StructTag[T]
  //  case class MapTag[T]() extends StructTag[T]
}

trait Structs extends StructTags { self: Scalan =>

  case class StructElem[T](items: Seq[(String, Elem[Any])]) extends Element[T] {
    override def isEntityType = items.exists(_._2.isEntityType)
    lazy val tag = {
//      val fieldNames = items.map(_._1).toList
//      val fieldTypes = items.map { case (fn, fe) => ???/*elemToManifest(fe)*/ }.toList
//      val parent = manifest[Product]
//      Manifest.refinedType[T](parent, fieldNames, fieldTypes)
???
    }
    protected def getDefaultRep = struct(items.map { case (fn,fe) => (fn, fe.defaultRepValue) }: _*)

    def get(fieldName: String): Option[Elem[Any]] = items.find(_._1 == fieldName).map(_._2)
    override def canEqual(other: Any) = other.isInstanceOf[StructElem[_]]
  }

  def structElement[A](items: Seq[(String, Elem[Any])]): Elem[A] =
    cachedElem[StructElem[A]](items)

  def struct[T](elems: (String, Rep[Any])*): Rep[T]
  def struct[T](tag: StructTag[T], elems: Seq[(String, Rep[Any])]): Rep[T]
  def field[T](struct: Rep[Any], index: String): Rep[T]
}

trait StructsSeq extends Structs { self: ScalanSeq =>
  def struct[T](elems: (String, Rep[Any])*): Rep[T] = ???
  def struct[T](tag: StructTag[T], elems: Seq[(String, Rep[Any])]): Rep[T] = ???
  def field[T](struct: Rep[Any], index: String): Rep[T] = ???
}

trait StructsExp extends Expressions with Structs with StructTags with EffectsExp
    with Utils with GraphVizExport { self: ScalanExp =>


  abstract class AbstractStruct[T] extends Def[T] {
    def tag: StructTag[T]
    def elems: Seq[(String, Rep[Any])]
    lazy val selfType = structElement[T](elems)
  }

  abstract class AbstractField[T] extends Def[T] {
    def struct: Rep[Any]
    def index: String
    lazy val selfType = Struct.getFieldElem(struct, index).asElem[T]
  }

  object Struct {
    def getFieldElem(struct: Rep[Any], fn: String): Elem[Any] = struct.elem match {
      case se: StructElem[_] =>
        se.get(fn).get
      case _ => !!!(s"Symbol with StructElem expected but found ${struct.elem}", struct)
    }

    def unapply[T](d: Def[T]) = unapplyStruct(d)
  }

  def unapplyStruct[T](d: Def[T]): Option[(StructTag[T], Seq[(String, Rep[Any])])] = d match {
    case s: AbstractStruct[T] => Some((s.tag, s.elems))
    case _ => None
  }

  object Field {
    def unapply[T](d: Def[T]) = unapplyField(d)
  }

  def unapplyField[T](d: Def[T]): Option[(Rep[Any], String)] = d match {
    case f: AbstractField[T] => Some((f.struct, f.index))
    case _ => None
  }

  case class SimpleStruct[T](tag: StructTag[T], elems: Seq[(String, Rep[Any])]) extends AbstractStruct[T]
  case class FieldApply[T](struct: Rep[Any], index: String) extends AbstractField[T]

  def struct[T](elems: (String, Rep[Any])*): Rep[T] = struct[T](SimpleTag("struct"), elems)
  def struct[T](tag: StructTag[T], elems: (String, Rep[Any])*)(implicit o: Overloaded2): Rep[T] = struct[T](tag, elems)
  def struct[T](tag: StructTag[T], elems: Seq[(String, Rep[Any])]): Rep[T] = SimpleStruct(tag, elems)
  def field[T](struct: Rep[Any], index: String): Rep[T] = FieldApply[T](struct, index)

  def structElement[A](items: Seq[(String, Rep[Any])])(implicit o1: Overloaded1): Elem[A] = {
    val e = StructElem[A](items.map { case (f, s) => (f, s.elem) })
    e
  }

  override def syms(e: Any): List[Exp[Any]] = e match {
    case s: AbstractStruct[_] => s.elems.flatMap(e => this.syms(e._2)).toList
    case _ => super.syms(e)
  }

  override def symsFreq(e: Any): List[(Exp[Any], Double)] = e match {
    case s: AbstractStruct[_] => s.elems.flatMap(e => symsFreq(e._2)).toList
    case _ => super.symsFreq(e)
  }

  override def effectSyms(e: Any): List[Exp[Any]] = e match {
    case s: AbstractStruct[_] => s.elems.flatMap(e => effectSyms(e._2)).toList
    case _ => super.effectSyms(e)
  }

  override def readSyms(e: Any): List[Exp[Any]] = e match {
    case s: AbstractStruct[_] => Nil //struct creation doesn't de-reference any of its inputs
    case _ => super.readSyms(e)
  }

  override def aliasSyms(e: Any): List[Exp[Any]] = e match {
    case SimpleStruct(tag,elems) => Nil
    case FieldApply(s,x) => Nil
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Exp[Any]] = e match {
    case SimpleStruct(tag,elems) => elems.collect { case (k, v: Sym[_]) => v }.toList
    case FieldApply(s,x) => Nil
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Exp[Any]] = e match {
    case SimpleStruct(tag,elems) => Nil
    case FieldApply(s,x) => syms(s)
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Exp[Any]] = e match {
    case SimpleStruct(tag,elems) => Nil
    case FieldApply(s,x) => Nil
    case _ => super.copySyms(e)
  }

}


