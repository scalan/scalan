package scalan.primitives

import scalan.common.Utils
import scalan.compilation.GraphVizExport
import scalan.staged.Expressions
import scalan.{ScalanExp, ScalanSeq, Scalan}
import scalan.common.OverloadHack._
import scala.reflect.{Manifest}
import scala.reflect.runtime._
import universe.internal._

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

trait RefinedManifest[T] extends Manifest[T] {
  override def canEqual(other: Any) = other match {
    case _: RefinedManifest[_] => true
    case _                     => false
  }

  /** Tests whether the type represented by this manifest is equal to
    * the type represented by `that` manifest, subject to the limitations
    * described in the header.
    */
  override def equals(that: Any): Boolean = that match {
    case m: RefinedManifest[_] => (m canEqual this) && (this.erasure == m.erasure)
    case _                     => false
  }
  override def hashCode = this.erasure.##

  def fields: List[(String, Manifest[_])]
}

trait Structs extends StructTags { self: Scalan =>
  /** Manifest for the refined type
    * `parent { val fieldNames(0) : fieldTypes(0) ; ... ; val fieldNames(n) : fieldTypes(n) }`.
    */
  def refinedType[T](parent: Manifest[_], fieldNames: List[String], fieldTypes: List[Manifest[_]]): Manifest[T] =
    new RefinedManifest[T] {
      def runtimeClass = parent.runtimeClass
      def fields = fieldNames zip fieldTypes
      override def toString = parent + (fieldNames zip fieldTypes).map{case(n, t) => "val "+ n +" : "+ t}.mkString("{","; ", "}")
    }

  case class StructElem[T](fields: Seq[(String, Elem[Any])]) extends Element[T] {
    override def isEntityType = fields.exists(_._2.isEntityType)
    lazy val tag = {
      val fieldNames = fields.map(_._1).toList
      val fieldTypes = fields.map { case (fn, fe) => elemToManifest(fe) }.toList
      val parent = manifest[Product]
      val structManifest = refinedType[T](parent, fieldNames, fieldTypes)
      val tt = manifestToTypeTag(currentMirror, structManifest).asInstanceOf[universe.WeakTypeTag[T]]
      tt
    }
    protected def getDefaultRep = struct(fields.map { case (fn,fe) => (fn, fe.defaultRepValue) }: _*)

    def get(fieldName: String): Option[Elem[Any]] = fields.find(_._1 == fieldName).map(_._2)
    override def canEqual(other: Any) = other.isInstanceOf[StructElem[_]]
  }

  def structElement[A](fields: Seq[(String, Elem[Any])]): Elem[A] =
    cachedElem[StructElem[A]](fields)

  def struct[T](fields: (String, Rep[Any])*): Rep[T]
  def struct[T](tag: StructTag[T], fields: Seq[(String, Rep[Any])]): Rep[T]
  def field[T](struct: Rep[Any], index: String): Rep[T]
}

trait StructsSeq extends Structs { self: ScalanSeq =>
  def struct[T](fields: (String, Rep[Any])*): Rep[T] = ???
  def struct[T](tag: StructTag[T], fields: Seq[(String, Rep[Any])]): Rep[T] = ???
  def field[T](struct: Rep[Any], index: String): Rep[T] = ???
}

trait StructsExp extends Expressions with Structs with StructTags with EffectsExp
    with Utils with GraphVizExport { self: ScalanExp =>


  abstract class AbstractStruct[T] extends Def[T] {
    def tag: StructTag[T]
    def fields: Seq[(String, Rep[Any])]
    lazy val selfType = structElement[T](fields)
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
    case s: AbstractStruct[T] => Some((s.tag, s.fields))
    case _ => None
  }

  object Field {
    def unapply[T](d: Def[T]) = unapplyField(d)
  }

  def unapplyField[T](d: Def[T]): Option[(Rep[Any], String)] = d match {
    case f: AbstractField[T] => Some((f.struct, f.index))
    case _ => None
  }

  case class SimpleStruct[T](tag: StructTag[T], fields: Seq[(String, Rep[Any])]) extends AbstractStruct[T]
  case class FieldApply[T](struct: Rep[Any], index: String) extends AbstractField[T]

  def struct[T](fields: (String, Rep[Any])*): Rep[T] = struct[T](SimpleTag("struct"), fields)
  def struct[T](tag: StructTag[T], fields: (String, Rep[Any])*)(implicit o: Overloaded2): Rep[T] = struct[T](tag, fields)
  def struct[T](tag: StructTag[T], fields: Seq[(String, Rep[Any])]): Rep[T] = SimpleStruct(tag, fields)
  def field[T](struct: Rep[Any], index: String): Rep[T] = FieldApply[T](struct, index)

  def structElement[A](items: Seq[(String, Rep[Any])])(implicit o1: Overloaded1): Elem[A] = {
    val e = StructElem[A](items.map { case (f, s) => (f, s.elem) })
    e
  }

  override def syms(e: Any): List[Exp[Any]] = e match {
    case s: AbstractStruct[_] => s.fields.flatMap(e => this.syms(e._2)).toList
    case _ => super.syms(e)
  }

  override def symsFreq(e: Any): List[(Exp[Any], Double)] = e match {
    case s: AbstractStruct[_] => s.fields.flatMap(e => symsFreq(e._2)).toList
    case _ => super.symsFreq(e)
  }

  override def effectSyms(e: Any): List[Exp[Any]] = e match {
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

}


