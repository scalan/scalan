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

trait Structs { self: Scalan =>
}

trait StructsSeq extends Structs { self: ScalanSeq =>
}

trait StructTags {
  abstract class StructTag[+T]
  case class SimpleTag[T](name: String) extends StructTag[T]
//  case class NestClassTag[C[_],T](elem: StructTag[T]) extends StructTag[C[T]]
//  case class AnonTag[T](fields: RefinedManifest[T]) extends StructTag[T]
//  case class MapTag[T]() extends StructTag[T]
}

trait StructsExp extends Expressions with Structs with StructTags with EffectsExp
    with Utils with GraphVizExport { self: ScalanExp =>

  case class StructElem[T](items: Seq[(String, Elem[Any])]) extends Element[T] {
    override def isEntityType = items.exists(_._2.isEntityType)
    lazy val tag = { ???
//      implicit val tA = eFst.tag
//      implicit val tB = eSnd.tag
//      weakTypeTag[(A, B)]
    }
    protected def getDefaultRep = ???

    def get(fieldName: String): Option[Elem[Any]] = items.find(_._1 == fieldName).map(_._2)
  }

  object StructElem {
    def fromSymbols[T](items: Seq[(String, Rep[Any])]): Elem[T] = {
      val e = StructElem[T](items.map { case (f, s) => (f, s.elem) })
      e
    }
  }

  abstract class AbstractStruct[T] extends Def[T] {
    def tag: StructTag[T]
    def elems: Seq[(String, Rep[Any])]
    lazy val selfType = StructElem.fromSymbols[T](elems)
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

  def struct[T](tag: StructTag[T], elems: (String, Rep[Any])*)(implicit o: Overloaded1): Rep[T] = struct[T](tag, elems)
  def struct[T](tag: StructTag[T], elems: Seq[(String, Rep[Any])]): Rep[T] = SimpleStruct(tag, elems)
  def field[T](struct: Rep[Any], index: String): Rep[T] = FieldApply[T](struct, index)

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
//    case FieldUpdate(s,x,b) => Nil
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Exp[Any]] = e match {
    case SimpleStruct(tag,elems) => elems.collect { case (k, v: Sym[_]) => v }.toList
    case FieldApply(s,x) => Nil
//    case FieldUpdate(s,x,b) => syms(b)
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Exp[Any]] = e match {
    case SimpleStruct(tag,elems) => Nil
    case FieldApply(s,x) => syms(s)
//    case FieldUpdate(s,x,b) => Nil
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Exp[Any]] = e match {
    case SimpleStruct(tag,elems) => Nil
    case FieldApply(s,x) => Nil
//    case FieldUpdate(s,x,b) => Nil
    case _ => super.copySyms(e)
  }



  // TODO: read/write/copy summary

//  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
//    case SimpleStruct(tag, elems) => struct(tag, elems map { case (k,v) => (k, f(v)) })(mtype(manifest[A]),pos)
//    case FieldApply(struct, key) => field(f(struct), key)(mtype(manifest[A]),pos)
//    case Reflect(FieldApply(struct, key), u, es) => reflectMirrored(Reflect(FieldApply(f(struct), key), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
//    case Reflect(FieldUpdate(struct, key, rhs), u, es) => reflectMirrored(Reflect(FieldUpdate(f(struct), key, f(rhs)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
//    case Reflect(SimpleStruct(tag, elems), u, es) => reflectMirrored(Reflect(SimpleStruct(tag, elems map { case (k,v) => (k, f(v)) }), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
//    case _ => super.mirror(e,f)
//  }).asInstanceOf[Exp[A]]

//  def structName[T](m: Manifest[T]): String = m match {
//    // FIXME: move to codegen? we should be able to have different policies/naming schemes
//    case rm: RefinedManifest[_] => "Anon" + math.abs(rm.fields.map(f => f._1.## + f._2.toString.##).sum)
//    case _ if (m <:< manifest[AnyVal]) => m.toString
//    case _ if m.erasure.isArray => "ArrayOf" + structName(m.typeArguments.head)
//    case _ => m.erasure.getSimpleName + m.typeArguments.map(a => structName(a)).mkString("")
//  }

//  def classTag[T:Manifest] = ClassTag[T](structName(manifest[T]))

//  override def object_tostring(x: Exp[Any]): Exp[String] = x match {
//    case Def(s@Struct(tag, elems)) => //tag(elem1, elem2, ...)
//      val e = elems.map(e => string_plus(unit(e._1 + " = "), object_tostring(e._2))).reduceLeft((l,r)=>string_plus(string_plus(l,unit(", ")),r))
//      string_plus(unit(structName(s.tp)+"("),string_plus(e,unit(")")))
//    case _ => super.object_tostring(x)
//  }

//  def registerStruct[T](name: String, elems: Seq[(String, Rep[Any])]) {
//    encounteredStructs += name -> elems.map(e => (e._1, e._2.tp))
//  }
//  val encounteredStructs = new scala.collection.mutable.HashMap[String, Seq[(String, Manifest[_])]]
}


