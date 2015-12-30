package scalan.compilation.lms.cxx.sharedptr

import java.io.PrintWriter

import scala.lms.common.{BaseGenStruct, Record, StructExp}
import scala.lms.internal.GenerationFailedException
import scala.reflect.RefinedManifest

trait CxxShptrGenStruct  extends CxxShptrCodegen with BaseGenStruct {
  val IR: StructExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Struct(tag, elems) =>
      sym.tp match {
        case tup2m if tup2m.runtimeClass == classOf[Tuple2[_,_]] =>
          emitConstruct(sym, elems.map(e => quote(e._2)).mkString(","))
        case _ =>
          registerStruct(structName(sym.tp), elems)
          emitValDef(sym, src"std::make_tuple(${elems.map(_._2)})")
      }
    case FieldApply(struct, index) =>
      struct.tp match {
        case m if m.runtimeClass == classOf[scala.Tuple2[_,_]] =>
          emitValDef(sym, src"$struct.${stdPairFieldName(index)}")
        case rm: RefinedManifest[_] =>
          val i = fieldIndexByName(struct, index, rm)
          emitValDef(sym, src"std::get<$i>($struct)")
      }
    case FieldUpdate(struct, index, rhs) =>
      struct.tp match {
        case m if m.runtimeClass == classOf[scala.Tuple2[_,_]] =>
          emitValDef(sym, src"$struct.${stdPairFieldName(index)} = $rhs")
        case rm: RefinedManifest[_] =>
          val i = fieldIndexByName(struct, index, rm)
          emitValDef(sym, src"std::get<$i>($struct) = $rhs")
      }
//      emitValDef(sym, src"$struct.$index = $rhs")
    case _ => super.emitNode(sym, rhs)
  }

  def stdPairFieldName(index: String): String = {
    index match {case "_1" => "first"; case "_2" => "second"}
  }

  def fieldIndexByName(struct: Exp[Any], index: String, rm: RefinedManifest[_]) = {
    rm.fields.zipWithIndex.find { case ((name, _), _) => name == index } match {
      case Some((_, i)) => i.toString
      case None => throw new GenerationFailedException(s"Field $index not found in $struct (manifest $rm)!")
    }
  }

  override def remap[A](m: Manifest[A]) = m match {
//    case s if s <::< manifest[Record] => structName(m)
    case rm: RefinedManifest[_] =>
      src"std::tuple<${rm.fields.map(_._2)}>"
    case _ => super.remap(m)
  }

  override protected def doNotWrap(m: Manifest[_]) = m.isInstanceOf[RefinedManifest[_]] ||
    super.doNotWrap(m)

  // TODO generate structs? For now we use std::tuple
//  override def emitDataStructures(stream: PrintWriter) {
//    for ((name, elems) <- encounteredStructs) {
//      stream.println()
//      stream.print("case class " + name + "(")
//      stream.println(elems.map(e => e._1 + ": " + remap(e._2)).mkString(", ") + ")")
//    }
//    stream.flush()
//    super.emitDataStructures(stream)
//  }
}
