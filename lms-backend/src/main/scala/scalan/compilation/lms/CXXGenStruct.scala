package scalan.compilation.lms

import java.io.PrintWriter

import scala.virtualization.lms.common._

/**
 * Created by zotov on 12/2/14.
 */
//TODO: there are many modifications and thinking required
trait CXXGenStruct extends CLikeGenBase with BaseGenStruct with CXXCodegen {
  val IR: StructExp
  import IR._

//  def structName[T](m: Manifest[T]): String = m match {
//    case rm: RefinedManifest[_] => "Anon" + math.abs(rm.fields.map(f => f._1.## + f._2.toString.##).sum)
//    case _ if (m <:< manifest[AnyVal]) => m.toString
//    case _ if m.erasure.isArray => "ArrayOf" + structName(m.typeArguments.head)
//    case _ if m.runtimeClass == classOf[scala.Tuple2[_,_]] =>
//      s"std::tuple<${remap(m.typeArguments(0))},${remap(m.typeArguments(1))}>"
//    case _ => m.erasure.getSimpleName + m.typeArguments.map(a => structName(a)).mkString("")
//  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Struct(tag, elems) =>
      registerStruct(structName(sym.tp), elems)
      emitValDef(sym, "new " + structName(sym.tp) + "(" + elems.map(e => quote(e._2)).mkString(",") + ")")
    case FieldApply(struct, index) =>
      struct.tp match {
        case m if m.runtimeClass == classOf[scala.Tuple2[_,_]] =>
          val fn = index match { case "_1" => "first"; case "_2" => "second" }
          emitValDef(quote(sym), manifest[auto_t], s"${quote(struct)}.${fn}")
        case _ =>
          emitValDef(sym, quote(struct) + "." + index)
      }
    case FieldUpdate(struct, index, rhs) =>
      emitValDef(sym, quote(struct) + "." + index + " = " + quote(rhs))
    case _ => super.emitNode(sym, rhs)
  }

  override def remap[A](m: Manifest[A]) = m match {
    case s if s <:< manifest[Record] => structName(m)
    case _ => super.remap(m)
  }

  override def emitDataStructures(stream: PrintWriter) {
    for ((name, elems) <- encounteredStructs) {
      stream.println()
      stream.print("case class " + name + "(")
      stream.println(elems.map(e => e._1 + ": " + remap(e._2)).mkString(", ") + ")")
    }
    stream.flush()
    super.emitDataStructures(stream)
  }
}
