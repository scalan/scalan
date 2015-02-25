package scalan.compilation.lms.cxx

import java.io.PrintWriter

import scala.virtualization.lms.common._

/**
 * Created by zotov on 12/2/14.
 */
//TODO: there are many modifications and thinking required
trait CXXGenStruct extends CLikeGenBase with BaseGenStruct with CXXCodegen {
  val IR: StructExp
  import IR._

  override def traverseStm(stm: Stm): Unit = {
    stm match {
      case TP(sym,rhs) =>
        rhs match {
          case Struct(tag, elems) =>
            sym.tp match {
              case tup2m if tup2m.runtimeClass == classOf[Tuple2[_,_]] =>
                moveableSyms += sym
              case _ =>
                ()
            }
          case _ =>
            ()
        }
      case _ =>
        ()
    }
    super.traverseStm(stm)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Struct(tag, elems) =>
      sym.tp match {
        case tup2m if tup2m.runtimeClass == classOf[Tuple2[_,_]] =>
          emitConstruct( sym, elems.map(e => quoteMove(e._2)).mkString(","))
        case _ =>
          registerStruct(structName(sym.tp), elems)
          emitValDef(sym, "new " + structName(sym.tp) + "(" + elems.map(e => quote(e._2)).mkString(",") + ")")
      }
    case FieldApply(struct, index) =>
      struct.tp match {
        case m if m.runtimeClass == classOf[scala.Tuple2[_,_]] =>
          val fn = index match { case "_1" => "first"; case "_2" => "second" }
          emitValDef(sym, s"${quote(struct)}.${fn}")
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
