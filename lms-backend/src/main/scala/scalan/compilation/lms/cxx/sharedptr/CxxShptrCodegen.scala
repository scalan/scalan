package scalan.compilation.lms.cxx.sharedptr

import java.io.PrintWriter

import scala.virtualization.lms.internal.{CLikeCodegen, GenerationFailedException, Expressions, GenericCodegen}


trait CxxShptrCodegen extends CLikeCodegen {
  val IR: Expressions
  import IR._

  trait size_t
  trait SharedPtr[T]

  def toShptrManifest(m: Manifest[_]): Manifest[_] = {
    val nArgs = m.typeArguments.length
    val newM = nArgs match {
      case 0 => m
      case 1 => Manifest.classType(m.runtimeClass, toShptrManifest(m.typeArguments(0)))
      case n => Manifest.classType(m.runtimeClass, toShptrManifest(m.typeArguments(0)), m.typeArguments.drop(1).map(toShptrManifest): _*)
    }

    wrapSharedPtr(newM)
  }

  def wrapSharedPtr:PartialFunction[Manifest[_],Manifest[_]] = {
    case m if m <:< Manifest.AnyVal => m
    case m if m.runtimeClass == classOf[scala.Tuple2[_, _]] => m
    case m if m.runtimeClass == classOf[Variable[_]] => m
    case m if m.runtimeClass == classOf[_=>_] => m
    case m =>
      Manifest.classType(classOf[SharedPtr[_]], m)
  }

  final override def emitValDef(sym: Sym[Any], rhs: String ): Unit = {
    val newTp = toShptrManifest(sym.tp)
    emitValDef(quote(sym), newTp, rhs)
  }

  final override def emitValDef(sym: String, tpe: Manifest[_], rhs: String): Unit = {
    if( !isVoidType(tpe) ) {
        stream.println(src"${remap(tpe)} $sym = $rhs; /*emitValDef(): ${sym.toString}: ${tpe.toString} = ${rhs.toString}*/")
    }
  }

  override def remap[A](m: Manifest[A]) : String = {
    m match {
      case _ if m.runtimeClass == classOf[SharedPtr[_]] =>
        s"std::shared_ptr<${remap(m.typeArguments(0))}>"
      case _ if m.runtimeClass == classOf[size_t] =>
        "size_t"
      case _ if m.runtimeClass == classOf[scala.Tuple2[_,_]] =>
        val mA = m.typeArguments(0)
        val mB = m.typeArguments(1)
        src"std::pair<${remap(mA)},${remap(mB)}>"
      case _ if m <:< Manifest.AnyVal =>
        super[CLikeCodegen].remap(m)
      case _ =>
        throw new GenerationFailedException(s"CxxShptrCodegen.remap(): $m can not be remaped.")
    }
  }

  final override def emitVarDecl(sym: Sym[Any]): Unit = {
    emitConstruct(sym)
  }

  final def emitConstruct(sym: Sym[Any], args: String*): Unit = {
    val newTp = toShptrManifest(sym.tp)
    if (newTp.runtimeClass == classOf[SharedPtr[_]])
      stream.println(s"${remap(newTp)} ${quote(sym)} = std::make_shared<${remap(newTp.typeArguments(0))}>(${args.mkString(",")}); /*emitConstruct(): ${sym.tp} ${sym}(${args.mkString(",")})*/")
    else
      stream.println(s"${remap(newTp)} ${quote(sym)} = ${remap(newTp)}(${args.mkString(",")}); /*emitConstruct(): ${sym.tp} ${sym}(${args.mkString(",")})*/")
  }
//  override def wrapSharedPtr(tpe: String): String = {
//    if(!isPrimitiveType(tpe) && !isVoidType(tpe))
//      "std::shared_ptr<" + tpe + ">"
//    else
//      tpe
//  }

  override def emitSource[A: Manifest](args: List[Sym[_]], body: Block[A], className: String, out: PrintWriter) = {
    val sA = remap(toShptrManifest(manifest[A]))

    //      val staticData = getFreeDataBlock(body)

    withStream(out) {
      stream.println(
          "#include <memory>\n" +
          "#include <vector>\n" +
          "#include <cstdlib>\n" +
          "#include <functional>\n" +
          "#include <algorithm>\n" +
          "/*****************************************\n" +
          "  Emitting Generated Code                  \n" +
          "*******************************************/")
      emitFileHeader()

      val indargs = scala.Range(0, args.length).zip(args);
      stream.println(s"${sA} apply(${indargs.map( p => s"${remap(toShptrManifest(p._2.tp))} ${quote(p._2)}").mkString(", ")} ) {")

      emitBlock(body)
      stream.println(s"return ${quote(getBlockResult(body))};")

      stream.println("}")
      stream.println("/*****************************************\n" +
        "  End of Generated Code                  \n" +
        "*******************************************/")
    }

    Nil
  }
}
