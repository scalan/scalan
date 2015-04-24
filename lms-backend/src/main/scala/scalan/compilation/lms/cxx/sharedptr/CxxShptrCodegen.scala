package scalan.compilation.lms.cxx.sharedptr

import java.io.PrintWriter

import scala.virtualization.lms.internal.{CLikeCodegen, Expressions, GenerationFailedException}
import scalan.compilation.lms.ManifestUtil
import scalan.compilation.lms.common.JNILmsOps


trait CxxShptrCodegen extends CLikeCodegen with ManifestUtil {
  val IR: Expressions
  import IR._

  trait size_t
  trait SharedPtr[T]
  trait auto_t

  var headerFiles: collection.mutable.HashSet[String] = collection.mutable.HashSet.empty

  headerFiles ++= Seq("memory")

  def toShptrManifest(m: Manifest[_]): Manifest[_] = {
    if( m.runtimeClass == classOf[SharedPtr[_]] )
      m
    else {
      val nArgs = m.typeArguments.length
      val newM = nArgs match {
        case 0 => m
        case 1 => Manifest.classType(m.runtimeClass, toShptrManifest(m.typeArguments(0)))
        case n => Manifest.classType(m.runtimeClass, toShptrManifest(m.typeArguments(0)), m.typeArguments.drop(1).map(toShptrManifest): _*)
      }

      wrapSharedPtr(newM)
    }
  }

  def wrapSharedPtr:PartialFunction[Manifest[_],Manifest[_]] = {
    case m if m.isPrimitive => m
    case m if m.runtimeClass == classOf[SharedPtr[_]] => m
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
      stream.println(src"${remap(tpe)} $sym = $rhs;")
  }

  override def remap[A](m: Manifest[A]) : String = {
    m match {
      case _ if m.runtimeClass == classOf[SharedPtr[_]] =>
        s"std::shared_ptr<${remap(m.typeArguments(0))}>"
      case _ if m.runtimeClass == classOf[auto_t] =>
        "auto"
      case _ if m.runtimeClass == classOf[size_t] =>
        "size_t"
      case _ if m.runtimeClass == classOf[scala.Tuple2[_,_]] =>
        val mA = m.typeArguments(0)
        val mB = m.typeArguments(1)
        src"std::pair<${remap(mA)},${remap(mB)}>"
      case _ if m.runtimeClass == classOf[Unit] ⇒
        "boost::blank"
      case _ if m.isPrimitive =>
        super[CLikeCodegen].remap(m)
      case _ =>
        throw new GenerationFailedException(s"CxxShptrCodegen.remap(): $m can not be remaped.")
    }
  }

  final override def emitVarDecl(sym: Sym[Any]): Unit = {
    emitConstruct(sym)
  }

  final override def emitVarDef(sym: Sym[Variable[Any]], rhs: String): Unit =
    emitValDef(sym, rhs)

  final def emitConstruct(sym: Sym[Any], args: String*): Unit = {
    val newTp = toShptrManifest(sym.tp)
    if (newTp.runtimeClass == classOf[SharedPtr[_]])
      stream.println(s"${remap(newTp)} ${quote(sym)} = std::make_shared<${remap(newTp.typeArguments(0))}>(${args.mkString(",")});")
    else
      stream.println(s"${remap(newTp)} ${quote(sym)} = ${remap(newTp)}(${args.mkString(",")});")
  }
  override def quote(x: Exp[Any]) = x match {
    case Const(s: Unit) => "scalan::unit_value"
    case _ => super.quote(x)
  }

  override def emitSource[A: Manifest](args: List[Sym[_]], body: Block[A], className: String, out: PrintWriter) = {
    val resultM = manifest[A]
    val sA = remap(toShptrManifest(resultM))

    //      val staticData = getFreeDataBlock(body)

    withStream(out) {
      headerFiles.map {fn => s"#include <${fn}>"} map ( stream.println _ )
      stream.println(
          "/*****************************************\n" +
          "  Emitting Generated Code                  \n" +
          "*******************************************/")
      emitFileHeader()

      val indargs = scala.Range(0, args.length).zip(args);
      val has = indargs.map(p => p._2.tp.runtimeClass).contains(classOf[JNILmsOps#JNIType[_]]) || resultM.runtimeClass == classOf[JNILmsOps#JNIType[_]]
      val jniEnv = if (has) "JNIEnv* env, " else ""
      stream.println(s"${sA} apply_$className(${jniEnv}${indargs.map( p => s"${remap(toShptrManifest(p._2.tp))} ${quote(p._2)}").mkString(", ")} ) {")

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
