package scalan.compilation.lms.uni

import java.io.PrintWriter

import scalan.compilation.lms.{BaseCodegen, LmsBackendFacade}
import scalan.compilation.lms.scalac.ScalaCoreCodegen

class JniCallCodegen [BackendCake <: LmsBackendFacade](backend: BackendCake, nativeCodegen: BaseCodegen[BackendCake], packageName:String) extends ScalaCoreCodegen(backend){
  import IR._

  def cppLibraryName(className:String) = "scalan_"+className.toLowerCase

  // See https://docs.oracle.com/javase/8/docs/technotes/guides/jni/spec/design.html#resolving_native_method_names
  // (Java Native Interface Specification Contents, Resolving Native Method Names in case URL changes)
  // Add other characters as needed
  // Native method overloading currently not supported, see above link if necessary
  def escapeJavaName(identifier: String) =
    identifier.replace("_", "_1").replace("$", "_00024").replace(".", "_")

  def cppFunctionName(className:String) = {
    // "$" because the native method is placed in an object
    val fullObjectName = (if (packageName.nonEmpty) s"$packageName." else "") + className + "$"
    Seq("Java", escapeJavaName(fullObjectName), "apply").mkString("_")
  }

  override def emitSource[A : Manifest](args: List[Sym[_]], body: Block[A], className: String, out: PrintWriter) = {

    val sA = remap(manifest[A])

    val staticData = getFreeDataBlock(body)


    withStream(out) {
      stream.println("/*****************************************\n"+
        "  Emitting Scala Code                  \n"+
        "*******************************************/")
      if (packageName.length > 0) {
        stream.println("package " + packageName)
      }
      emitFileHeader()

      val staticDataArgs = staticData.map(p => src"val p${p._1}: ${p._1.tp}")

      val argsDecl = args.map(a => src"$a: ${a.tp}")
      val code =
        src"""
             |class $className($staticDataArgs) extends ((${args.map(_.tp)}) => $sA) {
             |  def apply($argsDecl): $sA = {
             |    $className.apply($args)
             |  }
             |}
             |
             |object $className {
             |  @native def apply($argsDecl): $sA
             |
             |  System.loadLibrary("${cppLibraryName(className)}")
             |}
           """.stripMargin

      stream.println(code)

      stream.println("/*****************************************\n"+
        "  End of Generated Code                  \n"+
        "*******************************************/")
    }

    staticData
  }

}
