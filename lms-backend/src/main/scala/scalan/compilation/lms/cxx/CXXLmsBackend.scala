package scalan.compilation.lms.cxx

import java.io.PrintWriter

import scala.virtualization.lms.common._
import scalan.compilation.lms._

class CoreCXXLmsBackend extends CoreLmsBackendBase { self =>

  trait Codegen extends CLikeGenNumericOps
  with CLikeGenEqual
  with CLikeGenArrayOps
  with CLikeGenPrimitiveOps
  with CXXGenStruct
  with CXXGenFatArrayLoopsFusionOpt
  with LoopFusionOpt
  with CXXFatCodegen
  with CXXGenCastingOps
  with CXXCodegen
  {
    override val IR: self.type = self

    override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true

    //FIXME: yz: move it to CXXCodegen (require to resolve conflict with emitSource in CCodegen inherited by CXXGenFatArrayLoopsFusionOpt)
    override def emitSource[A: Manifest](args: List[Sym[_]], body: Block[A], className: String, out: PrintWriter) = {
      val sA = remap(manifest[A])

      //      val staticData = getFreeDataBlock(body)

      withStream(out) {
        stream.println(
          "#include <vector>\n" +
            "#include <cstdlib>\n" +
            "#include <pair>\n" +
            "/*****************************************\n" +
            "  Emitting Generated Code                  \n" +
            "*******************************************/")
        emitFileHeader()

        val indargs = (0 until args.length).asInstanceOf[Range].zip(args);
        stream.println(s"${sA} apply(${indargs.map( p => s"${remap(p._2.tp)} ${quote(p._2)}").mkString(", ")} ) {")

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

  override val codegen = new Codegen {}
}

class CommunityCXXLmsBackend extends CoreCXXLmsBackend with CommunityLmsBackendBase { self =>

  override val codegen = new Codegen with CXXGenJNIExtractor {
    override val IR: self.type = self

    override def emitSource[A: Manifest](args: List[Sym[_]], body: Block[A], className: String, out: PrintWriter) = {
      val sA = remap(manifest[A])

      //      val staticData = getFreeDataBlock(body)

      withStream(out) {
        stream.println(
          "#include <vector>\n" +
            "#include <cstdlib>\n" +
            "#include <jni-array-wrapper.hpp>\n" +
            "/*****************************************\n" +
            "  Emitting Generated Code                  \n" +
            "*******************************************/")
        emitFileHeader()

        val indargs = (0 until args.length) zip args;
        val has = indargs.map(p => p._2.tp.runtimeClass)
          .contains(classOf[scalan.compilation.lms.JNILmsOps#JNIType[_]])
        val jniEnv = if (has) "JNIEnv* env, " else ""
        stream.println(s"${sA} apply(${jniEnv}${indargs.map(p => s"${remap(p._2.tp)} ${quote(p._2)}").mkString(", ")} ) {")

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
}
