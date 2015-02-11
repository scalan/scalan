package scalan
package compilation
package lms

import java.io.File

import scalan.util.FileUtil

trait LmsCompiler extends Compiler { self: ScalanCtxExp with LmsBridge =>
  def emitSource[A, B](sourcesDir: File, extension: String, functionName: String, graph: PGraph, eInput: Elem[A], eOutput: Elem[B]): File = {
    (createManifest(eInput), createManifest(eOutput)) match {
      case (mA: Manifest[a], mB: Manifest[b]) =>
        val sourceFile = new File(sourcesDir, s"$functionName.$extension")
        FileUtil.withFile(sourceFile) { writer =>
          val codegen = lms.codegen
          val lmsFunc = apply[a, b](graph)
          codegen.emitSource[a, b](lmsFunc, functionName, writer)(mA, mB)
          //          val s = lms.fresh[a](mA)
          //          val body = codegen.reifyBlock(facade.apply(s))(mB)
          //          codegen.emitSource(List(s), body, functionName, writer)(mB)
          //          val lms.TP(sym,_) = lms.globalDefs.last
          //          codegen.emitDepGraph( sym, new File( sourcesDir, functionName + "-LMS.dot" ).getAbsolutePath )
          codegen.emitDataStructures(writer)
        }
        sourceFile
    }
  }
}
