package scalan
package compilation
package lms

import java.io.File

import scalan.util.FileUtil

abstract class LmsCompiler[+ScalanCake <: ScalanDsl](_scalan: ScalanCake) extends Compiler(_scalan) with CoreLmsBridge {
  import scalan._

  override def graphPasses(compilerConfig: CompilerConfig) = Seq(AllUnpackEnabler, AllInvokeEnabler)

  def emitSource[A, B](sourcesDir: File, functionName: String, graph: PGraph, eInput: Elem[A], eOutput: Elem[B], graphVizConfig: GraphVizConfig): File = {
    (elemToManifest(eInput), elemToManifest(eOutput)) match {
      case (mA: Manifest[a], mB: Manifest[b]) =>

        val lmsFunc = apply[a, b](graph)
        val codegen = lms.codegen
        emitLmsGraph(sourcesDir, functionName, graphVizConfig, lmsFunc, mA, mB)

        codegen.createFile(lmsFunc, functionName, sourcesDir)(mA, mB)

    }
  }

  def emitLmsGraph[A, B](sourcesDir: File, functionName: String, graphVizConfig: GraphVizConfig, lmsFunc: lms.Exp[A] => lms.Exp[B], mA: Manifest[A], mB: Manifest[B]): Unit = {
    val graphCodegen = lms.graphCodegen

    val log = new File(sourcesDir, s"${functionName}_lms.log")
    FileUtil.withFile(log) { writer =>
      try {
        graphCodegen.emitSource[A, B](lmsFunc, functionName, writer)(mA, mB)
      } catch {
        case e: Exception =>
          println("Exception in graphCodegen.emitSource:")
          e.printStackTrace()
      }
    }

    val dotFile = new File(sourcesDir, s"${functionName}_lms.dot")

    graphCodegen.graphStream.exportToGraphVis(dotFile, graphVizConfig)
  }
}
