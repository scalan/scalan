package scalan
package compilation
package lms

import java.io.File

import scalan.compilation.language.{CoreMethodMappingDSL, MethodMappingDSL}
import scalan.util.FileUtil

abstract class LmsCompiler[+ScalanCake <: ScalanCtxExp](_scalan: ScalanCake) extends Compiler(_scalan) with LmsBridge with CoreMethodMappingDSL {
  import scalan._

  override def graphPasses(compilerConfig: CompilerConfig) = Seq(AllUnpackEnabler, AllInvokeEnabler)

  def emitSource[A, B](sourcesDir: File, functionName: String, graph: PGraph, eInput: Elem[A], eOutput: Elem[B]): File = {
    (elemToManifest(eInput), elemToManifest(eOutput)) match {
      case (mA: Manifest[a], mB: Manifest[b]) =>

        // ***$$$*** transform graph in lms start
        /*
        val graphEmitter = new GraphLmsBackend
        val codegen = graphEmitter.codegen
        val lmsFunc = apply[a, b](graph)
        try {
          codegen.emitSource[a, b](lmsFunc, functionName, writer)(mA, mB)
        } catch {
          case e: Throwable => {
            println("ERROR in codegen.emitSource: " + e)
          }
        }
        println("=== codegen.graphStream.roots.head.toString ===" )
        println(codegen.graphStream.roots.head.toString)
        println("===============================================" )
        */
        // ***$$$*** transform graph in lms stop

        val codegen = lms.codegen
        val lmsFunc = apply[a, b](graph)

        codegen.createFile(lmsFunc, functionName, sourcesDir)(mA, mB)

    }
  }

  override def buildGraph[A, B](sourcesDir: File, functionName: String, func: => Exp[A => B], graphVizConfig: GraphVizConfig)(compilerConfig: CompilerConfig): CommonCompilerOutput[A, B] = {
    // pass scalan phases
    val output = super.buildGraph(sourcesDir, functionName, func, graphVizConfig)(compilerConfig)

    (elemToManifest(output.eInput), elemToManifest(output.eOutput)) match {
      case (mA: Manifest[a], mB: Manifest[b]) =>
        val codegen = lms.graphCodegen
        val lmsFunc = apply[a, b](output.graph)

        val log = new File(sourcesDir, s"${functionName}_lms.log")
        FileUtil.withFile(log) { writer =>
          try {
            codegen.emitSource[a, b](lmsFunc, functionName, writer)(mA, mB)
          } catch {
            case e: Exception =>
              println("Exception in codegen.emitSource:")
              e.printStackTrace()
          }
        }

        val dotFile = new File(sourcesDir, s"${functionName}_lms.dot")

        codegen.graphStream.exportToGraphVis(dotFile, graphVizConfig)
    }

    output
  }
}
