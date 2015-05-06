package scalan
package compilation
package lms

import java.io.File

import scalan.compilation.language.{CoreMethodMappingDSL, MethodMappingDSL}
import scalan.util.FileUtil

trait LmsCompiler extends Compiler with CoreMethodMappingDSL { self: ScalanCtxExp with LmsBridge =>
  def emitSource[A, B](sourcesDir: File, extension: String, functionName: String, graph: PGraph, eInput: Elem[A], eOutput: Elem[B]): File = {
    (createManifest(eInput), createManifest(eOutput)) match {
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

  override def buildGraph[A, B](sourcesDir: File, functionName: String, func: Exp[A => B], graphVizConfig: GraphVizConfig)(compilerConfig: CompilerConfig): PGraph = {
    // pass scalan phases
    val graph = super.buildGraph(sourcesDir, functionName, func, graphVizConfig)(compilerConfig)

    func.elem match
    {
      case f:FuncElem[a, b] =>
        println(s"func.elem ${f.eDom} ${f.eRange}")
        (createManifest(f.eDom), createManifest(f.eRange)) match {
          case (mA: Manifest[a], mB: Manifest[b]) =>
            val codegen = lms.graphCodegen
            val lmsFunc = apply[a, b](graph)

            val log = new File(sourcesDir, s"${functionName}_lms.log")
            FileUtil.withFile(log) { writer =>
              try {
                codegen.emitSource[a, b](lmsFunc, functionName, writer)(mA, mB)
              } catch {
                case e: RuntimeException =>
                  println("ERROR in codegen.emitSource: " + e)
              }
            }

            codegen.graphStream.roots.isEmpty match{
              case true => println("EMPTY graphStream")
              case false =>
                println(codegen.graphStream.roots.head.toString)
            }

            val dotFile = new File(sourcesDir, s"${functionName}_lms.dot")

            codegen.graphStream.exportToGraphVis(dotFile, graphVizConfig)
        }
      case _ =>
        throw new Exception("ERROR func.elem not mached to FuncElem // in LmsCompiler.buildGraph")
    }

    graph
  }
}
