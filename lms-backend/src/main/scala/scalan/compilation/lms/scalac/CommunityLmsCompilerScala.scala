package scalan.compilation.lms.scalac

import java.io.File

import scala.collection.Seq
import scalan.{ScalanCommunityScalaCompiler, ScalanCommunityDslExp}
import scalan.compilation.GraphVizConfig
import scalan.compilation.lms.CommunityBridgeScala

trait CommunityLmsCompilerScala extends LmsCompilerScala with CommunityBridgeScala with ScalanCommunityScalaCompiler { self: ScalanCommunityDslExp =>


  override protected def doBuildExecutable[A, B](sourcesDir: File, executableDir: File, functionName: String, graph: PGraph, graphVizConfig: GraphVizConfig)
                                                (compilerConfig: CompilerConfig, eInput: Elem[A], eOutput: Elem[B]) = {
    super.doBuildExecutable[A, B](sourcesDir, executableDir, functionName, graph, graphVizConfig)(compilerConfig, eInput, eOutput)
  }

  override def newObj[A: Manifest](m: LmsMirror, aClass: Class[_], args: Seq[Rep[_]], newKeyWord: Boolean): lms.Exp[A] = {
    val name = mappedClassName(aClass) match {
      case Some(n) => n
      case _ => aClass.getName
    }
    lms.newObj[A](name, args.map(v => m.symMirrorUntyped(v)), newKeyWord)
  }
}
