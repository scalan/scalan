package scalan.compilation.lms.cxx

import java.io._

import scalan.community.ScalanCommunityExp
import scalan.compilation.{GraphVizConfig, GraphVizExport}
import scalan.compilation.lms.LmsCompiler
import scalan.util.{FileUtil, ProcessUtil}

trait LmsCompilerCXX extends LmsCompiler { self: ScalanCommunityExp with GraphVizExport =>

  type CustomCompilerOutput = Unit

  type CompilerConfig = Unit
  implicit val defaultCompilerConfig = ()

  def graphPasses(compilerConfig: CompilerConfig) = Seq(AllUnpackEnabler, AllInvokeEnabler)

  def generate[A, B](sourcesDir: File, executableDir: File, functionName: String, func: Exp[A => B], graphVizConfig: GraphVizConfig)
                           (implicit compilerConfig: CompilerConfig): Unit = {
    sourcesDir.mkdirs()
    executableDir.mkdirs()
    val eFunc = func.elem
    val graph = buildGraph(sourcesDir, functionName, func, graphVizConfig)(compilerConfig)
    doGenerate(sourcesDir, executableDir, functionName, graph)(compilerConfig, eFunc.eDom, eFunc.eRange)
  }

  protected def doGenerate[A,B](sourcesDir: File, executableDir: File, functionName: String, graph: PGraph)
                                      (compilerConfig: CompilerConfig, eInput: Elem[A], eOutput: Elem[B]) = {

    val outputSource = new File(sourcesDir, functionName + ".cxx")

    (createManifest(eInput), createManifest(eOutput)) match {
      case (mA: Manifest[a], mB: Manifest[b]) =>
        val bridge = makeBridge[a, b]
        val facade = bridge.getFacade(graph.asInstanceOf[bridge.scalan.PGraph])
        val codegen = bridge.lms.codegen

        FileUtil.withFile(outputSource) { writer =>
          codegen.emitSource[a, b](facade.apply, functionName, writer)(mA, mB)
          //          val s = bridge.lms.fresh[a](mA)
          //          val body = codegen.reifyBlock(facade.apply(s))(mB)
          //          codegen.emitSource(List(s), body, functionName, writer)(mB)
          //          val bridge.lms.TP(sym,_) = bridge.lms.globalDefs.last
          //          codegen.emitDepGraph( sym, new File( sourcesDir, functionName + "-LMS.dot" ).getAbsolutePath )
          codegen.emitDataStructures(writer)
        }
    }
  }

  override protected def doBuildExecutable[A,B](sourcesDir: File, executableDir: File, functionName: String, graph: PGraph, graphVizConfig: GraphVizConfig)
                                      (compilerConfig: CompilerConfig, eInput: Elem[A], eOutput: Elem[B]) = {
    /* LMS stuff */

    val outputSource = new File(sourcesDir, functionName + ".cxx")

    (createManifest(eInput), createManifest(eOutput)) match {
      case (mA: Manifest[a], mB: Manifest[b]) =>
        val bridge = makeBridge[a, b]
        val facade = bridge.getFacade(graph.asInstanceOf[bridge.scalan.PGraph])
        val codegen = bridge.lms.codegen

        FileUtil.withFile(outputSource) { writer =>
          codegen.emitSource[a, b](facade.apply, functionName, writer)(mA, mB)
//          val s = bridge.lms.fresh[a](mA)
//          val body = codegen.reifyBlock(facade.apply(s))(mB)
//          codegen.emitSource(List(s), body, functionName, writer)(mB)
//          val bridge.lms.TP(sym,_) = bridge.lms.globalDefs.last
//          codegen.emitDepGraph( sym, new File( sourcesDir, functionName + "-LMS.dot" ).getAbsolutePath )
          codegen.emitDataStructures(writer)
        }
    }

//    val command = Seq("scalac", "-d", jarPath(functionName, executableDir)) ++ config.extraCompilerOptions :+
//      outputSource.getAbsolutePath
//
    val command = Seq("make")
    ProcessUtil.launch(new File(sourcesDir,"release"), command: _*)
  }

  override protected def doExecute[A, B](compilerOutput: CompilerOutput[A, B], input: A): B = {
//    val url = new File(jarPath(functionName, executableDir)).toURI.toURL
//    // ensure Scala library is available
//    val classLoader = new URLClassLoader(scala.Array(url), classOf[_ => _].getClassLoader)
//    val cls = classLoader.loadClass(functionName)
//    val argumentClass = eInput.classTag.runtimeClass
//    val method = cls.getMethod("apply", argumentClass)
//    val result = method.invoke(cls.newInstance(), input.asInstanceOf[AnyRef])
//    result.asInstanceOf[B]
    null.asInstanceOf[B]
  }
}
