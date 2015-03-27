package scalan.compilation.lms.cxx

import java.io._

import scalan.JNIExtractorOpsExp
import scalan.ScalanCommunityExp
import scalan.compilation.GraphVizConfig
import scalan.compilation.lms.{LmsBridge, LmsCompiler}

trait LmsCompilerCxx extends LmsCompiler with JNIExtractorOpsExp { self: ScalanCommunityExp with LmsBridge =>

  type CustomCompilerOutput = Unit

  type CompilerConfig = Unit
  implicit val defaultCompilerConfig = ()

  def graphPasses(compilerConfig: CompilerConfig) = Seq(AllUnpackEnabler, AllInvokeEnabler)

  override protected def doBuildExecutable[A,B](sourcesDir: File, executableDir: File, functionName: String, graph: PGraph, graphVizConfig: GraphVizConfig)
                                      (compilerConfig: CompilerConfig, eInput: Elem[A], eOutput: Elem[B]) = {
    /* LMS stuff */

    emitSource(sourcesDir, "cxx", functionName, graph, eInput, eOutput)
//    val command = Seq("scalac", "-d", jarPath(functionName, executableDir)) ++ config.extraCompilerOptions :+
//      sourceFile.getAbsolutePath
//
//    val command = Seq("make")
//    ProcessUtil.launch(new File(sourcesDir,"release"), command: _*)
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
