package scalan.compilation.lms.cxx

import java.io._

import scalan.ScalanDslExp
import scalan.compilation.GraphVizConfig
import scalan.compilation.lms.LmsCompiler
import scalan.compilation.lms.cxx.sharedptr.CoreCxxShptrLmsBackend

class LmsCompilerCxx[+ScalanCake <: ScalanDslExp](_scalan: ScalanCake) extends LmsCompiler(_scalan) {
  import scalan._
  val lms = new CoreCxxShptrLmsBackend

  type CustomCompilerOutput = Unit

  type CompilerConfig = Unit
  implicit val defaultCompilerConfig = ()

  override protected def doBuildExecutable[A,B](sourcesDir: File, executableDir: File, functionName: String, graph: PGraph, graphVizConfig: GraphVizConfig)
                                      (compilerConfig: CompilerConfig, eInput: Elem[A], eOutput: Elem[B]) = {
    /* LMS stuff */

    emitSource(sourcesDir, functionName, graph, eInput, eOutput, graphVizConfig)
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
//    val argumentClass = eInput.runtimeClass
//    val method = cls.getMethod("apply", argumentClass)
//    val result = method.invoke(cls.newInstance(), input.asInstanceOf[AnyRef])
//    result.asInstanceOf[B]
    null.asInstanceOf[B]
  }
}
