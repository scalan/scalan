package scalan.compilation.lms.scala

import java.io._
import java.net.URLClassLoader

import scalan.community.ScalanCommunityExp
import scalan.compilation.GraphVizExport
import scalan.compilation.lms.LmsCompiler
import scalan.util.{FileUtil, ProcessUtil}

trait LmsCompilerScala extends LmsCompiler { self: ScalanCommunityExp with GraphVizExport =>

  protected def doBuildExecutable[A,B](sourcesDir: File, executableDir: File, functionName: String, graph: PGraph, emitGraphs: Boolean)
                                      (config: Config, eInput: Elem[A], eOutput: Elem[B]) = {
    /* LMS stuff */

    val outputSource = new File(sourcesDir, functionName + ".scala")

    (createManifest(eInput), createManifest(eOutput)) match {
      case (mA: Manifest[a], mB: Manifest[b]) =>
        val bridge = makeBridge[a, b]
        val facade = bridge.getFacade(graph.asInstanceOf[bridge.scalan.PGraph])
        val codegen = bridge.lms.codegen

        FileUtil.withFile(outputSource) { writer =>
          codegen.emitSource[a, b](facade.apply, functionName, writer)(mA, mB)
          codegen.emitDataStructures(writer)
        }
    }

    val command = Seq("scalac", "-d", jarPath(functionName, executableDir)) ++ config.extraCompilerOptions :+
      outputSource.getAbsolutePath

    ProcessUtil.launch(sourcesDir, command: _*)
  }

  protected def doExecute[A, B](executableDir: File, functionName: String, input: A)
                               (config: Config, eInput: Elem[A], eOutput: Elem[B]): B = {
    val url = new File(jarPath(functionName, executableDir)).toURI.toURL
    // ensure Scala library is available
    val classLoader = new URLClassLoader(scala.Array(url), classOf[_ => _].getClassLoader)
    val cls = classLoader.loadClass(functionName)
    val argumentClass = eInput.classTag.runtimeClass
    val method = cls.getMethod("apply", argumentClass)
    val result = method.invoke(cls.newInstance(), input.asInstanceOf[AnyRef])
    result.asInstanceOf[B]
  }

  private def jarPath(functionName: String, executableDir: File) =
    s"${executableDir.getAbsolutePath}/$functionName.jar"
}
