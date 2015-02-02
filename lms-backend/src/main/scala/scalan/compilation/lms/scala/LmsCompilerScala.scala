package scalan.compilation.lms.scala

import java.io._
import java.net.URLClassLoader

import scala.util.Properties
import scalan.community.ScalanCommunityExp
import scalan.compilation.GraphVizExport
import scalan.compilation.lms.LmsCompiler
import scalan.util.{FileUtil, ProcessUtil}

trait LmsCompilerScala extends LmsCompiler { self: ScalanCommunityExp with GraphVizExport =>

  protected def doBuildExecutable[A, B](sourcesDir: File, executableDir: File, functionName: String, graph: PGraph, emitGraphs: Boolean)
                                       (config: Config, eInput: Elem[A], eOutput: Elem[B]) = {
    /* LMS stuff */

    val outputSource = new File(sourcesDir, functionName + ".scala")
    val buildSbtFile = new File(sourcesDir, "build.sbt")

    (createManifest(eInput), createManifest(eOutput)) match {
      case (mA: Manifest[a], mB: Manifest[b]) =>
        val bridge = makeBridge[a, b]
        val facade = bridge.getFacade(graph.asInstanceOf[bridge.scalan.PGraph])
        val codegen = bridge.lms.codegen

        FileUtil.withFile(outputSource) { writer =>
          codegen.emitSource[a, b](facade.apply, functionName, writer)(mA, mB)
          codegen.emitDataStructures(writer)
        }

        // we want a normal Scala version which is binary-compatible to the
        // current scala-library.jar
        // e.g. 2.10.2 for Scala-Virtualized 2.10.2
        val scalaVersion = Properties.versionNumberString.split('-')(0)

        val buildSbtText =
          s"""name := "$functionName"
             |
             |scalaVersion := "$scalaVersion"
             |
             |artifactPath in Compile in packageBin :=
             |  baseDirectory.value / "$functionName.jar"
             |
             |scalacOptions ++= Seq(${config.extraCompilerOptions.mkString(", ")})
           """.stripMargin

        FileUtil.write(buildSbtFile, buildSbtText)
    }

    val command = Seq("sbt", "package")

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
