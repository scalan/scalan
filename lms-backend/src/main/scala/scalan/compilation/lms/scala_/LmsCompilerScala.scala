package scalan
package compilation
package lms
package scala_ //Underscore is added intentionally to avoid name clash with root scala package

import java.io._
import java.net.URLClassLoader

import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.reporters.StoreReporter
import scalan.util.{FileUtil, ProcessUtil, StringUtil}

trait LmsCompilerScala extends LmsCompiler { self: ScalanCtxExp =>

  /**
   * If scalaVersion is None, uses scala-compiler.jar
   *
   * Otherwise uses SBT to compile with the desired version
   */
  case class CompilerConfig(scalaVersion: Option[String], extraCompilerOptions: Seq[String])

  implicit val defaultCompilerConfig = CompilerConfig(None, Seq.empty)

  case class CompilationOutput(jar: File)

  def graphPasses(compilerConfig: CompilerConfig) = Seq(AllUnpackEnabler, AllInvokeEnabler)

  protected def doBuildExecutable[A, B](sourcesDir: File, executableDir: File, functionName: String, graph: PGraph, graphVizConfig: GraphVizConfig)
                                       (compilerConfig: CompilerConfig, eInput: Elem[A], eOutput: Elem[B]) = {
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

        val jarFile = FileUtil.file(executableDir.getAbsoluteFile, s"$functionName.jar")
        val jarPath = jarFile.getAbsolutePath

        compilerConfig.scalaVersion match {
          case Some(scalaVersion) =>
            val buildSbtText =
              s"""name := "$functionName"
                 |
                 |scalaVersion := "$scalaVersion"
                 |
                 |artifactPath in Compile in packageBin := file("$jarPath")
                 |
                 |scalacOptions ++= Seq(${compilerConfig.extraCompilerOptions.map(StringUtil.quote).mkString(", ")})
                 |""".stripMargin

            FileUtil.write(buildSbtFile, buildSbtText)

            val command = Seq("sbt", "package")

            ProcessUtil.launch(sourcesDir, command: _*)
          case None =>
            val settings = new Settings
            settings.usejavacp.value = true
            // necessary to lauch compiler
            // see http://stackoverflow.com/questions/27934282/object-scala-in-compiler-mirror-not-found-running-scala-compiler-programatical
            settings.embeddedDefaults[LmsCompilerScala]
            val compilerOptions = "-d" :: jarPath :: compilerConfig.extraCompilerOptions.toList
            settings.processArguments(compilerOptions, false)
            val reporter = new StoreReporter
            val compiler: Global = new Global(settings, reporter)
            val run = new compiler.Run
            run.compile(List(outputSource.getAbsolutePath))
        }
        CompilationOutput(jarFile)
    }
  }

  def loadMethod(compilationOutput: CompilationOutput, functionName: String, eArg: Elem[_]) = {
    val url = compilationOutput.jar.toURI.toURL
    // ensure Scala library is available
    val classLoader = new URLClassLoader(Array(url), classOf[_ => _].getClassLoader)
    val cls = classLoader.loadClass(functionName)
    val argumentClass = eArg.classTag.runtimeClass
    (cls, cls.getMethod("apply", argumentClass))
  }

  protected def doExecute[A, B](compilationOutput: CompilationOutput, functionName: String, input: A)
                               (compilerConfig: CompilerConfig, eInput: Elem[A], eOutput: Elem[B]): B = {
    val (cls, method) = loadMethod(compilationOutput, functionName, eInput)
    val instance = cls.newInstance()
    val result = method.invoke(instance, input.asInstanceOf[AnyRef])
    result.asInstanceOf[B]
  }
}
