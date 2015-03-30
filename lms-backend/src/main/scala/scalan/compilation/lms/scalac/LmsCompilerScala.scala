package scalan
package compilation
package lms
package scalac

import java.io._
import java.net.{URL, URLClassLoader}

import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.{Global, Settings}
import scalan.compilation.language.MethodMappingDSL
import scalan.util.FileUtil
import scalan.util.FileUtil.file

trait LmsCompilerScala extends LmsCompiler with SbtCompiler with CoreBridge with MethodMappingDSL { self: ScalanCtxExp =>
  /**
   * If scalaVersion is None, uses scala-compiler.jar
   *
   * Otherwise uses SBT to compile with the desired version
   */
  case class CustomCompilerOutput(jar: URL, mainClass: Option[String] = None, output: Option[Array[String]] = None)
  case class CompilerConfig(scalaVersion: Option[String], extraCompilerOptions: Seq[String], sbt : SbtConfig = SbtConfig(), traits : Seq[String] = Seq.empty[String])
  implicit val defaultCompilerConfig = CompilerConfig(None, Seq.empty)

  def graphPasses(compilerConfig: CompilerConfig) = Seq(AllUnpackEnabler, AllInvokeEnabler)

  protected def doBuildExecutable[A, B](sourcesDir: File, executableDir: File, functionName: String, graph: PGraph, graphVizConfig: GraphVizConfig)
                                       (compilerConfig: CompilerConfig, eInput: Elem[A], eOutput: Elem[B]) = {
    /* LMS stuff */
    val sourceFile = emitSource(sourcesDir, "scala", functionName, graph, eInput, eOutput)
    val jarFile = file(executableDir.getAbsoluteFile, s"$functionName.jar")
    FileUtil.deleteIfExist(jarFile)
    val jarPath = jarFile.getAbsolutePath
    val logFile = file(executableDir.getAbsoluteFile, s"$functionName.log")
    FileUtil.deleteIfExist(logFile)
    val mainClass : Option[String] = compilerConfig.sbt.mainPack match {
      case Some(mainPack) => Some(mainPack + "." +  functionName)
      case _ =>  None
    }
    val output: Option[Array[String]] = compilerConfig.scalaVersion match {
      case Some(scalaVersion) => Some(sbtCompile(sourcesDir, executableDir, functionName, compilerConfig, sourceFile, jarPath))
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
        run.compile(List(sourceFile.getAbsolutePath))

        import java.io.PrintWriter
        val S = new PrintWriter(logFile)
        S.println(settings)
        S.println(s"${settings.classpath}\n")
        for(row <- reporter.infos) {
          S.println(s"${row.severity}: ${row.msg}")
          S.println(s"|${row.pos.source.path}:${row.pos.safeLine}")
          S.println(s"|${row.pos.lineContent}")
          S.println("|"+" "*(row.pos.column-1) + "^")
        }
        S.println(s"class $functionName compiled with ${reporter.ERROR.count} errors and ${reporter.WARNING.count} warnings")
        S.close()

        reporter.ERROR.count match {
          case 0 => //println(s"class $functionName compiled with ${reporter.WARNING.count} warnings")
          case _ => throw new Exception(s"class $functionName compiled with ${reporter.ERROR.count} errors and ${reporter.WARNING.count} warnings, see ${logFile.getAbsolutePath} for details")
        }
        None
    }
    CustomCompilerOutput(jarFile.toURI.toURL, mainClass, output)
  }

  def loadMethod(compilerOutput: CompilerOutput[_, _]) = {
    // ensure Scala library is available
    val classLoader = new URLClassLoader(Array(compilerOutput.custom.jar), self.getClass.getClassLoader)
    val cls = classLoader.loadClass(
      compilerOutput.custom.mainClass match {
        case Some(mainClass) => mainClass
        case _ => compilerOutput.common.name
      }
    )
    val argumentClass = compilerOutput.common.eInput.classTag.runtimeClass
    (cls, cls.getMethod("apply", argumentClass))
  }

  protected def doExecute[A, B](compilerOutput: CompilerOutput[A, B], input: A): B = {
    val (cls, method) = loadMethod(compilerOutput)
    val instance = cls.newInstance()
    val result = method.invoke(instance, input.asInstanceOf[AnyRef])
    result.asInstanceOf[B]
  }
}
