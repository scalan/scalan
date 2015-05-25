package scalan
package compilation
package lms
package scalac

import java.io._
import java.net.{URL, URLClassLoader}
import scalan.compilation.language.MethodMappingDSL
import scalan.compilation.lms.source2bin.{Nsc, SbtConfig, Sbt}
import scalan.util.FileUtil
import scalan.util.FileUtil.file

trait LmsCompilerScala extends LmsCompiler with CoreBridge with MethodMappingDSL { self: ScalanCtxExp =>
  /**
   * If scalaVersion is None, uses scala-compiler.jar
   *
   * Otherwise uses SBT to compile with the desired version
   */
  case class CustomCompilerOutput(jar: URL, mainClass: Option[String] = None, output: Option[Array[String]] = None)
  case class CompilerConfig(scalaVersion: Option[String], extraCompilerOptions: Seq[String], sbt : SbtConfig = SbtConfig(), traits : Seq[String] = Seq.empty[String])
  implicit val defaultCompilerConfig = CompilerConfig(None, Seq.empty)

  override def graphPasses(compilerConfig: CompilerConfig) = Seq(AllUnpackEnabler, AllInvokeEnabler)

  protected def doBuildExecutable[A, B](sourcesDir: File, executableDir: File, functionName: String, graph: PGraph, graphVizConfig: GraphVizConfig)
                                       (compilerConfig: CompilerConfig, eInput: Elem[A], eOutput: Elem[B]) = {
    Sbt.prepareDir(executableDir) //todo - check: is it sbt-specific function?
    /* LMS stuff */
    val sourceFile = emitSource(sourcesDir, "scala", functionName, graph, eInput, eOutput)
    val jarFile = file(executableDir.getAbsoluteFile, s"$functionName.jar")
    FileUtil.deleteIfExist(jarFile)
    val jarPath = jarFile.getAbsolutePath
    val mainClass : Option[String] = compilerConfig.sbt.mainPack match {
      case Some(mainPack) => Some(mainPack + "." +  functionName)
      case _ =>  None
    }
    val output: Option[Array[String]] = compilerConfig.scalaVersion match {
      case Some(scalaVersion) =>
        val dependencies:Array[String] = methodReplaceConf.flatMap(conf => conf.dependencies).toArray
        Some(Sbt.compile(sourcesDir, executableDir, functionName, compilerConfig, dependencies, sourceFile, jarPath))
      case None =>
        Nsc.compile(executableDir, functionName, compilerConfig.extraCompilerOptions.toList, sourceFile, jarPath)
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
