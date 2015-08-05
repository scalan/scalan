package scalan
package compilation
package lms
package scalac

import java.io._
import java.net.{URL, URLClassLoader}
import scalan.compilation.language.MethodMappingDSL
import scalan.compilation.lms.source2bin.{Nsc, SbtConfig, Sbt}
import scalan.compilation.lms.uni.NativeMethodsConfig
import scalan.util.FileUtil
import scalan.util.FileUtil.file

case class LmsCompilerScalaConfig(scalaVersion: Option[String], extraCompilerOptions: Seq[String], sbt : SbtConfig = SbtConfig(), traits : Seq[String] = Seq.empty[String], nativeMethods: NativeMethodsConfig = new NativeMethodsConfig)

abstract class LmsCompilerScala[ScalanCake <: ScalanCtxExp](_scalan: ScalanCake) extends LmsCompiler(_scalan) with CoreBridge with MethodMappingDSL {
  import scalan._
  /**
   * If scalaVersion is None, uses scala-compiler.jar
   *
   * Otherwise uses SBT to compile with the desired version
   */
  case class CustomCompilerOutput(sources:List[String], jar: URL, mainClass: Option[String] = None, output: Option[Array[String]] = None)
  type CompilerConfig = LmsCompilerScalaConfig
  implicit val defaultCompilerConfig = LmsCompilerScalaConfig(None, Seq.empty)

  protected def doBuildExecutable[A, B](sourcesDir: File, executableDir: File, functionName: String, graph: PGraph, graphVizConfig: GraphVizConfig)
                                       (compilerConfig: CompilerConfig, eInput: Elem[A], eOutput: Elem[B]) = {
    Sbt.prepareDir(executableDir) //todo - check: is it sbt-specific function?
    /* LMS stuff */
    val sourceFile = emitSource(sourcesDir, functionName, graph, eInput, eOutput)
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
    CustomCompilerOutput(List(sourceFile.getAbsolutePath), jarFile.toURI.toURL, mainClass, output)
  }

  def loadMethod(compilerOutput: CompilerOutput[_, _]) = {
    // ensure Scala library is available
    val classLoader = new URLClassLoader(Array(compilerOutput.custom.jar), getClass.getClassLoader)
    val cls = classLoader.loadClass(
      compilerOutput.custom.mainClass match {
        case Some(mainClass) => mainClass
        case _ => compilerOutput.common.name
      }
    )
    val argumentClass = compilerOutput.common.eInput.runtimeClass
    (cls, cls.getMethod("apply", argumentClass))
  }

  protected def doExecute[A, B](compilerOutput: CompilerOutput[A, B], input: A): B = {
    val (cls, method) = loadMethod(compilerOutput)
    val instance = cls.newInstance()
    val result = method.invoke(instance, input.asInstanceOf[AnyRef])
    result.asInstanceOf[B]
  }
}
