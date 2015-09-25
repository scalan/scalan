package scalan
package compilation
package lms
package scalac

import java.io._
import java.net.URLClassLoader
import scalan.compilation.language.MethodMappingDSL
import scalan.compilation.lms.source2bin.{SbtConfig, Nsc, Sbt}
import scalan.compilation.lms.uni.NativeMethodsConfig
import scalan.util.FileUtil
import scalan.util.FileUtil.file

case class LmsCompilerScalaConfig(extraCompilerOptions: Seq[String] = Seq.empty, sbtConfigOpt: Option[SbtConfig] = None, traits: Seq[String] = Seq.empty[String], nativeMethods: NativeMethodsConfig = new NativeMethodsConfig) {
  def withSbtConfig(sbtConfig: SbtConfig) = copy(sbtConfigOpt = Some(sbtConfig))
}

abstract class LmsCompilerScala[+ScalanCake <: ScalanCtxExp](_scalan: ScalanCake) extends LmsCompiler(_scalan) with CoreBridge with MethodMappingDSL {
  import scalan._
  case class CustomCompilerOutput(sources: List[File], jar: File, mainClass: String, output: Option[Array[String]]) {
    def jarUrl = jar.toURI.toURL
  }
  type CompilerConfig = LmsCompilerScalaConfig
  implicit val defaultCompilerConfig = LmsCompilerScalaConfig()

  protected def doBuildExecutable[A, B](sourcesDir: File, executableDir: File, functionName: String, graph: PGraph, graphVizConfig: GraphVizConfig)
                                       (compilerConfig: CompilerConfig, eInput: Elem[A], eOutput: Elem[B]) = {
    Sbt.prepareDir(executableDir) //todo - check: is it sbt-specific function?
    /* LMS stuff */
    val sourceFile = emitSource(sourcesDir, functionName, graph, eInput, eOutput)
    val jarFile = file(executableDir.getAbsoluteFile, s"$functionName.jar")
    FileUtil.deleteIfExist(jarFile)
    val jarPath = jarFile.getAbsolutePath
    val mainClass = mainClassName(functionName, compilerConfig)
    val output: Option[Array[String]] = compilerConfig.sbtConfigOpt match {
      case Some(sbtConfig) =>
        val dependencies:Array[String] = methodReplaceConf.flatMap(conf => conf.dependencies).toArray
        Some(Sbt.compile(sourcesDir, executableDir, functionName, compilerConfig.extraCompilerOptions, sbtConfig, dependencies, sourceFile, jarPath))
      case None =>
        Nsc.compile(executableDir, functionName, compilerConfig.extraCompilerOptions.toList, sourceFile, jarPath)
        None
    }
    CustomCompilerOutput(List(sourceFile), jarFile, mainClass, output)
  }

  def mainClassName(functionName: String, compilerConfig: LmsCompilerScalaConfig): String =
    compilerConfig.sbtConfigOpt.flatMap(_.mainPack).map(_ + ".") + functionName

  def loadMethod(compilerOutput: CompilerOutput[_, _]) = {
    // ensure Scala library is available
    val classLoader = getClassLoader(compilerOutput)
    val cls = classLoader.loadClass(compilerOutput.custom.mainClass)
    val argumentClass = compilerOutput.common.eInput.runtimeClass
    (cls, cls.getMethod("apply", argumentClass))
  }

  def getClassLoader(compilerOutput: CompilerOutput[_, _]): ClassLoader =
    new URLClassLoader(Array(compilerOutput.custom.jarUrl), getClass.getClassLoader)

  protected def doExecute[A, B](compilerOutput: CompilerOutput[A, B], input: A): B = {
    val (cls, method) = loadMethod(compilerOutput)
    val instance = cls.newInstance()
    val result = method.invoke(instance, input.asInstanceOf[AnyRef])
    result.asInstanceOf[B]
  }
}
