package scalan.compilation

import java.io.File

import com.github.kxbmap.configs.syntax._
import com.typesafe.config.{Config, ConfigFactory}

import scalan.util.{ClassLoaderUtil, FileUtil}
import scalan.{Plugins, ScalanDslExp}

// TODO Split into AbstractKernel and FileSystemKernel?
class Kernel[+ScalanCake <: ScalanDslExp, A, B](val kernelName: String, val kernelType: KernelType, _kernelFunc: => ScalanCake#Exp[A => B], val compiler: Compiler[ScalanCake], dir: File, _config: Config) extends (A => B) {
  val scalan: compiler.scalan.type = compiler.scalan
  lazy val kernelFunc = _kernelFunc.asInstanceOf[scalan.Exp[A => B]]
  lazy val compilerConfig = _config.get[Option[Config]]("compiler") match {
    case None =>
      compiler.defaultCompilerConfig
    case Some(config) =>
      compiler.compilerConfigFrom(config)
  }
  lazy val graphVizConfig = _config.get[Option[Config]]("graphviz") match {
    case None =>
      GraphVizConfig.default
    case Some(config) =>
      GraphVizConfig.from(config)
  }
  lazy val compiled =
    compiler.buildExecutable(dir, kernelName, kernelFunc, graphVizConfig)(compilerConfig)
  lazy val eA = compiled.common.eInput
  lazy val eB = compiled.common.eOutput

  def apply(input: A) = compiler.execute(compiled, input)
}

// TODO add listKernels, loadKernel
abstract class KernelStore[+ScalanCake <: ScalanDslExp] {
  val scalan: ScalanCake
  val storeConfig: Config
  import Plugins.{configWithPlugins, pluginClassLoader}

  private val compilers = collection.mutable.Map.empty[KernelType, Compiler[scalan.type]]

  private def compiler(kernelType: KernelType): Compiler[scalan.type] = compilers.getOrElseUpdate(kernelType, {
    val confKey = s"scalan.backend.${kernelType.confKey}.compilerClass"
    configWithPlugins.getOpt[String](confKey) match {
      case None =>
        val msg =
          s"""Compiler class for kernel type ${kernelType.name} not found under $confKey in config including plugins.
              |Class path: ${ClassLoaderUtil.classPath(pluginClassLoader).map(_.getAbsolutePath).mkString(File.pathSeparator)}.
              |If necessary directory or jar file is missing, add it to ${Plugins.extraClassPathKey} property in application.conf or -D command line argument.""".stripMargin
        throw new CompilationException(msg, null)
      case Some(className) =>
        createCompiler(scalan, storeConfig, className)
    }
  })

  private def createCompiler(scalan: ScalanDslExp, config: Config, className: String): Compiler[scalan.type] = {
    val compilerClass = Plugins.loadClass(className)
    compilerClass.getConstructors match {
      case Array(constructor) =>
        constructor.getParameterTypes match {
          case Array(scalanRequiredClass) =>
            if (scalanRequiredClass.isInstance(scalan)) {
              constructor.newInstance(scalan).asInstanceOf[Compiler[scalan.type]]
            } else
              throw new IllegalArgumentException(s"$className requires ${scalanRequiredClass.getName}, but $scalan is not an instance.")
          case parameterClasses =>
            throw new IllegalArgumentException(s"The constructor of $className takes ${parameterClasses.length} parameters, must take 1 (Scalan instance)")
        }
      case constructors =>
        throw new IllegalArgumentException(s"$className has ${constructors.length} constructors, must have 1")
    }
  }

  def createKernel[A,B](kernelId: String, kernelType: KernelType, f: => scalan.Exp[A => B], kernelConfig: Config = ConfigFactory.empty()): Kernel[scalan.type, A, B] = {
    val allConfig = kernelConfig.withFallback(storeConfig)
    val compiler = this.compiler(kernelType)
    internalCreateKernel(kernelId, kernelType, f, compiler, allConfig)
  }

  def internalCreateKernel[A, B](kernelId: String, kernelType: KernelType, f: => scalan.Exp[(A) => B], compiler: Compiler[scalan.type], allConfig: Config): Kernel[scalan.type, A, B]
}

object KernelStore {
  def open(scalan: ScalanDslExp, baseDir: File, config: Config = ConfigFactory.empty()): KernelStore[scalan.type] = {
    val configFile = new File(baseDir, "kernelStore.conf")
    val config1 = if (configFile.exists())
      ConfigFactory.parseFile(configFile).withFallback(config)
    else
      config
    // TODO decide kernel store type based on config
    new FileSystemKernelStore[scalan.type](scalan, baseDir, config1)
  }
}

// TODO move methods for actually storing results from Compiler to here
class FileSystemKernelStore[+ScalanCake <: ScalanDslExp](val scalan: ScalanCake, val baseDir: File, val storeConfig: Config) extends KernelStore[ScalanCake] {
  def internalCreateKernel[A, B](kernelId: String, kernelType: KernelType, f: => scalan.Exp[(A) => B], compiler: Compiler[scalan.type], allConfig: Config): Kernel[scalan.type, A, B] = {
    if (FileUtil.isBadFileName(kernelId)) {
      throw new IllegalArgumentException(s"kernel id $kernelId contains special characters")
    }

    val dir = FileUtil.file(baseDir, kernelId, FileUtil.cleanFileName(kernelType.name))
    new Kernel(kernelId, kernelType, f, compiler, dir, allConfig)
  }
}
