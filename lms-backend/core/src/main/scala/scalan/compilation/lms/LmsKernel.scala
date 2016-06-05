package scalan.compilation.lms

import java.io.File
import scalan.ScalanDslExp
import scalan.compilation._
import scalan.util.FileUtil
import scalan.util.FileUtil._

abstract class LmsKernel[+ScalanCake <: ScalanDslExp, C <: LmsCompiler[ScalanCake], A,B]
    (val compiler: C) extends Kernel[ScalanCake, A, B] {
  val scalan: ScalanCake = compiler.scalan
  import compiler.scalan._
}

abstract class LmsKernelStore[+ScalanCake <: ScalanDslExp, C <: LmsCompiler[ScalanCake]](val compiler: C, baseDir: File)
  extends KernelStore[ScalanCake] {
  val scalan: ScalanCake = compiler.scalan
  def graphVizConfig = GraphVizConfig.default

  private def sourceDir(functionName: String) = file(baseDir, functionName)
  private def prepareSourceDir(functionName: String, deleteBaseDir: Boolean = true) = {
    val baseDir = sourceDir(functionName)
    if (deleteBaseDir) {
      FileUtil.deleteIfExist(baseDir)
    }
    baseDir
  }

  def createKernel[A, B](kernelId: String, f: scalan.Exp[(A) => B]): Kernel[ScalanCake, A, B] = {
    val dir = prepareSourceDir(kernelId)
    assert(scalan == compiler.scalan)
    val out = compiler.buildExecutable(dir, kernelId, f.asInstanceOf[compiler.Exp[A => B]], graphVizConfig)(compiler.defaultCompilerConfig)
    new LmsKernel[ScalanCake, C, A, B](compiler) {
      def kernelFunc = f.asInstanceOf[scalan.Exp[A => B]]
      def apply(x: A): B = {
        val res = compiler.execute(out.asInstanceOf[compiler.CompilerOutput[A,B]], x)
        res
      }
    }
  }
}


