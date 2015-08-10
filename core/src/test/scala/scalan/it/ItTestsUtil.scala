package scalan.it

import java.io.File

import org.scalatest.{Matchers, Suite}

import scalan.{ScalanExp, TestsUtil}
import scalan.compilation.{GraphVizConfig, Compiler}
import scalan.util.FileUtil
import scalan.util.FileUtil.file

// extracted so it can be used with different suite styles
trait ItTestsUtil extends TestsUtil { self: Suite with Matchers =>
  override def testOutDir = "it-out"

  // can be overridden
  def graphVizConfig = GraphVizConfig.default

  def assertFileContentCheck(name: String): Unit =
    FileUtil.read(file(prefix, name)) should be(FileUtil.read(file(prefix, name + ".check")))

  // there are bad interactions between path-dependent types and default parameters, so
  // we can't simply make config a default parameter
  def getStagedOutput[A, B](back: Compiler[_ <: ScalanExp])(f: back.scalan.Exp[A => B], functionName: String, input: A): B =
    getStagedOutputConfig(back)(f, functionName, input, back.defaultCompilerConfig)

  def getStagedOutputConfig[A, B](back: Compiler[_ <: ScalanExp])(f: back.scalan.Exp[A => B], functionName: String, input: A, compilerConfig: back.CompilerConfig): B = {
    val compiled = compileSource(back)(f, functionName, compilerConfig)
    back.execute(compiled, input)
  }

  def compileSource[A, B](back: Compiler[_ <: ScalanExp])(f: back.scalan.Exp[A => B], functionName: String, compilerConfig: back.CompilerConfig) : back.CompilerOutput[A, B] = {
    back.buildExecutable(file(prefix, functionName), functionName, f, graphVizConfig)(compilerConfig)
  }

  implicit def defaultComparator[A](expected: A, actual: A) {
    actual should equal(expected)
  }

  def compareOutputWithSequential[A, B](back: Compiler[_ <: ScalanExp])
                                       (fSeq: A => B, f: back.scalan.Exp[A => B], functionName: String, input: A)
                                       (implicit comparator: (B, B) => Unit) {
    compareOutputWithSequentialConfig(back)(fSeq, f, functionName, input, back.defaultCompilerConfig)
  }

  def compareOutputWithSequentialConfig[A, B](back: Compiler[_ <: ScalanExp])
                                       (fSeq: A => B, f: back.scalan.Exp[A => B], functionName: String, input: A, config: back.CompilerConfig)
                                       (implicit comparator: (B, B) => Unit) {
    compareOutputWithExpectedConfig(back)(fSeq(input), f, functionName, input, config)
  }

  def compareOutputWithExpected[A, B](back: Compiler[_ <: ScalanExp])
                                     (expected: B, f: back.scalan.Exp[A => B], functionName: String, input: A)
                                     (implicit comparator: (B, B) => Unit) {
    compareOutputWithExpectedConfig(back)(expected, f, functionName, input, back.defaultCompilerConfig)
  }

  def compareOutputWithExpectedConfig[A, B](back: Compiler[_ <: ScalanExp])
                                     (expected: B, f: back.scalan.Exp[A => B], functionName: String, input: A, config: back.CompilerConfig)
                                     (implicit comparator: (B, B) => Unit) {
    val actual = getStagedOutputConfig(back)(f, functionName, input, config)
    comparator(expected, actual)
  }
}
