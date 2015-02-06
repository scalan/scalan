package scalan.it

import java.io.File

import org.scalatest.{Matchers, Suite}

import scalan.TestsUtil
import scalan.compilation.{GraphVizConfig, Compiler}
import scalan.util.FileUtil

// extracted so it can be used with different suite styles
trait ItTestsUtil extends TestsUtil { self: Suite with Matchers =>
  override def testOutDir = "it-out"

  // can be overridden
  def graphVizConfig = GraphVizConfig.default

  def assertFileContentCheck(name: String): Unit =
    FileUtil.read(FileUtil.file(prefix, name)) should be(FileUtil.read(FileUtil.file(prefix, name + ".check")))

  // there are bad interactions between path-dependent types and default parameters, so
  // we can't simply make config a default parameter
  def getStagedOutput[A, B](back: Compiler)(f: back.Exp[A => B], functionName: String, input: A): B =
    getStagedOutputConfig(back)(f, functionName, input, back.defaultCompilerConfig)

  def getStagedOutputConfig[A, B](back: Compiler)(f: back.Exp[A => B], functionName: String, input: A, compilerConfig: back.CompilerConfig): B = {
    val dir = FileUtil.file(prefix, functionName)

    val compiled = back.buildExecutable(dir, functionName, f, graphVizConfig)(compilerConfig)
    back.execute(compiled, functionName, input, f)(compilerConfig)
  }

  implicit def defaultComparator[A](expected: A, actual: A) {
    actual should equal(expected)
  }

  def compareOutputWithSequential[A, B](back: Compiler)
                                       (fSeq: A => B, f: back.Exp[A => B], functionName: String, input: A)
                                       (implicit comparator: (B, B) => Unit) {
    compareOutputWithSequentialConfig(back)(fSeq, f, functionName, input, back.defaultCompilerConfig)
  }

  def compareOutputWithSequentialConfig[A, B](back: Compiler)
                                       (fSeq: A => B, f: back.Exp[A => B], functionName: String, input: A, config: back.CompilerConfig)
                                       (implicit comparator: (B, B) => Unit) {
    compareOutputWithExpectedConfig(back)(fSeq(input), f, functionName, input, config)
  }

  def compareOutputWithExpected[A, B](back: Compiler)
                                     (expected: B, f: back.Exp[A => B], functionName: String, input: A)
                                     (implicit comparator: (B, B) => Unit) {
    compareOutputWithExpectedConfig(back)(expected, f, functionName, input, back.defaultCompilerConfig)
  }

  def compareOutputWithExpectedConfig[A, B](back: Compiler)
                                     (expected: B, f: back.Exp[A => B], functionName: String, input: A, config: back.CompilerConfig)
                                     (implicit comparator: (B, B) => Unit) {
    val actual = getStagedOutputConfig(back)(f, functionName, input, config)
    comparator(expected, actual)
  }
}
