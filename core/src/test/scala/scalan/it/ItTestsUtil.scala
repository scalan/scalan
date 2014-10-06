package scalan.it

import java.io.File

import org.scalatest.{Matchers, Suite}

import scalan.codegen.Backend
import scalan.util.FileUtil

// extracted so it can be used with different suite styles
trait ItTestsUtil { self: Suite with Matchers =>

  lazy val prefix: String =
    getClass.getSimpleName.stripSuffix("Tests").stripSuffix("It").stripSuffix("_")

  def emitGraphs: Boolean = true

  def assertFileContentCheck(name: String): Unit =
    FileUtil.read(new File(prefix, name)) should be(FileUtil.read(new File(prefix, name + ".check")))

  // there are bad interactions between path-dependent types and default parameters, so
  // we can't simply make config a default parameter
  def getStagedOutput[A, B](back: Backend)(f: back.Exp[A => B], functionName: String, input: A): B =
    getStagedOutputConfig(back)(f, functionName, input, back.defaultConfig)

  def getStagedOutputConfig[A, B](back: Backend)(f: back.Exp[A => B], functionName: String, input: A, config: back.Config): B = {
    val dir = new File(new File("it-out", prefix), functionName)

    back.buildExecutable(dir, functionName, f, emitGraphs)(config)
    back.execute(dir, functionName, input, f)(config)
  }

  implicit def defaultComparator[A](expected: A, actual: A) {
    actual should equal(expected)
  }

  def compareOutputWithSequential[A, B](back: Backend)
                                       (fSeq: A => B, f: back.Exp[A => B], functionName: String, input: A)
                                       (implicit comparator: (B, B) => Unit) {
    compareOutputWithSequentialConfig(back)(fSeq, f, functionName, input, back.defaultConfig)
  }

  def compareOutputWithSequentialConfig[A, B](back: Backend)
                                       (fSeq: A => B, f: back.Exp[A => B], functionName: String, input: A, config: back.Config)
                                       (implicit comparator: (B, B) => Unit) {
    compareOutputWithExpectedConfig(back)(fSeq(input), f, functionName, input, config)
  }

  def compareOutputWithExpected[A, B](back: Backend)
                                     (expected: B, f: back.Exp[A => B], functionName: String, input: A)
                                     (implicit comparator: (B, B) => Unit) {
    compareOutputWithExpectedConfig(back)(expected, f, functionName, input, back.defaultConfig)
  }

  def compareOutputWithExpectedConfig[A, B](back: Backend)
                                     (expected: B, f: back.Exp[A => B], functionName: String, input: A, config: back.Config)
                                     (implicit comparator: (B, B) => Unit) {
    val actual = getStagedOutputConfig(back)(f, functionName, input, config)
    comparator(expected, actual)
  }
}
