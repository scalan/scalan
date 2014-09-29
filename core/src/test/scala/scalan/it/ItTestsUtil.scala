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

  def getStagedOutput[A, B](back: Backend)(f: back.Exp[A => B], functionName: String, input: A): B = {
    val dir = new File(new File("it-out", prefix), functionName)

    back.buildExecutable(dir, functionName, f, emitGraphs)
    back.execute(dir, functionName, input, f)
  }

  implicit def defaultComparator[A](expected: A, actual: A) {
    actual should equal(expected)
  }

  def compareOutputWithSequential[A, B](back: Backend)
                                       (fSeq: A => B, f: back.Exp[A => B], functionName: String, input: A)
                                       (implicit comparator: (B, B) => Unit) {
    compareOutputWithExpected(back)(fSeq(input), f, functionName, input)
  }

  def compareOutputWithExpected[A, B](back: Backend)
                                     (expected: B, f: back.Exp[A => B], functionName: String, input: A)
                                     (implicit comparator: (B, B) => Unit) {
    val actual = getStagedOutput(back)(f, functionName, input)
    comparator(expected, actual)
  }
}
