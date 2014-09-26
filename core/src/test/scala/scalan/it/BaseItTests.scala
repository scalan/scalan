package scalan.it

import java.io.File

import scala.io.Source
import scalan.BaseTests
import scalan.codegen.Backend

abstract class BaseItTests extends BaseTests {

  lazy val prefix: String =
    getClass.getSimpleName.stripSuffix("Tests").stripSuffix("It").stripSuffix("_")

  def emitGraphs: Boolean = true

  def readFile(name: String): String =
    Source.fromFile(name).getLines().toIterator.mkString("\n")

  def assertFileContentCheck(name: String): Unit = {
    readFile(name + ".check") should be(readFile(name))
  }

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
