package scalan.collections

import scalan.compilation.lms.uni._
import scalan.compilation.lms.collections.{CollectionsLmsCompilerUni, CollectionsLmsCompilerScala}
import scalan.compilation.lms.scalac.LmsCompilerScalaConfig
import scalan.compilation.lms.source2bin.SbtConfig
import scalan.compilation.lms.uni.LmsCompilerUni
import scalan.compilation.lms.ScalaCoreLmsBackend
import scalan.arrays.ArrayOpsExp
import scalan.it.BaseItTests
import scalan.{JNIExtractorOpsExp, ScalanDslExp}

abstract class LmsJoinsItTests extends BaseItTests[JoinTests](new CollectionsDslStd with JoinTests) {
  class ProgExp extends CollectionsDslExp with JNIExtractorOpsExp with JoinTests

  val progStaged = new CollectionsLmsCompilerScala(new ProgExp)
  val progStagedU = new CollectionsLmsCompilerUni(new ProgExp)
  val compilerConfigU = LmsCompilerScalaConfig().withSbtConfig(SbtConfig(scalaVersion = "2.11.2"))

  val defaultCompilers = compilers(progStaged, cwc(progStagedU)(compilerConfigU))
  val progStagedOnly = compilers(progStaged)
}

class LmsJoinsItTestsSuite extends LmsJoinsItTests {

  val b = (_: (Int, Double))._1
  val f1 = (_: (Int, Double))._2
  val f = (x: (((Int, Double), (Int, Double)))) => x._1._2 * x._2._2

  val xs = Array((1, 2.0), (2, 3.0), (3, 4.0))
  val ys = Array((1, 1.0), (3, 2.0), (5, 3.0))

  test("innerJoin") {
    compareOutputWithStd(_.innerJoin)((xs, ys))
  }

  test("outerJoin") {
    compareOutputWithStd(_.outerJoin)((xs, ys))
  }

  test("innerJoinGeneric") {
    compareOutputWithStd(_.innerJoinGeneric)((xs, ys))
  }

  test("outerJoinGeneric") {
    compareOutputWithStd(_.outerJoinGeneric)((xs, ys))
  }

  test("innerJoinGenericWithFunctions") {
    compareOutputWithStd(_.innerJoinGenFull, progStagedOnly)((xs, (ys, (b, (b, f)))))
  }

  test("outerJoinGenericWithFunctions") {
    compareOutputWithStd(_.outerJoinGenFull, progStagedOnly)((xs, (ys, (b, (b, (f, (f1, f1)))))))
  }
}
