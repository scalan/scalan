package scalan.collections

import scalan.JNIExtractorOpsExp
import scalan.compilation.lms.collections.{CollectionsLmsCompilerScala, CollectionsLmsCompilerUni}
import scalan.compilation.lms.scalac.LmsCompilerScalaConfig
import scalan.compilation.lms.source2bin.SbtConfig
import scalan.it.BaseItTests

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
  val fm = (x: (((Int, Double), (Int, Double)))) => x._1._2 * x._2._2
  val fa = (x: (((Int, Double), (Int, Double)))) => x._1._2 + x._2._2

  val xs = Array((1, 2.0), (2, 3.0), (3, 4.0))
  val ys = Array((1, 1.0), (3, 2.0), (5, 3.0))

  test("pairedInnerJoin") {
    compareOutputWithStd(_.innerJoin)((xs, ys))
  }
  test("pairedOuterJoin") {
    compareOutputWithStd(_.outerJoin)((xs, ys))
  }
  test("commonInnerJoin") {
    compareOutputWithStd(_.innerJoinCommon)((xs, ys))
  }
  test("commonOuterJoin") {
    compareOutputWithStd(_.outerJoinCommon)((xs, ys))
  }
  test("SCALA ONLY commonInnerJoin with input functions") {
    compareOutputWithStd(_.innerJoinCommonFull, progStagedOnly)((xs, (ys, (b, (b, fm)))))
  }
  test("SCALA ONLY commonOuterJoin with input functions") {
    compareOutputWithStd(_.outerJoinCommonFull, progStagedOnly)((xs, (ys, (b, (b, (fa, (f1, f1)))))))
  }

  test("FAILING IN UniCompiler commonInnerJoin with input functions") {
    pending
    compareOutputWithStd(_.innerJoinCommonFull)((xs, (ys, (b, (b, fm)))))
  }
  test("FAILING IN UniCompiler commonOuterJoin with input functions") {
    pending
    compareOutputWithStd(_.outerJoinCommonFull)((xs, (ys, (b, (b, (fa, (f1, f1)))))))
  }
}
