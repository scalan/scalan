package scalan.collections

import scalan.JNIExtractorOpsExp
import scalan.compilation.lms.scalac.{LmsCompilerScala, LmsCompilerScalaConfig}
import scalan.compilation.lms.uni.LmsCompilerUni
import scalan.compilation.lms.source2bin.SbtConfig
import scalan.it.BaseItTests

abstract class LmsJoinsItTests extends BaseItTests[JoinTests](new CollectionsDslStd with JoinTests) {
  class ProgExp extends CollectionsDslExp with JNIExtractorOpsExp with JoinTests

  val progStaged = new LmsCompilerScala(new ProgExp)
  val progStagedU = new LmsCompilerUni(new ProgExp)
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
    compareOutputWithStd(_.pairedInnerJoin, progStagedOnly)((xs, ys))
  }
  test("pairedOuterJoin") {
    compareOutputWithStd(_.pairedOuterJoin, progStagedOnly)((xs, ys))
  }
  test("commonInnerJoin") {
    compareOutputWithStd(_.commonInnerJoin, progStagedOnly)((xs, ys))
  }
  test("commonOuterJoin") {
    compareOutputWithStd(_.commonOuterJoin, progStagedOnly)((xs, ys))
  }
  test("SCALA ONLY commonInnerJoin with input functions") {
    compareOutputWithStd(_.commonInnerJoinFull, progStagedOnly)((xs, (ys, (b, (b, fm)))))
  }
  test("SCALA ONLY commonOuterJoin with input functions") {
    compareOutputWithStd(_.commonOuterJoinFull, progStagedOnly)((xs, (ys, (b, (b, (fa, (f1, f1)))))))
  }

  test("FAILING IN UniCompiler commonInnerJoin with input functions") {
    pending
    compareOutputWithStd(_.commonInnerJoinFull)((xs, (ys, (b, (b, fm)))))
  }
  test("FAILING IN UniCompiler commonOuterJoin with input functions") {
    pending
    compareOutputWithStd(_.commonOuterJoinFull)((xs, (ys, (b, (b, (fa, (f1, f1)))))))
  }

  test("extension innerMult1") {
    compareOutputWithStd(_.innerMult1, progStagedOnly)((xs, ys))
  }
  test("extension innerMult2") {
    compareOutputWithStd(_.innerMult2, progStagedOnly)((xs, ys))
  }
  test("extension innerMult3") {
    compareOutputWithStd(_.innerMult3, progStagedOnly)((xs, ys))
  }
  test("extension innerMult4") {
    compareOutputWithStd(_.innerMult4, progStagedOnly)((xs, ys))
  }
  test("extension outerSum1") {
    compareOutputWithStd(_.outerSum1, progStagedOnly)((xs, ys))
  }
  test("extension outerSum2") {
    compareOutputWithStd(_.outerSum2, progStagedOnly)((xs, ys))
  }
  test("extension outerSum3") {
    compareOutputWithStd(_.outerSum3, progStagedOnly)((xs, ys))
  }
  test("extension outerSum4") {
    compareOutputWithStd(_.outerSum4, progStagedOnly)((xs, ys))
  }
  test("extension outerSubtr1") {
    compareOutputWithStd(_.outerSubtr1, progStagedOnly)((xs, ys))
  }
  test("extension outerSubtr2") {
    compareOutputWithStd(_.outerSubtr2, progStagedOnly)((xs, ys))
  }
  test("extension outerSubtr3") {
    compareOutputWithStd(_.outerSubtr3, progStagedOnly)((xs, ys))
  }
  test("extension outerSubtr4") {
    compareOutputWithStd(_.outerSubtr4, progStagedOnly)((xs, ys))
  }
}
