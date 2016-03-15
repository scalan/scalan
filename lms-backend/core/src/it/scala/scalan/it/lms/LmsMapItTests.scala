package scalan.it.lms

import scalan.collections.{MapItTests, SimpleMapProg}
import scalan.compilation.lms.cxx.LmsCompilerCxx
import scalan.compilation.lms.scalac.LmsCompilerScala
import scalan.compilation.lms.uni.LmsCompilerUni
import scalan.it.BaseItTests
import scalan.{ScalanDslStd, JNIExtractorOpsExp, ScalanDslExp}

class LmsMapItTests extends MapItTests {
  class ProgExp extends ScalanDslExp with SimpleMapProg with JNIExtractorOpsExp

  val progStaged = new LmsCompilerScala(new ProgExp)

  val progStagedU = new LmsCompilerUni(new ProgExp)

  // TODO lack of maps support in LMS C++ backend
  val defaultCompilers = compilers(progStaged/*, progStagedU*/)
}

class LmsMapCxxItTests extends BaseItTests[SimpleMapProg](new ScalanDslStd with SimpleMapProg) {
  class ProgExp extends ScalanDslExp with SimpleMapProg with JNIExtractorOpsExp

  val progStaged = new LmsCompilerScala(new ProgExp)

  val progStagedU = new LmsCompilerUni(new ProgExp)
  val progStagedCxx = new LmsCompilerCxx(new ProgExp)

  // TODO lack of maps support in LMS C++ backend
  val defaultCompilers = compilers(progStaged)

  test("mapPutContains") {
    compareOutputWithStd(_.mapPutContains)(Tuple2(314,3.14))
  }
//  test("mapAsSet") {
//    compareOutputWithStd(_.mapAsSet)(314)
//  }
//  test("unionMaps") {
//    val in = (Array((1, 1.1), (2, 2.2), (3, 3.3), (4, 4.4), (5, 5.5)), Array((0, 0.0), (2, 2.0), (4, 4.0), (6, 6.0)))
//    compareOutputWithStd(_.unionMaps)(in)
//  }
//  test("differenceMaps") {
//    val in = (Array((1, 1.1), (2, 2.2), (3, 3.3), (4, 4.4), (5, 5.5)), Array((0, 0.0), (2, 2.0), (4, 4.0), (6, 6.0)))
//    compareOutputWithStd(_.differenceMaps)(in)
//  }
//  test("iterateMap") {
//    val in = Array((1, 1.1), (2, 2.2), (3, 3.3))
//    compareOutputWithStd(_.iterateMap)(in)
//  }
//  test("mapReduce") {
//    val in = Array(1, 2, 1, 1, 2, 3, 4, 1, 5, 4, 3, 2, 5, 2, 1)
//    compareOutputWithStd(_.mapReduceByKey)(in)
//  }
//  test("joinMaps") {
//    val in = (Array((1, 1.1), (2, 2.2), (3, 3.3), (4, 4.4), (5, 5.5)), Array((0, 0.0), (2, 2.0), (4, 4.0), (6, 6.0)))
//    compareOutputWithStd(_.joinMaps)(in)
//  }
  test("compoundMapKey") {
    val in = (Array((2, 1.0), (3, 2.0), (1, 3.0), (5, 4.0), (4, 5.0)), Array(1, 2, 3, 4, 5))
    compareOutputWithStd(_.compoundMapKey)(in)
  }
//  test("reduceMaps") {
//    val in = (Array((1, 1.1), (2, 2.2), (3, 3.3), (4, 4.4), (5, 5.5)), Array((0, 0.0), (2, 2.0), (4, 4.0), (6, 6.0)))
//    compareOutputWithStd(_.reduceMaps)(in)
//  }
//  test("groupByCount") {
//    val in = Array((2, 1), (3, 2), (2, 5), (1, 3), (5, 4), (1, 3), (4, 5), (2, 4))
//    compareOutputWithStd(_.groupByCount)(in)
//  }
//  test("groupBySum") {
//    val in = Array((2, 1), (3, 2), (2, 5), (1, 3), (5, 4), (1, 3), (4, 5), (2, 4))
//    compareOutputWithStd(_.groupBySum)(in)
//  }
//  test("compoundMapValue") {
//    val in = (Array("one", "two", "three"), Array((1, 1.1), (2, 2.2), (3, 3.3)))
//    compareOutputWithStd(_.compoundMapValue)(in)
//  }
}
