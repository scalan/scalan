package scalan.it.lms

import scalan.compilation.lms.CommunityBridge
import scalan.compilation.lms.uni.LmsCompilerUni
import scalan.{CommunityMethodMappingDSL, ScalanCommunityDslExp, JNIExtractorOpsExp}
import scalan.collections.{SimpleMapProg, MapItTests}
import scalan.compilation.lms.scalac.CommunityLmsCompilerScala

class LmsMapItTests extends MapItTests {
  class ProgExp extends ScalanCommunityDslExp with SimpleMapProg with JNIExtractorOpsExp

  val progStaged = new CommunityLmsCompilerScala(new ProgExp) with CommunityBridge

  val progStagedU = new LmsCompilerUni(new ProgExp) with CommunityBridge with CommunityMethodMappingDSL

  // TODO lack of maps support in LMS C++ backend
  val defaultCompilers = compilers(progStaged/*, progStagedU*/)

  test("mapPutContains") {
    val in = (314,3.14)
    compareOutputWithSequential(_.mapPutContains, "mapPutContains")(in)
  }
  test("mapAsSet") {
    val in = 314
    compareOutputWithSequential(_.mapAsSet, "mapAsSet")(in)
  }
  test("test9unionMaps") {
    val in = (Array((1, 1.1), (2, 2.2), (3, 3.3), (4, 4.4), (5, 5.5)), Array((0, 0.0), (2, 2.0), (4, 4.0), (6, 6.0)))
    compareOutputWithSequential(_.unionMaps, "unionMaps")(in)
  }
  test("test10differenceMaps") {
    val in = (Array((1, 1.1), (2, 2.2), (3, 3.3), (4, 4.4), (5, 5.5)), Array((0, 0.0), (2, 2.0), (4, 4.0), (6, 6.0)))
    compareOutputWithSequential(_.differenceMaps, "differenceMaps")(in)
  }
  test("test11iterateMap") {
    val in = Array((1, 1.1), (2, 2.2), (3, 3.3))
    compareOutputWithSequential(_.iterateMap, "iterateMap")(in)
  }
  test("test12mapReduce") {
    val in = Array(1, 2, 1, 1, 2, 3, 4, 1, 5, 4, 3, 2, 5, 2, 1)
    compareOutputWithSequential(_.mapReduceByKey, "mapReduce")(in)
  }
  test("test15join") {
    val in = (Array((1, 1.1), (2, 2.2), (3, 3.3), (4, 4.4), (5, 5.5)), Array((0, 0.0), (2, 2.0), (4, 4.0), (6, 6.0)))
    compareOutputWithSequential(_.joinMaps, "joinMaps")(in)
  }
  test("test16compoundMapKey") {
    val in = (Array((2, 1.0), (3, 2.0), (1, 3.0), (5, 4.0), (4, 5.0)), Array(1, 2, 3, 4, 5))
    compareOutputWithSequential(_.compoundMapKey, "compoundMapKey")(in)
  }
  test("test17reduceMaps") {
    val in = (Array((1, 1.1), (2, 2.2), (3, 3.3), (4, 4.4), (5, 5.5)), Array((0, 0.0), (2, 2.0), (4, 4.0), (6, 6.0)))
    compareOutputWithSequential(_.reduceMaps, "reduceMaps")(in)
  }
  test("test18groupByCount") {
    val in = Array((2, 1), (3, 2), (2, 5), (1, 3), (5, 4), (1, 3), (4, 5), (2, 4))
    compareOutputWithSequential(_.groupByCount, "groupByCount")(in)
  }
  test("test19groupBySum") {
    val in = Array((2, 1), (3, 2), (2, 5), (1, 3), (5, 4), (1, 3), (4, 5), (2, 4))
    compareOutputWithSequential(_.groupBySum, "groupBySum")(in)
  }
  test("test21compoundMapValue") {
    val in = (Array("one", "two", "three"), Array((1, 1.1), (2, 2.2), (3, 3.3)))
    compareOutputWithSequential(_.compoundMapValue, "compoundMapValue")(in)
  }

}
