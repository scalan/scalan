package scalan.universe.api

import scala.language.reflectiveCalls
import scalan.BaseCtxTests
import scalan.common.SegmentsModule

class UniverseUtilsTests extends BaseCtxTests { suite =>
  import UniverseUtils._
  class Ctx extends TestContext with SegmentsModule {
  }

  def f(i: Int) = (1 to i).toIterator

  test("getTuples1") {
    val list = List(3)
    val res = genTuples(list)(f)
    assertResult(List(List(1),List(2),List(3)))(res)
  }

  test("genTuples2") {
    val list = List(3,3)
    val res = genTuples(list)(f)
    assertResult(List(List(1,1),List(1,2),List(1,3),List(2,1),List(2,2),List(2,3),List(3,1),List(3,2),List(3,3)))(res)
  }

  test("genTuples3") {
    val list = List(1,2,3)
    val res = genTuples(list)(f)
    assertResult(List(List(1,1,1),List(1,1,2),List(1,1,3),List(1,2,1),List(1,2,2),List(1,2,3)))(res)
  }

  test("genTuples4") {
    val list = List(1,1,1)
    val res = genTuples(list)(f)
    assertResult(List(List(1,1,1)))(res)
  }

  test("genTuples5") {
    val list = List(1,0,1)
    val res = genTuples(list)(f)
    assertResult(List())(res)
  }


}
