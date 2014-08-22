package scalan.primitives

import scalan.BaseTests

import scalan.{Scalan, ScalanCtxExp, ScalanCtxSeq}

trait IfThenElseTests extends BaseTests with Scalan {
  test("simpleIf") {
    val res = IF (true) THEN 1 ELSE 0
    res shouldEqual toRep(1)
  }

  test("elseIf") {
    val res = IF (false) THEN 0 ELSEIF false THEN 1 ELSE 2
    res shouldEqual toRep(2)
  }

  test("nestedElseIf") {
    val res1 = IF (false) THEN 0 ELSEIF false THEN 1 ELSEIF true THEN 2 ELSE 3
    val res2 = IF (false) THEN 0 ELSEIF false THEN 1 ELSEIF false THEN 2 ELSE 3
    res1 shouldEqual toRep(2)
    res2 shouldEqual toRep(3)
  }
}


class IfThenElseTestsSeq extends IfThenElseTests with ScalanCtxSeq

// Note: these tests pass thanks to rewriting of
class IfThenElseTestsExp extends IfThenElseTests with ScalanCtxExp /*with StagedTesting*/