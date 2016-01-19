package scalan.primitives

import scalan.BaseTests

import scalan.{Scalan, ScalanDslExp, ScalanDslStd}

abstract class IfThenElseTests(scalan: Scalan) extends BaseTests {
  import scalan._

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


class IfThenElseTestsSeq extends IfThenElseTests(new ScalanDslStd)

// Note: these tests pass thanks to rewriting of IF with constants
class IfThenElseTestsExp extends IfThenElseTests(new ScalanDslExp)