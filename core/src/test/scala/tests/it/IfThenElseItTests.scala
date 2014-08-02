package tests.it.smoke

import scalan.codegen.GraphVizExport
import scalan.{ScalanCtxSeq, ScalanCtxStaged, ScalanDsl}
import tests.it.ItTests

class IfThenElseItTests extends ItTests {
  override lazy val prefix = "01-if"
  override val emitGraphs = true // set to true if you want to emit the graphs

  trait Prog extends ScalanDsl {

    lazy val trivialIf = fun { a: Rep[Boolean] =>
      IF (a) THEN { a } ELSE { a }
    }

    lazy val simpleIf = fun { a: Rep[Int] =>
      IF (a > 0) THEN { a * a + 1} ELSE { a + 2 }
    }

    lazy val nestedIfCond = fun { a: Rep[Int] =>
      IF (IF (a > 0) THEN { a < 10 } ELSE { a > 20 }) THEN { a * a + 1 } ELSE { a + 2 }
    }

    lazy val nestedIfThen = fun { a: Rep[Int] =>
      IF (a > 0) THEN { IF (a < 10) THEN {a * a + 1} ELSE {a + 2} } ELSE { a - 1 }
    }

    lazy val nestedIfElse = fun { a: Rep[Int] =>
      IF (a > 0) THEN {  a - 1 } ELSE { IF (a < 10) THEN {a + 1} ELSE {a * a + 2} }
    }

    lazy val nestedIfElseCond = fun { a: Rep[Int] =>
      IF (a < 0) THEN {
        a - 1
      } ELSE {
        IF (IF (a > 0) THEN { a < 10 } ELSE { a > 20 }) THEN {
          a * a + 1
        } ELSE {
          a + 2
        }
      }
    }

    lazy val ifCommonUsage = fun { a: Rep[Int] =>
      val b = a * 2
      IF (a > 0) THEN { a + b } ELSE { a * a - b * b }
    }

    lazy val ifCommonNestedUsage = fun { a: Rep[Int] =>
      IF (a > 0) THEN {
        val b = a * 2
        IF (a < 10) THEN {a + b} ELSE {a * a - b * b}
      } ELSE {
        a - 1
      }
    }

    lazy val ifCommonCrossLevelUsage = fun { a: Rep[Int] =>
      val b = a * 2
      IF (a > 0) THEN {
        IF (a < 10) THEN {a + b} ELSE {a - 1}
      } ELSE {
        a - b
      }
    }

    lazy val ifCommonCrossLevelCondUsage = fun { a: Rep[Int] =>
      val b = a * 2
      IF (a > b) THEN {
        IF (a < 10) THEN {a + b} ELSE {a - 1}
      } ELSE {
        a - 1
      }
    }

    lazy val ifCommonCrossLevelCondNestedUsage = fun { a: Rep[Int] =>
      val c = a * 3
      IF (a < c + 1) {
        val b = a * 2
        IF (a > b * b) THEN {
          IF (a < 10) THEN {a + b + c} ELSE {a - 1}
        } ELSE {
          a - 1
        }
      } ELSE {
        a - 2
      }
    }

    lazy val ifCommonCrossLevelOuterUsage = fun { a: Rep[Int] =>
      val b = a * 2
      val c = IF (a > 0) THEN {
        IF (a < 10) THEN {a + b} ELSE {a - 1}
      } ELSE {
        a - 1
      }
      c + b
    }

//    lazy val ifLoopInit = fun { a: Rep[Int] =>
//      val b = a * 2
//      val r = loopUntil[Int](IF (a < 10) THEN {a + b} ELSE {a - 1})((x: Rep[Int]) => x < 10, (x: Rep[Int]) => x + 1)
//      r + a
//    }
//
//    lazy val ifLoopState = fun { a: Rep[Int] =>
//      val b = a * 2
//      val r = loopUntil[Int](0)((x: Rep[Int]) => x < 10, (x: Rep[Int]) => IF (x < 10) THEN {x + b} ELSE {a - 1})
//      r + a
//    }
//
//    lazy val ifLoopNestedState = fun { a: Rep[Int] =>
//      val b = a * 2
//      val step = (x: Rep[Int]) => {
//        loopUntil(0)((y: Rep[Int]) => y < 10, (y: Rep[Int]) => IF (y < 10) THEN {y + x} ELSE {a - 1})
//      }
//      val r = loopUntil(0)((x: Rep[Int]) => x < 10, step)
//      r + a
//    }
//
//    lazy val ifWithLoopInBranch = fun { a: Rep[Int] =>
//      val b = a * 2
//      IF (b > 0) THEN {
//        val step = (x: Rep[Int]) => {
//          loopUntil(0)((y: Rep[Int]) => y < 10, (y: Rep[Int]) => IF (y < 10) THEN {y + x} ELSE {a - 1})
//        }
//        val r = loopUntil(0)((x: Rep[Int]) => x < 10, step)
//        r + a
//      } ELSE {a}
//    }
//
//    lazy val ifLoopIfLoopThenOp = fun { a: Rep[Int] =>
//      val b = a * 2
//
//      val step = (x: Rep[Int]) => {
//        IF (b > x) THEN {
//          val r = loopUntil(0)((y: Rep[Int]) => y < 10, (y: Rep[Int]) => IF (y < 10) THEN {y + x} ELSE {a - 1})
//          r + x
//        } ELSE { x }
//      }
//      val r = loopUntil(0)((x: Rep[Int]) => x < 10, step)
//      r + a
//    }
//
//    lazy val ifLoopIfLoopThenOpPairs = fun { p: Rep[(Int,Int)] =>
//      val b = p._1 * 2
//
//      val step = (xp: Rep[(Int,Int)]) => {
//        IF (b > xp._1) THEN {
//          val r = loopUntil((0,0))((y: Rep[(Int,Int)]) => y._1 < 10, (y: Rep[(Int,Int)]) => IF (y._1 < 10) THEN {Pair(y._1 + xp._1, y._2 + xp._2)} ELSE {Pair(p._1 - 1, p._2 - 1)})
//          Pair(r._1 + xp._1,0)
//        } ELSE { xp }
//      }
//      val r = loopUntil((0,0))((x: Rep[(Int,Int)]) => x._1 < 10, step)
//      val res = r._2 + p._1
//      Pair(res, res)
//    }

  }

  class ProgStaged extends Prog with ScalanCtxStaged
  class ProgSeq extends Prog with ScalanCtxSeq
  val progStaged = new ProgStaged()
  val progSeq = new ProgSeq()

  import progSeq._

// TODO move to ScalanEE
//  test("trivialIf") {
//    val (in, out) = true -> true
//    progSeq.trivialIf(in) should be(out)
//    checkRun(progSeq, progStaged)(progSeq.trivialIf, progStaged.trivialIf)("trivialIf", in)
//  }
//
//  test("simpleIf") {
//    val (in, out) = 1 -> 2
//    Assert.assertEquals(out, progSeq.simpleIf(in))
//    checkRun(progSeq, progStaged)(progSeq.simpleIf, progStaged.simpleIf)("simpleIf", in)
//  }
//
//  test("nestedIfCond") {
//    val (in, out) = 1 -> 2
//    Assert.assertEquals(out, progSeq.nestedIfCond(in))
//    checkRun(progSeq, progStaged)(progSeq.nestedIfCond, progStaged.nestedIfCond)("nestedIfCond", in)
//  }
//
//  test("nestedIfThen") {
//    val (in, out) = 1 -> 2
//    Assert.assertEquals(out, progSeq.nestedIfThen(in))
//    checkRun(progSeq, progStaged)(progSeq.nestedIfThen, progStaged.nestedIfThen)("nestedIfThen", in)
//  }
//
//  test("nestedIfElse") {
//    val (in, out) = 1 -> 0
//    Assert.assertEquals(out, progSeq.nestedIfElse(in))
//    checkRun(progSeq, progStaged)(progSeq.nestedIfElse, progStaged.nestedIfElse)("nestedIfElse", in)
//  }
//
//  test("nestedIfElseCond") {
//    val (in, out) = 1 -> 2
//    Assert.assertEquals(out, progSeq.nestedIfElseCond(in))
//    checkRun(progSeq, progStaged)(progSeq.nestedIfElseCond, progStaged.nestedIfElseCond)("nestedIfElseCond", in)
//  }
//
//  test("ifCommonUsage") {
//    val (in, out) = 1 -> 3
//    Assert.assertEquals(out, progSeq.ifCommonUsage(in))
//    checkRun(progSeq, progStaged)(progSeq.ifCommonUsage, progStaged.ifCommonUsage)("ifCommonUsage", in)
//  }
//
//  test("ifCommonNestedUsage") {
//    val (in, out) = 1 -> 3
//    Assert.assertEquals(out, progSeq.ifCommonNestedUsage(in))
//    checkRun(progSeq, progStaged)(progSeq.ifCommonNestedUsage, progStaged.ifCommonNestedUsage)("ifCommonNestedUsage", in)
//  }
//
//  test("ifCommonCrossLevelUsage") {
//    val (in, out) = 1 -> 3
//    Assert.assertEquals(out, progSeq.ifCommonCrossLevelUsage(in))
//    checkRun(progSeq, progStaged)(progSeq.ifCommonCrossLevelUsage, progStaged.ifCommonCrossLevelUsage)("ifCommonCrossLevelUsage", in)
//  }
//
//  test("ifCommonCrossLevelCondUsage") {
//    val (in, out) = 1 -> 0
//    Assert.assertEquals(out, progSeq.ifCommonCrossLevelCondUsage(in))
//    checkRun(progSeq, progStaged)(progSeq.ifCommonCrossLevelCondUsage, progStaged.ifCommonCrossLevelCondUsage)("ifCommonCrossLevelCondUsage", in)
//  }
//
//  test("ifCommonCrossLevelCondNestedUsage") {
//    val (in, out) = 1 -> 0
//    Assert.assertEquals(out, progSeq.ifCommonCrossLevelCondNestedUsage(in))
//    checkRun(progSeq, progStaged)(progSeq.ifCommonCrossLevelCondNestedUsage, progStaged.ifCommonCrossLevelCondNestedUsage)("ifCommonCrossLevelCondNestedUsage", in)
//  }
//
//  test("ifCommonCrossLevelOuterUsage") {
//    val (in, out) = 1 -> 5
//    Assert.assertEquals(out, progSeq.ifCommonCrossLevelOuterUsage(in))
//    checkRun(progSeq, progStaged)(progSeq.ifCommonCrossLevelOuterUsage, progStaged.ifCommonCrossLevelOuterUsage)("ifCommonCrossLevelOuterUsage", in)
//  }
//
//  test("ifLoopInit") {
//    val (in, out) = 1 -> 4
//    Assert.assertEquals(out, progSeq.ifLoopInit(in))
//    checkRun(progSeq, progStaged)(progSeq.ifLoopInit, progStaged.ifLoopInit)("ifLoopInit", in)
//  }
//
//  test("ifLoopState") {
//    val (in, out) = 1 -> 1
//    Assert.assertEquals(out, progSeq.ifLoopState(in))
//    checkRun(progSeq, progStaged)(progSeq.ifLoopState, progStaged.ifLoopState)("ifLoopState", in)
//  }
//
//  test("ifLoopNestedState") {
//    val (in, out) = 1 -> 1
//    Assert.assertEquals(out, progSeq.ifLoopNestedState(in))
//    checkRun(progSeq, progStaged)(progSeq.ifLoopNestedState, progStaged.ifLoopNestedState)("ifLoopNestedState", in)
//  }
//
//  test("ifWithLoopInBranch") {
//    val (in, out) = 1 -> 1
//    Assert.assertEquals(out, progSeq.ifWithLoopInBranch(in))
//    checkRun(progSeq, progStaged)(progSeq.ifWithLoopInBranch, progStaged.ifWithLoopInBranch)("ifWithLoopInBranch", in)
//  }
//
//  test("ifLoopIfLoopThenOp") {
//    val (in, out) = 1 -> 1
//    Assert.assertEquals(out, progSeq.ifLoopIfLoopThenOp(in))
//    checkRun(progSeq, progStaged)(progSeq.ifLoopIfLoopThenOp, progStaged.ifLoopIfLoopThenOp)("ifLoopIfLoopThenOp", in)
//  }
//
//  test("ifLoopIfLoopThenOpPairs") {
//    val (in, out) = (1,1) -> (1,1)
//    Assert.assertEquals(out, progSeq.ifLoopIfLoopThenOpPairs(in))
//    checkRun(progSeq, progStaged)(progSeq.ifLoopIfLoopThenOpPairs, progStaged.ifLoopIfLoopThenOpPairs)("ifLoopIfLoopThenOpPairs", in)
//  }

}
