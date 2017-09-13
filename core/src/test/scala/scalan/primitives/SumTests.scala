package scalan.primitives

import scala.language.reflectiveCalls
import scalan.BaseCtxTests
import scalan.common.{CommonExamples, SegmentsModule, ViewExamples}

class SumTests extends BaseCtxTests {

  test("IsSumMapLambda") {
    val ctx = new TestContext with SegmentsModule {
      lazy val t1 = fun { x: Rep[Int|Unit] => x.mapSum(l => l + 1, r => r) }
    }
    import ctx._
    t1 should matchPattern { case Def(IsSumMapLambda(_)) => }
  }

  test("constant propagation from If to SumFold") {
    val ctx = new TestContext("fromIfToSumFold") with SegmentsModule {
      lazy val t1 = fun { x: Rep[Int] =>
        val s = IF (x > 0) THEN { (x + 1).asLeft[Int] } ELSE { (x + 2).asRight[Int] }
        s.fold(l => l + 1, r => r - 2)
      }
      lazy val t2 = fun { x: Rep[Int] =>
        val s = IF (x > 0) THEN { (x + 1).asRight[Int] } ELSE { (x + 2).asLeft[Int] }
        s.fold(l => l + 1, r => r - 2)
      }
    }
    import ctx._
    emit("t1", t1)
    emit("t2", t2)
  }

  test("SumMap(Right(x)) rewriting") {
    val ctx = new TestContext("SumMapRightRewriting") {
      lazy val t1 = fun { x: Rep[Int] =>
        x.asRight[Int].mapSum(_ + 1, _ - 1)
      }
    }
    import ctx._

    emit("t1", t1)
    t1.getLambda.y should matchPattern { case Def(SRight(_)) => }
  }

  test("SumMap(Left(x)) rewriting") {
    val ctx = new TestContext("SumMapLeftRewriting") {
      lazy val t1 = fun { x: Rep[Int] =>
        x.asLeft[Int].mapSum(_ + 1, _ - 1)
      }
    }
    import ctx._

    emit("t1", t1)
    t1.getLambda.y should matchPattern { case Def(SLeft(_)) => }
  }

  test("isLeft and isRight work") {
    val ctx = new TestContext() {
      lazy val isLeftFun = fun { x: Rep[Int | Double] => x.isLeft }
      lazy val isRightFun = fun { x: Rep[Int | Double] => x.isRight }

      lazy val shouldBeTrue = toRep(10).asLeft[Double].isLeft
    }
    import ctx._

    isLeftFun.getLambda.y should matchPattern { case Def(IsLeft(_)) => }
    isRightFun.getLambda.y should matchPattern { case Def(IsRight(_)) => }
    shouldBeTrue shouldEqual toRep(true)
  }
}
