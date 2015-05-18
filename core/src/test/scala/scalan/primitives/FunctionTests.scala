package scalan.primitives

import scala.language.reflectiveCalls
import scalan.common.SegmentsDslExp
import scalan.{BaseTests, TestContext}

class FunctionTests extends BaseTests { suite =>

  test("identity functions are equal") {
    val ctx = new TestContext(suite, "identityFuns") with SegmentsDslExp {
      lazy val t1 = identityFun[Int]
      lazy val t2 = identityFun[Int]
      lazy val t3 = identityFun[Double]
    }
    import ctx._
    t1 shouldEqual t2
    t1 shouldNot equal(t3)
  }

  test("const functions are equal") {
    val ctx = new TestContext(suite, "constFuns1") {
      lazy val t1 = constFun[Int, Int](1)
      lazy val t2 = constFun[Int, Int](1)
      lazy val t3 = constFun[Double, Int](1)
    }
    import ctx._

    t1 shouldEqual t2
    t1 shouldNot equal(t3)
  }

  test("ConstantLambda matcher works") {
    val ctx = new TestContext(suite, "constFuns2") {
      lazy val t1 = constFun[Int, Int](1)
      lazy val t2 = fun { x: Rep[Int] => fun { y: Rep[Int] => x } }
    }
    import ctx._

    emit("t1", t1)
    emit("t2", t2)
    t1 should matchPattern { case Def(ConstantLambda(_)) => }
    t2.getLambda.y should matchPattern { case Def(ConstantLambda(_)) => }
  }

}
