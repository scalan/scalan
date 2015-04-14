package scalan.collections

import scala.language.reflectiveCalls
import scalan.{ScalanCommunityDslExp, ViewTestsCtx, BaseTests}

class CollectionViewTests extends BaseTests { suite =>

  test("LambdaResultHasViews") {
    val ctx = new ViewTestsCtx(this, "LambdaResultHasViews")
                  with CollectionsDslExp with ScalanCommunityDslExp {
      lazy val t1 = fun { (in: Rep[Array[Int]]) => BaseCollection(in) }
      lazy val t2 = fun { (in: Rep[Array[Int]]) => (BaseCollection(in), in.length) }
      lazy val t3 = fun { (in: Rep[Array[Int]]) => (BaseCollection(in), in) }
      lazy val t4 = fun { (in: Rep[Array[Int]]) => PairCollection(BaseCollection(in), ListCollection(in.toList)) }
    }
    import ctx._
    testLambdaResultHasViews("t1", t1, element[Array[Int]])
    testLambdaResultHasViews("t2", t2, element[(Array[Int], Int)])
    testLambdaResultHasViews("t3", t3, element[(Array[Int], Array[Int])])
    testLambdaResultHasViews("t4", t4, element[(Collection[Int], Collection[Int])])
  }

}
