package scalanizer.collections

import scala.language.reflectiveCalls
import scala.wrappers.WrappersModule
import scalan._

//class ColsTests extends BaseCtxTests {
//  class Ctx extends TestContext with ColsModule {
//    lazy val t1 = fun { (xs: Rep[WArray[Double]]) =>
//      Col.ddmvm(xs)
//    }
////        lazy val t2 = fun { (xs: Rep[WArray[Int]]) => xs(10) }
////        lazy val t3 = fun { (xs: Rep[WArray[Int]]) => xs.zip(WArray.fill(xs.length, Thunk(10))) }
////        lazy val t4 = fun { (xs: Rep[WArray[Int]]) => xs.map(fun {x => x + 1}) }
//  }
//
//  test("Col methods") {
//    val ctx = new Ctx {
//      val M = WArrayMethods; val C = WArrayCompanionMethods
//      def test() = {
////        { val Def(Lambda(_, _, x, M.length(obj))) = t1; assert(x == obj) }
////        { val Def(Lambda(_, _, x, M.apply(obj, Def(Const(10))))) = t2; assert(x == obj) }
////        { val Def(Lambda(_, _, x, M.zip(xs, C.fill(M.length(xs1), th)))) = t3;
////          assert(x == xs)
////          assert(xs == xs1) }
//      }
//    }
//    ctx.test()
//    ctx.emit("t1", ctx.t1)
////    ctx.emit("t2", ctx.t2)
////    ctx.emit("t3", ctx.t3)
////    ctx.emit("t4", ctx.t4)
//  }
//
//}
