package scalan.json

import scala.wrappers.WrappersModule
import scalan.Scalan

class ApiJsonTests extends JsonTests {
  class Ctx extends Scalan with WrappersModule
  describe("Wrapped methods <-> Json") {
    val tester = getTester(new Ctx)
    import tester._
    import protocol._
    import ctx._
    def testLam[A, B](f: Rep[A => B], fileName: String = ""): Unit = {
      val g = new PGraph(f)
      test(g, fileName)
    }

    testLam(fun { xs: Rep[WArray[Int]] => xs.length }, "lambda with Pair argument")
  }
}
