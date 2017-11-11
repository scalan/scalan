package scalan.json

import scala.wrappers.WrappersModule
import scalan.Scalan

class ConstJsonTests extends JsonTests {

  class Ctx extends Scalan with WrappersModule

  describe("Const value <-> Json") {
    val tester = getTester(new Ctx)
    import tester._
    import protocol._
    import ctx._
    def test[A: Elem](v: A, fileName: String = ""): Unit = {
      val eA = element[A]
      it(s"for ${eA.name}") {
        val js = toJsonValue(v, eA)
        val v1 = fromJsonValue(js, eA)
        v1 should be(v)
        println(s"$v -> $js -> $v1")
      }
    }

    test(true)
    test("abc")
    test(10)
    test(10L)
    test(10: Short)
    test(10: Byte)
    test(10.0f)
    test(10.0d)
    test(())
    test('a')
    test((10, "abc"))
    test(((10, "abc"), true))
    test[Int|String](Left(10))
    test[Boolean|String](Right("abc"))
    test[(Boolean, String)|Unit](Left((true, "abc")))
    test[Unit|(Int, String)](Right((10, "abc")))
    test[(Int|String, Int|String)]((Left(10), Right("abc")))
  }
}
