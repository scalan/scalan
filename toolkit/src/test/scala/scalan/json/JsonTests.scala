package scalan.json

import scalan.{BaseNestedTests, Scalan}
import spray.json.JsonFormat
import spray.json._

import scalan.util.FileUtil

trait JsonTests extends BaseNestedTests {
  val resourcesDir = "toolkit/src/test/resources/scalan/json"

  class JsonFormatTester[C <: Scalan](val protocol: ScalanJsonProtocol[C]) {
    import protocol._
    import ctx._

    def parse[T: JsonFormat](str: String, expected: T): Unit = {
      it(s"Parse $str") {
        val json = str.parseJson
        val parsed = json.convertTo[T]
        parsed should be(expected)
      }
    }

    def print[T: JsonFormat](e: T, expected: String): Unit = {
      it(s"Print $e") {
        val json = e.toJson
        val printed = json.prettyPrint
        printed should be(expected)
      }
    }

    def test[T: JsonFormat](e: T, fileName: String = ""): Unit = {
      val testName = if (fileName.nonEmpty) fileName else e.toString
      it(s"for $testName") {
        val json = e.toJson
        val printed = json.prettyPrint

        if (fileName.nonEmpty)
          FileUtil.withFile(FileUtil.file(prefix, fileName + ".json")) { w =>
            w.println(printed)
          }

        val parsed = printed.parseJson.convertTo[T]
        parsed should be(e)
      }
    }
  }

  def getTester[C <: Scalan](ctx: C) = {
    val tester = new JsonFormatTester(new ScalanJsonProtocol(ctx))
    tester
  }
}
