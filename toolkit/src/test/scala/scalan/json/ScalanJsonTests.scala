package scalan.json

import scalan.{BaseNestedTests, Scalan}
import spray.json._

class ScalanJsonTests extends JsonTests {
  describe("Elem <-> Json iso") {
    object ScalanJsonProtocol extends ScalanJsonProtocol(new Scalan)
    import ScalanJsonProtocol._
    import ScalanJsonProtocol.ctx._
    val tester = new JsonFormatTester(ScalanJsonProtocol.ctx)
    import tester._
    describe("Elem parsing") {
      parse(""" "String" """, element[String])
      parse(""" "Int" """, element[Int])
      parse(""" "Float" """, element[Float])
      parse(""" "(String,Float)" """, element[(String, Float)])
      parse(""" "scala.Tuple2[String, Float]" """, element[(String, Float)])
      parse(""" "scala.Function1[String, Float]" """, element[(String => Float)])
    }
    describe("Elem printing") {
      print(element[String], """"String"""")
      print(element[Int], """"Int"""")
      print(element[Float], """"Float"""")
      print(element[(String, Float)], """"scala.Tuple2[String, Float]"""")
      print(element[(String => Float)], """"scala.Function1[String, Float]"""")
    }
    describe("Elem <-> Json iso property") {
      test(element[String])
      test(element[Int])
      test(element[Float])
      test(element[(String, Float)])
      test(element[(String => Float)])
    }
  }
  describe("ProgramGraph <-> Json iso") {
    object ScalanJsonProtocol extends ScalanJsonProtocol(new Scalan)
    import ScalanJsonProtocol._
    import ScalanJsonProtocol.ctx._
    val tester = new JsonFormatTester(ScalanJsonProtocol.ctx)
    import tester._
    val f = fun { x: Rep[Int] => x + 1 }
    describe("Lambda parsing") {

    }
    describe("Lambda printing") {
      val g = new PGraph(f)
      print(g,
        """{
         |  "type": "ProgramGraph",
         |  "s1": ["Const", "1", "Int"],
         |  "s2": {
         |    "type": "Lambda",
         |    "var": ["s4", "Int"],
         |    "s3": ["+(s4, s1)", "Int"]
         |  }
         |}""".stripMargin)
    }
  }
}
