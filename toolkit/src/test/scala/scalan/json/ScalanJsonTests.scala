package scalan.json

import scalan.{BaseNestedTests, Scalan}
import spray.json._

class ScalanJsonTests extends JsonTests {
  describe("Extraction of UnOps") {
    it("list classes") {
      val tester = getTester
      import tester._
      import protocol._
      import ctx._
      val ints = ctx.getClass.getInterfaces
      val unOpClasses = for {
        int <- ints
        c <- int.getDeclaredClasses if classOf[UnOp[_,_]].isAssignableFrom(c)
      } yield c
      println(unOpClasses)
    }
  }
  describe("Elem <-> Json iso") {
    val tester = getTester
    import tester._
    import protocol._
    import ctx._
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
    val graphJson =
      """{
       |  "type": "ProgramGraph",
       |  "s1": ["Const", "1", "Int"],
       |  "s2": {
       |    "type": "Lambda",
       |    "var": ["s4", "Int"],
       |    "s3": ["+(s4, s1)", "Int"]
       |  }
       |}""".stripMargin
    describe("Lambda printing") {
      val tester = getTester
      import tester._
      import protocol._
      import ctx._
      val f = fun { x: Rep[Int] => x + 1 }
      val g = new PGraph(f)
      print(g, graphJson)
    }
    describe("Lambda parsing") {
      val tester = getTester
      import tester._
      import protocol._
      import ctx._
      val f = fun { x: Rep[Int] => x + 1 }
      val g = new PGraph(f)
      parse(graphJson, g)
    }

  }

}
