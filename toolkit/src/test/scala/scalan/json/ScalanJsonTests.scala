package scalan.json

import scalan.{BaseNestedTests, Scalan}
import spray.json._
import DefaultJsonProtocol._

class ScalanJsonTests extends BaseNestedTests with JsonTests {

  class Ctx extends Scalan {
  }

  class ScalanJsonProtocol[C <: Scalan](val ctx: C) extends DefaultJsonProtocol {
    import ctx._
    import parsers._
    implicit val context = parsers.context  // for resolution and disambiguation
    implicit val parseCtx = new ParseCtx(false)
    implicit val genCtx = new GenCtx(context, toRep = false)

    implicit def elementFormat[T]: JsonFormat[Elem[T]] = new JsonFormat[Elem[T]] {
      def write(e: Elem[T]) = {
        val tpe = e.toTpeExpr
        val ty = genTypeExpr(tpe)
        val tpeStr = global.showCode(ty)
        JsString(tpeStr)
      }
      def read(json: JsValue) = json match {
        case JsString(tpeStr) =>
          val ty = parseType(tpeStr)
          val elem = TypeDesc(ty, emptySubst).asElem[T]
          elem
        case _ => deserializationError("String expected of type term")
      }
    }
  }

  object ScalanJsonProtocol extends ScalanJsonProtocol(new Ctx)
  import ScalanJsonProtocol._
  import ScalanJsonProtocol.ctx._

  def parseElem[A](sElem: String, eExpected: Elem[A]): Unit = {
    it(s"Parse $sElem") {
      val json = sElem.parseJson
      val eParsed = json.convertTo[Elem[A]]
      eParsed should be(eExpected)
    }
  }
  def printElem[A](e: Elem[A], expected: String): Unit = {
    it(s"Print $e") {
      val json = e.toJson
      val printed = json.prettyPrint
      printed should be(expected)
    }
  }
  def test[A](e: Elem[A]): Unit = {
    it(s"for $e"){
      val json = e.toJson
      val printed = json.prettyPrint
      val eParsed = printed.parseJson.convertTo[Elem[A]]
      eParsed should be(e)
    }
  }
  describe("Elem parsing") {
    parseElem(""" "String" """, element[String])
    parseElem(""" "Int" """, element[Int])
    parseElem(""" "Float" """, element[Float])
    parseElem(""" "(String,Float)" """, element[(String, Float)])
    parseElem(""" "scala.Tuple2[String, Float]" """, element[(String, Float)])
    parseElem(""" "scala.Function1[String, Float]" """, element[(String => Float)])
  }

  describe("Elem printing") {
    printElem(element[String], """"String"""")
    printElem(element[Int], """"Int"""")
    printElem(element[Float], """"Float"""")
    printElem(element[(String, Float)], """"scala.Tuple2[String, Float]"""")
    printElem(element[(String => Float)], """"scala.Function1[String, Float]"""")
  }

  describe("Elem <-> Json iso property") {
    test(element[String])
    test(element[Int])
    test(element[Float])
    test(element[(String, Float)])
    test(element[(String => Float)])
  }
}
