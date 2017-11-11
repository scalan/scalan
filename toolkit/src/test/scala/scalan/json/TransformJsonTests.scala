package scalan.json

import scala.wrappers.WrappersModule
import scalan.Scalan
import spray.json._

import scalan.util.FileUtil

class TransformJsonTests extends JsonTests {
  val transformDir = resourcesDir + "/transform"

  class Ctx extends Scalan with WrappersModule

  describe("Wrapped methods <-> Json") {
    val pipeline = new TransformPipeline(new Ctx) {
      override def apply(graph: ctx.PGraph): ctx.PGraph = {
        graph
      }
    }
    val trans = new JsonTransformer(pipeline)

    def test[A, B](name: String, whatIsTested: String = ""): Unit = {
      import FileUtil._
      val sourceFile = file(s"$transformDir/$name/source.json")
      val targetFile = file(s"$transformDir/$name/target.json")
      it(s"$whatIsTested ($name)") {
        val source = read(sourceFile)
        val target = read(targetFile)
        val res = trans(source)
        res should be(target)
      }
    }
    test("example1")
  }
}
