package scalan.json

import scala.wrappers.WrappersModule
import scalan.Scalan
import spray.json._
import scalan.util.FileUtil

class TransformJsonTests extends JsonTests {
  val transformDir = resourcesDir + "/transform"

  class Ctx extends Scalan with WrappersModule

  class PipelineTester[C <: Scalan](pipeline: TransformPipeline[C]) {
    val trans = new JsonTransformer(pipeline)

    def test[A, B](name: String, whatIsTested: String = ""): Unit = {
      import FileUtil._
      val sourceFile = file(s"$transformDir/$name/source.json")
      val targetFile = file(s"$transformDir/$name/target.json")
      it(s"$whatIsTested ($name)") {
        val source = read(sourceFile)
        val target = read(targetFile)
        val res = trans(source)
        trans.read(res) should be(trans.read(target))
      }
    }
  }

  describe("Identity pipeline") {
    val pipeline = new TransformPipeline(new Ctx) {
      override def apply(graph: ctx.PGraph): ctx.PGraph = {
        graph
      }
    }
    val pt = new PipelineTester(pipeline)
    pt.test("example1")
  }

  /** Define <code>rewriter</code> to implement concrete transformation */
  abstract class SingleStagePipeline[C <: Scalan](c: C) extends TransformPipeline(c) {
    val rewriter: ctx.Rewriter
    override def apply(graph: ctx.PGraph): ctx.PGraph = {
      graph.transform(ctx.DefaultMirror, rewriter, ctx.MapTransformer.Empty)
    }
  }

  describe("Single stage pipeline") {
    val pipeline = new SingleStagePipeline(new Ctx) {
      import ctx.{Rewriter, Exp, Def, RepForSomeExtension}
      val rewriter = new Rewriter {
        def apply[T](x: Exp[T]): Exp[T] = (x match {
          case _ => x
        }).asRep[T]
      }
    }
    val pt = new PipelineTester(pipeline)
    pt.test("example1")
  }
}
