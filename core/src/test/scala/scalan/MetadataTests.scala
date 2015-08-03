package scalan

import scalan.compilation.{GraphVizConfig, DummyCompilerWithPasses}
import scalan.util.FileUtil

class MetadataTests extends BaseTests {
  private val mainStr = "main"

  trait Prog extends Scalan {
    val functionNameKey = MetaKey[String]("name")

    val main = fun { x: Rep[Int] => x + 1 }

    main.setMetadata(functionNameKey)(mainStr)
  }

  class ProgExp extends ScalanCtxExp with Prog

  describe("Metadata") {
    it("survives compilation passes") {
      val compiler = new DummyCompilerWithPasses {
        override val scalan = new ProgExp
      }
      import compiler._
      import compiler.scalan._

      val graph = buildGraph(FileUtil.currentWorkingDir, mainStr, main, GraphVizConfig.none)(defaultCompilerConfig)

      val finalMain = graph.roots.head

      finalMain.getMetadata(functionNameKey) shouldEqual Some(mainStr)
    }

    it("can be changed by mirror") {
      val compiler = new DummyCompilerWithPasses {
        override val scalan = new ProgExp

        import scalan._

        val functionNameMirror = new Mirror[MapTransformer] {
          override protected def mirrorMetadata[A, B](t: MapTransformer, old: Exp[A], mirrored: Exp[B]) = {
            val newMeta = old.allMetadata.updateIfExists(functionNameKey)(_ + "1")
            (t, newMeta)
          }
        }

        override def graphPasses(compilerConfig: CompilerConfig) =
          super.graphPasses(compilerConfig) :+ constantPass(GraphTransformPass("functionNameMirror", functionNameMirror, NoRewriting))
      }

      import compiler._
      import compiler.scalan._

      val graph = buildGraph(FileUtil.currentWorkingDir, mainStr, main, GraphVizConfig.none)(defaultCompilerConfig)

      val finalMain = graph.roots.head

      main.getMetadata(functionNameKey) shouldEqual Some(mainStr)

      finalMain.getMetadata(functionNameKey) shouldEqual Some(mainStr + "1")
    }
  }
}
