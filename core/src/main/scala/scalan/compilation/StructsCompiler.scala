package scalan.compilation

import scalan.ScalanDslExp
import scalan.primitives.StructsDslExp

/**
  * Created by slesarenko on 30/12/15.
  */
trait StructsCompiler[ScalanCake <: ScalanDslExp with StructsDslExp] extends Compiler[ScalanCake] {
  import scalan._

  override def graphPasses(compilerConfig: CompilerConfig) =
    super.graphPasses(compilerConfig) ++
      Seq(AllInvokeEnabler,
          constantPass(StructsPass(DefaultMirror, StructsRewriter)))

  case class StructsPass(mirror: Mirror[MapTransformer], rewriter: Rewriter) extends GraphPass {
    def name = "structs"
    override val config = PassConfig(shouldUnpackTuples = true)
    def apply(graph: PGraph): PGraph = {
      graph.transform(mirror, rewriter, MapTransformer.Empty)
    }
  }

}
