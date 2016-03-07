package scalan.compilation

import scalan.ScalanDslExp
import scalan.primitives.StructsDslExp

trait StructsCompiler[+ScalanCake <: ScalanDslExp with StructsDslExp] extends Compiler[ScalanCake] {
  import scalan._

  override def graphPasses(compilerConfig: CompilerConfig) = {
    val passes = super.graphPasses(compilerConfig) ++
      Seq(AllInvokeEnabler,
        constantPass(new StructsPass(DefaultMirror, StructsRewriter)))
    passes.distinct
  }

  class StructsPass(mirror: Mirror[MapTransformer], rewriter: Rewriter) extends GraphPass {
    def name = "structs"
    override val config = PassConfig(shouldUnpackTuples = true)
    def apply(graph: PGraph): PGraph = {
      graph.transform(mirror, rewriter, MapTransformer.Empty)
    }
  }

}
