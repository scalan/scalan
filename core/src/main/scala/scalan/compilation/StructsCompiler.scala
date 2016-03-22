package scalan.compilation

import scalan.ScalanDslExp
import scalan.primitives.StructsDslExp

trait StructsCompiler[+ScalanCake <: ScalanDslExp with StructsDslExp] extends Compiler[ScalanCake] {
  import scalan._

  override def graphPasses(compilerConfig: CompilerConfig) = {
    val passes = super.graphPasses(compilerConfig) ++
      Seq(AllInvokeEnabler,
        constantPass[StructsPass](StructsPass.name, b => new StructsPass(b, DefaultMirror, StructsRewriter)))
    passes.distinct
  }

  class StructsPass(val builder: PassBuilder[GraphPass], mirror: Mirror[MapTransformer], rewriter: Rewriter) extends GraphPass {
    def name = StructsPass.name
    override val config = PassConfig(shouldUnpackTuples = true)
    def apply(graph: PGraph): PGraph = {
      graph.transform(mirror, rewriter, MapTransformer.Empty)
    }
  }
  object StructsPass {
    val name = "structs"
  }
}
