package scalan.staged

import scalan.ScalanStaged
import scalan.common.GraphUtil

trait ProgramGraphs extends Scheduling with Transforming with AstGraphs { self: ScalanStaged =>

  class PGraph(roots: List[Exp[Any]],
               mapping: MapTransformer = new MapTransformer()) extends ProgramGraph[MapTransformer](roots, mapping) {
    def this(root: Exp[Any]) = this(List(root))
  }

  // immutable program graph
  class ProgramGraph[Ctx <: Transformer : TransformerOps](val roots: List[Exp[Any]], val mapping: Ctx)
  	  extends AstGraph
  {
    def transform(m: Mirror[Ctx], rw: Rewriter, t: Ctx): ProgramGraph[Ctx] = {
      val t0 = t merge mapping
      val (t1, _) = m.mirrorSymbols(t0, rw, schedule map { _.sym })
      val newRoots = roots map { t1(_) }
      new ProgramGraph(newRoots, t1)
    }
  }
}
