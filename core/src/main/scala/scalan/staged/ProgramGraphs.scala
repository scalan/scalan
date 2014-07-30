package scalan.staged

import scalan.ScalanStaged
import scalan.common.GraphUtil

trait ProgramGraphs extends Scheduling with Transforming with AstGraphs { self: ScalanStaged =>

  case class Node(usages: List[Exp[_]], definition: Option[Def[_]]) {
    def addUsage(usage: Exp[_]) = copy(usages = usage :: this.usages)
  }
  
  type PGraph = ProgramGraph[MapTransformer]

  // immutable program graph
  class ProgramGraph[Ctx <: Transformer : TransformerOps](val roots: List[Exp[_]], val mapping: Ctx)
  	  extends AstGraph {
    def this(roots: List[Exp[_]]) { this(roots, implicitly[TransformerOps[Ctx]].empty) }
    def this(root: Exp[_]) { this(List(root)) }

    def transform(m: Mirror[Ctx], rw: Rewriter, t: Ctx): ProgramGraph[Ctx] = {
      val t0 = t merge mapping
      val (t1, _) = m.mirrorSymbols(t0, rw, schedule map { _.sym })
      val newRoots = roots map { t1(_) }
      new ProgramGraph(newRoots, t1)
    }
  }
}
