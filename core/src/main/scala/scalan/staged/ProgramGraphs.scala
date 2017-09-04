package scalan.staged

import scalan.Scalan
import scalan.common.GraphUtil

trait ProgramGraphs extends AstGraphs { self: Scalan =>

  type PGraph = ProgramGraph[MapTransformer]

  // immutable program graph
  case class ProgramGraph[Ctx <: Transformer : TransformerOps](roots: List[Exp[_]], mapping: Ctx)
  	  extends AstGraph {
    def this(roots: List[Exp[_]]) { this(roots, implicitly[TransformerOps[Ctx]].empty) }
    def this(root: Exp[_]) { this(List(root)) }

    override def boundVars = Nil
    override def freeVars = Set()
    override lazy val schedule = super.schedule

    def transform(m: Mirror[Ctx], rw: Rewriter, t: Ctx): ProgramGraph[Ctx] = {
      val t0 = t merge mapping
      val (t1, _) = m.mirrorSymbols(t0, rw, this, scheduleSyms)
      val newRoots = roots map { t1(_) }
      new ProgramGraph(newRoots, t1)
    }

    def transformOne(oldExp:Exp[_], newExp:Exp[_]): ProgramGraph[Ctx] = {
      val newRoots = roots map (x => x match {case v: Exp[_] if v == oldExp => newExp; case t => t }  )
      new ProgramGraph(newRoots, mapping)
    }


    def withoutContext = ProgramGraph(roots, implicitly[TransformerOps[Ctx]].empty)
  }

  object ProgramGraph {
    def transform[A](s: Exp[A], rw: Rewriter = NoRewriting, t: MapTransformer = MapTransformer.Empty): Exp[A] = {
      val g = ProgramGraph(List(s), t)
      val g1 = g.transform(DefaultMirror, rw, t)
      g1.roots(0).asRep[A]
    }
  }
}
