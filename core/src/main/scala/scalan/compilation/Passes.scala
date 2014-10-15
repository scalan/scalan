package scalan.compilation

import scalan.ScalanExp

trait Passes { self: ScalanExp =>
  trait Pass {
    def name: String
  }

  abstract class GraphPass[Ctx <: Transformer : TransformerOps] extends Pass {
    def apply(graph: ProgramGraph[Ctx]): ProgramGraph[Ctx]
  }

  trait ExpPass extends Pass {
    def apply[A](s: Exp[A]): Exp[_]
  }

  case class GraphTransformPass[Ctx <: Transformer : TransformerOps](name: String, mirror: Mirror[Ctx], rewriter: Rewriter, t: Ctx) extends GraphPass[Ctx] {
    def apply(graph: ProgramGraph[Ctx]): ProgramGraph[Ctx] = graph.transform(mirror, rewriter, t)
  }
}
