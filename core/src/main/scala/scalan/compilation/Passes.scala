package scalan.compilation

import scalan.ScalanExp

trait Passes { self: ScalanExp =>
  trait Pass {
    def name: String

    // TODO what arguments?
    def doFinalization(): Unit = {}
  }

  abstract class GraphPass extends Pass {
    def apply(graph: PGraph): PGraph
  }

  trait ExpPass extends Pass {
    def apply[A](s: Exp[A]): Exp[_]
  }

  case class GraphTransformPass(name: String, mirror: Mirror[MapTransformer], rewriter: Rewriter) extends GraphPass {
    def apply(graph: PGraph): PGraph = graph.transform(mirror, rewriter, MapTransformer.Empty)
  }

  class EnableInvokePass(methodsDescription: String, pred: InvokeTester) extends GraphPass {
    def name = s"enable_invoke_$methodsDescription"

    def apply(graph: PGraph) = {
      addInvokeTester(pred)
      graph.transform(DefaultMirror, NoRewriting, MapTransformer.Empty)
    }

    override def doFinalization(): Unit = {
      removeInvokeTester(pred)
    }
  }

  def constantPass(pass: GraphPass) = (_: PGraph) => pass

  val AllInvokeEnabler = constantPass(new EnableInvokePass("all", (_, _) => true))
}
