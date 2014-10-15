package scalan.compilation

import java.lang.reflect.Method

import scalan.ScalanExp

trait Passes { self: ScalanExp =>
  trait Pass {
    def name: String

    // TODO what arguments?
    def doFinalization(): Unit = {}
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

  class EnableInvokePass[Ctx <: Transformer : TransformerOps](methodsDescription: String, pred: InvokeTester) extends GraphPass {
    def name = s"enable_invoke_$methodsDescription"

    def apply(graph: ProgramGraph[Ctx]) = {
      addInvokeTester(pred)
      graph.transform(mirror[Ctx], NoRewriting, implicitly[TransformerOps[Ctx]].empty)
    }

    override def doFinalization(): Unit = {
      removeInvokeTester(pred)
    }
  }

  val AllInvokeEnabler = new EnableInvokePass[MapTransformer]("all", (_, _) => true)
}
