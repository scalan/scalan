package scalan.compilation

import scalan.ScalanExp

trait Passes {
  val scalan: ScalanExp
  import scalan._

  // to avoid need to import compiler.scalan.Exp in many places
  type Exp[+T] = scalan.Exp[T]

  abstract class GraphPass extends Pass {
    def apply(graph: PGraph): PGraph
  }

  trait ExpPass extends Pass {
    def apply[A](s: Exp[A]): Exp[_]
  }

  case class GraphTransformPass(name: String, mirror: Mirror[MapTransformer], rewriter: Rewriter) extends GraphPass {
    def apply(graph: PGraph): PGraph = graph.transform(mirror, rewriter, MapTransformer.Empty)
  }

  class EnableInvokePass(methodsDescription: String)(invokePred: InvokeTester) extends GraphPass {
    def name = s"enable_invoke_$methodsDescription"

    def apply(graph: PGraph) = {
      addInvokeTester(invokePred)
      graph.transform(DefaultMirror, InvokeRewriter, MapTransformer.Empty)
    }

    override def doFinalization(): Unit = {
      removeInvokeTester(invokePred)
    }
  }

  class EnableUnpackPass(methodsDescription: String)(unpackPred: UnpackTester) extends GraphPass {
    def name = s"enable_unpack_$methodsDescription"

    def apply(graph: PGraph) = {
      addUnpackTester(unpackPred)
      graph.transform(DefaultMirror, NoRewriting, MapTransformer.Empty)
    }

    override def doFinalization(): Unit = {
      removeUnpackTester(unpackPred)
    }
  }

  def constantPass(pass: GraphPass) = (_: PGraph) => pass

  def invokeEnabler(name: String)(pred: InvokeTester) = constantPass(new EnableInvokePass(name)(pred))

  val AllInvokeEnabler = invokeEnabler("all") { (_, _) => true }

  def unpackEnabler(name: String)(pred: UnpackTester) = constantPass(new EnableUnpackPass(name)(pred))

  val AllUnpackEnabler = unpackEnabler("all") { _ => true }
}
