package scalan.compilation

import scalan.ScalanExp
import scala.collection._

trait Passes {
  val scalan: ScalanExp
  import scalan._

  // to avoid need to import compiler.scalan.Exp in many places
  type Exp[+T] = scalan.Exp[T]

  abstract class Analysis {
    def name: String
    def apply(graph: PGraph): Unit
    override def toString = s"Analysis($name)"
  }

  abstract class GraphPass extends Pass {
    def analyse(graph: PGraph): Unit = {
      for (a <- analyses) {
        a(graph)
      }
    }
    def apply(graph: PGraph): PGraph
    private val analyses = mutable.ArrayBuffer[Analysis]()
    def addAnalysis(a: Analysis) = {
      if (analyses.exists(_.name == a.name))
        !!!(s"Duplicate analysis ${a.name} for the phase ${this.name}, existing analyses: $analyses")
      analyses += a
    }
  }

  trait ExpPass extends Pass {
    def apply[A](s: Exp[A]): Exp[_]
  }

  class GraphTransformPass(val name: String, mirror: Mirror[MapTransformer], rewriter: Rewriter) extends GraphPass {
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

  def invokeEnabler(name: String)(pred: InvokeTester) =
    constantPass(new EnableInvokePass(name)(NamedInvokeTester(name, pred)))

  lazy val AllInvokeEnabler = invokeEnabler("all") { (_, _) => true }

  def unpackEnabler(name: String)(pred: UnpackTester) =
    constantPass(new EnableUnpackPass(name)(NamedUnpackTester(name, pred)))

  lazy val AllUnpackEnabler = unpackEnabler("all") { _ => true }
}
