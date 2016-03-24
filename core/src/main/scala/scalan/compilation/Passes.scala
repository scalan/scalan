package scalan.compilation

import scalan.ScalanExp
import scala.collection._

trait Passes {
  val scalan: ScalanExp
  import scalan._

  // to avoid need to import compiler.scalan.Exp in many places
  type Exp[+T] = scalan.Exp[T]

  abstract class GraphPass extends Pass {
    def builder: PassBuilder[GraphPass]
    def backwardAnalyse(graph: PGraph): Unit = {
      builder.backwardAnalyze(graph)
    }
    def apply(graph: PGraph): PGraph
  }

  trait PassBuilder[+P <: Pass] extends (PGraph => P) {
    def name: String
    type Analyzer = BackwardAnalyzer[M] forSome {type M[_]}
    val backwardAnalyses = mutable.ArrayBuffer[Analyzer]()
    def addAnalysis[M[_]](a: BackwardAnalyzer[M]) = {
      if (backwardAnalyses.exists(_.name == a.name))
        !!!(s"Duplicate analysis ${a.name} for the phase ${this.name}, existing analyses: $backwardAnalyses")
      backwardAnalyses += a.asInstanceOf[Analyzer]
    }

    def backwardAnalyze(g: AstGraph): Unit = {
      val revSchedule = g.schedule.reverseIterator
      for (TableEntry(s: Exp[t], d) <- revSchedule) {
        for ((a: BackwardAnalyzer[m]) <- backwardAnalyses) {
          val outMark = a.getMark(s)
          val inMarks = a.getInboundMarkings(d, outMark)
          for ((s, mark) <- inMarks) {
            a.updateOutboundMarking(s, mark)
          }
        }
        d match {
          case l: Lambda[a,b] =>
            backwardAnalyze(l)   // analize lambda after the markings were assigned to the l.y
          case _ =>
        }
      }
    }
  }

  class GraphPassBuilder[+P <: GraphPass](val name: String, createPass: (PassBuilder[P], PGraph) => P)
      extends PassBuilder[P] {
    def apply(g: PGraph) = createPass(this, g)
  }

  class ConstPassBuilder[+P <: GraphPass](name: String, createPass: (PassBuilder[P], PGraph) => P)
      extends GraphPassBuilder[P](name, createPass)

  trait ExpPass extends Pass {
    def apply[A](s: Exp[A]): Exp[_]
  }

  class GraphTransformPass(val builder: PassBuilder[GraphPass], val name: String, mirror: Mirror[MapTransformer], rewriter: Rewriter) extends GraphPass {
    def apply(graph: PGraph): PGraph = graph.transform(mirror, rewriter, MapTransformer.Empty)
  }

  class EnableInvokePass(val builder: PassBuilder[GraphPass], methodsDescription: String)(invokePred: InvokeTester) extends GraphPass {
    def name = s"enable_invoke_$methodsDescription"

    def apply(graph: PGraph) = {
      addInvokeTester(invokePred)
      graph.transform(DefaultMirror, InvokeRewriter, MapTransformer.Empty)
    }

    override def doFinalization(): Unit = {
      removeInvokeTester(invokePred)
    }
  }

  class EnableUnpackPass(val builder: PassBuilder[GraphPass], methodsDescription: String)(unpackPred: UnpackTester) extends GraphPass {
    def name = s"enable_unpack_$methodsDescription"

    def apply(graph: PGraph) = {
      addUnpackTester(unpackPred)
      graph.transform(DefaultMirror, NoRewriting, MapTransformer.Empty)
    }

    override def doFinalization(): Unit = {
      removeUnpackTester(unpackPred)
    }
  }

  def constantPass[P <: GraphPass](name: String, creator: PassBuilder[P] => P) =
    new ConstPassBuilder[P](name, (b,_) => creator(b))

  def invokeEnabler(name: String)(pred: InvokeTester): PassBuilder[GraphPass] =
    constantPass[GraphPass](name, b => new EnableInvokePass(b, name)(NamedInvokeTester(name, pred)))

  lazy val AllInvokeEnabler = invokeEnabler("invoke") { (_, _) => true }

  def unpackEnabler(name: String)(pred: UnpackTester): PassBuilder[GraphPass] =
    constantPass[GraphPass](name, b => new EnableUnpackPass(b, name)(NamedUnpackTester(name, pred)))

  lazy val AllUnpackEnabler = unpackEnabler("unpack") { _ => true }
}
