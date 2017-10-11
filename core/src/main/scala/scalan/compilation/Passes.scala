package scalan.compilation

import scalan.Scalan
import scala.collection._

trait Passes {
  val scalan: Scalan
  import scalan._

  // to avoid need to import compiler.scalan.Exp in many places
  type Exp[+T] = scalan.Exp[T]

  abstract class GraphPass extends Pass {
    def builder: PassBuilder[GraphPass]
    def backwardAnalyse(graph: PGraph): Unit = {
      builder.backwardAnalyze(graph)
    }
    def clearMarkings(graph: PGraph): Unit = {
      builder.clearMarkings(graph)
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

    def clearMarkings(g: AstGraph): Unit = {
      // first clear markings for all analyzers
      g.scheduleAll.foreach(te => {
        for ((a: BackwardAnalyzer[m]) <- backwardAnalyses) {
          a.clearMark(te.sym)
        }
      })
    }

    def backwardAnalyze(g: AstGraph): Unit = {
      // assign new markings
      for ((a: BackwardAnalyzer[m]) <- backwardAnalyses) {
        a.backwardAnalyzeRec(g)
      }
    }

//    def analyzeNestedLambdas(g: AstGraph) = {
//      for (te @ TableEntry(s: Exp[t], d) <- g.schedule) {
//        d match {
//          case l: Lambda[a,b] =>
//            for ((a: BackwardAnalyzer[m]) <- backwardAnalyses) {
//              a.beforeAnalyze(l)
//            }
//            backwardAnalyzeRec(l)
//          case _ =>
//        }
//      }
//    }

//    def backwardAnalyzeRec(g: AstGraph): Unit = {
//      // first analyze nested lambdas
//      analyzeNestedLambdas(g)
//
//      val revSchedule = g.schedule.reverseIterator
//      for (te @ TableEntry(s: Exp[t], d) <- revSchedule) {
//        // back-propagate analysis information (including from Lambda to Lambda.y, see LevelAnalyzer)
//        for ((a: BackwardAnalyzer[m]) <- backwardAnalyses) {
//          val outMark = a.getMark(s)
//          val inMarks = a.getInboundMarkings(te, outMark)
//          for ((s, mark) <- inMarks) {
//            a.updateOutboundMarking(s, mark)
//          }
//        }
//        d match {
//          // additionally if it is Lambda
//          case l: Lambda[a,b] =>
//            // analize lambda after the markings were assigned to the l.y (this is second round)
//            backwardAnalyzeRec(l)
//            // assign marking to l if not identity
//            for ((a: BackwardAnalyzer[m]) <- backwardAnalyses) {
//              val argMark = a.getMark(l.x)
//              val lMark = a.getLambdaMarking(l, argMark)
//              a.updateOutboundMarking(l.self, lMark)
//            }
//          case _ =>
//        }
//      }
//    }
  }

  class GraphPassBuilder[+P <: GraphPass](val name: String, createPass: (PassBuilder[P], PGraph) => P)
      extends PassBuilder[P] {
    def apply(g: PGraph) = createPass(this, g)
  }

  class ConstPassBuilder[+P <: GraphPass](name: String, createPass: (PassBuilder[P], PGraph) => P)
      extends GraphPassBuilder[P](name, createPass)

  trait ExpPass extends Pass {
    def apply[A](s: Exp[A]): Sym
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
