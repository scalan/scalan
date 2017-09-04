package scalan.compilation

import scalan.ScalanDsl
import scalan.staged.Slicing

trait SlicingCompiler[+ScalanCake <: ScalanDsl with Slicing] extends Compiler[ScalanCake] {
  import scalan._

  override def graphPasses(compilerConfig: CompilerConfig) =
    super.graphPasses(compilerConfig) :+ SlicingPass.makePass()

  class SlicingPass(val builder: PassBuilder[GraphPass], sliceAnalyzer: SliceAnalyzer, okInit: Boolean) extends GraphPass {
    def name = SlicingPass.name
    override val config = PassConfig(shouldSlice = true)
    override def backwardAnalyse(graph: PGraph): Unit = {
      graph.roots.foreach {
        case f @ Def(l: Lambda[a, b]) =>
          val r = l.y
          val rm = if (okInit) {
            // assume full content of root symbols is requested
            // setup corresponding markings as initial state for analysis
            r.elem.toMarking
          }
          else {
            // assume the markings for the root symbol are set up from outside
            // get them and use as initial
            sliceAnalyzer.getMark(r)
          }
          val fm = sliceAnalyzer.analyzeFunc(f.asRep[a => b], rm)
        //            sliceAnalyzer.updateOutboundMarking(f, fm)

        //          case r: Exp[a] =>
        //            sliceAnalyzer.updateOutboundMarking(r, r.elem.toMarking)
      }
      //      super.backwardAnalyse(graph)
    }
    def apply(graph: PGraph): PGraph = {
      val rw = new SlicingRewriter(sliceAnalyzer, graph)
      val m = new SlicingMirror(sliceAnalyzer, graph)
      graph.transform(m, rw, MapTransformer.Empty)
    }
  }
  object SlicingPass {
    val name = "slicing"

    def makePass(okInit: Boolean = true) = constantPass[SlicingPass](
      SlicingPass.name,
      b => {
        b.addAnalysis(sliceAnalyzer)
        new SlicingPass(b, sliceAnalyzer, okInit = okInit)
      })
  }
}
