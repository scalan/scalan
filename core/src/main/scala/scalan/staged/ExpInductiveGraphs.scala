package scalan.staged

import scalan.ScalanExp
import scalan.common.{GraphOperations, InductiveGraphs}

trait ExpInductiveGraphs extends BaseExp { self: ScalanExp =>

  val graphs = new GraphOperations[Exp[_]] {}
  import graphs._

  type ExpGraph = Graph[Unit, Unit]

  def mkContext(e: Exp[_], ins: List[Exp[_]]) = Context(ins map (Edge((), _)), e, (), Seq())

  implicit class IndGraphForAstGraph(astGraph: AstGraph) {

    def indGraph: ExpGraph = {
      val projections = astGraph.lambdaBoundProjections
      val initial = projections.foldLeft[ExpGraph](Empty){(g, v) => mkContext(v, Nil) &: g }

      astGraph.scheduleFromProjections.foldLeft[ExpGraph](initial)((g, tp) => {
        val s = tp.sym
        val in =
          if (astGraph.isLambdaBoundProjection(s)) Nil
          else astGraph.node(s).toList.flatMap(_.inputSyms)

        val c = mkContext(s, in)
        c &: g
      })
    }

  }

}
