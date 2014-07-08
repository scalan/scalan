package scalan.staged

import scalan.ScalanExp
import scalan.common.InductiveGraphs

trait ExpInductiveGraphs extends BaseExp { self: ScalanExp =>

  val graphs = new InductiveGraphs[Exp[Any]] {}
  import graphs._
  type ExpGraph = Graph[Unit, Unit]

  def mkContext(e: Exp[Any], ins: List[Exp[Any]]) = Context(ins map (Edge((), _)), e, (), Seq())

  implicit class IndGraphForLambda[A,B](lam: Lambda[A,B]) {

    def indGraph: ExpGraph = {
      val initial = mkContext(lam.x, Nil) &: Empty
      lam.scheduleSyms.foldLeft[ExpGraph](initial)((g, s) => {
        val in = s.getDeps
        val c = mkContext(s, in)
        c &: g
      })
    }

  }

}
