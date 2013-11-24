package scalan.staged

import scalan.ScalanStaged
import scalan.common.GraphUtil

trait Scheduling extends BaseExp { self: ScalanStaged =>

  def buildScheduleForResult(start: Exp[_]): List[TP[_]] = buildScheduleForResult(syms(start))

  def buildScheduleForResult(st: List[Exp[Any]]): List[TP[_]] = buildScheduleForResult(st, dep)

  def buildScheduleForResult(st: List[Exp[Any]], neighbours: Def[Any] => List[Exp[Any]]): List[TP[_]] = {
    val startNodes = st.flatMap(e => findDefinition(e).toList)

    def succ(tp: TP[_]): List[TP[_]] = {
      //println("dep"+d +"="+dep(d.rhs))
      val ns = neighbours(tp.rhs)
      ns.flatMap { e =>
        //println(d + "->" + e)
        findDefinition(e).toList
      }
    }

    GraphUtil.stronglyConnectedComponents[TP[_]](startNodes, succ).flatten.reverse // inefficient!
  }
}
