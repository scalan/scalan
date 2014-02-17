package scalan.staged

import scalan.ScalanStaged
import scalan.common.GraphUtil

trait Scheduling extends BaseExp { self: ScalanStaged =>

  def buildScheduleForResult(start: Exp[_]): List[TableEntry[_]] = buildScheduleForResult(syms(start))

  def buildScheduleForResult(st: List[Exp[Any]]): List[TableEntry[_]] = buildScheduleForResult(st, dep)

  def buildScheduleForResult(st: List[Exp[Any]], neighbours: Def[Any] => List[Exp[Any]]): List[TableEntry[_]] = {
    val startNodes = st.flatMap(e => findDefinition(e).toList)

    def succ(tp: TableEntry[_]): List[TableEntry[_]] = {
      //println("dep"+d +"="+dep(d.rhs))
      val ns = neighbours(tp.rhs)
      ns.flatMap { e =>
        //println(d + "->" + e)
        findDefinition(e).toList
      }
    }

    GraphUtil.stronglyConnectedComponents[TableEntry[_]](startNodes, succ).flatten.reverse //TODO inefficient!
  }
}
