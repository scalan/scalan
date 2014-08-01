package scalan.staged

import scalan.ScalanStaged
import scalan.common.GraphUtil

trait Scheduling extends BaseExp { self: ScalanStaged =>

  def buildScheduleForResult(start: Exp[_]): Seq[TableEntry[_]] = buildScheduleForResult(syms(start))

  def buildScheduleForResult(st: Seq[Exp[_]]): Seq[TableEntry[_]] = buildScheduleForResult(st, dep)

  def buildScheduleForResult(st: Seq[Exp[_]], neighbours: Exp[_] => Seq[Exp[_]]): Seq[TableEntry[_]] = {
    val startNodes = st.flatMap(e => findDefinition(e).toList)

    def succ(tp: TableEntry[_]): Seq[TableEntry[_]] = {
      //println("dep"+d +"="+dep(d.rhs))
      val ns = neighbours(tp.sym)
      ns.flatMap { e =>
        //println(d + "->" + e)
        findDefinition(e).toList
      }
    }

    GraphUtil.stronglyConnectedComponents[TableEntry[_]](startNodes)(succ).flatten
  }
}
