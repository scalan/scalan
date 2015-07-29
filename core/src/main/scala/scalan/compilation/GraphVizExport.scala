package scalan.compilation

import java.io.{File, PrintWriter}

import _root_.scalan.ScalanExp
import scalan.util.{StringUtil, FileUtil}

trait GraphVizExport { self: ScalanExp =>

  // TODO it would be better to have nodeColor(elem: Elem[_], optDef: Option[Def[_]]) to
  // avoid looking up definition, but this leads to ClassFormatError (likely Scala bug)
  protected def nodeColor(sym: Exp[_])(implicit config: GraphVizConfig): String = sym match {
    case Def(_: View[_, _]) => "darkgreen"
    case _ => sym.elem match {
      case _: ViewElem[_, _] => "green"
      case _: FuncElem[_, _] => "magenta"
      case _: CompanionElem[_] => "lightgray"
      case _ => "gray"
    }
  }

  // ensures nice line wrapping
  final protected def nodeLabel(parts: String*)(implicit config: GraphVizConfig) = {
    config.nodeLabel(parts)
  }

  protected def emitNode(sym: Exp[_], rhs: Def[_])(implicit stream: PrintWriter, config: GraphVizConfig) = {
    rhs match {
      case l: Lambda[_, _] =>
        val x = l.x
        stream.println(StringUtil.quote(x) + " [")
        stream.println(nodeLabel(x.toStringWithType))
        stream.println(s"color=${nodeColor(x)}")
        stream.println("]")
      case _ =>
    }
    stream.println(StringUtil.quote(sym) + " [")
    stream.println(nodeLabel(sym.toStringWithType + " =", formatDef(rhs)))
    stream.println(s"shape=box,color=${nodeColor(sym)},tooltip=${StringUtil.quote(sym.toStringWithType)}")
    stream.println("]")
  }

  protected def formatDef(d: Def[_])(implicit config: GraphVizConfig): String = d match {
    case l: Lambda[_, _] =>
      val y = l.y
      val bodyStr = y match {
        case Def(b) => s"$y = ${formatDef(b)}"
        case _ => y.toString
      }
      s"${l.x} => $bodyStr"
    /*case v: View[_,_] =>
      val viewStr = d.toString
      val isoStr = v.iso.toString
      s"$viewStr (iso: $isoStr)" */
    case Apply(f, arg) => s"$f($arg)"
    case Tup(a, b) => s"($a, $b)"
    case First(pair) => s"$pair._1"
    case Second(pair) => s"$pair._2"
    case IfThenElse(c, t, e) => s"if ($c) $t else $e"
    case LoopUntil(start, step, isMatch) => s"from $start do $step until $isMatch"
    case ApplyBinOp(op, lhs, rhs) => s"$lhs ${op.opName} $rhs"
    case ApplyUnOp(op, arg) => op match {
      case NumericToFloat(_) => s"$arg.toFloat"
      case NumericToDouble(_) => s"$arg.toDouble"
      case NumericToInt(_) => s"$arg.toInt"
      case _ => s"${op.opName} $arg"
    }
    case _ => d.toString
  }

  private def emitEdges(sym: Exp[_], rhs: Def[_])(implicit stream: PrintWriter, config: GraphVizConfig) = {
    def emitEdgesTo(list: List[Exp[_]], params: String) =
      list.foreach { dep => stream.println(s"${StringUtil.quote(dep)} -> ${StringUtil.quote(sym)} $params") }

    val (deps, lambdaVars) = rhs match {
      case l: Lambda[_, _] => lambdaDeps(l)
      case _ => (dep(rhs), Nil)
    }
    emitEdgesTo(lambdaVars, "[style=dashed, color=lightgray, weight=0]")
    emitEdgesTo(deps, "[style=solid]")
  }

  // can be overridden if desired
  def defaultGraphVizConfig: GraphVizConfig =
    GraphVizConfig.default

  def emitDepGraph(d: Def[_], file: File)(implicit config: GraphVizConfig): Unit =
    emitDepGraph(dep(d), file)(config)
  def emitDepGraph(start: Exp[_], file: File)(implicit config: GraphVizConfig): Unit =
    emitDepGraph(List(start), file)(config)
  def emitDepGraph(ss: Seq[Exp[_]], file: File)(implicit config: GraphVizConfig): Unit =
    emitDepGraph(new PGraph(ss.toList), file)(config)
  def emitGraphOnException(contextName: String, ss: Exp[_]*) =
    emitDepGraph(ss, FileUtil.file("test-out", "exceptions", s"${contextName}_context.dot"))(defaultGraphVizConfig)
  def emitDepGraph(graph: AstGraph, file: File)(implicit config: GraphVizConfig): Unit =
    if (config.emitGraphs) {
      FileUtil.withFile(file) {
        emitDepGraph(graph, file.getName)(_, config)
      }
    }

  private def lambdaDeps(l: Lambda[_, _]): (List[Exp[_]], List[Exp[_]]) = l.y match {
    case Def(l1: Lambda[_, _]) =>
      val (ds, vs) = lambdaDeps(l1)
      (ds, l.x :: vs)
    case _ => (dep(l.y), List(l.x))
  }

  protected def clusterColor(g: AstGraph) = g match {
    case _: ProgramGraph[_] => None
    case _: Lambda[_, _] => Some("#FFCCFF")
    case _: ThunkDef[_] => Some("#FFCCCC")
    case _ => Some("lightgray")
  }

  protected def clusterSchedule(g: AstGraph) = g match {
    case lam: Lambda[_, _] => lam.schedule.filter(_.sym != lam.y)
    case _ => g.schedule
  }

  protected def shouldEmitCluster(g: AstGraph) = g match {
    case lam: Lambda[_, _] => !lam.isIdentity
    case _ => true
  }

  // This function accumulates all emitted nodes so that dependency edges are placed outside any clusters.
  // Otherwise dot will incorrectly show nodes inside clusters they don't belong to.
  private def emitCluster(g: AstGraph, acc: Map[Exp[_], Def[_]])(implicit stream: PrintWriter, config: GraphVizConfig): Map[Exp[_], Def[_]] = {
    val schedule = clusterSchedule(g)

    schedule.foldLeft(acc) { case (acc1, TableEntry(s, d)) =>
      val acc2 = acc1 + (s -> d)
      d match {
        case g: AstGraph if shouldEmitCluster(g) =>
          if (config.subgraphClusters) {
            stream.println(s"subgraph cluster_$s {")
            clusterColor(g).foreach { color =>
              stream.println(s"style=dashed; color=${StringUtil.quote(color)}")
            }
          }
          emitNode(s, d)
          // for lambdas, do we want lambdaDeps instead?
          val sources = g.boundVars
          if (config.subgraphClusters && sources.nonEmpty) {
            stream.println(s"{rank=source; ${sources.mkString("; ")}}")
          }
          val acc3 = emitCluster(g, acc2)
          if (config.subgraphClusters) {
            stream.println(s"{rank=sink; $s}")
            stream.println("}")
          }
          acc3
        case _ =>
          emitNode(s, d)
          acc2
      }
    }
  }

  private def emitDepGraph(graph: AstGraph, name: String)(implicit stream: PrintWriter, config: GraphVizConfig): Unit = {
    stream.println(s"""digraph "${name}" {""")

    stream.println("concentrate=true")
    stream.println(config.orientationString)

    val nodes = emitCluster(graph, Map.empty)

    nodes.foreach { case (sym, rhs) =>
      emitEdges(sym, rhs)
    }
    stream.println("}")
    stream.close()
  }
}

sealed trait Orientation
object Portrait extends Orientation
object Landscape extends Orientation

sealed trait ControlFlowStyle
object ControlFlowWithBoxes extends ControlFlowStyle
object ControlFlowWithArrows extends ControlFlowStyle

// outside the cake to be usable from ItTestsUtil
case class GraphVizConfig(emitGraphs: Boolean,
                          orientation: Orientation,
                          maxLabelLineLength: Int,
                          subgraphClusters: Boolean) {

  // ensures nice line wrapping
  def nodeLabel(parts: Seq[String]):String = {
    var lineLength = 0
    val sb = new StringBuilder()
    var isFirst = true
    parts.foreach { part =>
      if (isFirst) {
        isFirst = false
      } else if (lineLength + part.length + 1 <= maxLabelLineLength) {
        sb.append(" ")
        lineLength += 1
      } else {
        sb.append("\\l")
        lineLength = 0
      }
      sb.append(part)
      lineLength += part.length
    }
    s"label=${StringUtil.quote(sb.result)}"
  }

  def orientationString = if (orientation == Landscape) "rankdir=LR" else ""

}

object GraphVizConfig {
  // not made implicit because it would be too easy to use
  // it accidentally instead of passing up
  def default = GraphVizConfig(
    emitGraphs = true,
    orientation = Portrait,
    maxLabelLineLength = 40,
    subgraphClusters = true)

  def none = default.copy(emitGraphs = false)
}