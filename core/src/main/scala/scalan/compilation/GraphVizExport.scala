package scalan.compilation

import java.io.{File, PrintWriter}
import java.lang.reflect.Method

import _root_.scalan.ScalanExp
import scalan.util.{FileUtil, ScalaNameUtil}

trait GraphVizExport { self: ScalanExp =>

  protected def quote(x: Any) = "\"" + x + "\""

  // TODO it would be better to have nodeColor(elem: Elem[_], optDef: Option[Def[_]]) to
  // avoid looking up definition, but this leads to ClassFormatError (likely Scala bug)
  protected def nodeColor(sym: Exp[_]): String = sym match {
    case Def(_: View[_, _]) => "darkgreen"
    case _ => sym.elem match {
      case _: ViewElem[_, _] => "green"
      case _: FuncElem[_, _] => "magenta"
      case _: CompanionElem[_] => "lightgray"
      case _ => "gray"
    }
  }

  protected def maxLabelLineLength = 40

  // ensures nice line wrapping
  final protected def nodeLabel(parts: String*) = {
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
    s"label=${quote(sb.result)}"
  }

  protected def emitNode(sym: Exp[_], rhs: Def[_])(implicit stream: PrintWriter) = {
    rhs match {
      case l: Lambda[_, _] =>
        val x = l.x
        stream.println(quote(x) + " [")
        stream.println(nodeLabel(x.toStringWithType))
        stream.println(s"color=${nodeColor(x)}")
        stream.println("]")
      case _ =>
    }
    stream.println(quote(sym) + " [")
    stream.println(nodeLabel(sym.toStringWithType + " =", formatDef(rhs)))
    stream.println(s"shape=box,color=${nodeColor(sym)},tooltip=${quote(sym.toStringWithType)}")
    stream.println("]")

    emitDeps(sym, rhs)
  }

  protected def formatDef(d: Def[_]): String = d match {
    case l: Lambda[_, _] =>
      val y = l.y
      val bodyStr = y match {
        case Def(b) => s"$y = ${formatDef(b)}"
        case _ => y.toString
      }
      s"${l.x} => $bodyStr"
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

  private def emitDeps(sym: Exp[_], rhs: Def[_])(implicit stream: PrintWriter) = {
    def emitDepList(list: List[Exp[_]], params: String) =
      list.foreach { dep => stream.println(s"${quote(dep)} -> ${quote(sym)} $params") }

    val (deps, lambdaVars) = rhs match {
      case l: Lambda[_, _] => lambdaDeps(l)
      case _ => (dep(rhs), Nil)
    }
    emitDepList(lambdaVars, "[style=dashed, color=lightgray, weight=0]")
    emitDepList(deps, "[style=solid]")
  }

  def emitDepGraph(d: Def[_], file: File, landscape: Boolean): Unit =
    emitDepGraph(dep(d), file, landscape)
  def emitDepGraph(start: Exp[_], file: File, landscape: Boolean = false): Unit =
    emitDepGraph(List(start), file, landscape)
  def emitDepGraph(ss: Seq[Exp[_]], file: File, landscape: Boolean): Unit =
    FileUtil.withFile(file) {
      emitDepGraph(ss, _, landscape)
    }
  // this can be made the main method in the future
  // to avoid potential inconsistencies in schedules
  // or to add information accessible from the graph
  def emitDepGraph(graph: AstGraph, file: File, landscape: Boolean): Unit =
    emitDepGraph(graph.roots, file, landscape)

  private def lambdaDeps(l: Lambda[_, _]): (List[Exp[_]], List[Exp[_]]) = l.y match {
    case Def(l1: Lambda[_, _]) =>
      val (ds, vs) = lambdaDeps(l1)
      (ds, l.x :: vs)
    case _ => (dep(l.y), List(l.x))
  }

  protected def clusterColor(g: AstGraph) = g match {
    case _: Lambda[_, _] => "#FFCCFF"
    case _: ThunkDef[_] => "#FFCCCC"
    case _ => "lightgray"
  }

  protected def clusterSchedule(g: AstGraph) = g match {
    case lam: Lambda[_, _] => lam.schedule.filter(_.sym != lam.y)
    case _ => g.schedule
  }

  protected def shouldEmitCluster(g: AstGraph) = g match {
    case lam: Lambda[_, _] => !lam.isIdentity
    case _ => true
  }

  private def emitClusters(schedule: Schedule, emitted: Set[Exp[_]])(implicit stream: PrintWriter): Set[Exp[_]] = {
    schedule.foldRight(emitted) {
      case (TableEntry(s, d), emitted1) =>
        if (!emitted1.contains(s)) {
          d match {
            case g: AstGraph if shouldEmitCluster(g) =>
              stream.println(s"subgraph cluster_$s {")
              stream.println(s"style=dashed; color=${quote(clusterColor(g))}")
              emitNode(s, d)
              // for lambdas, do we want lambdaDeps instead?
              val sources = g.boundVars
              if (sources.nonEmpty) {
                stream.println(s"{rank=source; ${sources.mkString("; ")}}")
              }
              val schedule1 = clusterSchedule(g)
              val emitted2 = emitClusters(schedule1, emitted1 + s)
              stream.println(s"{rank=sink; $s}")
              stream.println("}")
              emitted2
            case _ =>
              emitNode(s, d)
              emitted1 + s
          }
        } else {
          emitted1
        }
    }
  }

  private def emitDepGraph(ss: Seq[Exp[_]], stream: PrintWriter, landscape: Boolean): Unit = {
    stream.println("digraph G {")

    val deflist = buildScheduleForResult(ss, dep)

    stream.println("concentrate=true")
    if (landscape) {
      stream.println("rankdir=LR")
    }

    val lambdaBodies: Set[Exp[_]] = deflist.collect {
      case TableEntry(_, lam: Lambda[_, _]) => lam.y
    }.toSet

    val deflist1 = deflist.filterNot(tp => lambdaBodies.contains(tp.sym))

    emitClusters(deflist1, Set.empty)(stream)

    stream.println("}")
    stream.close()
  }
}
