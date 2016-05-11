package scalan.compilation

import java.awt.Desktop
import java.io.{File, PrintWriter}

import _root_.scalan.{Base, ScalanExp}
import scalan.util.{FileUtil, ScalaNameUtil, StringUtil}

import GraphVizExport.lineBreak

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
    val parts = List(sym.toStringWithType + " =", formatDef(rhs)) ::: (if (config.emitMetadata) List(formatMetadata(sym)) else Nil)
    stream.println(nodeLabel(parts: _*))
    stream.println(s"shape=box,color=${nodeColor(sym)},tooltip=${StringUtil.quote(sym.toStringWithType)}")
    stream.println("]")
  }

  protected def formatMetadata(s: Exp[_]): String = {
    val metaNode = s.allMetadata
    metaNode.meta.map { case (k, v) => s"$k:${v.value}" }.mkString("{", ";", "}")
  }

  protected def formatDef(d: Def[_])(implicit config: GraphVizConfig): String = d match {
    case Const(x) =>
      val formattedX = formatConst(x)
      s"Const($formattedX)"
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
      case NumericToString() => s"$arg.toString"
      case _ => s"${op.opName} $arg"
    }
    case _ => d.toString
  }

  protected def formatConst(x: Any): String = x match {
    case str: String =>
      val escQuote = """\""""
      str.lines.map(_.replace("\"", escQuote).replace("""\""", """\\""")).toSeq match {
        case Seq() =>
          // should be impossible, but see SI-9773
          escQuote + escQuote
        case Seq(line) =>
          escQuote + line + escQuote
        case lines =>
          val tripleQuote = escQuote * 3
          (tripleQuote +: lines :+ tripleQuote).mkString(lineBreak)
      }
    case c: Char => s"'$c'"
    case f: Float => s"${f}f"
    case l: Long => s"${l}l"
    case arr: Array[_] => s"Array(${arr.toSeq.map(formatConst).mkString(", ")})"
    case seq: Seq[_] =>
      s"${seq.stringPrefix}(${seq.map(formatConst).mkString(", ")})"
    case null => "null"
    case _ => x.toString
  }

  private def emitDepEdges(sym: Exp[_], rhs: Def[_])(implicit stream: PrintWriter, config: GraphVizConfig) = {
    val (deps, lambdaVars) = rhs match {
      case l: Lambda[_, _] => lambdaDeps(l)
      case _ => (dep(rhs), Nil)
    }
    emitEdges(lambdaVars, sym, "[style=dashed, color=lightgray, weight=0]")
    emitEdges(deps, sym, "[style=solid]")
  }

  private def emitEdges(list: Seq[Any], target: Any, params: String)(implicit stream: PrintWriter) =
    list.foreach(emitEdge(_, target, params))

  private def emitEdge(source: Any, target: Any, params: String)(implicit stream: PrintWriter): Unit =
    stream.println(s"${StringUtil.quote(source)} -> ${StringUtil.quote(target)} $params")

  // can be overridden if desired
  def defaultGraphVizConfig: GraphVizConfig =
    GraphVizConfig.default

  def emitDepGraph(d: Def[_], file: File)(implicit config: GraphVizConfig): Unit =
    emitDepGraph(dep(d), file)(config)
  def emitDepGraph(start: Exp[_], file: File)(implicit config: GraphVizConfig): Unit =
    emitDepGraph(List(start), file)(config)
  def emitDepGraph(ss: Seq[Exp[_]], file: File)(implicit config: GraphVizConfig): Unit =
    emitDepGraph(new PGraph(ss.toList), file)(config)
  def emitExceptionGraph(e: Throwable, file: File)(implicit config: GraphVizConfig): Unit =
    emitDepGraph(Left(e), file)
  def emitDepGraph(graph: AstGraph, file: File)(implicit config: GraphVizConfig): Unit =
    emitDepGraph(Right(graph), file)
  def emitDepGraph(exceptionOrGraph: Either[Throwable, AstGraph], file: File)(implicit config: GraphVizConfig): Unit =
    if (config.emitGraphs) {
      FileUtil.withFile(file) {
        emitDepGraph(exceptionOrGraph, file.getName)(_, config)
      }
    }
  def emitDot(dotText: String, file: File)(implicit config: GraphVizConfig): Unit =
    if (config.emitGraphs) {
      FileUtil.withFile(file) {
        _.println(dotText)
      }
    }

  implicit class ExpExtensionsForEmitGraph(symbol: Exp[_]) {
//    def emitGraph(file: File) = emitDepGraph(symbol, file)(defaultGraphVizConfig)
    // Not default argument to allow use from the debugger
    def show(): Unit = show(defaultGraphVizConfig)
    def show(config: GraphVizConfig): Unit = showGraphs(symbol)(config)
  }

  implicit class SeqExpExtensionsForEmitGraph(symbols: Seq[Exp[_]]) {
    // Not default argument to allow use from the debugger
    def show(): Unit = show(defaultGraphVizConfig)
    def show(config: GraphVizConfig): Unit = showGraphs(symbols: _*)(config)
  }

  def showGraphs(rootSyms: Exp[_]*)(implicit config: GraphVizConfig): Unit = {
    val prefix = rootSyms.mkString("_")
    val file = File.createTempFile(s"graph_${prefix}_", ".dot")
    // unfortunately can end up deleting the file before the process reads it
    // file.deleteOnExit()
    emitDepGraph(rootSyms, file)(config)
    openDotFile(file)
  }

  def openDotFile(file: File): Unit = {
    Base.config.getProperty("scalan.graphviz.viewer") match {
      case null =>
        Desktop.getDesktop.open(file)
      case command =>
        val builder = new ProcessBuilder(command, file.getAbsolutePath)
        val _ = builder.start()
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

  private def emitExceptionCluster(e: Throwable, depth: Int, acc: Map[Exp[_], Def[_]])(implicit stream: PrintWriter, config: GraphVizConfig): Map[Exp[_], Def[_]] = {
    // edges appear before their end nodes but it shouldn't be a problem
    val nodeName = exceptionNodeName(depth)

    val acc1 = e.getCause match {
      case null => acc
      case cause =>
        val causeDepth = depth + 1
        emitEdge(exceptionNodeName(causeDepth), nodeName, "[style=dashed, color=red]")
        emitExceptionCluster(cause, causeDepth, acc)
    }

    val acc2 = e match {
      case e: StagingException if e.syms.nonEmpty =>
        val syms1 = e.syms
        val depGraph = new PGraph(syms1.toList)
        emitEdges(syms1, nodeName, "[color=red]")
        emitCluster(depGraph, acc1)
      case _ =>
        acc1
    }
    emitExceptionNode(e, depth)
    acc2
  }

  private def exceptionNodeName(depth: Int) = "e" + depth

  private def emitExceptionNode(e: Throwable, depth: Int)(implicit stream: PrintWriter, config: GraphVizConfig) = {
    stream.println(StringUtil.quote(exceptionNodeName(depth)) + " [")
    // complete stacks are huge; we'd like to put them into tooltip, but \l doesn't work there and xdot doesn't show
    // the tooltip anyway
    val firstUsefulStackTraceLine = e.getStackTrace.find { ste =>
      val methodName = ScalaNameUtil.cleanScalaName(ste.getMethodName)
      // skip ???, !!!, throwInvocationException and other methods which just build and throw exceptions
      !(methodName.endsWith("???") || methodName.endsWith("!!!") || methodName.startsWith("throw"))
    }.map(ste => ScalaNameUtil.cleanScalaName(ste.toString)).toList
    stream.println(config.nodeLabel(e.toString.lines.toList ++ firstUsefulStackTraceLine))
    stream.println(s"shape=note,color=red")
    stream.println("]")
  }

  private def emitDepGraph(exceptionOrGraph: Either[Throwable, AstGraph], name: String)(implicit stream: PrintWriter, config: GraphVizConfig): Unit = {
    stream.println(s"""digraph "${name}" {""")

    stream.println("concentrate=true")
    stream.println(config.orientationString)

    val nodes = exceptionOrGraph match {
      case Left(e) => emitExceptionCluster(e, 0, Map.empty)
      case Right(graph) => emitCluster(graph, Map.empty)
    }

    nodes.foreach { case (sym, rhs) =>
      emitDepEdges(sym, rhs)
    }
    stream.println("}")
    stream.close()
  }
}

object GraphVizExport {
  final val lineBreak = """\l"""
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
                          subgraphClusters: Boolean,
                          emitMetadata: Boolean = false) {

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
        sb.append(lineBreak)
        lineLength = 0
      }
      sb.append(part)
      lineLength += part.length
    }
    val label0 = sb.result()
    // left-justify the last line if there are multiple lines
    val label = if (label0.contains(lineBreak))
      label0 + lineBreak
    else
      label0
    s"label=${StringUtil.quote(label0)}"
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