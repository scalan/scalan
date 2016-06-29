package scalan.compilation

import java.awt.Desktop
import java.io.{File, PrintWriter}

import com.github.kxbmap.configs.Configs
import com.github.kxbmap.configs.syntax._
import com.typesafe.config.{Config, ConfigUtil}

import scalan.{Base, ScalanExp}
import scalan.util.{FileUtil, ProcessUtil, ScalaNameUtil, StringUtil}

case class GraphFile(file: File, fileType: String) {
  def open() = {
    Base.config.getOpt[String](ConfigUtil.joinPath("graphviz", "viewer", fileType)) match {
      case None =>
        Desktop.getDesktop.open(file)
      case Some(command) =>
        ProcessUtil.launch(Seq(command, file.getAbsolutePath))
    }
  }
}

trait GraphVizExport { self: ScalanExp =>

  // TODO it would be better to have nodeColor(elem: Elem[_], optDef: Option[Def[_]]) to
  // avoid looking up definition, but this leads to ClassFormatError (likely Scala bug)
  protected def nodeColor(td: TypeDesc, d: Def[_])(implicit config: GraphVizConfig): String = d match {
    case _: View[_, _] => "darkgreen"
    case _ => nodeColor(td)
  }

  protected def nodeColor(td: TypeDesc): String = td match {
    case _: ViewElem[_, _] => "green"
    case _: FuncElem[_, _] => "magenta"
    case _: CompanionElem[_] => "lightgray"
    case _ => "gray"
  }

  // ensures nice line wrapping
  final protected def nodeLabel(parts: String*)(implicit config: GraphVizConfig) = {
    config.nodeLabel(parts)
  }

  private def emitNode0(x: Exp[_], d: Option[Def[_]], acc: GraphData)(implicit stream: PrintWriter, config: GraphVizConfig) = {
    stream.println(StringUtil.quote(x) + " [")
    val acc1 = acc.addNode(x, d)
    val xElem = x.elem
    val label = acc1.typeString(xElem)
    val xAndD = d match {
      case Some(rhs) =>
        val lhsStr = s"$x: $label ="
        val rhsStr = formatDef(rhs)
        val rhsElem = rhs.selfType
        if (rhsElem != xElem) {
          List(lhsStr, s"$rhsStr:", acc1.typeString(rhsElem))
        } else {
          List(lhsStr, rhsStr)
        }
      case None =>
        List(s"$x: $label")
    }
    val metadata = if (config.emitMetadata) formatMetadata(x) else Nil
    val allParts = xAndD ++ metadata
    stream.println(nodeLabel(allParts: _*))

    val (shape, color) = d match {
      case Some(rhs) =>
        ("box", nodeColor(xElem, rhs))
      case None =>
        ("oval", nodeColor(xElem))
    }
    // use full type name for the tooltip
    stream.println(s"shape=$shape, color=$color, tooltip=${StringUtil.quote(x.toStringWithType)}, style=filled, fillcolor=white")
    stream.println("]")
    acc1
  }

  private def emitNode(sym: Exp[_], rhs: Def[_], acc: GraphData)(implicit stream: PrintWriter, config: GraphVizConfig) = {
    val acc1 = rhs match {
      case g: AstGraph =>
        g.boundVars.foldLeft(acc)((acc2, x) => emitNode0(x, None, acc2))
      case _ =>
        acc
    }
    emitNode0(sym, Some(rhs), acc1)
  }

  protected def formatMetadata(s: Exp[_]): List[String] = {
    val metadata = s.allMetadata.meta
    if (metadata.nonEmpty)
      "Metadata:" :: metadata.map { case (k, v) => s"$k:${formatConst(v.value)}" }.toList
    else
      Nil
  }

  protected def formatDef(d: Def[_])(implicit config: GraphVizConfig): String = d match {
    case Const(x) =>
      val formattedX = formatConst(x)
      s"Const($formattedX)"
    case l: Lambda[_, _] =>
      val y = l.y
      val bodyStr = y match {
        case Def(b) =>
          val rhs = formatDef(b)
          if (config.showLambdaReturnSym)
            s"$y = $rhs"
          else
            rhs
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
      case ToString() => s"$arg.toString"
      case HashCode() => s"$arg.hashCode"
      case _ => s"${op.opName} $arg"
    }
    case _ => d.toString
  }

  protected def formatConst(x: Any): String = x match {
    case str: String =>
      val tripleQuote = "\"\"\""
      str.lines.toSeq match {
        case Seq() =>
          "\"\""
        case Seq(line) =>
          if (line.contains("\""))
            tripleQuote + line + tripleQuote
          else
            StringUtil.quote(line)
        case lines =>
          (tripleQuote +: lines :+ tripleQuote).mkString("\n")
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

  def emitDepGraph(d: Def[_], directory: File, fileName: String)(implicit config: GraphVizConfig): Option[GraphFile] =
    emitDepGraph(dep(d), directory, fileName)(config)
  def emitDepGraph(start: Exp[_], directory: File, fileName: String)(implicit config: GraphVizConfig): Option[GraphFile] =
    emitDepGraph(List(start), directory, fileName)(config)
  def emitDepGraph(ss: Seq[Exp[_]], directory: File, fileName: String)(implicit config: GraphVizConfig): Option[GraphFile] =
    emitDepGraph(new PGraph(ss.toList), directory, fileName)(config)
  def emitExceptionGraph(e: Throwable, directory: File, fileName: String)(implicit config: GraphVizConfig): Option[GraphFile] =
    emitDepGraph(Left(e), directory, fileName)
  def emitDepGraph(graph: AstGraph, directory: File, fileName: String)(implicit config: GraphVizConfig): Option[GraphFile] =
    emitDepGraph(Right(graph), directory, fileName)
  def emitDepGraph(exceptionOrGraph: Either[Throwable, AstGraph], directory: File, fileName: String)(implicit config: GraphVizConfig): Option[GraphFile] =
    emitGraphFile(directory, fileName)(emitDepGraph(exceptionOrGraph, fileName)(_, config))

  def emitDot(dotText: String, directory: File, fileName: String)(implicit config: GraphVizConfig): Option[GraphFile] =
    emitGraphFile(directory, fileName)(_.println(dotText))

  private def emitGraphFile(directory: File, fileName: String)(f: PrintWriter => Unit)(implicit config: GraphVizConfig): Option[GraphFile] = {
    if (config.emitGraphs) {
      val dotFileName = s"$fileName.dot"
      val file = new File(directory, dotFileName)
      FileUtil.withFile(file)(f)

      val dotFile = new File(directory, dotFileName)
      val dotGraphFile = GraphFile(dotFile, "dot")

      Some(config.format match {
        case "dot" =>
          dotGraphFile
        case format =>
          val convertedFileName = FileUtil.withExtension(dotFileName, format)
          try {
            ProcessUtil.launch(Seq("dot", s"-T$format", "-o", convertedFileName, dotFileName), directory)
            GraphFile(new File(directory, convertedFileName), format)
          } catch {
            case e: Exception =>
              logger.warn(s"Failed to convert ${dotFile.getAbsolutePath} to $format: ${e.getMessage}")
              dotGraphFile
          }
      })
    } else None
  }

  implicit class SeqExpExtensionsForEmitGraph(symbols: Seq[Exp[_]]) {
    // Not default argument to allow use from the debugger
    def show(): Unit = show(defaultGraphVizConfig)
    def show(config: GraphVizConfig): Unit = showGraphs(symbols: _*)(config)
  }

  def showGraphs(roots: Exp[_]*)(implicit config: GraphVizConfig): Unit =
    showGraphs(new PGraph(roots.toList))

  def showGraphs(graph: AstGraph)(implicit config: GraphVizConfig): Unit = {
    val prefix = graph.roots.mkString("_")
    val file = File.createTempFile(s"graph_${prefix}_", ".dot")
    // unfortunately can end up deleting the file before the process reads it
    // file.deleteOnExit()
    val directory = file.getAbsoluteFile.getParentFile
    val fileName = FileUtil.stripExtension(file.getName)
    emitDepGraph(graph, directory, fileName)(config).foreach(_.open())
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
  private def emitCluster(g: AstGraph, acc: GraphData)(implicit stream: PrintWriter, config: GraphVizConfig): GraphData = {
    val schedule = clusterSchedule(g)

    schedule.foldLeft(acc) { case (acc1, TableEntry(s, d)) =>
      d match {
        case g: AstGraph if shouldEmitCluster(g) =>
          if (config.subgraphClusters) {
            stream.println(s"subgraph cluster_$s {")
            clusterColor(g).foreach { color =>
              stream.println(s"style=dashed; color=${StringUtil.quote(color)}")
            }
          }
          val acc2 = emitNode(s, d, acc1)
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
          emitNode(s, d, acc1)
      }
    }
  }

  private def emitExceptionCluster(e: Throwable, depth: Int, acc: GraphData)(implicit stream: PrintWriter, config: GraphVizConfig): GraphData = {
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
    stream.println(s"shape=note,color=red,style=filled,fillcolor=white")
    stream.println("]")
  }

  private sealed trait Label {
    def label: String
  }
  private case class NoAlias(label: String) extends Label
  private case class Alias(label: String, rhs: String, td: TypeDesc) extends Label

  private case class GraphData(nodes: Map[Exp[_], Option[Def[_]]], labels: Map[TypeDesc, Label], aliases: List[Alias], aliasCounter: Int)(implicit config: GraphVizConfig) {
    def addNode(s: Exp[_], d: Option[Def[_]]): GraphData = {
      val withType = config.maxTypeNameLength match {
        case Some(maxLength) =>
          val elems = d.map(_.selfType).toSet + s.elem
          elems.foldLeft(this)(_.registerType(_, maxLength))
        case None =>
          this
      }
      val nodes1 = withType.nodes + (s -> d)
      withType.copy(nodes = nodes1)
    }

    def typeString(td: TypeDesc) = {
      def f(td1: TypeDesc, recurse: Boolean): String =
        labels.get(td1) match {
          case Some(l) =>
            l.label
          case None =>
            if (recurse) {
              td1.getName(f(_, false))
            } else {
              // TODO this should never happen, but does with TypeWrappers
              // !!!(s"$td1 is used in $td.getName, but isn't registered in $labels")
              td1.name
            }
        }

      f(td, true)
    }

    private def partsIterator(td: TypeDesc) = td match {
      case se: StructElem[_] =>
        se.fieldElems.iterator
      case e: Elem[_] =>
        e.typeArgsIterator
      case _: Cont[_] =>
        Iterator.empty
    }

    private def registerType(td: TypeDesc, maxLength: Int): GraphData = {
      if (labels.contains(td))
        this
      else {
        val withTypeArgs = partsIterator(td).foldLeft(this)(_.registerType(_, maxLength))
        withTypeArgs.registerType0(td, maxLength)
      }
    }

    private def registerType0(td: TypeDesc, maxLength: Int): GraphData = {
      val labelString0 = typeString(td)

      val (label, newAliases, newAliasCounter) =
        if (labelString0.length > maxLength) {
          val alias = Alias(s"T$aliasCounter", labelString0, td)
          (alias, alias :: aliases, aliasCounter + 1)
        } else
          (NoAlias(labelString0), aliases, aliasCounter)

      val newLabels = labels + (td -> label)
      copy(labels = newLabels, aliases = newAliases, aliasCounter = newAliasCounter)
    }

    def finishGraph(implicit stream: PrintWriter) = {
      nodes.foreach {
        case (sym, Some(rhs)) =>
          emitDepEdges(sym, rhs)
        case _ =>
      }

      def emitAliasEdge(node: Any, td: TypeDesc): Unit =
        labels.get(td) match {
          case Some(Alias(label1, _, _)) =>
            // dotted doesn't work in xdot
            emitEdge(node, label1, """[style=dashed, color=turquoise]""")
          case _ =>
        }

      if (aliasCounter > 0) {
        stream.println(s"subgraph cluster_aliases {")
        stream.println("""label="Type Aliases"""")
        val orderedAliases = aliases.reverse
        orderedAliases.foreach {
          case Alias(label, rhs, td) =>
            stream.println(s"""$label [label="type $label = $rhs", shape=box, style=rounded, color=${nodeColor(td)}, fillcolor=white]""")
            // Below code doesn't show the dependencies correctly
//            if (config.typeAliasEdges) {
//              partsIterator(td).foreach(emitAliasEdge(label, _))
//            }
        }
        if (aliases.length > 1) {
          stream.println(orderedAliases.map(_.label).mkString(" -> ") + " [style=invis]")
        }
        stream.println("}")
        if (config.typeAliasEdges) {
          nodes.keysIterator.foreach {
            s => emitAliasEdge(s, s.elem)
          }
        }
      }
    }
  }
  private object GraphData {
    def empty(implicit config: GraphVizConfig) = GraphData(Map.empty, Map.empty, Nil, 0)
  }

  private def emitDepGraph(exceptionOrGraph: Either[Throwable, AstGraph], name: String)(implicit stream: PrintWriter, config: GraphVizConfig): Unit = {
    stream.println(s"""digraph "$name" {""")

    stream.println("concentrate=true")
    stream.println(s"node [style=filled, fillcolor=orangered]")
    stream.println(config.orientationString)

    val finalData = exceptionOrGraph match {
      case Left(e) => emitExceptionCluster(e, 0, GraphData.empty)
      case Right(graph) => emitCluster(graph, GraphData.empty)
    }

    finalData.finishGraph(stream)
    stream.println("}")
    stream.close()
  }
}

sealed trait Orientation
case object Portrait extends Orientation
case object Landscape extends Orientation

object Orientation {
  implicit val orientationC: Configs[Orientation] = Configs.of[Orientation]
}

sealed trait ControlFlowStyle
case object ControlFlowWithBoxes extends ControlFlowStyle
case object ControlFlowWithArrows extends ControlFlowStyle

// outside the cake to be usable from ItTestsUtil
case class GraphVizConfig(emitGraphs: Boolean,
                          format: String,
                          orientation: Orientation,
                          maxLabelLineLength: Int,
                          subgraphClusters: Boolean,
                          maxTypeNameLength: Option[Int],
                          typeAliasEdges: Boolean,
                          emitMetadata: Boolean,
                          showLambdaReturnSym: Boolean
                         ) {

  // ensures nice line wrapping
  def nodeLabel(parts: Seq[String]): String = {
    def escape(s: String) = s.replace("""\""", """\\""").replace("\"", """\"""")
    val lineBreak = """\l"""

    var lineLength = 0
    val sb = new StringBuilder()
    var isFirst = true
    parts.foreach { part =>
      val lines = part.lines.toSeq
      if (isFirst) {
        isFirst = false
      } else {
        lines match {
          case Seq() =>
            // do nothing
          case Seq(line) =>
            val lineLengthIfNoNewLine = lineLength + 1 + line.length
            lineLength = if (lineLengthIfNoNewLine <= maxLabelLineLength) {
              sb.append(" ")
              lineLengthIfNoNewLine
            } else {
              sb.append(lineBreak)
              line.length
            }
          case _ =>
            sb.append(lineBreak)
            lineLength = lines.last.length
        }
      }
      sb.append(lines.map(escape).mkString(lineBreak))
    }
    val label0 = sb.result()
    // left-justify the last line if there are multiple lines
    val label = if (label0.contains(lineBreak) && !label0.endsWith(lineBreak))
      label0 + lineBreak
    else
      label0
    s"label=${StringUtil.quote(label)}"
  }

  def orientationString = if (orientation == Landscape) "rankdir=LR" else ""
}

object GraphVizConfig {
  val config = Base.config.getConfig("graphviz")
  // not made implicit because it would be too easy to use
  // it accidentally instead of passing up
  // For some reason, return type has to be given explicitly
  val default: GraphVizConfig = config.extract[GraphVizConfig]

  val none: GraphVizConfig = default.copy(emitGraphs = false)

  def from(config: Config): GraphVizConfig = config.withFallback(this.config).extract[GraphVizConfig]
}
