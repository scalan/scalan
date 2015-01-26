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

  private def emitDeps(sym: Exp[_], deps: List[Exp[_]], lambdaVars: List[Exp[_]])(implicit stream: PrintWriter) = {
    def emitDepList(deps: List[Exp[_]], params: String) =
      deps.foreach { dep => stream.println(s"${quote(dep)} -> ${quote(sym)} $params") }

    emitDepList(lambdaVars, "[style=dashed, color=lightgray, weight=0]")
    emitDepList(deps, "[style=solid]")
  }

  def emitDepGraph(d: Def[_], file: File, landscape: Boolean): Unit =
    emitDepGraph(dep(d), file, landscape)
  def emitDepGraph(start: Exp[_], file: File, landscape: Boolean = false): Unit =
    emitDepGraph(List(start), file, landscape)
  def emitDepGraph(ss: List[Exp[_]], file: File, landscape: Boolean): Unit =
    FileUtil.withFile(file) {
      emitDepGraph(ss, _, landscape)
    }

  private def lambdaDeps(l: Lambda[_, _]): (List[Exp[_]], List[Exp[_]]) = l.y match {
    case Def(l1: Lambda[_, _]) =>
      val (ds, vs) = lambdaDeps(l1)
      (ds, l.x :: vs)
    case _ => (dep(l.y), List(l.x))
  }

  private def emitClusters(schedule: Schedule, listNodes: Boolean, stream: PrintWriter): Unit = {
    schedule.reverse.foreach {
      case TableEntry(s, d) =>
        d match {
          case lam: Lambda[_, _] if !lam.isIdentity =>
            stream.println(s"subgraph cluster_$s {")
            stream.println("style=dashed; color=lightgray")
            stream.println(s"{rank=min; ${lam.x}}")
            val schedule1 = lam.schedule.filter(_.sym != lam.y)
            emitClusters(schedule1, true, stream)
            stream.println(s"{rank=max; $s}")
            stream.println("}")
          case _ =>
            if (listNodes) stream.println(s) else {}
        }
    }
  }

  private def emitDepGraph(ss: List[Exp[_]], stream: PrintWriter, landscape: Boolean): Unit = {
    stream.println("digraph G {")

    val deflist = buildScheduleForResult(ss, dep)

    stream.println("concentrate=true")
    if (landscape) {
      stream.println("rankdir=LR")
    }

    val lambdaBodies: Map[Exp[_], Exp[_]] = (deflist collect {
      case TableEntry(s, lam: Lambda[_, _]) => (lam.y, s)
    }).toMap

    for (tp @ TableEntry(sym, rhs) <- deflist) {
      if (!lambdaBodies.contains(sym)) {
        val (deps, lambdaVars) = rhs match {
          case l: Lambda[_, _] => lambdaDeps(l)
          case _ => (dep(rhs), Nil)
        }
        // emit node
        emitNode(sym, rhs)(stream)

        emitDeps(sym, deps, lambdaVars)(stream)
      } else {
        // skip lambda bodies
      }
    }

    emitClusters(deflist, false, stream)

    stream.println("}")
    stream.close()
  }
}
