package scalan.codegen

import _root_.scalan.primitives.FunctionsExp
import _root_.scalan.ScalanStagedImplementation
import _root_.scalan.staged.Scheduling
import java.io.{ File, PrintWriter, FileOutputStream }

trait GraphVizExport extends Scheduling { self: ScalanStagedImplementation =>

  def emitDot(dotContent: String, file: String): Unit = {
    val f = new File(file)
    f.getParentFile.mkdirs()
    val stream = new java.io.PrintWriter(new java.io.FileOutputStream(file))
    stream.println(dotContent)
    stream.close()
  }

  private def quote(x: Any) = "\"" + x + "\""

  private def nodeColor(sym: Exp[_]): String = "color=" + (sym.elem match {
    //case _: PArrayElem[_] => "blue"
    case _: ViewElem[_, _] => "green"
    //case _: PipeElem[_] => "brown"
    case _: FuncElem[_, _] => "magenta"
    //case _: ChunksElem[_] => "black"
    case _ => "grey"
  })

  private def nodeLabel(str: String) = s"label=${quote(str)}"

  private def emitNode(sym: Exp[_], rhs: Def[_])(implicit stream: PrintWriter) = {
    rhs match {
      case l: Lambda[_, _] =>
        val x = l.x
        stream.println(quote(x) + " [")
        stream.println(nodeLabel(x.toStringWithType))
        stream.println(nodeColor(x))
        stream.println("]")
      case _ =>
    }
    stream.println(quote(sym) + " [")
    stream.println(nodeLabel(sym.toString + " = " + formatDef(rhs)))
    stream.println("shape=box," + nodeColor(sym) + ",tooltip=" + quote(sym.toStringWithType))
    stream.println("]")
  }

  private def formatDef(d: Def[_]): String = d match {
    case l: Lambda[_, _] => s"\\\\${l.x} -> ${l.y match { case Def(b) => formatDef(b) case y => y.toString}}"
    case Apply(f, arg) => s"$f($arg)"
    case MethodCall(receiver, method, args) => {
      val className = method.getDeclaringClass.getName()
      "%s.%s(%s)".format(receiver, className.substring(className.lastIndexOf("$")+1) + "." + method.getName(), args.mkString("", ",", ""))
    }
    case Tup(a, b) => s"($a, $b)"
    case First(pair) => s"$pair._1"
    case Second(pair) => s"$pair._2"
    case EqualsClass(lhs, rhs) => s"$lhs == $rhs"
    case NotEqual(lhs, rhs) => s"$lhs != $rhs"
    case NumericToFloat(arg, _) => s"$arg.toFloat"
    case _ => d.toString
  }

  private def emitDeps(sym: Exp[_], deps: List[Exp[_]], dotted: Boolean)(implicit stream: PrintWriter) = {
    for (dep <- deps) {
      val depLabel = dep.toString //dep.isVar match { case true => dep.toStringWithType case _ => dep.toString }
      val params = if (dotted) " [style=dotted]" else ""
      stream.println(s"${quote(depLabel)} -> ${quote(sym)}$params")
    }
  }

  def emitDepGraph(d: Def[_], file: String, landscape: Boolean): Unit =
    emitDepGraph(dep(d), file, landscape)
  def emitDepGraph(start: Exp[_], file: String, landscape: Boolean = false): Unit =
    emitDepGraph(List(start), file, landscape)
  def emitDepGraph(ss: List[Exp[_]], file: String, landscape: Boolean): Unit = {
    val f = new File(file)
    f.getParentFile.mkdirs()
    emitDepGraph(ss, new java.io.PrintWriter(new java.io.FileOutputStream(file)), landscape)
  }

  private def lambdaDeps(l: Lambda[_, _]): (List[Exp[_]], List[Exp[_]]) = l.y match {
    case Def(l1: Lambda[_, _]) =>
      val (ds, vs) = lambdaDeps(l1)
      (ds, l.x :: vs)
    case _ => (dep(l.y), List(l.x))
  }

  private def emitDepGraph(ss: List[Exp[_]], stream: PrintWriter, landscape: Boolean): Unit = {
    stream.println("digraph G {")

    val deflist = buildScheduleForResult(ss /* map { (_.asSymbol) }*/ )

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

        emitDeps(sym, deps, false)(stream)
        emitDeps(sym, lambdaVars, true)(stream)

        // emit lambda refs
        tp.lambda match {
          case Some(lam) =>
            stream.println(s"${quote(tp.sym)} -> ${quote(lam)} [style=dotted,color=grey]")
          case _ =>
        }

      } else {
        // skip lambda bodies
      }
    }

    stream.println("}")
    stream.close()
  }

}
