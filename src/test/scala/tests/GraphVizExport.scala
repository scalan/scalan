package tests

import _root_.scalan.primitives.FunctionsExp
import _root_.scalan.ScalanStaged
import _root_.scalan.staged.Scheduling
import java.io.{File, PrintWriter, FileOutputStream}

trait GraphVizExport extends Scheduling with FunctionsExp { self: ScalanStaged =>

  def quote(x: Any) = "\""+x+"\""

  def nodeColor(sym: Exp[_]): String = sym.elem match {
    //case _: PArrayElem[_] => "blue"
    case _: ViewElem[_,_] => "green"
    //case _: PipeElem[_] => "brown"
    case _: FuncElem[_,_] => "magenta"
    //case _: ChunksElem[_] => "black"
    case _ => "grey"
  }

  def emitNode(sym: Exp[_], rhs: Def[_])(implicit stream: PrintWriter) = {
    rhs match {
      case Lambda(_, _,x,_) =>
        stream.println(quote(x) + " [")
        stream.println("label=" + quote(x.toStringWithType))
        stream.println("color=" + nodeColor(x))
        stream.println("]")
      case _ =>
    }
    stream.println(quote(sym) + " [")
    stream.println("label=" + quote(sym + " = " + formatDef(rhs)))
    stream.println("shape=box,color=" + nodeColor(sym))
    stream.println("]")
  }

  def emitDeps(sym: Exp[_], deps: List[Exp[Any]], dotted: Boolean)(implicit stream: PrintWriter) = {
    for (dep <- deps) {
      val depLabel = dep.toString  //dep.isVar match { case true => dep.toStringWithType case _ => dep.toString }
      val params = dotted match { case true => " [style=dotted]" case _ => ""}
      stream.println("\"" + depLabel + "\" -> \"" + sym + "\"" + params)
    }
  }

  def emitDepGraph(d: Def[Any], file: String, landscape: Boolean): Unit =
    emitDepGraph(dep(d), file, landscape)
  def emitDepGraph(start: Exp[Any], file: String, landscape: Boolean = false): Unit =
    emitDepGraph(List(start), file, landscape)
  def emitDepGraph(ss: List[Exp[Any]], file: String, landscape: Boolean): Unit = {
    val f = new File(file)
    f.getParentFile.mkdirs()
    emitDepGraph(ss, new java.io.PrintWriter(new java.io.FileOutputStream(file)), landscape)
  }

  def lambdaDeps(l: Lambda[_,_]): (List[Exp[_]],List[Exp[_]]) = l.y match {
    case Def(Lambda(l1,_,_,_)) =>  val (ds, vs) = lambdaDeps(l1); (ds, l.x :: vs)
    case _ => (dep(l.y), List(l.x))
  }

  def emitDepGraph(ss: List[Exp[Any]], stream: PrintWriter, landscape: Boolean): Unit = {
    stream.println("digraph G {")

    val deflist = buildScheduleForResult(ss/* map { (_.asSymbol) }*/)

    landscape match {
      case true => stream.println("rankdir=LR")
      case _ =>
    }

    val lambdaBodies: Map[Exp[Any], Exp[Any]] = (deflist collect {
      case TP(s, Lambda(_, _, _, body)) => (body, s)
    }).toMap

    for (tp@TP(sym, rhs) <- deflist) {

      (lambdaBodies.contains(sym)) match {
        case false =>
          val (deps, lambdaVars) = rhs match {
            case Lambda(l,_,_,_) => lambdaDeps(l)
            case _ => (dep(rhs), Nil)
          }
          // emit node
          emitNode(sym, rhs)(stream)

          emitDeps(sym, deps, false)(stream)
          emitDeps(sym, lambdaVars, true)(stream)

          // emit lambda refs
          tp.lambda match {
            case Some(lam) =>
              stream.println("\"%s\" -> \"%s\" [style=dotted,color=grey]".format(tp.sym, lam))
            case _ =>
          }

        case _ =>
          // skip lambda bodies
      }
    }

    stream.println("}")
    stream.close()
  }
 
  
  
}
