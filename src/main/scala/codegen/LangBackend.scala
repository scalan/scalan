package scalan.codegen

import scalan.codegen.emit.ast._
import scalan.ScalanStaged
import scalan.staged.BaseExp

trait LangBackend extends BaseExp { self: ScalanStaged =>

  def buildCompilationUnit[Ctx <: Transformer](g: ProgramGraph[Ctx]) : AbstractCompilationUnit
  def run(dir: String, fileName: String, func: Exp[_], emitGraphs: Boolean = false)

  protected def isGlobalConst(d: Def[_]): Boolean = d match {
    case Const(_) => true
    case Lambda(_, _, _, _) => false
    case _ => d.getDeps.forall {
      case Def(d1) => isGlobalConst(d1)
      case _ => false
    }
  }

  protected def argsToForce[A](d: Def[A]): List[Exp[_]] = d match {
    case Tup(a, b) => List(a, b)
    case _ => List()
  }

  class LangCodegen[Ctx <: Transformer](g: ProgramGraph[Ctx]) {

    lazy val globalConsts = g.scheduleAll.collect {case tp@TP(_, d) if isGlobalConst(d) => tp}

    lazy val forceSymbols = {
      val res = g.scheduleAll collect { case TP(s, d) => argsToForce(d) }
      res.flatten.toSet ++ globalConsts.map({ _.sym }).toSet
    }
  }
}


