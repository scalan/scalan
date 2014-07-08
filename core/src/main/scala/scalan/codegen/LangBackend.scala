package scalan.codegen

import scalan.ScalanStaged
import scalan.staged.BaseExp

trait LangBackend extends BaseExp { self: ScalanStaged =>

  def run[A,B](dir: String, fileName: String, func: Exp[A=>B], emitGraphs: Boolean = false)

  protected def isGlobalConst(d: Def[_]): Boolean = d match {
    case Const(_) => true
    case _: Lambda[_, _] => false
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

    lazy val globalConsts = g.scheduleAll.collect {case tp@TableEntry(_, d) if isGlobalConst(d) => tp}

    lazy val forceSymbols = {
      val res = g.scheduleAll collect { case TableEntry(s, d) => argsToForce(d) }
      res.flatten.toSet ++ globalConsts.map({ _.sym }).toSet
    }
  }
}


