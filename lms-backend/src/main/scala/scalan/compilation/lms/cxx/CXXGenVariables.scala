package scalan.compilation.lms.cxx

import scala.virtualization.lms.common.VariablesExp

trait CXXGenVariables extends CXXCodegen {
  val IR: VariablesExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case ReadVar(Variable(a)) => emitValDef(sym, quote(a))
      case NewVar(init) =>
        emitVarDef(sym.asInstanceOf[Sym[Variable[Any]]], quoteMove(init))
      case Assign(Variable(a), b) =>
        emitAssignment(a.asInstanceOf[Sym[Variable[Any]]], quoteMove(b))
      case VarPlusEquals(Variable(a), b) => stream.println(quote(a) + " += " + quote(b) + ";")
      case VarMinusEquals(Variable(a), b) =>stream.println(quote(a) + " -= " + quote(b) + ";")
      case VarTimesEquals(Variable(a), b) => stream.println(quote(a) + " *= " + quote(b) + ";")
      case VarDivideEquals(Variable(a), b) => stream.println(quote(a) + " /= " + quote(b) + ";")
      case _ => super.emitNode(sym, rhs)
    }
  }
}
