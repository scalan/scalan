package scalan
package compilation
package lms
package cxx
package sharedptr

import scala.virtualization.lms.common.VariablesExp

trait CxxShptrGenVariables extends CxxShptrCodegen {
  val IR: VariablesExp
  import IR._

  override def remap[A](m: Manifest[A]) : String = {
    m match {
      case _ if m.runtimeClass == classOf[Variable[_]] =>
        remap(m.typeArguments(0))
      case _ =>
        super.remap(m)
    }
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case ReadVar(Variable(a)) =>
        emitValDef(sym, quote(a))
      case NewVar(init) =>
        emitVarDef(sym.asInstanceOf[Sym[Variable[Any]]], src"$init")
      case Assign(Variable(a), b) =>
        emitAssignment(a.asInstanceOf[Sym[Variable[Any]]], src"$b")
      case VarPlusEquals(Variable(a), b) => stream.println(quote(a) + " += " + quote(b) + ";")
      case VarMinusEquals(Variable(a), b) =>stream.println(quote(a) + " -= " + quote(b) + ";")
      case VarTimesEquals(Variable(a), b) => stream.println(quote(a) + " *= " + quote(b) + ";")
      case VarDivideEquals(Variable(a), b) => stream.println(quote(a) + " /= " + quote(b) + ";")
      case _ => super.emitNode(sym, rhs)
    }
  }
}
