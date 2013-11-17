package scalan.codegen.emit

import ast._
import ast.CompoundStat
import ast.IfStat
import ast.PragmaDecl

trait CodeEmitter {

  def emitList[A <: AST](list: List[A], separator: String)(implicit f: Formatter): Unit =
    list.foldLeft(""){ (sep, p) => f << sep << emit(p); separator }

  def emit(ast: AST)(implicit f: Formatter): Unit =
  {
    ast match {
      case CompoundStat(stats) =>
        f --> {
          implicit f => {
            f << "{"
            stats map ( p => f << "\n" << emit(p) << endOfLine(p))
          }
        }
        f << "\n}"

      case BooleanConst(b) =>
        f << b.toString
      case IntConst(n) =>
        f << n.toString

      case Operator(op) =>
        f << " " << op << " "

      case Var(name) => f << name

      case SimpleType(name) =>
        f << name

      case BinExpr(e1, e2, op) =>
        f << emit(e1) << emit(op) << emit(e2)

      case IfStat(cond, ts, es) =>
        f << "if (" << emit(cond) << ") " << emit(ts) << "else " << emit(es)

      case ClassField(inst, field) =>
        f << emit(inst) << "." << field

      case ReturnStat(e) =>
        f << "return " << this.emit(e)

      case _ => !!!("don't know how to emit", ast)
    }
  }

  def endOfLine( ast : AST) : String = ast match
  {
    case PragmaDecl(_) => ""
    case CompoundStat(_) => ""
    case IfStat(_,_,_) => ""
    case FuncDecl(_,_,_,_,_,_) => ""
    case _ => ";"
  }
}
