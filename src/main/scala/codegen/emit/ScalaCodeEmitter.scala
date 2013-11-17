package scalan.codegen.emit

import scalan.codegen.emit.ast._

class ScalaCodeEmitter extends CodeEmitter {
  def varPrefix(s: Boolean) = if (s) "val " else "var "

  override def emit(ast: AST)(implicit f: Formatter): Unit =  ast match {
    case VarDecl(t, v, isStatic) =>
      f << varPrefix(isStatic) << emit(v) << ": " << emit(t)
    case VarDeclInit(t, v, e, isStatic) =>
      f << varPrefix(isStatic) << emit(v) << ": " << emit(t) << " = " << emit(e)
    case d@VarDeclConstr(t, v, params) =>
      f << varPrefix(d.isStatic) << emit(v) << ": " << emit(t) <<
          " = new " << emit(t) << "(" << (emitList(params, ", ")) << ")"
//    case ForLoop(stat1, expr2, stat3, body) =>
//      f << "for (" << emit(stat1) << "; " << emit(expr2) << "; " << emit(stat3)
//      f << ") " << emit(body)
    case FuncCall(name, types, params) =>
      def targs: Unit = if (!types.isEmpty) f << "[" << emitList(types, ",") << "]"
      f << emit(name) << targs << "("
      emitList(params, ", ")
      f << ")"
      /*
    case FuncDecl(returnType, name, args, body, isStatic) =>
      if (isStatic) f << "static "
      f << emit(returnType) << " " << name << "(" << (emitList(args, ", ")) << ") " << emit(body)
      */
    case ArrIndex(arr, i) =>
      f << emit(arr) << "(" << emit(i) << ")"
    case FloatConst(Float.PositiveInfinity) => f << "Float.PositiveInfinity"
    case FloatConst(n) => f << n.toString
    case MethodCall(inst, method, types, params) =>
      def targs: Unit = if (!types.isEmpty) f << "[" << emitList(types, ",") << "]"
      f << emit(inst) << "." << method << targs << "("
      emitList(params, ", ")
      f << ")"
    case ParenExpr(expr) =>
      f << "(" << emit(expr) << ")"
    case UnaryExpr(op, expr, isPostfix) =>
      if (isPostfix)
        f << emit(expr) << emit(op)
      else
        f << emit(op) << emit(expr)
    case ReturnStat(e) =>
      f << "return " << (this.emit(e))
    case BreakStat => f << "break"
    case ClassType(name) =>
      f << "class " << name
    case TemplateType(name, types) =>
      f << name << "["
      emitList(types, ", ")
      f << "]"
    case PAType(t) => f << "PA[" << emit(t) << "]"
    case NAType(t) => f << "NA[" << emit(t) << "]"
    case PairType(t1, t2) => f << "(" << emit(t1) << "," << emit(t2) << ")"
    case PairArrayType(t1, t2) => f << "PairArray[" << emit(t1) << "," << emit(t2) << "]"
    case TypeCast(t, e) => f << emit(e) << ".asInstanceOf[" << emit(t) << "]"
    case TypeDef(t, name) => f << "type " << name << " = " << emit(t)
    case CompilationUnit(cs, cls, fs, _) =>
      emitList(cs, ";\n")
      emitList(fs, "\n")

    case FuncDecl(typeArgs, rt, name, args, CompoundStat(stats), isStatic) =>
      def types: Unit = typeArgs.isEmpty match {
        case false => f << "[" << emitList(typeArgs, ",") << "]"
        case true =>
      }
      f << "def " << name << types << "(" << emitList(args, ", ") << ")"
      f --> {
        implicit f => {
          f << "{"
          stats map { p => f << "\n" << emit(p) << endOfLine(p)}
        }
      }
      f<< "\n}"

//    case ClassDecl( tyArgs, name, typedefs, initMembers,
//                    (membersPriv, membersPub), constructors, (bodyPriv, bodyPub)) =>
//      f << "class " << name
//      f --> {
//        implicit f => {
//          f << "{\n"
//          f << "private : "
//          membersPriv map { p => f << "\n" << emit(p) << endOfLine(p)}
//          bodyPriv map  { p => f << "\n" << emit(p) << endOfLine(p)}
//
//          f << "\npublic : "
//          typedefs map { p => f << "\n" << emit(p) << endOfLine(p)}
//          constructors map{ p => f << "\n" << emit(p) << endOfLine(p)}
//          membersPub map { p => f << "\n" << emit(p) << endOfLine(p)}
//          bodyPub map  { p => f << "\n" << emit(p) << endOfLine(p)}
//        }
//      }
//      f << "\n};"
//      initMembers map { case VarDeclInit(curType, Var(memberName), zero, isStatic) => {
//        /* Append var name by class name */
//        val init = VarDeclInit(curType, Var(name +"::" + memberName), zero, isStatic)
//        f << "\n" << emit(init) << endOfLine(init)
//      }}

    case FuncArg(t, name) =>
      f << name << ": " << emit(t)

    case EmptyExpr =>

    case _ => super.emit(ast)
  }

}
