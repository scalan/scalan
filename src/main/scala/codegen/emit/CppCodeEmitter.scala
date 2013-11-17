package scalan.codegen.emit

import ast._

class CppCodeEmitter extends CodeEmitter {

  override def emit(ast: AST)(implicit f: Formatter): Unit =  ast match {
    case VarDecl(t, v, isStatic) =>
      val static = if (isStatic) "static " else ""
      f << static << emit(t) << " " << emit(v)
    case VarDeclInit(t, v, e, isStatic) =>
      val static = if (isStatic) "static " else ""
      f << static << emit(t) << " " << emit(v) << " = " << emit(e)
    case VarDeclConstr(t, v, params) =>
      f << (emit(t)) << " " << (emit(v)) << "(" << (emitList(params, ", ")) << ")"
    case ForLoop(stat1, expr2, stat3, body) =>
      f << "for (" << emit(stat1) << "; " << emit(expr2) << "; " << emit(stat3)
      f << ") " << emit(body)
    case FuncCall(name, types, params) =>
      def targs: Unit = if (!types.isEmpty) f << "<" << emitList(types, ",") << ">"
      f << emit(name) << targs << "("
      emitList(params, ", ")
      f << ")"
      /*
    case FuncDecl(returnType, name, args, body, isStatic) =>
      if (isStatic) f << "static "
      f << emit(returnType) << " " << name << "(" << (emitList(args, ", ")) << ") " << emit(body)
      */
    case ArrIndex(arr, i) =>
      f << emit(arr) << "[" << emit(i) << "]"
    case FloatConst(Float.PositiveInfinity) => f << "FLT_MAX"
    case FloatConst(n) => f << n.toString
    case MethodCall(inst, method, types, params) =>
      def targs: Unit = if (!types.isEmpty) f << "<" << emitList(types, ",") << ">"
      f << emit(inst) << "." << method << targs << "("
      emitList(params, ", ")
      f << ")"
    case StaticMethodCall(classType, method, params) =>
      f << emit(classType) << "::" << method << "("
      emitList(params, ", ")
      f << ")"
    case NewArray(t, expr) =>
      f << "new " << emit(t) << "[" << emit(expr) << "]"
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
    case RefType(t) =>
      f << emit(t) << "&"
    case ConstType(t) =>
      f << "const " << emit(t)
    case ConstexprType(t) =>
      f << "constexpr " << emit(t)
    case PtrType(t) =>
      f << emit(t) << "*"
    case TemplateType(name, types) =>
      f << name << "< "
      emitList(types, ", ")
      f << " >"
    case PAType(t) => f << "PArray<" << emit(t) << ">"
    case NAType(t) => f << "NArray<" << emit(t) << ">"
    case PairType(t1, t2) => f << "std::pair<" << emit(t1) << "," << emit(t2) << ">"
    case PairArrayType(t1, t2) => f << "PairArray<" << emit(t1) << "," << emit(t2) << ">"
    case PairChunksType(t1, t2) => f << "ChunkedZip<" << emit(t1) << "," << emit(t2) << ">"
    case ChunksType(t) => f << "Chunks<" << emit(t) << ">"
    case PAChunksType(t) => f << "ChunkedPA<" << emit(t) << ">"
    case TypeCast(t, e) => f << "((" << emit(t) << ")" << emit(e) << ")"
    case TypeDef(t, name) => f << "typedef " << emit(t) << " " << name
    case PragmaDecl(text) => f << "#pragma " << text
    case CompilationUnit(cs, cls, fs, _) =>
      emitList(cs, ";\n")
      emitList(fs, "\n")

    case FuncDecl(templateArgs, rt, name, args, CompoundStat(stats), isStatic) =>
      templateArgs.isEmpty match {
        case false => f << "template <" << emitList(templateArgs, ", class ") << ">\n"
        case true =>
      }
      val static = if (isStatic) "static " else ""
      f << static << emit(rt) << " " << name << "(" << emitList(args, ", ") << ")"
      f --> {
        implicit f => {
          f << "{"
          stats map { p => f << "\n" << emit(p) << endOfLine(p)}
        }
      }
      f<< "\n}"

    case ClassDecl( tyArgs, name, typedefs, initMembers,
                    (membersPriv, membersPub), constructors, (bodyPriv, bodyPub)) =>
      f << "class " << name
      f --> {
        implicit f => {
          f << "{\n"
          f << "private : "
          membersPriv map { p => f << "\n" << emit(p) << endOfLine(p)}
          bodyPriv map  { p => f << "\n" << emit(p) << endOfLine(p)}

          f << "\npublic : "
          typedefs map { p => f << "\n" << emit(p) << endOfLine(p)}
          constructors map{ p => f << "\n" << emit(p) << endOfLine(p)}
          membersPub map { p => f << "\n" << emit(p) << endOfLine(p)}
          bodyPub map  { p => f << "\n" << emit(p) << endOfLine(p)}
        }
      }
      f << "\n};"
      initMembers map { case VarDeclInit(curType, Var(memberName), zero, isStatic) => {
        /* Append var name by class name */
        val init = VarDeclInit(curType, Var(name +"::" + memberName), zero, isStatic)
        f << "\n" << emit(init) << endOfLine(init)
      }}

    case FuncArg(t, name) =>
      f << emit(t) << " " << name

    case EmptyExpr =>

    case _ => super.emit(ast)
  }

}
