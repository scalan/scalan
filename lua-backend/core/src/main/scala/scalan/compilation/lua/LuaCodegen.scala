package scalan.compilation.lua

import java.io.{File, PrintWriter}

import scalan.ScalanDslExp
import scalan.compilation.{BaseCodegen, IndentLevel}

class LuaCodegen[+ScalanCake <: ScalanDslExp](_scalan: ScalanCake) extends BaseCodegen(_scalan) {
  import scalan._

  def languageName = "Lua"

  override def fileExtension = "lua"

  override def indent = "  "

  def emitHeader(graph: PGraph, functionName: String)(implicit stream: PrintWriter) = {
    val Def(lam: Lambda[_, _]) = graph.roots.head
    stream.println(src"function $functionName(${lam.x})")
  }

  def emitFooter(graph: PGraph, functionName: String)(implicit stream: PrintWriter) = {
    // TODO this is ugly,
    val f @ Def(lam: Lambda[_, _]) = graph.roots.head
    stream.println(src"return $f(${lam.x})")
    stream.println("end")
  }

  def simpleNode(sym: Exp[_], d: Def[_]) = src"local $sym = $d"

  override def emitNode(sym: Exp[_], d: Def[_], graph: AstGraph)(implicit stream: PrintWriter, indentLevel: IndentLevel) = {
    def initSym(rhs: Any = "{}") =
      emit(src"local $sym = $rhs")
    d match {
      // TODO See how to inline f in ArrayMap/ArrayFilter/etc.
      case ArrayMap(xs, f) =>
        initSym()
        emit(src"for i, v in ipairs($xs) do $sym[i] = $f(v) end")
      case ArrayFilter(xs, f) =>
        initSym()
        emit(src"for _, v in ipairs($xs) do if $f(v) then table.insert($sym, v) end end")
      case ArrayZip(xs, ys) =>
        initSym()
        emit(src"for i = 1, math.min(#$xs, #$ys) do $sym[i] = {$xs[i], $ys[i]} end")
      case ArrayReduce(xs, m) =>
        initSym(m.zero)
        val rhs = m.opName match {
          case "+" | "*" => src"$sym ${m.opName} v"
          case "||" => src"$sym or v"
          case "&&" => src"$sym and v"
          case _ => src"${m.append}({$sym, v})"
        }
        emit(src"for _, v in ipairs($xs) do $sym = $rhs end")
      case ArrayReplicate(len, v) =>
        initSym()
        emit(src"for i = 1, $len do $sym[i] = $v end")
      case ArrayRangeFrom0(n) =>
        initSym()
        emit(src"for i = 1, $n do $sym[i] = i - 1 end")
      case Lambda(lam, _, x, y) =>
        emit(src"local function $sym($x)")
        indented { implicit indentLevel =>
          emitSchedule(lam)
          emit(src"return $y")
        }
        emit("end")
      case IfThenElse(c, t, e) =>
        emit(src"local $sym")
        val optBranches = graph.branches.ifBranches.get(sym)
        emit(src"if $c then")
        indented { implicit indentLevel =>
          optBranches.foreach { branches => emitSchedule(branches.thenBody) }
          emit(src"$sym = $t")
        }
        emit("else")
        indented { implicit indentLevel =>
          optBranches.foreach { branches => emitSchedule(branches.elseBody) }
          emit(src"$sym = $e")
        }
        emit("end")
      case _ => super.emitNode(sym, d, graph)
    }
  }

  override def rhs(d: Def[_]) = d match {
    case Tup(x, y) => src"{$x, $y}"
    case First(pair) => src"$pair[1]"
    case Second(pair) => src"$pair[2]"
    case ArrayApply(xs, i) => src"$xs[$i + 1]"
    case ArrayLength(xs) => src"#$xs"
    case ArrayEmpty() => "{}"
    case SimpleStruct(_, fields) =>
      tableLit(fields.map { case (key, value) => s"""["$key"] = $value""" })
    case FieldApply(struct, key) => src"""$struct["$key"]"""
    case SymsArray(syms) =>
      tableLit(syms.zipWithIndex.map { case (s, i) => src"""[${(i + 1).toString}] = $s""" })
    case Semicolon(_,b) => src"$b"
    case SemicolonMulti(_,b) => src"$b"
    case _ => super.rhs(d)
  }

  override def literal(value: Any): String = value match {
    // No F or L suffixes for literals in Lua
    case (_: Float) | (_: Long) => value.toString
    case null => "nil"
    case xs: Array[_] => tableLit(xs.map(literal))
    case xs: Seq[_] => tableLit(xs.map(literal))
    case map: Map[_, _] => tableLit(map.map { case (k, v) => s"""[$k] = $v""" })
    case _: Unit => "{}"
    case _ => super.literal(value)
  }

  override def unOp(op: UnOp[_, _], x: Exp[_]): String = op match {
    case Not => src"not $x"
    case _ => super.unOp(op, x)
  }

  override def binOp(op: BinOp[_, _], x: Exp[_], y: Exp[_]): String = op match {
    case And => src"$x and $y"
    case Or => src"$x or $y"
    case _ => super.binOp(op, x, y)
  }

  def tableLit(elems: Iterable[String]) = "{" + elems.mkString(", ") + "}"
}
