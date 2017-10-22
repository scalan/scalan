package scalan.compilation.kotlin

import java.io.PrintWriter

import scalan.{ModuleInfo, Scalan}
import scalan.compilation.{IndentLevel, FileCodegen, CodegenConfig}
import scalan.meta.ScalanAst._
import scalan.meta.PrintExtensions._
import scalan.meta.{ScalanAstTransformers, Name}

case class GenCtx(module: SModuleDef, writer: PrintWriter)

class KotlinFileCodegen[+IR <: Scalan](_scalan: IR, config: CodegenConfig) extends FileCodegen(_scalan, config) {
  import scalan._
  implicit val context = parsers.context
  val PairType = Name("kotlin", "Pair")

  def languageName = "Kotlin"

  override def fileExtension = "kt"

  implicit def ctxToWriter(implicit ctx: GenCtx): PrintWriter = ctx.writer
  
  def transitiveClosureSet(modules: Seq[SModuleDef], deps: SModuleDef => Iterable[SModuleDef]): Set[Name] = {
    //TODO implement transitive closure algorithm
    modules.map(m => Name(m.packageName, m.name)).toSet
  }

  def modules: Map[Name, SModuleDef] = {
    val nameToModule = scalan.getModules
    val modules = config.moduleNames.map(n =>
      nameToModule.getOrElse(n.mkFullName, sys.error(s"Module $n not found")))
    val closure = transitiveClosureSet(modules, m => m.dependencies)
    closure.map(n => (n, nameToModule(n.mkFullName))).toMap
  }

  def genParents(ancestors: List[STypeApply])(implicit ctx: GenCtx): List[String] = {
    ancestors.map { a =>
      val tpeName = a.tpe.name
      val tpts = a.tpe.tpeSExprs.map(genTpeExpr)
      s"$tpeName${tpts.optList("<", ">")}"
    }
  }

  def genTypeArgs(tpeArgs: STpeArgs)
                 (implicit ctx: GenCtx): List[String] = tpeArgs.map(genTypeArg)

  def genTypeArg(arg: STpeArg)(implicit ctx: GenCtx): String = {
    val tpname = arg.name
    val tparams = arg.tparams.map(genTypeArg)
    s"$tpname${tparams.optList("<", ">")}"
  }

  def genClassArg(arg: SClassArg)(implicit ctx: GenCtx): String = {
    val tname = arg.name
    val tpt = genTpeExpr(arg.tpe)
    val valFlag = if (arg.valFlag) "val " else ""
    val overFlag = if (arg.overFlag) "override " else ""
    s"$overFlag$valFlag$tname: $tpt"
  }

  def genClassArgs(args: SClassArgs, implicitArgs: SClassArgs)
                  (implicit ctx: GenCtx): List[List[String]] = {
    val repArgs = args.args.map(genClassArg)
    val repImplArgs = implicitArgs.args.map(genClassArg)
    val repClassArgs = List[List[String]](repArgs, repImplArgs)
    repClassArgs.filterNot(_.isEmpty)
  }

  def genMethodArg(arg: SMethodArg)
                  (implicit ctx: GenCtx): String = {
    val tname = arg.name
    val tpt = genTpeExpr(arg.tpe)
    s"$tname: $tpt"
  }

  def genMethodArgs(argSections: List[SMethodArgs])
                   (implicit ctx: GenCtx): List[String] = {
    argSections.map(sec => s"(${sec.args.map(genMethodArg).rep()})")
  }

  def emitBodyItem(x: SBodyItem)(implicit ctx: GenCtx, l: IndentLevel) = x match {
    case SValDef(n, tpe, isLazy, _, expr) =>
      val optType = tpe.opt(t => s": ${genTpeExpr(t)}")
      //      val rhs = emit(expr)
      emit(s"""val $n${optType} = ???""")
    case md: SMethodDef =>
      val sections = md.argSections
      val args = genMethodArgs(if (sections.isEmpty) SMethodDef.emptyArgSection else sections)
      val optType = md.tpeRes.opt(t => s": ${genTpeExpr(t)}")
      md.body match {
        case Some(expr) =>
          emit(s"""fun ${md.name}${args.rep(sep = "")}${optType} {""")
          indented { implicit l =>
            emit(s"""TODO("$expr")""")
          }
          emit("}")
        case None =>
          emit(s"""fun ${md.name}${args.rep(sep = "")}${optType}""")
      }

    case _ =>
      emit(s"<<<Don't know how to emit ${x}>>>")
  }

  def emitBody(body: List[SBodyItem])(implicit ctx: GenCtx, l: IndentLevel): Unit = {
    body foreach { item =>
      emitBodyItem(item)
    }
  }

  def genTypeSel(ref: String, name: String)(implicit ctx: GenCtx) = s"$ref.$name"

  def genTuple2(first: String, second: String)(implicit ctx: GenCtx): String = {
    val tpt = genTypeSel("kotlin", "Pair")
    val tpts = first :: second :: Nil
    s"$tpt${tpts.optList("<", ">")}>"
  }

  def genTuples(elems: List[STpeExpr])(implicit ctx: GenCtx): String = elems match {
    case x :: y :: Nil => genTuple2(genTpeExpr(x), genTpeExpr(y))
    case x :: xs => genTuple2(genTpeExpr(x), genTuples(xs))
    case Nil => throw new IllegalArgumentException("Tuple must have at least 2 elements.")
  }

  def mapTypeName(name: String)(implicit ctx: GenCtx) = name match {
    case ctx.module.WrapperEntity(e, externalName) => externalName
    case _ => name
  }

  def genTpeExpr(tpeExpr: STpeExpr)(implicit ctx: GenCtx): String = tpeExpr match {
    case STpeEmpty() => ""
    case STpePrimitive(name: String, _) => name
    case STraitCall(n: String, args: List[STpeExpr]) =>
      val name = mapTypeName(n)
      val targs = args.map(genTpeExpr)
      s"$name${targs.optList("<", ">")}"
    case STpeTypeBounds(lo: STpeExpr, hi: STpeExpr) =>
      ??? //TODO  TypeBoundsTree(genTypeExpr(lo), genTypeExpr(hi))
    case STpeTuple(items: List[STpeExpr]) => genTuples(items)
    case STpeFunc(domain: STpeExpr, range: STpeExpr) =>
      val tDom = genTpeExpr(domain)
      val tRange = genTpeExpr(range)
      s"($tDom) -> $tRange"
    case _ => throw new NotImplementedError(s"genTypeExpr($tpeExpr)")
  }

  def emitTrait(t: STraitDef)(implicit ctx: GenCtx, l: IndentLevel) = {
    val traitName = t.name
    assert(t.selfType.isEmpty, "self types are not supported")
    val parents = genParents(t.ancestors)
    val tparams = t.tpeArgs.map(genTypeArg)
    emit(s"""interface $traitName${tparams.optList("<", ">")}${parents.optList(" : ", "")} { """)
    indented { implicit l =>
      emitBody(t.body)
    }
    emit("}")
  }

  def emitClass(c: SClassDef)(implicit ctx: GenCtx, l: IndentLevel) = {
    val className = c.name
    assert(c.selfType.isEmpty, "self types are not supported")
    val parents = genParents(c.ancestors)
    val paramss = genClassArgs(c.args, c.implicitArgs)
    val tparams = c.tpeArgs.map(genTypeArg)
    emit(s"""class $className${tparams.optList("<", ">")}${paramss.rep(sec => s"(${sec.rep()})")}${parents.optList(" : ", "")} { """)
    indented { implicit l =>
      emitBody(c.body)
    }
    emit("}")
  }

  val devirtPipeline = new ScalanAstTransformers.DevirtPipeline()

  def emitModule(name: Name)(implicit writer: PrintWriter, indentLevel: IndentLevel) = {
    val md = modules.getOrElse(name, { sys.error(s"Cannot find module $name") })
    val devirt = devirtPipeline(md)
    implicit val gctx = GenCtx(devirt, writer)
    for (t <- devirt.entities) {
      emitTrait(t)
    }
    for (c <- devirt.concreteSClasses) {
      emitClass(c)
    }
  }

  def emitLambdaHeader(f: Exp[_], lam: Lambda[_, _], functionName: String)
                      (implicit stream: PrintWriter, indentLevel: IndentLevel) = {
    emit(src"fun $f(${lam.x }: ${lam.x.elem }): ${lam.y.elem } {")
  }

  def emitLambdaFooter(lam: Lambda[_, _], functionName: String)
                      (implicit stream: PrintWriter, indentLevel: IndentLevel) = {
    indented { implicit indentLevel =>
      emit(src"return ${lam.y }")
    }
    emit("}")
  }

  def emitHeader(graph: PGraph, functionName: String)(implicit stream: PrintWriter) = {
    ???
  }

  def emitFooter(graph: PGraph, functionName: String)(implicit stream: PrintWriter) = {
    ???
  }

  override def tpe(elem: Elem[_]): String = elem.name

  def simpleNode(sym: Exp[_], d: Def[_]) = src"local $sym = $d"

  override def emitNode(sym: Exp[_], d: Def[_], graph: AstGraph)
                       (implicit stream: PrintWriter, indentLevel: IndentLevel) = {
    def initSym(rhs: Any = "{}") =
      emit(src"local $sym = $rhs")

    d match {
      // TODO See how to inline f in ArrayMap/ArrayFilter/etc.
      //      case ArrayMap(xs, f) =>
      //        initSym()
      //        emit(src"for i, v in ipairs($xs) do $sym[i] = $f(v) end")
      //      case ArrayFilter(xs, f) =>
      //        initSym()
      //        emit(src"for _, v in ipairs($xs) do if $f(v) then table.insert($sym, v) end end")
      //      case ArrayZip(xs, ys) =>
      //        initSym()
      //        emit(src"for i = 1, math.min(#$xs, #$ys) do $sym[i] = {$xs[i], $ys[i]} end")
      //      case ArrayReduce(xs, m) =>
      //        initSym(m.zero)
      //        val rhs = m.opName match {
      //          case "+" | "*" => src"$sym ${m.opName} v"
      //          case "||" => src"$sym or v"
      //          case "&&" => src"$sym and v"
      //          case _ => src"${m.append}({$sym, v})"
      //        }
      //        emit(src"for _, v in ipairs($xs) do $sym = $rhs end")
      //      case ArrayReplicate(len, v) =>
      //        initSym()
      //        emit(src"for i = 1, $len do $sym[i] = $v end")
      //      case ArrayRangeFrom0(n) =>
      //        initSym()
      //        emit(src"for i = 1, $n do $sym[i] = i - 1 end")
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

  def functionHeader(sym: Exp[_], args: List[Exp[_]]): String =
    src"local function $sym($args)"

  def functionReturn(y: Exp[_]): String = src"return $y"

  def functionFooter(): Option[String] = Some("end")

  override def rhs(d: Def[_]) = d match {
    case Tup(x, y) => src"$PairType($x, $y)"
    case First(pair) => src"$pair[1]"
    case Second(pair) => src"$pair[2]"
    //    case ArrayApply(xs, i) => src"$xs[$i + 1]"
    //    case ArrayLength(xs) => src"#$xs"
    //    case ArrayEmpty() => "{}"
    //    case SymsArray(syms) =>
    //      tableLit(syms.map(translate))
    case SimpleStruct(_, fields) =>
      tableLit(fields.map { case (key, value) => s"""["$key"] = $value""" })
    case FieldApply(struct, key) => src"""$struct["$key"]"""
    case StringCharAt(str, index) =>
      src"$str[$index + 1]"
    case StringSubstring(str, start, end) =>
      // indices start with 1. Also, end in Java/Scalan is exclusive, in Lua it's inclusive,
      // so the last argument isn't `$end + 1`
      src"string.sub($str, $start + 1, $end)"
    case MethodCall(receiver, method, args, _) =>
      // TODO "static" methods
      val args1 = args.flatMap(translateMethodArg)
      src"$receiver:${method.getName }($args1)"
    case Semicolon(_, b) => src"$b"
    case _ => super.rhs(d)
  }

  def translateMethodArg(arg: AnyRef): Option[String] = arg match {
    case _: TypeDesc | _: Numeric[_] | _: Ordering[_] => None
    case _ => Some(translateToSrc(arg))
  }

  override def literal(value: Any): String = value match {
    // No F or L suffixes for literals in Lua
    case (_: Float) | (_: Long) => value.toString
    case () => "nil"
    case null => "nil"
    case xs: Array[_] => tableLit(xs.map(literal))
    case xs: Seq[_] => tableLit(xs.map(literal))
    case map: Map[_, _] => tableLit(map.map { case (k, v) => s"""[${literal(k) }] = ${literal(v) }""" })
    case str: String if str.contains("\n") || str.contains("\"") =>
      val delimitersInString = """(\[=*\[)|(\]=*\])""".r.findAllMatchIn(str)
      val equalSigns =
        "=" * (if (delimitersInString.isEmpty) 0 else delimitersInString.map(x => x.end - x.start - 2).max + 1)
      s"[$equalSigns[\n$str]$equalSigns]"
    case _ => super.literal(value)
  }

  override def specialNumericLiteral(x: SpecialNumericValue, t: BaseNumericType): String = x match {
    case PosInf => "1/0"
    case NegInf => "-1/0"
    case NaN => "0/0"
  }

  override def unOp(op: UnOp[_, _], x: Exp[_]): String = op match {
    case ToString() => src"tostring($x)"
    case StringToDouble =>
      src"tonumber($x)"
    case StringToInt =>
      src"math.floor(tonumber($x))"
    case StringLength =>
      src"string.len($x)"
    case NumericToDouble(_) | NumericToFloat(_) =>
      src"$x"
    case NumericToInt(_) | NumericToLong(_) =>
      src"math.floor($x)"
    case Not => src"not $x"
    case _ => super.unOp(op, x)
  }

  override def binOp(op: BinOp[_, _], x: Exp[_], y: Exp[_]): String = op match {
    case StringConcat =>
      src"$x .. $y"
    case StringContains =>
      // true for non-pattern search
      src"string.find($x, $y, 1, true) ~= nil"
    case StringMatches =>
      // TODO Lua has some differences from standard regexes, check what we use
      src"string.find($x, $y) ~= nil"
    case StringEndsWith =>
      // TODO escape special characters in y (next case as well)
      src"""string.match($x, $y .. "$$")"""
    case StringStartsWith =>
      src"""string.match($x, "^" .. $y)"""
    case IntegralDivide(_) =>
      src"""math.floor($x / $y)"""
    case OrderingCompare(_) =>
      // see http://lua-users.org/wiki/TernaryOperator
      src"$x < $y and -1 or ($x > $y and 1 or 0)"
    case OrderingMax(_) =>
      s"$x < $y and $y or $x"
    case OrderingMin(_) =>
      s"$x < $y and $x or $y"
    case And => src"$x and $y"
    case Or => src"$x or $y"
    case _ => super.binOp(op, x, y)
  }

  def tableLit(elems: Iterable[String]) = "{" + elems.mkString(", ") + "}"

  override protected def translateToSrc(arg: Any): String = arg match {
    case _ =>
      super.translateToSrc(arg)
  }

  class SrcStringHelperKotlin(sc: StringContext) extends SrcStringHelperBase(sc) {}

  override implicit def srcStringHelper(sc: StringContext): SrcStringHelper = new SrcStringHelperKotlin(sc)
}
