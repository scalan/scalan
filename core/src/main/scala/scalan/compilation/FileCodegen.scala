package scalan.compilation

import java.io.{PrintWriter, File}

import scala.collection.mutable
import scalan.Scalan
import scalan.util.FileUtil

case class IndentLevel(level: Int) {
  def incr = IndentLevel(level + 1)
}

/** Base class for code generators */
abstract class FileCodegen[+ScalanCake <: Scalan](val scalan: ScalanCake) {
  import scalan._

  /** Codegen name (used in error messages) */
  lazy val codegenName = this.getClass.getSimpleName

  override def toString = codegenName

  /** Name of language generated (used in error messages) */
  def languageName: String

  /** File extension for generated code */
  def fileExtension: String = !!!(s"$codegenName must override either `fileExtension` or `fileName`")

  /** Standard indentation for one level is 4 spaces, can be according to defaults of the specific language */
  protected def indent = "    "

  /** File name (with extension) for generated code */
  def fileName(baseName: String) = baseName + "." + fileExtension

  /** Prepares to work with a kernel */
  def init(functionName: String, graph: PGraph) = {}

  /** Finishes working with a kernel */
  def reset() = {}

  /** Executes given function <code>f</code> with incremented identation.
    * Current identation at call site is passed as implicit parameter. */
  def indented(f: IndentLevel => Unit)(implicit indentLevel: IndentLevel) = f(indentLevel.incr)

  val importBuilder = new ImportBuilder

  def emit(string: String)(implicit stream: PrintWriter, indentLevel: IndentLevel) = {
    stream.print(indent * indentLevel.level)
    stream.println(string)
  }

  // May need to be split into emitFileHeader and emitKernelHeader if there are multiple kernels
  /** Emits the initial part of the file (before the kernel) */
  def emitHeader(graph: PGraph, functionName: String)(implicit stream: PrintWriter)

  /** Emits the kernel */
  def emitKernel(graph: PGraph, functionName: String)(implicit stream: PrintWriter) =
    emitSchedule(graph)(stream, IndentLevel(0))

  /** Emits the final part of the file (after the kernel) */
  def emitFooter(graph: PGraph, functionName: String)(implicit stream: PrintWriter)

  /** Emits the complete source file */
  def emitSourceFile(graph: PGraph, functionName: String, sourcesDir: File) = {
    init(functionName, graph)
    val sourceFile = this.sourceFile(sourcesDir, functionName)
    FileUtil.withFile(sourceFile) { implicit stream =>
      emitHeader(graph, functionName)
      emitKernel(graph, functionName)
      emitFooter(graph, functionName)
    }
    reset()
    sourceFile
  }

  def sourceFile(sourcesDir: File, functionName: String): File = {
    new File(sourcesDir, fileName(functionName))
  }

  def emitSchedule(graph: AstGraph, f: Schedule => Schedule = identity)
                  (implicit stream: PrintWriter, indentLevel: IndentLevel) = {
    val originalSchedule = graph.schedule
    val schedule = f(originalSchedule)
    schedule.foreach { te =>
      emitNode(te.sym, te.rhs, graph)
    }
  }

  /** Emits a node in the schedule. Override this for nodes which need more than one line, and
    * `rhs` for the simple cases. */
  def emitNode(sym: Sym, d: Def[_], graph: AstGraph)
              (implicit stream: PrintWriter, indentLevel: IndentLevel) = d match {
    case Lambda(lam, _, x, y) =>
      val args = argList(sym, x)
      emitFunction(sym, args, Some(y), lam)
    case th@ThunkDef(root, schedule) =>
      emitFunction(sym, Nil, Some(root), th)
    case _ => emit(simpleNode(sym, d))
  }

  /** Assume x is right-nested tuple like (a_1, (a_2, (a_3, ... (a_{N-1}, a_N)...)
    *
    * @return a list of a_i */
  def argList(f: Sym, x: Sym): List[Sym] = {
    def argList(x: Sym, n: Int): List[Sym] =
      n match {
        case 1 =>
          x :: Nil
        case _ =>
          x.elem match {
            case _: PairElem[a, b] =>
              val Pair(head, tail) = x.asRep[(a, b)]
              head :: argList(tail, n - 1)
            case _ =>
              !!!(s"$n arguments expected, but ${x.toStringWithDefinition } is not a nested tuple")
          }
      }

    val numArgs = getMetadata(f, MultipleArgsKey).getOrElse(1)
    argList(x, numArgs)
  }

  def emitFunction(sym: Sym,
                   args: List[Sym],
                   returnValue: Option[Exp[Any]],
                   lambdaOrThunk: AstGraph,
                   f: Schedule => Schedule = identity)(implicit stream: PrintWriter, indentLevel: IndentLevel): Unit = {
    emit(functionHeader(sym, args))
    indented { implicit indentLevel =>
      emitSchedule(lambdaOrThunk, (s: Schedule) => f(s.filterNot(te => args.contains(te.sym))))
      returnValue.foreach(x => emit(functionReturn(x)))
    }
    functionFooter().foreach(emit(_))
  }

  def functionHeader(sym: Sym, args: List[Sym]): String

  def functionReturn(y: Sym): String

  def functionFooter(): Option[String]

  /** Translation of a simple (non-complex) node. Normally calls `tpe(sym.elem)` (in typed languages),
    * `id(sym)` and `rhs(d)`. Example for C: `src"${sym.elem} $sym = $d;`.
    */
  protected def simpleNode(sym: Sym, d: Def[_]): String

  /** Translation of the type represented by an `Elem` to the generated language
    * (no need to implement for weakly typed or fully type-inferred languages). */
  def tpe(elem: Elem[_]): String = !!!(s"$codegenName can't map ${elem.name } to $languageName")

  /** Translation of a `Sym` to variable name */
  def id(s: Sym) = s.varName

  /** Translation of a literal. The default is C/Java literals for primitives and `null`. */
  def literal(value: Any): String = value match {
    case Double.PositiveInfinity => specialNumericLiteral(PosInf, DOUBLE)
    case Float.PositiveInfinity => specialNumericLiteral(PosInf, FLOAT)
    case Double.NegativeInfinity => specialNumericLiteral(NegInf, DOUBLE)
    case Float.NegativeInfinity => specialNumericLiteral(NegInf, FLOAT)
    case d: Double if d.isNaN => specialNumericLiteral(NaN, DOUBLE)
    case f: Float if f.isNaN => specialNumericLiteral(NaN, FLOAT)
    case (_: Int) | (_: Double) | (_: Boolean) => value.toString
    case f: Float => "%1.10f".format(f) + "F"
    case l: Long => l.toString + "L"
    case s: String =>
      "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n").replace("\r", "\\r").replace("\t", "\\t") + "\""
    case c: Char =>
      "'" + c.toString.replace("'", "\\'").replace("\n", "\\n").replace("\r", "\\r").replace("\t", "\\t") + "'"
    case null => "null"
    case _ => !!!(s"$codegenName doesn't know literals for ${value.getClass } in $languageName")
  }

  sealed trait SpecialNumericValue

  case object PosInf extends SpecialNumericValue

  case object NegInf extends SpecialNumericValue

  case object NaN extends SpecialNumericValue

  sealed trait BaseNumericType

  case object FLOAT extends BaseNumericType

  case object DOUBLE extends BaseNumericType

  def specialNumericLiteral(x: SpecialNumericValue, t: BaseNumericType): String

  def unOp(op: UnOp[_, _], x: Sym): String =
    src"${op.opName } $x"

  def binOp(op: BinOp[_, _], x: Sym, y: Sym): String =
    src"$x ${op.opName } $y"

  /** Translation of a `Def` */
  def rhs(d: Def[_]): String = d match {
    case Const(x) => literal(x)
    case ApplyUnOp(op, x) => unOp(op, x)
    case ApplyBinOp(op, x, y) => binOp(op, x, y)
    case Apply(f, x) =>
      val args = argList(f, x)
      applyFunction(f, args)
    case ThunkForce(th) =>
      applyFunction(th, Nil)
    case _ => !!!(s"$codegenName can't translate definition $d (type ${d.selfType.name }) to $languageName")
  }

  def applyFunction(f: Sym, args: Seq[Sym]): String = src"$f($args)"

  protected def translateToSrc(arg: Any): String = arg match {
    case elem: Elem[_] => tpe(elem)
    case s: Sym => id(s)
    case d: Def[_] => rhs(d)
    case str: String => str
    case num: Number => num.toString
    case xs: Seq[_] => xs.map(translateToSrc).mkString(", ")
    case xs: Array[_] => xs.map(translateToSrc).mkString(", ")
    case n @ Name(p, name) =>
      if (importBuilder.findImportItem(n).isDefined) name
      else
      if (importBuilder.addImport(n)) name
      else s"$p.$name"
    case _ => !!!(s"$codegenName can't translate $arg to $languageName")
  }

  trait SrcStringHelper {
    def src(args: Any*): String
  }

  implicit def srcStringHelper(sc: StringContext): SrcStringHelper

  class SrcStringHelperBase(sc: StringContext) extends SrcStringHelper {
    /** Generates code from interpolated string using `translate` for Scalan types (`Exp`, `Def`) */
    def src(args: Any*): String = {
      sc.raw(args.map(translateToSrc): _*).stripMargin
    }
  }

}
