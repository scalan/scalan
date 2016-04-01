package scalan.compilation

import java.io.{File, PrintWriter}

import scalan.ScalanDslExp
import scalan.util.FileUtil

case class IndentLevel(level: Int) {
  def incr = IndentLevel(level + 1)
}

// based on scala.lms.internal.GenericCodegen
/** Base class for code generators */
abstract class BaseCodegen[+ScalanCake <: ScalanDslExp](val scalan: ScalanCake) {
  import scalan._

  /** Codegen name (used in error messages) */
  lazy val codegenName = this.getClass.getSimpleName

  override def toString = codegenName

  /** Name of language generated (used in error messages) */
  def languageName: String

  /** File extension for generated code */
  def fileExtension: String = !!!(s"$codegenName must override either `fileExtension` or `fileName`")

  /** Standard indentation for one level */
  protected def indent = "    "

  /** File name (with extension) for generated code */
  def fileName(baseName: String) = baseName + "." + fileExtension

  /** Prepares to work with a kernel */
  def init(functionName: String, graph: PGraph) = {}

  /** Finishes working with a kernel */
  def reset() = {}

  def indented(f: IndentLevel => Unit)(implicit indentLevel: IndentLevel) = f(indentLevel.incr)

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
    val sourceFile = new File(sourcesDir, fileName(functionName))
    FileUtil.withFile(sourceFile) { implicit stream =>
      emitHeader(graph, functionName)
      emitKernel(graph, functionName)
      emitFooter(graph, functionName)
    }
    reset()
    sourceFile
  }

  def emitSchedule(graph: AstGraph)(implicit stream: PrintWriter, indentLevel: IndentLevel) =
    graph.schedule.foreach { te => emitNode(te.sym, te.rhs, graph) }

  /** Emits a node in the schedule. Override this for nodes which need more than one line, and
    * `rhs` for the simple cases. */
  def emitNode(sym: Exp[_], d: Def[_], graph: AstGraph)(implicit stream: PrintWriter, indentLevel: IndentLevel) =
    emit(simpleNode(sym, d))

  /** Translation of a simple (non-complex) node. Normally calls `tpe(sym.elem)` (in typed languages),
    * `id(sym)` and `rhs(d)`. Example for C: `src"${sym.elem} $sym = $d;`.
    */
  protected def simpleNode(sym: Exp[_], d: Def[_]): String

  /** Translation of the type represented by an `Elem` to the generated language
    * (no need to implement for weakly typed or fully type-inferred languages). */
  def tpe(elem: Elem[_]): String = !!!(s"$codegenName can't map ${elem.name} to $languageName")

  /** Translation of an `Exp` */
  def id(s: Exp[_]) = s.varName

  /** Translation of a literal. The default is C/Java literals for primitives and `null`. */
  def literal(value: Any): String = value match {
    case (_: Int) | (_: Double) | (_: Boolean) => value.toString
    case f: Float => "%1.10f".format(f) + "F"
    case l: Long => l.toString + "L"
    case s: String =>
      "\""+s.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n").replace("\r", "\\r").replace("\t", "\\t")+"\""
    case c: Char =>
      "'"+c.toString.replace("'", "\\'").replace("\n", "\\n").replace("\r", "\\r").replace("\t", "\\t")+"'"
    case null => "null"
    case _ => !!!(s"$codegenName doesn't know literals for ${value.getClass} in $languageName")
  }

  def unOp(op: UnOp[_, _], x: Exp[_]): String =
    src"${op.opName} $x"

  def binOp(op: BinOp[_, _], x: Exp[_], y: Exp[_]): String =
    src"$x ${op.opName} $y"

  /** Translation of a `Def` */
  def rhs(d: Def[_]): String = d match {
    case Const(x) => literal(x)
    case ApplyUnOp(op, x) => unOp(op, x)
    case ApplyBinOp(op, x, y) => binOp(op, x, y)
    case _ => !!!(s"$codegenName can't translate definition $d (type ${d.selfType.name}) to $languageName")
  }

  protected def translate(arg: Any): String = arg match {
    case elem: Elem[_] => tpe(elem)
    case e: Exp[_] => id(e)
    case d: Def[_] => rhs(d)
    case str: String => str
    case xs: Seq[_] => xs.map(translate).mkString(", ")
    case xs: Array[_] => xs.map(translate).mkString(", ")
    case _ => s"$codegenName can't translate $arg to $languageName"
  }

  implicit class CodegenHelper(sc: StringContext) {
    /** Generates code from interpolated string using `translate` for Scalan types (`Exp`, `Def`) */
    def src(args: Any*): String = {
      sc.raw(args.map(translate): _*).stripMargin
    }
  }
}
