package scalan.meta

import java.io.File

import scala.tools.nsc.Global
import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.StoreReporter
import scala.reflect.internal.util.{BatchSourceFile, SourceFile}
import scalan.meta.ScalanAst.{SUnitDef, STpeExpr, SExpr, SBodyItem, AstContext}

trait ScalanParsersEx[G <: Global]
  extends ScalanParsers[G] with ScalanGens[G] {
  val settings = new Settings
  settings.embeddedDefaults(getClass.getClassLoader)
  settings.usejavacp.value = true
  val reporter = new StoreReporter

  // Credit due to Li Haoyi in Ammonite:
  // Initialize scalac to the parser phase immediately, so we can start
  // using Compiler#parse even if we haven't compiled any compilation
  // units yet due to caching
  def initCompiler() = {
    val run = new compiler.Run()
    compiler.phase = run.parserPhase
    run.cancel()
  }

  import compiler._

  case class TestModule(moduleName: String, text: String, isVirt: Boolean)

  sealed trait TreeKind
  case object TopLevel extends TreeKind
  case object Type extends TreeKind
  case object Member extends TreeKind
  case object Expr extends TreeKind
  case object Annotation extends TreeKind
  case object AnnotationArg extends TreeKind

  def parseString(kind: TreeKind, prog: String): Tree = {
    // wrap the string into a complete file
    val prog1 = kind match {
      case TopLevel => prog
      case Type => s"object o { val x: $prog }"
      case Member => s"object o { $prog }"
      case Expr => s"object o { val x = $prog }"
      case Annotation => s"object o { @$prog val x = null }"
      case AnnotationArg => s"object o { @OverloadId($prog) val x = null }"
    }
    val fakeSourceFile = new BatchSourceFile("<no file>", prog1.toCharArray)
    // extract the part corresponding to original prog
    (kind, parseFile(fakeSourceFile)) match {
      case (TopLevel, tree) => tree
      case (Member, PackageDef(_, List(ModuleDef(_, _, Template(_, _, List(_, tree)))))) =>
        tree
      case (Type, PackageDef(_, List(ModuleDef(_, _, Template(_, _, List(_, ValDef(_, _, tree, _))))))) =>
        tree
      case (Expr, PackageDef(_, List(ModuleDef(_, _, Template(_, _, List(_, ValDef(_, _, _, tree))))))) =>
        tree
      case (Annotation, PackageDef(_, List(ModuleDef(_, _, Template(_, _, List(_, ValDef(Modifiers(_,_,List(tree)), _, _, _))))))) =>
        tree
      case (AnnotationArg, PackageDef(_, List(ModuleDef(_, _, Template(_, _, List(_, ValDef(Modifiers(_,_,List(ExtractAnnotation(_,List(tree)))), _, _, _))))))) =>
        tree
      case (kind, tree) =>
        ???(tree)
    }
  }

  def parseType(tpeString: String)(implicit ctx: ParseCtx): STpeExpr = {
    val tree = parseString(Type, tpeString)
    val tpe = tpeExpr(tree)
    tpe
  }

  def parseExpr(exprString: String)(implicit ctx: ParseCtx): SExpr = {
    val tree = parseString(Expr, exprString)
    val expr = parseExpr(tree)
    expr
  }

  def parseBodyItem(defString: String)(implicit ctx: ParseCtx): SBodyItem = {
    val tree = parseString(Member, defString)
    val res = optBodyItem(tree, None).get
    res
  }

  def parseModule(module: TestModule): SUnitDef = {
    implicit val ctx = new ParseCtx(module.isVirt)(context)
    val pkg = parseString(TopLevel, module.text).asInstanceOf[PackageDef]
    val m = moduleDefFromPackageDef(module.moduleName, pkg)
    m
  }

}
