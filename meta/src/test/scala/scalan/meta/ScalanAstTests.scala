package scalan.meta

import org.scalatest.{FlatSpec, Matchers}

import scala.tools.nsc.Global
import scala.reflect.internal.util.BatchSourceFile
import scalan.BaseNestedTests
import scalan.meta.ScalanAst.{STpeExpr, SExpr, SModuleDef, SClassDef, STpePrimitives, STraitDef, SMethodDef, SBodyItem, AstContext}

/**
  * Created by slesarenko on 16/10/2017.
  */
trait ScalanAstTests extends BaseNestedTests with ScalanParsersEx[Global] {
  def getGlobal = new Global(settings, reporter)
  initCompiler()
  implicit val context = new AstContext(BoilerplateToolRun.allConfigs)
  implicit val ctx = new ParseCtx(true)

  val ast: this.type = this
  import scalan.meta.ScalanAst.{STraitCall => TC, SModuleDef => EMD, SClassDef => CD, STpeTuple => T, SMethodArg => MA, STraitDef => TD, SMethodDef => MD, SMethodArgs => MAs, SImportStat => IS}
  import scala.{List => L}
  import compiler._

  val INT = STpePrimitives("Int")
  val BOOL = STpePrimitives("Boolean")
  val FLOAT = STpePrimitives("Float")

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
  
  def parseModule(moduleText: String)(implicit ctx: ParseCtx): SModuleDef = {
    val pkg = parseString(TopLevel, moduleText).asInstanceOf[PackageDef]
    val module = moduleDefFromPackageDef(pkg)
    module
  }

  def test[A](kind: TreeKind, prog: String, expected: A)(f: Tree => A) {
    it(prog) {
      val tree = parseString(kind, prog)
      val res = f(tree)
      assertResult(expected)(res)
    }
  }

  def testModule(prog: String, expected: SModuleDef)(implicit ctx: ParseCtx) {
    test(TopLevel, prog, expected) { case tree: PackageDef => moduleDefFromPackageDef(tree) }
  }

  def testTrait(prog: String, expected: STraitDef)(implicit ctx: ParseCtx) {
    test(Member, prog, expected) { case tree: ClassDef => traitDef(tree, Some(tree)) }
  }
  def testSClass(prog: String, expected: SClassDef)(implicit ctx: ParseCtx) {
    test(Member, prog, expected) { case tree: ClassDef => classDef(tree, Some(tree)) }
  }

  def testSTpe(prog: String, expected: STpeExpr)(implicit ctx: ParseCtx) {
    test(Type, prog, expected)(tpeExpr)
  }
  def testSMethod(prog: String, expected: SMethodDef)(implicit ctx: ParseCtx) {
    test(Member, prog, expected) { case tree: DefDef => methodDef(tree) }
  }
}
