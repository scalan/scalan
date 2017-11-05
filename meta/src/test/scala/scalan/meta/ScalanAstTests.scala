package scalan.meta

import org.scalatest.{FlatSpec, Matchers}

import scala.tools.nsc.Global
import scala.reflect.internal.util.BatchSourceFile
import scalan.BaseNestedTests
import scalan.meta.ScalanAst.{STpeExpr, SExpr, SUnitDef, SClassDef, STpePrimitives, STraitDef, SMethodDef, SBodyItem, AstContext}

trait ScalanAstTests extends BaseNestedTests with ScalanParsersEx[Global] {
  def getGlobal = new Global(settings, reporter)
  initCompiler()
  implicit val context = new AstContext(Nil, this)
  context.loadModulesFromFolders()

  import compiler._

  override def parseModule(module: TestModule): SUnitDef = {
    val m = super.parseModule(module)
    assertResult(module.isVirt)(m.isVirtualized)
    m
  }

  def test[A](kind: TreeKind, prog: String, expected: A)(f: Tree => A) {
    it(prog) {
      val tree = parseString(kind, prog)
      val res = f(tree)
      assertResult(expected)(res)
    }
  }

  def testModule(module: TestModule, expected: SUnitDef)(implicit ctx: ParseCtx) {
    test(TopLevel, module.text, expected) { case tree: PackageDef =>
       moduleDefFromPackageDef(module.moduleName, tree)
    }
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
