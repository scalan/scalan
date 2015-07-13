package scalan

import java.io.File

import scalan.compilation.{GraphVizExport, GraphVizConfig}

trait RewriteRuleSuite[A] extends BaseShouldTests {
  lazy val folder = new File(prefix, suiteName)

  def getCtx: TestCtx

  trait TestCtx extends ScalanCtxExp {
    def testLemma: EqLemma[A]
    def testExpr(): Exp[A]
    def expected: Exp[A]

    lazy val rule = rewriteRuleFromEqLemma(testLemma)
  }

  "ScalanCtx" should "stage Lemma" in {
    val ctx = getCtx
    ctx.emitDepGraph(ctx.testLemma, new File(folder, "testLemma.dot"))(GraphVizConfig.default)
  }

  it should "create LemmaRule" in {
    val ctx = getCtx
    import ctx._
    ctx.emitDepGraph(List(testLemma, rule.lhs, rule.rhs), new File(folder, "testRule.dot"))(GraphVizConfig.default)
  }

  it should "create ProjectionTree in pattern" in {
    val ctx = getCtx
    import ctx._
    ctx.emitDepGraph(List(rule.lhs, rule.rhs), new File(folder, "testPatternAndRhs.dot"))(GraphVizConfig.default)
  }

  it should "recognize pattern" in {
    val ctx = getCtx
    import ctx._
    patternMatch(rule.lhs, testExpr()) match {
      case Some(subst) =>
        subst should not be(Map.empty)
      case _ =>
        fail("should recognize pattern")
    }

  }

  it should "apply pattern" in {
    val ctx = getCtx
    import ctx._
    val test = testExpr()
    val rewritten = rule(test)
    rewritten match {
      case Some(res) =>
        ctx.emitDepGraph(List(Pair(test, res)), new File(folder, "LemmaRule/originalAndRewritten.dot"))(GraphVizConfig.default)
      case _ =>
        fail("should apply pattern")
    }
  }

  it should "rewrite when registered" in {
    val ctx = getCtx
    import ctx._
    val withoutRule = testExpr()
    addRewriteRules(rule)
    val withRule = testExpr()
    removeRewriteRules(rule)
    ctx.emitDepGraph(List(withoutRule, withRule), new File(folder, "LemmaRule/ruleRewriting.dot"))(GraphVizConfig.default)

    val expectedResult = expected
    alphaEqual(withRule, expectedResult) should be(true)
    alphaEqual(withoutRule, expectedResult) should be(false)

    val afterRemoval = testExpr()
    ctx.emitDepGraph(List(withoutRule, withRule, afterRemoval), new File(folder, "LemmaRule/ruleRewriting.dot"))(GraphVizConfig.default)
    alphaEqual(afterRemoval, withoutRule) should be(true)
  }

}
