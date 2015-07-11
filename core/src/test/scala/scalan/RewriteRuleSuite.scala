package scalan

import java.io.File

import scalan.compilation.GraphVizConfig

trait RewriteRuleSuite[A,B,C,D] extends BaseShouldTests {
  lazy val folder = new File(prefix, suiteName)

  def getCtx: TestCtx[A,B,C,D]

  "ScalanCtx" should "stage Lemma" in {
    val ctx = getCtx
    ctx.emitDepGraph(ctx.testLemma, new File(folder, "testLemma.dot"))(GraphVizConfig.default)
  }

  it should "create LemmaRule" in {
    val ctx = getCtx
    import ctx._
    ctx.emitDepGraph(List(testLemma, rule.pattern, rule.rhs), new File(folder, "testRule.dot"))(GraphVizConfig.default)
  }

  it should "create ProjectionTree in pattern" in {
    val ctx = getCtx
    import ctx._
    val tree = rule.pattern.argsTree
    println(tree)
    ctx.emitDepGraph(List(rule.pattern, rule.rhs), new File(folder, "testPatternAndRhs.dot"))(GraphVizConfig.default)
  }

  "LemmaRule" should "build PatternGraph" in {
    val ctx = getCtx
    import ctx.graphs._
    ctx.emitDepGraph(List(ctx.rule.pattern), new File(folder, "testPattern.dot"))(GraphVizConfig.default)
    val dot = Graph.asDot(ctx.patGraph)
    ctx.emitDot(dot, new File(folder, "PatternGraph.dot"))(GraphVizConfig.default)
  }

  it should "recognize pattern" in {
    val ctx = getCtx
    import ctx._
    val lam = testFunc.getLambda
    ctx.emitDepGraph(List(rule.pattern, testFunc), new File(folder, "LemmaRule/patternAndTestFunc.dot"))(GraphVizConfig.default)
    rule.matchWith(lam.y) match {
      case Some((res, subst)) =>
        res should be(graphs.SimilarityEmbeded)
        subst should not be(Map.empty)

      case _ =>
        fail("should recognize pattern")
    }

  }

  it should "apply pattern" in {
    val ctx = getCtx
    import ctx._
    val lam = testFunc.getLambda
    val rewritten = rule(lam.y)
    rewritten match {
      case Some(res) =>
        ctx.emitDepGraph(List(Pair(lam.y, res)), new File(folder, "LemmaRule/originalAndRewritten.dot"))(GraphVizConfig.default)
      case _ =>
        fail("should apply pattern")
    }
  }

  it should "rewrite when registered" in {
    val ctx = getCtx
    import ctx._
    val withoutRule = testFunc
    addRewriteRules(rule)
    val withRule = fun(test)
    removeRewriteRules(rule)
    ctx.emitDepGraph(List(withoutRule, withRule), new File(folder, "LemmaRule/ruleRewriting.dot"))(GraphVizConfig.default)

    val expectedResult = fun(expected)
    alphaEqual(withRule, expectedResult) should be(true)
    alphaEqual(withoutRule, expectedResult) should be(false)

    val afterRemoval = fun(test)
    ctx.emitDepGraph(List(withoutRule, withRule, afterRemoval), new File(folder, "LemmaRule/ruleRewriting.dot"))(GraphVizConfig.default)
    alphaEqual(afterRemoval, withoutRule) should be(true)
  }

}
