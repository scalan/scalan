package tests.scalan.primitives

import java.io.File
import java.lang.reflect.Method

import scalan.compilation.{GraphVizConfig, GraphVizExport}
import scalan.{BaseShouldTests, ScalanCtxExp}

class ReflEqualitySuite extends BaseShouldTests {

  class Ctx extends ScalanCtxExp with GraphVizExport {
    override def isInvokeEnabled(d: Def[_], m: Method) = true
    lazy val testLemma = postulate[Int, Int, Int, Int]((x, y, z) => x * y + x * z <=> x * (y + z))
    lazy val rule = rewriteRuleFromEqLemma(testLemma)
    lazy val patGraph = rule.patternGraph
  }

  def getCtx = new Ctx

  "ScalanStaged" should "created Lemma" in {
    val ctx = getCtx
    import ctx._
    ctx.emitDepGraph(ctx.testLemma, new File(prefix, "testLemma.dot"))(GraphVizConfig.default)
  }

  it should "create LemmaRule" in {
    val ctx = getCtx
    import ctx._
    ctx.emitDepGraph(Seq[Exp[_]](testLemma, rule.pattern, rule.rhs), new File(prefix, "testRule.dot"))(GraphVizConfig.default)
  }

  it should "create ProjectionTree in pattern" in {
    val ctx = getCtx
    import ctx._
    val tree = rule.pattern.argsTree
    ctx.emitDepGraph(Seq[Exp[_]](rule.pattern, rule.rhs), new File(prefix, "testPatternAndRhs.dot"))(GraphVizConfig.default)
  }

  "LemmaRule" should "build PatternGraph" in {
    val ctx = getCtx
    import ctx._
    import ctx.graphs.Graph
    ctx.emitDepGraph(ctx.rule.pattern, new File(prefix, "testPattern.dot"))(GraphVizConfig.default)
    ctx.emitDepGraph(Graph.asDot(patGraph), new File(prefix, "patternGraph.dot"))(GraphVizConfig.default)
  }

  it should "recognize pattern" in {
    val ctx = getCtx
    import ctx._

  }

}
