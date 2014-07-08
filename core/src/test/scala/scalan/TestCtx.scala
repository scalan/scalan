package scalan

import scalan.compilation.GraphVizExport

trait TestCtx[A,B,C,D] extends ScalanCtxExp with GraphVizExport {
  type LemmaArg = A
  type LemmaRes = B
  type TestArg = C
  type TestRes = D

  implicit def eA: Elem[A]
  implicit def eC: Elem[C]
  implicit def eD: Elem[D]
  def testLemma: EqLemma[A,B]
  def test: Exp[C] => Exp[D]
  def expected: Exp[C] => Exp[D]

  lazy val rule = rewriteRuleFromEqLemma(testLemma)
  lazy val patGraph = rule.patternGraph
  lazy val testFunc = fun(test)
}
