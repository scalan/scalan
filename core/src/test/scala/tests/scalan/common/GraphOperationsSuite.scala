package tests.scalan.common

import org.scalatest.{Matchers, FunSpec}
import scalan.common.{InductiveGraphs, GraphOperations}

class GraphOperationsSuite extends FunSpec with Matchers with InductiveGraphTesting {

  val graphs = new GraphOperations[Int] {}
  import graphs._

  type TestGraph = Graph[Int, Unit]
  def isVar(n: Int) = n >= 100
  val nodes = Map(
    1 -> "a", 2 -> "b", 3 -> "c", 4 -> "d",
    10 -> "a", 20 -> "b", 30 -> "c",
    100 -> "", 200 -> "", 300 -> "")

  def comp(n1: Int, n2: Int) = {
    nodes(n1) == nodes(n2)
  }

  var printStates = false

  def printState[A,B](s: BisimulatorState[A,B]) = {
    def printContext(c: NodeContext[A,B]) = s"([${c.in.map(e ⇒ e.node) mkString ","}]→ ${c.node} →[${c.out.map(e ⇒ e.node) mkString ","}])"
    def printGraph(g: Graph[A,B]):String = g match {
      case (c: NodeContext[A,B]) &: (g: Graph[A,B]) => s"${printContext(c)} &: ${printGraph(g)}"
      case Empty => "Empty"
    }
    s"State(${printGraph(s.leftGraph)}, ${s.leftStack}, ${printGraph(s.rightGraph)}, ${s.rightStack}, ${s.fromSubst}, ${s.kind})"
  }

  def similaritySubst(g: TestGraph, gstart: Int, p: TestGraph, pstart: Int, msg: String = "Bisimulation failed") = {
    val b = new Bisimulator[Int, Unit](g, p, defaultContextCompare(comp, isVar))
    val res = b.genStates(List(gstart), List(pstart)).toSeq.map(s => {if (printStates) println(printState(s)); s }).last
    if (res.kind == SimilarityFailed)
      fail(msg)
    res.fromSubst
  }

  def bisimilationState(g: TestGraph, gstart: Int, p: TestGraph, pstart: Int) = {
    val b = new Bisimulator[Int, Unit](g, p, defaultContextCompare(comp, isVar))
    val res = b.genStates(List(gstart), List(pstart)).toSeq.last
    res
  }

  describe("Bisimulation") {

    it("admits equal singletons") {
      val g = node(1, Seq()) &+: Empty
      val p = node(10, Seq()) &+: Empty

      similaritySubst(g, 1, p, 10) should be(Map(10 -> 1))
    }

    it("compare nodes") {
      val g = node(1, Seq()) &+: Empty
      val p = node(20, Seq()) &+: Empty

      bisimilationState(g, 1, p, 20).isFailed should be(true)
    }

    val threeNodeGraph = node(3, Seq(1,2)) &+: node(2, Seq()) &+: node(1, Seq()) &+: Empty

    it("compare number of adjacent edges") {
      val p = node(30, Seq(10)) &+: node(10, Seq()) &+: Empty

      bisimilationState(threeNodeGraph, 3, p, 30).isFailed should be(true)
    }

    it("dont compare adjacent edges for leaves") {
      val leaf = node(300, Seq()) &+: Empty

      similaritySubst(threeNodeGraph, 3, leaf, 300) should be(Map(300 -> 3))
    }

    it("admits equal graphs up to renaming of nodes") {
      val g = node(2, Seq(1)) &+: node(1, Seq()) &+: Empty
      val p = node(20, Seq(10)) &+: node(10, Seq()) &+: Empty

      similaritySubst(g, 2, p, 20) should be(Map(10 -> 1, 20 -> 2))
    }

    /*        4
    *         |
    *    1   2   10  200(var)
    *     \ |     \  |
    *      3       30
    * */
    it("finds tree-like prefix") {
      val g = node(3, Seq(1,2)) &+: node(2, Seq(4)) &+: node(1, Seq())  &+: node(4, Seq()) &+: Empty
      val p = node(30, Seq(10, 200)) &+: node(200, Seq()) &+: node(10, Seq()) &+: Empty

      similaritySubst(g, 3, p, 30) should be(Map(10 -> 1, 200 -> 2, 30 -> 3))
    }

    /*     5
    *      |
    *      4       100(var)
    *     / \     /  \
    *    1   2   10  20
    *     \ |     \  |
    *      3       30
    * */
    it("finds rhombus-like prefix") {
      val g = node(3, Seq(1,2)) &+: node(2, Seq(4)) &+: node(1, Seq(4))  &+: node(4, Seq(5)) &+: node(5, Seq()) &+: Empty
      val p = node(30, Seq(10, 20)) &+: node(20, Seq(100)) &+: node(10, Seq(100)) &+: node(100, Seq()) &+: Empty

      printStates = true
      similaritySubst(g, 3, p, 30) should be(Map(10 -> 1, 20 -> 2, 30 -> 3, 100 -> 4))
    }

  }

}
