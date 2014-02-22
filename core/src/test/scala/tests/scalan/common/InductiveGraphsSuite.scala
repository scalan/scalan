/**
 * This implementation of inductive graphs is taken from https://github.com/fgeller/inductive-graphs
 * It follows “Inductive Graphs and Functional Graph Algorithms” (Erwig 2001) 
 * http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.28.9377
 * 
 * It is parameterized by node type which should implement structural equality pattern correctly.
 */
package test.scalan.common

import org.scalatest.{Matchers, FunSpec}
import scalan.common.InductiveGraphs
import tests.scalan.common.InductiveGraphTesting

class InductiveGraphsSuite extends FunSpec with Matchers with InductiveGraphTesting {

  val graphs = new InductiveGraphs[Int] {}
  import graphs._
  
  describe("NodeContext") {

    it("knows the successor nodes") {
      Context(Seq(), 1, 1, Seq(Edge(2, 2))).suc should be(Set(2))
      Context(Seq(), 1, 1, Seq()).suc should be(Set())
    }
  }

  val names = (0 to 13).map(num ⇒ num → num).toMap
  val bigGraph =
      node(0, Seq(1)) &+:
      node(1, Seq(2)) &+:
      node(2, Seq(10, 3)) &+:
      Context(Seq(Edge((), 5), Edge((), 4)), 3, names(3), Seq()) &+:
      Context(Seq(Edge((), 5)), 4, names(4), Seq()) &+:
      Context(Seq(Edge((), 8), Edge((), 6)), 5, names(5), Seq()) &+:
      Context(Seq(Edge((), 11)), 6, names(6), Seq()) &+:
      Context(Seq(Edge((), 8)), 7, names(7), Seq()) &+:
      Context(Seq(Edge((), 11)), 8, names(8), Seq()) &+:
      Context(Seq(Edge((), 13), Edge((), 10)), 9, names(9), Seq()) &+:
      Context(Seq(Edge((), 11)), 10, names(10), Seq()) &+:
      Context(Seq(Edge((), 13)), 11, names(11), Seq()) &+:
      Context(Seq(Edge((), 13)), 12, names(12), Seq()) &+:
      Context(Seq(), 13, names(13), Seq()) &+: Empty

  describe("Graphs") {

    it("can be empty") {
      Empty.isEmpty should be(true)
      (Context(Seq(), 3, 'c', Seq()) &+: Empty).isEmpty should be(false)
    }

    it("can map a function over its nodes") {
      def fun[A, B](ctx: NodeContext[A, B]) = Context(ctx.in, ctx.node, "☉", ctx.out)
      (Context(Seq(), 1, 'a', Seq()) &+: Empty).gmap(fun).toString should include("☉")
    }

    it("can reverse edge directions") {
      val g = (Context(Seq(Edge("left", 2)), 1, 'a', Seq()) &+: Context(Seq(), 2, 'b', Seq()) &+: Empty)
      g.toString should include("[(left,2)]→")
      g.grev.toString should include("→[(left,2)]")
    }

    it("can make a graph undirectional") {
      val g = (Context(Seq(Edge("left", 2)), 1, 'a', Seq()) &+: Context(Seq(), 2, 'b', Seq()) &+: Empty)
      g.undir.toString should include("[(left,2)]→")
      g.undir.toString should include("→[(left,2)]")
    }

    it("can use fold to sum up values") {
      val g = (Context(Seq(Edge("left", 2)), 1, 23, Seq()) &+:
        Context(Seq(), 2, 23, Seq()) &+:
        Empty)
      g.ufold(0) { (memo, ctx) ⇒ memo + ctx.value } should be(46)
    }

    it("can use an extractor to find a context") {
      val testGraph = (Context(Seq(Edge("left", 2)), 1, 23, Seq()) &+:
        Context(Seq(), 2, 46, Seq()) &+:
        Empty)
      val testNode = 2

      SearchNode(testGraph, testNode) match {
        case FoundNode(Context(in, 2, label, out), restGraph) ⇒
          out should be(Seq(Edge("left", 1)))
          label should be(46)
          restGraph should not be testGraph
        case _ ⇒ fail("should find the context for node 2")
      }
    }

    it("knows the degree of a node") {
      val testGraph = (Context(Seq(Edge("left", 2)), 1, 23, Seq()) &+:
        Context(Seq(), 2, 46, Seq()) &+:
        Empty)

      testGraph.degree(2) should be(Some(1))
      testGraph.degree(1) should be(Some(1))
      testGraph.degree(0) should be(None)
    }

    it("finds the successors of a node") {
      val testGraph = (Context(Seq(), 1, 23, Seq(Edge("left", 2))) &+:
        Context(Seq(), 2, 46, Seq()) &+:
        Empty)

      testGraph.gsuc(1) should be(Set(2))
      testGraph.gsuc(2) should be(Set())
    }

    it("can delete a node") {
      val testGraph = (Context(Seq(Edge("left", 1)), 2, 23, Seq()) &:
        Context(Seq(), 1, 46, Seq()) &:
        Empty)

      testGraph.delete(2) should be(Context(Seq(), 1, 46, Seq()) &: Empty)
      testGraph.delete(1) should be(Context(Seq(), 2, 23, Seq()) &: Empty)
      testGraph.delete(0) should be(testGraph)
    }

    it("knows its nodes") {
      Empty.nodes should be('empty)
      (Context(Seq(Edge("left", 2)), 1, 23, Seq()) &+:
        Context(Seq(), 2, 23, Seq()) &+:
        Empty).nodes should be(Set(2, 1))
    }

    it("can reconstruct the base graph from figure 1") {
      val g = Context(Seq(Edge("left", 2), Edge("up", 3)), 1, 'a', Seq(Edge("right", 2))) &+:
        Context(Seq(), 2, 'b', Seq(Edge("down", 3))) &+:
        Context(Seq(), 3, 'c', Seq()) &+: Empty

      g.toString should endWith("&: Empty")
      Seq(1, 2, 3) map (_.toString) foreach { node ⇒
        g.toString should include(node)
      }
    }

    it("can be visualized via dot/graphviz") {
      val g = Context(Seq(Edge("left", 2), Edge("up", 3)), 1, 'a', Seq(Edge("right", 2))) &+:
        Context(Seq(), 2, 'b', Seq(Edge("down", 3))) &+:
        Context(Seq(), 3, 'c', Seq()) &+: Empty

      Graph.asDot(g) should be("""digraph g {
"2" -> "1";
"3" -> "1";
"1" -> "2";
"2" -> "3";
}""")
    }

    it("knows its leaves") {
      val g =
        Context(Seq(Edge((), 4), Edge((), 2)), 1, 'a', Seq()) &+:
          Context(Seq(Edge((), 3)), 2, 'r', Seq()) &+:
          Context(Seq(Edge((), 5)), 3, 'd', Seq()) &+:
          Context(Seq(Edge((), 5)), 4, 's', Seq()) &+:
          Context(Seq(), 5, 'w', Seq()) &+: Empty

      g.leaves should be(Set(5))
    }

    it("knows its roots") {
      val g =
        Context(Seq(Edge((), 4), Edge((), 2)), 1, 'a', Seq()) &+:
          Context(Seq(Edge((), 3)), 2, 'r', Seq()) &+:
          Context(Seq(Edge((), 5)), 3, 'd', Seq()) &+:
          Context(Seq(Edge((), 5)), 4, 's', Seq()) &+:
          Context(Seq(), 5, 'w', Seq()) &+: Empty

      g.roots should be(Set(1))
    }

    it("the roots and their children are all nodes") {
      val g =
        Context(Seq(Edge((), 1)), 0, 'z', Seq()) &+:
          Context(Seq(Edge((), 4), Edge((), 2)), 1, 'a', Seq()) &+:
          Context(Seq(Edge((), 3)), 2, 'r', Seq()) &+:
          Context(Seq(Edge((), 5)), 3, 'd', Seq()) &+:
          Context(Seq(Edge((), 5)), 4, 's', Seq()) &+:
          Context(Seq(), 5, 'w', Seq()) &+: Empty

      g.roots ++ g.roots.foldLeft[Set[graphs.NodeType]](Set()) { (memo, root) ⇒
        g.children(root).map(_.node).toSet ++ memo
      }.toSet should be(g.nodes)
    }

    it("can identify children") {
      val g =
        Context(Seq(Edge((), 4), Edge((), 2)), 1, 'a', Seq()) &+:
          Context(Seq(Edge((), 3)), 2, 'r', Seq()) &+:
          Context(Seq(Edge((), 5)), 3, 'd', Seq()) &+:
          Context(Seq(Edge((), 5)), 4, 's', Seq()) &+:
          Context(Seq(), 5, 'w', Seq()) &+: Empty

      g.children(1).map(_.node) should be(List(4, 2, 3, 5))
    }

    it("its children do not include unneeded nodes") {
      val g =
        Context(Seq(Edge((), 1)), 0, 'z', Seq()) &+:
          Context(Seq(Edge((), 4), Edge((), 2)), 1, 'a', Seq()) &+:
          Context(Seq(Edge((), 3)), 2, 'r', Seq()) &+:
          Context(Seq(Edge((), 5)), 3, 'd', Seq()) &+:
          Context(Seq(Edge((), 5)), 4, 's', Seq()) &+:
          Context(Seq(), 5, 'w', Seq()) &+: Empty

      g.children(1).map(_.node) should be(List(4, 2, 3, 5))
    }

    it("more children tests") {
      val g = bigGraph
      println(Graph.asDot(g))
      g.children(13).map(_.node) should be(List())
      g.children(12).map(_.node) should be(List(13))
      g.children(7).map(_.node) should be(List(8, 11, 13))
      g.children(5).map(_.node) should be(List(8, 6, 11, 13))
      g.children(0).map(_.node).toSet should be(Set(1, 2, 3, 4, 5, 6, 8, 10, 11, 13))
    }

    it("finds a node context for a node") {
      val contextOne = Context(Seq(), 1, (), Seq())
      val contextTwo =  Context(Seq(), 2, (), Seq())
      val testGraph = contextTwo &+: contextOne &+: Empty

      testGraph.nodeContext(1) should be(Some(contextOne))
      testGraph.nodeContext(2) should be(Some(contextTwo))
      testGraph.nodeContext(3) should be(None)
    }
  }

  describe("Pair implementation") {

    it("maintains incoming edges") {
      val testGraph = Context(Seq(), 2, (), Seq(Edge("2 to 1", 1))) &+: Context(Seq(), 1, (), Seq()) &+: Empty

      SearchNode(testGraph, 1) match {
        case FoundNode(Context(in, 1, label, out), restGraph) ⇒
          in.contains(Edge("2 to 1", 2)) should be(true)
        case _ ⇒ fail("should find the context for node 1")
      }
    }

    it("maintains outgoing edges") {
      val testGraph = Context(Seq(Edge("1 to 2", 1)), 2, (), Seq()) &+: Context(Seq(), 1, (), Seq()) &+: Empty

      SearchNode(testGraph, 1) match {
        case FoundNode(Context(in, 1, label, out), restGraph) ⇒
          out.contains(Edge("1 to 2", 2)) should be(true)
        case _ ⇒ fail("should find the context for node 1")
      }
    }

  }

  describe("GraphOps") {

    it("dfs") {
      val dfsList = bigGraph.dfs(List(0))(c => c.in.map(_.node).toSet.toList)
      dfsList should be(List(0, 1, 2, 10, 11, 13, 3, 5, 8, 6, 4))
    }


  }
}
