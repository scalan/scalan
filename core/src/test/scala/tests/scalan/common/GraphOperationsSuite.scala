package tests.scalan.common

import org.scalatest.{Matchers, FunSpec}
import scalan.common.{InductiveGraphs, GraphOperations}

class GraphOperationsSuite extends FunSpec with Matchers with InductiveGraphTesting {

  val graphs = new GraphOperations[Int] {}
  import graphs._

  describe("Prefix search") {

    def isVar(n: Int) = n >= 100
    val nodes = Map(
      1 -> "a", 2 -> "b", 3 -> "c", 4 -> "d",
      10 -> "a", 20 -> "b", 30 -> "c",
      100 -> "", 200 -> "", 300 -> "")

    def comp(n1: Int, n2: Int) = {
      nodes(n1) == nodes(n2)
    }

    it("admits equal singletons") {
      val g = node(1, Seq()) &+: Empty
      val p = node(10, Seq()) &+: Empty

      SearchPrefix(g, 1, p, 10, comp) match {
        case FoundPrefix(subst) =>
          subst should be(Map(10 -> 1))
      }
    }

    it("compare nodes") {
      val g = node(1, Seq()) &+: Empty
      val p = node(20, Seq()) &+: Empty

      SearchPrefix(g, 1, p, 20, comp) match {
        case FoundPrefix(subst) =>
          fail("not a prefix")
        case _ =>
      }
    }

    it("compare number of adjacent edges") {
      val g = node(3, Seq(1,2)) &+: node(2, Seq()) &+: node(1, Seq()) &+: Empty
      val p = node(30, Seq(10)) &+: node(10, Seq()) &+: Empty

      SearchPrefix(g, 3, p, 30, comp) match {
        case FoundPrefix(subst) =>
          fail("not a prefix")
        case _ =>
      }
    }

    it("don't compare adjacent edges for leaves") {
      val g = node(3, Seq(1,2)) &+: node(2, Seq()) &+: node(1, Seq()) &+: Empty
      val leaf = node(30, Seq()) &+: Empty

      SearchPrefix(g, 3, leaf, 30, comp) match {
        case FoundPrefix(subst) =>
          subst should be(Map(30 -> 3))
      }
    }

    it("admits equal graphs up to renaming of nodes") {
      val g = node(2, Seq(1)) &+: node(1, Seq()) &+: Empty
      val p = node(20, Seq(10)) &+: node(10, Seq()) &+: Empty

      SearchPrefix(g, 2, p, 20, comp) match {
        case FoundPrefix(subst) =>
          subst should be(Map(10 -> 1, 20 -> 2))
      }
    }

    it("finds tree-like prefix") {
      val g = node(3, Seq(1,2)) &+: node(2, Seq(4)) &+: node(1, Seq())  &+: node(4, Seq()) &+: Empty
      val p = node(30, Seq(10, 20)) &+: node(20, Seq()) &+: node(10, Seq()) &+: Empty

      SearchPrefix(g, 3, p, 30, comp) match {
        case FoundPrefix(subst) =>
          subst should be(Map(10 -> 1, 20 -> 2, 30 -> 3))
      }
    }

//    it("finds rhombus-like prefix") {
//      fail("")
//      val g = node(3, Seq(1,2)) &+: node(2, Seq(4)) &+: node(1, Seq())  &+: node(4, Seq()) &+: Empty
//      val p = node(30, Seq(10, 20)) &+: node(20, Seq()) &+: node(10, Seq()) &+: Empty
//
//      SearchPrefix(g, 3, p, 30, comp) match {
//        case FoundPrefix(subst) =>
//          subst should be(Map(10 -> 1, 20 -> 2, 30 -> 3))
//      }
//    }

  }
}
