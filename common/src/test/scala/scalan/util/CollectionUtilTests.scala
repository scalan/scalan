package scalan.util

import scalan.BaseTests
import scala.collection.{Seq, mutable}

class CollectionUtilTests extends BaseTests {
  import scalan.util.CollectionUtil._

  def join(l: Map[Int,Int], r: Map[Int,Int]) =
    outerJoin(l, r)((_,l) => l, (_,r) => r, (k,l,r) => l + r)
  def joinSeqs(l: Seq[Int], r: Seq[Int]) =
    outerJoinSeqs(l, r)(l => l, r => r)((_,l) => l, (_,r) => r, (k,l,r) => l + r).map(_._2)
  def joinPairs(l: Seq[(String,Int)], r: Seq[(String,Int)]) =
    outerJoinSeqs(l, r)(l => l._1, r => r._1)((_,l) => l._2, (_,r) => r._2, (k,l,r) => l._2 + r._2)

  test("outerJoin maps") {
    val left = Map(1 -> 1, 2 -> 2, 3 -> 3)
    val right = Map(2 -> 2, 3 -> 3, 4 -> 4)

    assertResult(Map(1 -> 1, 2 -> 4, 3 -> 6, 4 -> 4))(join(left,right))
    assertResult(Map(1 -> 1, 2 -> 2, 3 -> 3))(join(left,Map()))
    assertResult(Map(2 -> 2, 3 -> 3, 4 -> 4))(join(Map(), right))
    assertResult(Map(2 -> 4, 3 -> 6, 4 -> 8))(join(right, right))
  }

  test("outerJoinSeqs") {
    val left = Seq(1, 2, 3)
    val right = Seq(2, 3, 4)

    assertResult(Seq(1, 4, 6, 4))(joinSeqs(left, right))
    assertResult(Seq(1, 2, 3))(joinSeqs(left,Seq()))
    assertResult(Seq(2, 3, 4))(joinSeqs(Seq(), right))
    assertResult(Seq(4, 6, 8))(joinSeqs(right, right))

    val inner = Seq("a" -> 1, "b" -> 2, "c" -> 3)
    val outer = Seq("b" -> 2, "c" -> 3, "d" -> 4)

    assertResult(Seq("a" -> 1, "b" -> 4, "c" -> 6, "d" -> 4))(joinPairs(inner, outer))
    assertResult(Seq("a" -> 1, "b" -> 2, "c" -> 3))(joinPairs(inner,Seq()))
    assertResult(Seq("b" -> 2, "c" -> 3, "d" -> 4))(joinPairs(Seq(), outer))
    assertResult(Seq("b" -> 4, "c" -> 6, "d" -> 8))(joinPairs(outer, outer))
  }

  test("mapFirst") {
    val xs = List(1, 2, 3)
    xs.mapFirst(x => if (x > 2) Some(s"x = $x") else None) should be(Some("x = 3"))
  }

  test("distinctBy") {
    val items = Array((1, "a"), (2, "b"), (1, "c")).toIterable
    val res = items.distinctBy(_._1)
    assertResult(Array((1, "a"), (2, "b")))(res)
  }

  test("zipWithExpandedBy") {
    assertResult(Array((2, 0), (2, 1)))(2.zipWithExpandedBy(x => List.range(0,x)))
    assertResult(Array((3, 0), (3, 1), (3, 2)))(3.zipWithExpandedBy(x => List.range(0,x)))
  }

  def treeStep(tree: Array[List[Int]]): Int => List[Int] = i => tree(i)

  test("traverseDepthFirst") {
    {
      val tree = Array(
        List(1, 2), // 0
        List(),     // 1
        List(3),    // 2
        List())     // 3
      assertResult(List(0, 1, 2, 3))(0.traverseDepthFirst(treeStep(tree)))
    }
    {
      /*
       0
         1
           3
             5
             6
         2
           4
      */
      val tree = Array(
        List(1, 2),  // 0
        List(3),     // 1
        List(4),     // 2
        List(5,6),   // 3
        List(),      // 4
        List(),      // 5
        List()       // 6
      )
      assertResult(List(0, 1, 3, 5, 6, 2, 4))(0.traverseDepthFirst(treeStep(tree)))
    }
  }
}
