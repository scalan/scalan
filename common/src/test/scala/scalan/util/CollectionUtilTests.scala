package scalan.util

import scalan.BaseTests
import scalan.util.CollectionUtil._

class CollectionUtilTests extends BaseTests {
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
}
