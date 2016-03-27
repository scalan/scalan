package scalan.util

import scalan.BaseTests
import scalan.util.CollectionUtil._

class CollectionUtilTests extends BaseTests {
  def join(l: Map[Int,Int], r: Map[Int,Int]) = outerJoin(l, r)((_,l) => l, (_,r) => r, (k,l,r) => l + r)

  test("outerJoin maps") {
    val left = Map(1 -> 1, 2 -> 2, 3 -> 3)
    val right = Map(2 -> 2, 3 -> 3, 4 -> 4)

    assertResult(Map(1 -> 1, 2 -> 4, 3 -> 6, 4 -> 4))(join(left,right))
    assertResult(Map(1 -> 1, 2 -> 2, 3 -> 3))(join(left,Map()))
    assertResult(Map(2 -> 2, 3 -> 3, 4 -> 4))(join(Map(), right))
    assertResult(Map(2 -> 4, 3 -> 6, 4 -> 8))(join(right, right))
  }
}
