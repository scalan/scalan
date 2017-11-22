package scalan.util

import scalan.BaseNestedTests

class GraphUtilTests extends BaseNestedTests {
  import GraphUtil._

  describe("Collecting dependencies") {
    val graph = Array(
      List(1, 2), // 0
      List(3),    // 1
      List(4),    // 2
      List(5, 6), // 3
      List(6),    // 4
      List(6),    // 5
      List()      // 6
    )

    def neighbours(node: Int): List[Int] = graph(node)

    it("depthFirstSetFrom") {
      depthFirstSetFrom(Set(6))(neighbours) shouldBe(Set(6))
      depthFirstSetFrom(Set(5))(neighbours) shouldBe(Set(5,6))
      depthFirstSetFrom(Set(3))(neighbours) shouldBe(Set(3,5,6))
      depthFirstSetFrom(Set(2))(neighbours) shouldBe(Set(2,4,6))
      depthFirstSetFrom(Set(0))(neighbours) shouldBe(Set(0,1,2,3,4,5,6))
    }
  }
}
