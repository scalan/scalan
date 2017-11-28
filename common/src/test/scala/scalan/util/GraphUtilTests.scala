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

  describe("StronglyConnectedComponents") {
    val graph: String => List[String] = {
      case "A" => List("B") case "B" => List("C") case "C" => List("A","D") case "D" => Nil
    }
    it("accessAll") {
      val result = stronglyConnectedComponents(List("A"))(graph).map(_.toSet)
      val expected = Seq(Set("D"), Set("A", "B", "C"))

      result.shouldEqual(expected)
    }

    it("accessOne") {
      val result = stronglyConnectedComponents(List("D"))(graph).map(_.toSet)
      val expected = Seq(Set("D"))

      result.shouldEqual(expected)
    }

    it("accessAllByManyStarts") {
      val result = stronglyConnectedComponents(List("A","B","C"))(graph).map(_.toSet)
      val expected = Seq(Set("D"), Set("A", "B", "C"))

      result.shouldEqual(expected)
    }

    it("manyComponents") {
      val result = stronglyConnectedComponents(List("D")){
        case "D" => List("A")
        case "A" => List("B", "C")
        case "B" => List("A", "C")
        case "C" => List("A", "B", "E")
        case "E" => List("F")
        case "F" => List("E")
      }.map(_.toSet)
      val expected = Seq(Set("E", "F"), Set("A", "B", "C"), Set("D"))

      result.shouldEqual(expected)
    }

    val dag: String => List[String] = {
      case "A" => List("B", "C")
      case "B" => List("C")
      case "C" => Nil
      case "D" => List("E", "F")
      case "E" => List("G", "H")
      case "F" => Nil
      case "G" => Nil
      case "H" => Nil
    }
    it("topologicallySortDag") {
        val result = stronglyConnectedComponents(List("A"))(dag)
        result.flatten.shouldEqual(Seq("C", "B", "A"))
    }

    it("topologicallySortTree") {
      val result = stronglyConnectedComponents(List("D"))(dag)
      result.flatten.shouldEqual(Seq("G", "H", "E", "F", "D"))
    }
  }
}
