package scalan

import scalan.util.GraphUtil

class StronglyConnectedComponentsTests extends BaseTests {
  test("test1") {
    val result = GraphUtil.stronglyConnectedComponents(List("A")){ 
      case "A" => List("B") case "B" => List("C") case "C" => List("A","D") case "D" => Nil
    }.map(_.toSet)
    val expected = Seq(Set("D"), Set("A", "B", "C"))
    
    result.shouldEqual(expected)
  }

  test("test2") {
    val result = GraphUtil.stronglyConnectedComponents(List("A","B","C")){ 
      case "A" => List("B") case "B" => List("C") case "C" => List("A","D") case "D" => Nil
    }.map(_.toSet)
    val expected = Seq(Set("D"), Set("A", "B", "C"))

    result.shouldEqual(expected)
  }

  test("test3") {
    val result = GraphUtil.stronglyConnectedComponents(List("D")){
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
}