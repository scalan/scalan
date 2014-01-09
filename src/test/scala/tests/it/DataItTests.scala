package tests.it

import scalan.{ScalanSeqImplementation, ScalanDsl}

/**
 * Checks that data serialization/deserialization works correctly
 */
class DataItTests extends ItTests {
  val prefix: String = ""

  class ProgSeq extends ScalanSeqImplementation {
    lazy val intRep: Rep[Int] = 1
    lazy val intPair: Rep[(Int, Int)] = (1, 2)
    lazy val nestedIntPair: Rep[((Int, Int), (Int, Int))] = ((1, 2), (3, 4))

    val intPairElem = element[(Int,Int)]
    val nestedIntPairElem = pairElement(intPairElem, intPairElem)
  }

  val progSeq = new ProgSeq()
  import progSeq._

  test("writePrimitives") {
    serialize(progSeq)(intRep) should be ("1")
    serialize(progSeq)(intPair) should be ("(1, 2)")
    serialize(progSeq)(nestedIntPair) should be ("((1, 2), (3, 4))")
  }

  test("readPrimitives") {
    deserialize(progSeq)("1")(intElement) should be (1)
    deserialize(progSeq)("1.0")(floatElement) should be (1.0)
    deserialize(progSeq)("(1, 2)")(intPairElem) should be ((1,2))
    deserialize(progSeq)("((1, 2), (3, 4))")(nestedIntPairElem) should be (((1,2),(3,4)))
  }

}
