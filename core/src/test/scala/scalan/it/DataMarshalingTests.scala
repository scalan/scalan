//package scalan.it
//
//import scalan.{ScalanDslStd, ScalanDsl}
//
///**
// * Checks that data serialization/deserialization works correctly
// */
//class DataMarshalingTests extends ItTests {
//  class ProgSeq extends ScalanDslStd {
//    lazy val intRep: Rep[Int] = 1
//    lazy val intPair: Rep[(Int, Int)] = (1, 2)
//    lazy val nestedIntPair: Rep[((Int, Int), (Int, Int))] = ((1, 2), (3, 4))
//
//    val intPairElem = element[(Int,Int)]
//    val nestedIntPairElem = pairElement(intPairElem, intPairElem)
//  }
//
//  val progSeq = new ProgSeq()
//  import progSeq._
//
//  test("writePrimitives") {
//    serialize(progSeq)(intRep) should be ("1")
//    serialize(progSeq)(intPair) should be ("(1, 2)")
//    serialize(progSeq)(nestedIntPair) should be ("((1, 2), (3, 4))")
//  }
//
//  test("readPrimitives") {
//    deserialize(progSeq)("1")(IntElement) should be (1)
//    deserialize(progSeq)("1.0")(FloatElement) should be (1.0)
//    deserialize(progSeq)("(1, 2)")(intPairElem) should be ((1,2))
//    deserialize(progSeq)("((1, 2), (3, 4))")(nestedIntPairElem) should be (((1,2),(3,4)))
//  }
//
//}
