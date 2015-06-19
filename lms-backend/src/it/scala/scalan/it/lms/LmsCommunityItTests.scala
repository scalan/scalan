package scalan.it.lms

import scalan.{CommunityMethodMappingDSL, ScalanCommunityDslExp}
import scalan.compilation.lms._
import scalan.compilation.lms.scalac.CommunityLmsCompilerScala
import scalan.compilation.lms.uni.LmsCompilerUni
import scalan.it.smoke.CommunitySmokeItTests

class LmsCommunityItTests extends CommunitySmokeItTests {

  class ProgCommunityExp extends ProgCommunity with ScalanCommunityDslExp with CommunityBridge with CommunityMethodMappingDSL {
    val lms = new CommunityLmsBackend
  }

  val progStaged = new ProgCommunityExp with CommunityLmsCompilerScala
  val progStagedU = new ProgCommunityExp with LmsCompilerUni

  val cC = progStagedU.defaultCompilerConfig.copy(scalaVersion = Some("2.11.2"))

  test("expBaseArrays") {
    val in = Array(Array(2, 3), Array(4, 5))
    compareOutputWithSequential(progStaged)(progSeq.expBaseArrays, progStaged.expBaseArrays, "expBaseArrays", in)
    compareOutputWithSequentialConfig(progStagedU)(progSeq.expBaseArrays, progStagedU.expBaseArrays, "expBaseArrays", in, cC)
  }

  test("seqsEmpty") {
    val in = -11
    compareOutputWithSequential(progStaged)(progSeq.seqsEmpty, progStaged.seqsEmpty, "seqsEmpty", in)
  }

  test("seqsSingle") {
    val in = -11
    compareOutputWithSequential(progStaged)(progSeq.seqsSingle, progStaged.seqsSingle, "seqsSingle", in)
  }

  test("seqsSimpleMap") {
    val in = Seq(2, 3)
    compareOutputWithSequential(progStaged)(progSeq.seqsSimpleMap, progStaged.seqsSimpleMap, "seqsSimpleMap", in)
  }

  test("seqsArrayMap") {
    val in = Array(Array(2, 3))
    compareOutputWithSequential(progStaged)(progSeq.seqsArrayMap, progStaged.seqsArrayMap, "seqsArrayMap", in)
  }

  test("seqsFromArray") {
    val in = Array(2, 3)
    compareOutputWithSequential(progStaged)(progSeq.seqsFromArray, progStaged.seqsFromArray, "seqsFromArray", in)
  }
}
