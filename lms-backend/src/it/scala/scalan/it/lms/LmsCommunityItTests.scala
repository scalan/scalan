package scalan.it.lms

import scalan.compilation.lms.source2bin.SbtConfig
import scalan.{JNIExtractorOpsExp, CommunityMethodMappingDSL, ScalanCommunityDslExp, ScalanCommunityDslSeq}
import scalan.compilation.lms._
import scalan.compilation.lms.scalac.{LmsCompilerScalaConfig, CommunityLmsCompilerScala}
import scalan.compilation.lms.uni.{LmsBackendUni, LmsCompilerUni}
import scalan.it.smoke.CommunitySmokeItTests

class LmsCommunityItTests extends CommunitySmokeItTests {

  val progStaged = new CommunityLmsCompilerScala(new ProgCommunityExp) with CommunityBridge with CommunityMethodMappingDSL
  val progStagedU = new LmsCompilerUni(new ProgCommunityExp) with CommunityBridge with CommunityMethodMappingDSL
  val progSeq = new ProgCommunity with ScalanCommunityDslSeq

  val cC = LmsCompilerScalaConfig().withSbtConfig(SbtConfig(scalaVersion = "2.11.2"))

  test("listRangeFrom0") {
    val in = 3
    compareOutputWithSequential(progStaged, progSeq)(_.listRangeFrom0, "listRangeFrom0", in)
  }

  test("expBaseArrays") {
    val in = Array(Array(2, 3), Array(4, 5))
    compareOutputWithSequential(progStaged, progSeq)(_.expBaseArrays, "expBaseArrays", in)
    compareOutputWithSequential(progStagedU, progSeq)(_.expBaseArrays, "expBaseArrays", in, cC)
  }

  test("seqsEmpty") {
    val in = -11
    compareOutputWithSequential(progStaged, progSeq)(_.seqsEmpty, "seqsEmpty", in)
  }

  test("seqsSingle") {
    val in = -11
    compareOutputWithSequential(progStaged, progSeq)(_.seqsSingle, "seqsSingle", in)
  }

  test("seqsSimpleMap") {
    val in = Seq(2, 3)
    compareOutputWithSequential(progStaged, progSeq)(_.seqsSimpleMap, "seqsSimpleMap", in)
  }

  test("seqsArrayMap") {
    val in = Array(Array(2, 3))
    compareOutputWithSequential(progStaged, progSeq)(_.seqsArrayMap, "seqsArrayMap", in)
  }

  test("seqsFromArray") {
    val in = Array(2, 3)
    compareOutputWithSequential(progStaged, progSeq)(_.seqsFromArray, "seqsFromArray", in)
  }
}
