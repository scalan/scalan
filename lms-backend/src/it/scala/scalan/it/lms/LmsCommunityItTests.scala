package scalan.it.lms

import scalan.{JNIExtractorOpsExp, CommunityMethodMappingDSL, ScalanCommunityDslExp, ScalanCommunityDslSeq}
import scalan.compilation.lms._
import scalan.compilation.lms.scalac.CommunityLmsCompilerScala
import scalan.compilation.lms.uni.{LmsBackendUni, LmsCompilerUni}
import scalan.it.smoke.CommunitySmokeItTests

class LmsCommunityItTests extends CommunitySmokeItTests {

  val progStaged = new CommunityLmsCompilerScala(new ProgCommunityExp) with CommunityBridge with CommunityMethodMappingDSL
  val progStagedU = new LmsCompilerUni(new ProgCommunityExp) with CommunityBridge with CommunityMethodMappingDSL
  val progSeq = new ProgCommunity with ScalanCommunityDslSeq

  val cC = progStagedU.defaultCompilerConfig.copy(scalaVersion = Some("2.11.2"))

  test("listRangeFrom0") {
    val in = 3
    compareOutputWithSequential(progStaged)(progSeq.listRangeFrom0, progStaged.scalan.listRangeFrom0, "listRangeFrom0", in)
  }

  test("expBaseArrays") {
    val in = Array(Array(2, 3), Array(4, 5))
    compareOutputWithSequential(progStaged)(progSeq.expBaseArrays, progStaged.scalan.expBaseArrays, "expBaseArrays", in)
    compareOutputWithSequentialConfig(progStagedU)(progSeq.expBaseArrays, progStagedU.scalan.expBaseArrays, "expBaseArrays", in, cC)
  }

  test("seqsEmpty") {
    val in = -11
    compareOutputWithSequential(progStaged)(progSeq.seqsEmpty, progStaged.scalan.seqsEmpty, "seqsEmpty", in)
  }

  test("seqsSingle") {
    val in = -11
    compareOutputWithSequential(progStaged)(progSeq.seqsSingle, progStaged.scalan.seqsSingle, "seqsSingle", in)
  }

  test("seqsSimpleMap") {
    val in = Seq(2, 3)
    compareOutputWithSequential(progStaged)(progSeq.seqsSimpleMap, progStaged.scalan.seqsSimpleMap, "seqsSimpleMap", in)
  }

  test("seqsArrayMap") {
    val in = Array(Array(2, 3))
    compareOutputWithSequential(progStaged)(progSeq.seqsArrayMap, progStaged.scalan.seqsArrayMap, "seqsArrayMap", in)
  }

  test("seqsFromArray") {
    val in = Array(2, 3)
    compareOutputWithSequential(progStaged)(progSeq.seqsFromArray, progStaged.scalan.seqsFromArray, "seqsFromArray", in)
  }
}
