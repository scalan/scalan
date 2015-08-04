package scalan.it.lms

import scalan.{JNIExtractorOpsExp, CommunityMethodMappingDSL, ScalanCommunityDslExp, ScalanCommunityDslSeq}
import scalan.compilation.lms._
import scalan.compilation.lms.scalac.CommunityLmsCompilerScala
import scalan.compilation.lms.uni.{LmsBackendUni, LmsCompilerUni}
import scalan.it.smoke.CommunitySmokeItTests

class LmsCommunityItTests extends CommunitySmokeItTests {

  class ProgCommunityExp extends ProgCommunity with ScalanCommunityDslExp with JNIExtractorOpsExp

  val progStaged = new CommunityLmsCompilerScala with CommunityBridge with CommunityMethodMappingDSL {
    val scalan = new ProgCommunityExp
  }
  val progStagedU = new LmsCompilerUni with CommunityBridge with CommunityMethodMappingDSL {
    val scalan = new ProgCommunityExp
  }
  val progSeq = new ProgCommunity with ScalanCommunityDslSeq

  val cC = progStagedU.defaultCompilerConfig.copy(scalaVersion = Some("2.11.2"))

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
