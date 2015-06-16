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

  test("seqsSimpleMap") {
    pending
    val in = Seq(2, 3)
    compileSource(progStaged)(progStaged.seqsSimpleMap, "seqsSimpleMap", progStaged.defaultCompilerConfig)
    //compareOutputWithSequential(progStaged)(progSeq.seqsSimpleMap, progStaged.seqsSimpleMap, "seqsSimpleMap", in)
  }
}
