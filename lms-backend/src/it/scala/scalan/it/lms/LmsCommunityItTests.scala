package scalan.it.lms

import scalan.CommunityMethodMappingDSL
import scalan.compilation.lms._
import scalan.compilation.lms.scalac.{CommunityLmsCompilerScala, LmsCompilerScalaConfig}
import scalan.compilation.lms.source2bin.SbtConfig
import scalan.compilation.lms.uni.LmsCompilerUni
import scalan.it.smoke.CommunitySmokeItTests

class LmsCommunityItTests extends CommunitySmokeItTests {

  val progStaged = new CommunityLmsCompilerScala(new ProgCommunityExp) with CommunityBridge with CommunityMethodMappingDSL
  val progStagedU = new LmsCompilerUni(new ProgCommunityExp) with CommunityBridge with CommunityMethodMappingDSL
  val progSeq = new ProgCommunitySeq

  val cC = LmsCompilerScalaConfig().withSbtConfig(SbtConfig(scalaVersion = "2.11.2"))

  val defaultCompilers = compilers(progStaged, cwc(progStagedU)(cC))
  val progStagedOnly = compilers(progStaged)

  test("listRangeFrom0") {
    val in = 3
    compareOutputWithSequential(_.listRangeFrom0, "listRangeFrom0", progStagedOnly)(in)
  }

  test("expBaseArrays") {
    val in = Array(Array(2, 3), Array(4, 5))
    compareOutputWithSequential(_.expBaseArrays, "expBaseArrays")(in)
  }

  test("seqsEmpty") {
    val in = -11
    compareOutputWithSequential(_.seqsEmpty, "seqsEmpty", progStagedOnly)(in)
  }

  test("seqsSingle") {
    val in = -11
    compareOutputWithSequential(_.seqsSingle, "seqsSingle", progStagedOnly)(in)
  }

  test("seqsSimpleMap") {
    val in = Seq(2, 3)
    compareOutputWithSequential(_.seqsSimpleMap, "seqsSimpleMap", progStagedOnly)(in)
  }

  test("seqsArrayMap") {
    val in = Array(Array(2, 3))
    compareOutputWithSequential(_.seqsArrayMap, "seqsArrayMap", progStagedOnly)(in)
  }

  test("seqsFromArray") {
    val in = Array(2, 3)
    compareOutputWithSequential(_.seqsFromArray, "seqsFromArray", progStagedOnly)(in)
  }
}
