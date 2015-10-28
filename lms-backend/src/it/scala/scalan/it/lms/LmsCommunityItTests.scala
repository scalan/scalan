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

  val cC = LmsCompilerScalaConfig().withSbtConfig(SbtConfig(scalaVersion = "2.11.2"))

  val defaultCompilers = compilers(progStaged, cwc(progStagedU)(cC))
  val progStagedOnly = compilers(progStaged)

  test("seqsEmpty") {
    compareOutputWithSequential(_.seqsEmpty, progStagedOnly)(-11)
  }

  test("seqsSingle") {
    compareOutputWithSequential(_.seqsSingle, progStagedOnly)(-11)
  }

  test("seqsSimpleMap") {
    compareOutputWithSequential(_.seqsSimpleMap, progStagedOnly)(Seq(2, 3))
  }

  test("seqsArrayMap") {
    compareOutputWithSequential(_.seqsArrayMap, progStagedOnly)(Array(Array(2, 3)))
  }

  test("seqsFromArray") {
    compareOutputWithSequential(_.seqsFromArray, progStagedOnly)(Array(2, 3))
  }
  test("unionMultiMaps") {
    val in = (Array((1, 1.1), (2, 2.2), (1, 3.3), (1, 4.4), (2, 5.5)), Array((0, 0.0), (2, 2.0), (1, 4.0), (1, 6.0)))
    compareOutputWithSequential(_.unionMultiMaps, progStagedOnly)(in)
  }
  test("appendMultiMap") {
    val in = Array((1, 1.1), (2, 2.2), (1, 3.3), (1, 4.4), (2, 5.5))
    compareOutputWithSequential(_.appendMultiMap, progStagedOnly)(in)
  }
  test("makeMap") {
    val in = Array((1, 1.1), (2, 2.2), (1, 3.3), (1, 4.4), (2, 5.5))
    compareOutputWithSequential(_.makeMap, progStagedOnly)(in)
  }
  test("ifSpecialize") {
    compareOutputWithSequential(_.ifSpecialize, progStagedOnly)(Array(1, 2, 3))
  }
}
