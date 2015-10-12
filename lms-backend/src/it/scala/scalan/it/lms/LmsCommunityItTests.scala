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

  test ("convertPairCollectionSOA")  {
    val in = Array(Array((1,2.0),(2,3.0)), Array((3,4.0), (5,6.0)))
    compareOutputWithSequential(_.convertPairCollectionSOA, "convertPairCollectionSOA")(in)
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
  test("test23unionMultiMaps") {
    val in = (Array((1, 1.1), (2, 2.2), (1, 3.3), (1, 4.4), (2, 5.5)), Array((0, 0.0), (2, 2.0), (1, 4.0), (1, 6.0)))
    compareOutputWithSequential(_.unionMultiMaps, "unionMultiMaps", progStagedOnly)(in)
    //TODO: lack of maps support in LMS C++ backend
    //    compareOutputWithSequential(progStagedU, progSeq)(_.unionMultiMaps, "unionMultiMaps")(in)
  }
  test("test38ifSpecialize") {
    val in = Array(1,2,3)
    compareOutputWithSequential(_.ifSpecialize, "ifSpecialize", progStagedOnly)(in)
  }
}
