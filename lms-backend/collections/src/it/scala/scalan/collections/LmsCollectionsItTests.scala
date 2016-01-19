package scalan.collections

import scalan.compilation.lms.scalac.{LmsCompilerScala, LmsCompilerScalaConfig}
import scalan.compilation.lms.source2bin.SbtConfig
import scalan.compilation.lms.uni.LmsCompilerUni

class LmsCollectionsItTests extends CollectionsItTests {

  val progStaged = new LmsCompilerScala(new CollectionsProgExp) with SeqsScalaMethodMapping
  val progStagedU = new LmsCompilerUni(new CollectionsProgExp) with SeqsScalaMethodMapping

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
  test("makeMap") {
    val in = Array((1, 1.1), (2, 2.2), (1, 3.3), (1, 4.4), (2, 5.5))
    compareOutputWithSequential(_.makeMap, progStagedOnly)(in)
  }
  test("ifSpecialize") {
    compareOutputWithSequential(_.ifSpecialize, progStagedOnly)(Array(1, 2, 3))
  }
  test("collPairIf") {
    compareOutputWithSequential(_.collPairIf, progStagedOnly)((Array(1, 2, 3), 2))
  }
}
