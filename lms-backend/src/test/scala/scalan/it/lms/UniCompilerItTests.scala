package scalan.it.lms

import scalan.linalgebra.{MatricesDslSeq, LinearAlgebraExamples}
import scalan.{ScalanCommunityDslSeq, CommunityMethodMappingDSL, ScalanCommunityDslExp}
import scalan.compilation.lms.uni.LmsCompilerUni
import scalan.compilation.lms.{CommunityLmsBackend, CommunityBridge}
import scalan.compilation.lms.scalac.CommunityLmsCompilerScala
import scalan.it.smoke.CommunitySmokeItTests

/**
 * Created by adel on 5/15/15.
 */

class UniCompilerItTests  extends CommunitySmokeItTests {

  trait ProgUniTest extends ProgCommunity with LinearAlgebraExamples with CommunityMethodMappingDSL {
    lazy val test00_nop = fun { p: Rep[Double] =>
      p
    }
    lazy val test01_oneOp = fun { p: Rep[Double] =>
      p+2.0
    }
  }

  class ProgCommunityExp extends ProgUniTest with ScalanCommunityDslExp with LmsCompilerUni with CommunityBridge {
    val lms = new CommunityLmsBackend
  }

  val progStaged = new ProgCommunityExp

  class ProgSeq extends ProgUniTest with MatricesDslSeq with ScalanCommunityDslSeq

  val progSeqU = new ProgSeq


   /*
  test("expBaseArraysUni") {
    val in = Array(Array(2, 3), Array(4, 5))
    compareOutputWithSequential(progStaged)(progSeq.expBaseArrays, progStaged.expBaseArrays, "expBaseArraysUni", in)
  }

  test("joinMapsUni") {
    val in = (Array((2, 2.0), (3, 3.0)), Array((1, 1.5), (3, 3.5), (5, 5.5)))
    compareOutputWithSequential(progStaged)(progSeq.joinMaps, progStaged.joinMaps, "joinMapsUni", in)
  }
     */

  ignore("seqsSimpleMapUni") {
    pending
    val in = Seq(2, 3)
    compileSource(progStaged)(progStaged.seqsSimpleMap, "seqsSimpleMapUni", progStaged.defaultCompilerConfig)
    //compareOutputWithSequential(progStaged)(progSeq.seqsSimpleMap, progStaged.seqsSimpleMap, "seqsSimpleMap", in)
  }

  test("ddmvm") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = Array(2.0, 3.0)
    val in = Tuple2(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged)(progSeqU.ddmvm, progStaged.ddmvm, "ddmvm", in)
  }

  test("test00_nop") {
    val in = 5.0
    compareOutputWithSequential(progStaged)(progSeqU.test00_nop, progStaged.test00_nop, "test00_nop", in)
  }

  test("test01_oneOp") {
    val in = 5.0
    compareOutputWithSequential(progStaged)(progSeqU.test01_oneOp, progStaged.test01_oneOp, "test01_oneOp", in)
  }


}

