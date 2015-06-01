package scalan.it.lms

import scalan.graphs.{GraphsDslExp, GraphsDslSeq}
import scalan.linalgebra.{MatricesDslSeq, LinearAlgebraExamples}
import scalan.{ScalanCtxExp, ScalanCommunityDslSeq, CommunityMethodMappingDSL, ScalanCommunityDslExp}
import scalan.compilation.lms.uni.LmsCompilerUni
import scalan.compilation.lms.{CommunityLmsBackend, CommunityBridge}

/**
 * Created by adel on 5/15/15.
 */

class UniCompilerItTests  extends LmsMsfItTests {

  //trait ProgUniTest extends ProgCommunity with MsfFuncs with LinearAlgebraExamples with CommunityMethodMappingDSL {
  trait ProgUniTest extends MsfFuncs with LinearAlgebraExamples with CommunityMethodMappingDSL {
    lazy val test00_nop = fun { p: Rep[Double] =>
      p
    }
    lazy val test01_oneOp = fun { p: Rep[Double] =>
      p+2.0
    }

    lazy val test02_mapArray = fun { p: Rep[Array[Double]] =>  //Rep[(Array[Array[Double]], Array[Double])]
      val vector = DenseVector(Collection(p))
      val res = vector.mapBy( fun{ r => r + 2.0 } )
      res.items.arr
    }

    /*lazy val multifunc = fun { p: Rep[Array[Double]] =>
      p+2.0
    }*/

  }

                 //ProgExp extends CommunityLmsCompilerScala with CommunityBridge
  class ProgCommunityExp extends ProgUniTest with GraphsDslExp with ScalanCtxExp with ScalanCommunityDslExp with LmsCompilerUni with CommunityBridge

  val progStaged = new ProgCommunityExp

  class ProgSeq extends ProgUniTest with GraphsDslSeq with MatricesDslSeq with ScalanCommunityDslSeq

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

  /*ignore("seqsSimpleMapUni") {
    pending
    val in = Seq(2, 3)
    compileSource(progStaged)(progStaged.seqsSimpleMap, "seqsSimpleMapUni", progStaged.defaultCompilerConfig)
    //compareOutputWithSequential(progStaged)(progSeq.seqsSimpleMap, progStaged.seqsSimpleMap, "seqsSimpleMap", in)
  }*/

  test("ddmvm") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = Array(2.0, 3.0)
    val in = Tuple2(inM, inV)
    compareOutputWithSequential(progStaged)(progSeqU.ddmvm, progStaged.ddmvm, "ddmvm00", in)
  }

  test("msfFunAdjBase") {
    val links = graph.flatMap( i=> i)
    val edgeVals = graphValues.flatMap(i => i)
    val lens = graph.map(i => i.length)
    val offs = Array(0,2,5,9,12,14,18,21,24,28,30,32) //(Array(0) :+ lens.scan.slice(lens.length-1)
    val input = (links, (edgeVals, (offs, lens)))

    compareOutputWithSequential(progStaged)(progSeqU.msfFunAdjBase, progStaged.msfFunAdjBase, "msfFunAdjBase", input)
  }

  test("test00_nop") {
    val in = 5.0
    compareOutputWithSequential(progStaged)(progSeqU.test00_nop, progStaged.test00_nop, "test00_nop", in)
  }

  test("test01_oneOp") {
    val in = 5.0
    compareOutputWithSequential(progStaged)(progSeqU.test01_oneOp, progStaged.test01_oneOp, "test01_oneOp", in)
  }

  test("test02_mapArray") {
    val in = Array(2.0, 3.0)
    compareOutputWithSequential(progStaged)(progSeqU.test02_mapArray, progStaged.test02_mapArray, "test02_mapArray", in)
  }

}

