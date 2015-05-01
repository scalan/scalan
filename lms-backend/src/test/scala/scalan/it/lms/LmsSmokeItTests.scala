package scalan
package it.lms

import scalan.collections.SimpleMapTests
import scalan.compilation.lms._
import scalan.compilation.lms.scalac.CommunityLmsCompilerScala
import scalan.it.smoke.CommunitySmokeItTests

class LmsSmokeItTests extends CommunitySmokeItTests with SimpleMapTests {
  class ProgExp extends ProgCommunity with SimpleMapProg with ScalanCommunityDslExp with CommunityLmsCompilerScala with CoreBridge {
    val lms = new CommunityLmsBackend
  }

  override val progSeq = new ProgCommunitySeq with SimpleMapProg
  override val progStaged = new ProgExp

  test ("convertPairCollectionSOA")  {
    val in = Array(Array((1,2.0),(2,3.0)), Array((3,4.0), (5,6.0)))
    compareOutputWithSequential(progStaged)(progSeq.convertPairCollectionSOA, progStaged.convertPairCollectionSOA, "convertPairCollectionSOA", in)
  }

  test("mapPutContains") {
    val in = (314,3.14)
    compareOutputWithSequential(progStaged)(progSeq.mapPutContains, progStaged.mapPutContains, "mapPutContains", in)
  }
  test("mapAsSet") {
    val in = 314
    compareOutputWithSequential(progStaged)(progSeq.mapAsSet, progStaged.mapAsSet, "mapAsSet", in)
  }
  test("simpleArith") {
    val in = 2
    compareOutputWithSequential(progStaged)(progSeq.simpleArith, progStaged.simpleArith, "simpleArith", in)
  }
  test("simpleArrGet") {
    val in = (Array(2,3), 1)
    compareOutputWithSequential(progStaged)(progSeq.simpleArrGet, progStaged.simpleArrGet, "simpleArrGet", in)
  }
  test("simpleMap") {
    val in = Array(2,3)
    compareOutputWithSequential(progStaged)(progSeq.simpleMap, progStaged.simpleMap, "simpleMap", in)
  }
  test("simpleMapNested") {
    val in = (Array(Array(2.0,3.0), Array(3.0,4.0)), 1)
    compareOutputWithSequential(progStaged)(progSeq.simpleMapNested, progStaged.simpleMapNested, "simpleMapNested", in)
  }
  test("simpleZip") {
    val in = Array(2,3)
    compareOutputWithSequential(progStaged)(progSeq.simpleZip, progStaged.simpleZip, "simpleZip", in)
  }
  test("simpleZipWith") {
    val in = Array(2,3)
    compareOutputWithSequential(progStaged)(progSeq.simpleZipWith, progStaged.simpleZipWith, "simpleZipWith", in)
  }
  test("simpleReduce") {
    val in = Array(2,3)
    compareOutputWithSequential(progStaged)(progSeq.simpleReduce, progStaged.simpleReduce, "simpleArith", in)
  }
  test("mvMul") {
    val in = (Array(Array(2,3), Array(4,5)), Array(6,7))
    compareOutputWithSequential(progStaged)(progSeq.mvMul, progStaged.mvMul, "mvMul", in)
  }
  test("simpleIf") {
    val in = (Array(2.0,3.0), 4.0)
    compareOutputWithSequential(progStaged)(progSeq.simpleIf, progStaged.simpleIf, "simpleIf", in)
  }
  test("simpleSum") {
    //pending
    val in = 7
    compareOutputWithSequential(progStaged)(progSeq.simpleSum, progStaged.simpleSum, "simpleSum", in)
  }
  test("sumOps") {
    val sum = Right[Unit, Int](7)
    compareOutputWithSequential(progStaged)(progSeq.sumOps, progStaged.sumOps, "sumOps", sum)
  }
  test("optionOps") {
    val in = 7
    compareOutputWithSequential(progStaged)(progSeq.optionOps, progStaged.optionOps, "optionOps", in)
  }
  test("lambdaApply") {
    val x = 7
    val f = (_: Int) * 2
    compareOutputWithSequential(progStaged)(progSeq.lambdaApply, progStaged.lambdaApply, "lambdaApply", (x, f))
  }
  test("lambdaConst") {
    val in = 7
    getStagedOutput(progStaged)(progStaged.lambdaConst, "lambdaConst", in).isInstanceOf[Right[_, _]]
  }
  test("logicalOps") {
    val in = (true, false)
    compareOutputWithSequential(progStaged)(progSeq.logicalOps, progStaged.logicalOps, "logicalOps", in)
  }
  test("test9unionMaps") {
    val in = (Array((1, 1.1), (2, 2.2), (3, 3.3), (4, 4.4), (5, 5.5)), Array((0, 0.0), (2, 2.0), (4, 4.0), (6, 6.0)))
    compareOutputWithSequential(progStaged)(progSeq.unionMaps, progStaged.unionMaps, "unionMaps", in)
  }
  test("test10differenceMaps") {
    val in = (Array((1, 1.1), (2, 2.2), (3, 3.3), (4, 4.4), (5, 5.5)), Array((0, 0.0), (2, 2.0), (4, 4.0), (6, 6.0)))
    compareOutputWithSequential(progStaged)(progSeq.differenceMaps, progStaged.differenceMaps, "differenceMaps", in)
  }
  test("test11iterateMap") {
    val in = Array((1, 1.1), (2, 2.2), (3, 3.3))
    compareOutputWithSequential(progStaged)(progSeq.iterateMap, progStaged.iterateMap, "iterateMap", in)
  }
  test("test12mapReduce") {
    val in = Array(1, 2, 1, 1, 2, 3, 4, 1, 5, 4, 3, 2, 5, 2, 1)
    compareOutputWithSequential(progStaged)(progSeq.mapReduceByKey, progStaged.mapReduceByKey, "mapReduce", in)
  }
  test("test13filterCompound") {
    val in = Array((11, (12, 13)), (21, (22, 23)), (31, (32, 33)))
    compareOutputWithSequential(progStaged)(progSeq.filterCompound, progStaged.filterCompound, "filterCompound", in)
  }
  /* Not working because of scalan/meta problem
  test("test13filterUDT") { 
    val in = Array((11, (12, 13)), (21, (22, 23)), (31, (32, 33)))
    compareOutputWithSequential(progStaged)(progSeq.filterUDT, progStaged.filterUDT, "filterUDT", in)
  }
  */
  test("test14aggregates") {
    val in = Array(1, 2, 3, 4, 5)
    compareOutputWithSequential(progStaged)(progSeq.aggregates, progStaged.aggregates, "aggregates", in)
  }
  test("test15sortBy") {
    val in = Array((2, 1), (3, 2), (1, 3), (5, 4), (4, 5))
    compareOutputWithSequential(progStaged)(progSeq.sortBy, progStaged.sortBy, "sortBy", in)
  }
  test("test15join") {
    val in = (Array((1, 1.1), (2, 2.2), (3, 3.3), (4, 4.4), (5, 5.5)), Array((0, 0.0), (2, 2.0), (4, 4.0), (6, 6.0)))
    compareOutputWithSequential(progStaged)(progSeq.joinMaps, progStaged.joinMaps, "joinMaps", in)
  }
  test("test16compoundMapKey") {
    val in = (Array((2, 1.0), (3, 2.0), (1, 3.0), (5, 4.0), (4, 5.0)), Array(1, 2, 3, 4, 5))
    compareOutputWithSequential(progStaged)(progSeq.compoundMapKey, progStaged.compoundMapKey, "compoundMapKey", in)
  }
  test("test17reduceMaps") {
    val in = (Array((1, 1.1), (2, 2.2), (3, 3.3), (4, 4.4), (5, 5.5)), Array((0, 0.0), (2, 2.0), (4, 4.0), (6, 6.0)))
    compareOutputWithSequential(progStaged)(progSeq.reduceMaps, progStaged.reduceMaps, "reduceMaps", in)
  }
  test("test18groupByCount") {
    val in = Array((2, 1), (3, 2), (2, 5), (1, 3), (5, 4), (1, 3), (4, 5), (2, 4))
    compareOutputWithSequential(progStaged)(progSeq.groupByCount, progStaged.groupByCount, "groupByCount", in)
  }
  test("test19groupBySum") {
    val in = Array((2, 1), (3, 2), (2, 5), (1, 3), (5, 4), (1, 3), (4, 5), (2, 4))
    compareOutputWithSequential(progStaged)(progSeq.groupBySum, progStaged.groupBySum, "groupBySum", in)
  }
  /*
  test("test20filterCompoundPArray") { 
    val in = Array((11, (12, 13)), (21, (22, 23)), (31, (32, 33)))
    compareOutputWithSequential(progStaged)(progSeq.filterCompoundPArray, progStaged.filterCompoundPArray, "filterCompoundPArray", in)
  }
  */
  test("test21compoundMapValue") {
    val in = (Array("one", "two", "three"), Array((1, 1.1), (2, 2.2), (3, 3.3)))
    compareOutputWithSequential(progStaged)(progSeq.compoundMapValue, progStaged.compoundMapValue, "compoundMapValue", in)
  }
  test("test22fillArrayBuffer") {
    val in = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
    compareOutputWithSequential(progStaged)(progSeq.fillArrayBuffer, progStaged.fillArrayBuffer, "fillArrayBuffer", in)
  }
  test("test23unionMultiMaps") {
    val in = (Array((1, 1.1), (2, 2.2), (1, 3.3), (1, 4.4), (2, 5.5)), Array((0, 0.0), (2, 2.0), (1, 4.0), (1, 6.0)))
    compareOutputWithSequential(progStaged)(progSeq.unionMultiMaps, progStaged.unionMultiMaps, "unionMultiMaps", in)
  }
  test("test33reuseTest") {
    val in = 5
    compareOutputWithSequential(progStaged)(progSeq.reuseTest, progStaged.reuseTest, "reuseTest", in)
  }
  test("test34arrayEmpty") {
    val in = 0
    val stgOut = getStagedOutput[Int,Array[Int]](progStaged)(progStaged.arrayEmpty, "arrayEmpty", in)
    val seqOut:Array[Int] = progSeq.arrayEmpty(in)

    assert(stgOut.isEmpty, "stgOut.isEmpty")
    assert(seqOut.isEmpty, "seqOut.isEmpty")

    compareOutputWithSequential(progStaged)(progSeq.arrayEmpty, progStaged.arrayEmpty, "arrayEmpty", in)
  }

  test("test35arrayReplicate") {
    val in = (3, 3.14)
    val res = Array(3.14, 3.14, 3.14)
    val stgOut = getStagedOutput[(Int,Double),Array[Double]](progStaged)(progStaged.arrayReplicate, "arrayReplicate", in)
    val seqOut:Array[Double] = progSeq.arrayReplicate(in)

    assert(stgOut.sameElements( res ), "stgOut.sameElements( res )")
    assert(seqOut.sameElements( res ), "seqOut.sameElements( res )")

    compareOutputWithSequential(progStaged)(progSeq.arrayReplicate, progStaged.arrayReplicate, "arrayReplicate", in)
  }

  test("test36emptyNestedUnitArray") {
    // Wrong type is generated in SBT/TeamCity, right in IDEA
    pending
    val in = 3
    val seqOut = progSeq.emptyNestedUnitArray(in)
    println(seqOut)

    val stgOut = getStagedOutput(progStaged)(progStaged.emptyNestedUnitArray, "emptyNestedUnitArray", in)
    println(stgOut)
  }
}
