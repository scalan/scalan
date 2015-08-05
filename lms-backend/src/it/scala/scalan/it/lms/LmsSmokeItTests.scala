package scalan
package it.lms

import scalan.collections.SimpleMapTests
import scalan.compilation.lms._
import scalan.compilation.lms.scalac.CommunityLmsCompilerScala
import scalan.compilation.lms.uni.{LmsBackendUni, LmsCompilerUni}
import scalan.it.smoke.CommunitySmokeItTests

class LmsSmokeItTests extends CommunitySmokeItTests with SimpleMapTests {
  class ProgExp extends ProgCommunity with SimpleMapProg with ScalanCommunityDslExp with JNIExtractorOpsExp

  val progSeq = new ProgCommunitySeq with SimpleMapProg
  val progStaged = new CommunityLmsCompilerScala(new ProgExp) with CommunityBridge

  val progStagedU = new LmsCompilerUni(new ProgExp) with CommunityBridge with CommunityMethodMappingDSL

  test("applyLambda2Array") {
    pending //FIXME: applying lambda to Array don't compile in Uni compiler (see issue #50)
    val in = Array(1,2,3,4)
    compareOutputWithSequential(progStaged)(progSeq.applyLambda2Array, progStaged.scalan.applyLambda2Array, "applyLambda2Array", in)
    compareOutputWithSequential(progStagedU)(progSeq.applyLambda2Array, progStagedU.scalan.applyLambda2Array, "applyLambda2Array", in)
  }

  test ("convertPairCollectionSOA")  {
    val in = Array(Array((1,2.0),(2,3.0)), Array((3,4.0), (5,6.0)))
    compareOutputWithSequential(progStaged)(progSeq.convertPairCollectionSOA, progStaged.scalan.convertPairCollectionSOA, "convertPairCollectionSOA", in)
    compareOutputWithSequential(progStagedU)(progSeq.convertPairCollectionSOA, progStagedU.scalan.convertPairCollectionSOA, "convertPairCollectionSOA", in)
  }

  test("mapPutContains") {
    val in = (314,3.14)
    compareOutputWithSequential(progStaged)(progSeq.mapPutContains, progStaged.scalan.mapPutContains, "mapPutContains", in)
//TODO: lack of maps support in LMS C++ backend
//    compareOutputWithSequential(progStagedU)(progSeq.mapPutContains, progStagedU.scalan.mapPutContains, "mapPutContains", in)
  }
  test("mapAsSet") {
    val in = 314
    compareOutputWithSequential(progStaged)(progSeq.mapAsSet, progStaged.scalan.mapAsSet, "mapAsSet", in)
//TODO: lack of maps support in LMS C++ backend
//    compareOutputWithSequential(progStagedU)(progSeq.mapAsSet, progStagedU.scalan.mapAsSet, "mapAsSet", in)
  }
  test("simpleArith") {
    val in = 2
    compareOutputWithSequential(progStaged)(progSeq.simpleArith, progStaged.scalan.simpleArith, "simpleArith", in)
    compareOutputWithSequential(progStagedU)(progSeq.simpleArith, progStagedU.scalan.simpleArith, "simpleArith", in)
  }
  test("simpleArrGet") {
    val in = (Array(2,3), 1)
    compareOutputWithSequential(progStaged)(progSeq.simpleArrGet, progStaged.scalan.simpleArrGet, "simpleArrGet", in)
    compareOutputWithSequential(progStagedU)(progSeq.simpleArrGet, progStagedU.scalan.simpleArrGet, "simpleArrGet", in)
  }
  test("simpleMap") {
    val in = Array(2,3)
    compareOutputWithSequential(progStaged)(progSeq.simpleMap, progStaged.scalan.simpleMap, "simpleMap", in)
    compareOutputWithSequential(progStagedU)(progSeq.simpleMap, progStagedU.scalan.simpleMap, "simpleMap", in)
  }
  test("simpleMapNested") {
    val in = (Array(Array(2.0,3.0), Array(3.0,4.0)), 1)
    compareOutputWithSequential(progStaged)(progSeq.simpleMapNested, progStaged.scalan.simpleMapNested, "simpleMapNested", in)
    compareOutputWithSequential(progStagedU)(progSeq.simpleMapNested, progStagedU.scalan.simpleMapNested, "simpleMapNested", in)
  }
  test("simpleZip") {
    val in = Array(2,3)
    compareOutputWithSequential(progStaged)(progSeq.simpleZip, progStaged.scalan.simpleZip, "simpleZip", in)
    compareOutputWithSequential(progStagedU)(progSeq.simpleZip, progStagedU.scalan.simpleZip, "simpleZip", in)
  }
  test("simpleZipWith") {
    val in = Array(2,3)
    compareOutputWithSequential(progStaged)(progSeq.simpleZipWith, progStaged.scalan.simpleZipWith, "simpleZipWith", in)
    compareOutputWithSequential(progStagedU)(progSeq.simpleZipWith, progStagedU.scalan.simpleZipWith, "simpleZipWith", in)
  }
  test("simpleReduce") {
    val in = Array(2,3)
    compareOutputWithSequential(progStaged)(progSeq.simpleReduce, progStaged.scalan.simpleReduce, "simpleReduce", in)
    compareOutputWithSequential(progStagedU)(progSeq.simpleReduce, progStagedU.scalan.simpleReduce, "simpleReduce", in)
  }
  test("mvMul") {
    val in = (Array(Array(2,3), Array(4,5)), Array(6,7))
    compareOutputWithSequential(progStaged)(progSeq.mvMul, progStaged.scalan.mvMul, "mvMul", in)
    compareOutputWithSequential(progStagedU)(progSeq.mvMul, progStagedU.scalan.mvMul, "mvMul", in)
  }
  test("simpleIf") {
    val in = (Array(2.0,3.0), 4.0)
    compareOutputWithSequential(progStaged)(progSeq.simpleIf, progStaged.scalan.simpleIf, "simpleIf", in)
    compareOutputWithSequential(progStagedU)(progSeq.simpleIf, progStagedU.scalan.simpleIf, "simpleIf", in)
  }
  test("simpleSum") {
    //pending
    val in = 7
    compareOutputWithSequential(progStaged)(progSeq.simpleSum, progStaged.scalan.simpleSum, "simpleSum", in)
//TODO: lack of Either[A,B] support in JNIExtractorOps
//    compareOutputWithSequential(progStagedU)(progSeq.simpleSum, progStagedU.scalan.simpleSum, "simpleSum", in)
  }
  test("sumOps") {
    val sum = Right[Unit, Int](7)
    compareOutputWithSequential(progStaged)(progSeq.sumOps, progStaged.scalan.sumOps, "sumOps", sum)
//TODO: lack of Either[A,B] support in JNIExtractorOps
//    compareOutputWithSequential(progStagedU)(progSeq.sumOps, progStagedU.scalan.sumOps, "sumOps", sum)
  }
  test("optionOps") {
    val in = 7
    compareOutputWithSequential(progStaged)(progSeq.optionOps, progStaged.scalan.optionOps, "optionOps", in)
    compareOutputWithSequential(progStagedU)(progSeq.optionOps, progStagedU.scalan.optionOps, "optionOps", in)
  }
  test("lambdaApply") {
    val x = 7
    val f = (_: Int) * 2
    compareOutputWithSequential(progStaged)(progSeq.lambdaApply, progStaged.scalan.lambdaApply, "lambdaApply", (x, f))
//TODO: can not pass lambda via JNI
//    compareOutputWithSequential(progStagedU)(progSeq.lambdaApply, progStagedU.scalan.lambdaApply, "lambdaApply", (x, f))
  }
  test("lambdaConst") {
    val in = 7
    getStagedOutput(progStaged)(progStaged.scalan.lambdaConst, "lambdaConst", in).isInstanceOf[Right[_, _]]
//TODO: lack of Either[A,B] support in JNIExtractorOps
//    getStagedOutput(progStagedU)(progStagedU.scalan.lambdaConst, "lambdaConst", in).isInstanceOf[Right[_, _]]
  }
  test("logicalOps") {
    val in = (true, false)
    compareOutputWithSequential(progStaged)(progSeq.logicalOps, progStaged.scalan.logicalOps, "logicalOps", in)
    compareOutputWithSequential(progStagedU)(progSeq.logicalOps, progStagedU.scalan.logicalOps, "logicalOps", in)
  }
  test("test9unionMaps") {
    val in = (Array((1, 1.1), (2, 2.2), (3, 3.3), (4, 4.4), (5, 5.5)), Array((0, 0.0), (2, 2.0), (4, 4.0), (6, 6.0)))
    compareOutputWithSequential(progStaged)(progSeq.unionMaps, progStaged.scalan.unionMaps, "unionMaps", in)
//TODO: lack of maps support in LMS C++ backend
//    compareOutputWithSequential(progStagedU)(progSeq.unionMaps, progStagedU.scalan.unionMaps, "unionMaps", in)
  }
  test("test10differenceMaps") {
    val in = (Array((1, 1.1), (2, 2.2), (3, 3.3), (4, 4.4), (5, 5.5)), Array((0, 0.0), (2, 2.0), (4, 4.0), (6, 6.0)))
    compareOutputWithSequential(progStaged)(progSeq.differenceMaps, progStaged.scalan.differenceMaps, "differenceMaps", in)
//TODO: lack of maps support in LMS C++ backend
//    compareOutputWithSequential(progStagedU)(progSeq.differenceMaps, progStagedU.scalan.differenceMaps, "differenceMaps", in)
  }
  test("test11iterateMap") {
    val in = Array((1, 1.1), (2, 2.2), (3, 3.3))
    compareOutputWithSequential(progStaged)(progSeq.iterateMap, progStaged.scalan.iterateMap, "iterateMap", in)
//TODO: lack of maps support in LMS C++ backend
//    compareOutputWithSequential(progStagedU)(progSeq.iterateMap, progStagedU.scalan.iterateMap, "iterateMap", in)
  }
  test("test12mapReduce") {
    val in = Array(1, 2, 1, 1, 2, 3, 4, 1, 5, 4, 3, 2, 5, 2, 1)
    compareOutputWithSequential(progStaged)(progSeq.mapReduceByKey, progStaged.scalan.mapReduceByKey, "mapReduce", in)
//TODO: lack of maps support in LMS C++ backend
//    compareOutputWithSequential(progStagedU)(progSeq.mapReduceByKey, progStagedU.scalan.mapReduceByKey, "mapReduce", in)
  }
  test("test13filterCompound") {
    val in = Array((11, (12, 13)), (21, (22, 23)), (31, (32, 33)))
    compareOutputWithSequential(progStaged)(progSeq.filterCompound, progStaged.scalan.filterCompound, "filterCompound", in)
    compareOutputWithSequential(progStagedU)(progSeq.filterCompound, progStagedU.scalan.filterCompound, "filterCompound", in)
  }
  /* Not working because of scalan/meta problem
  test("test13filterUDT") { 
    val in = Array((11, (12, 13)), (21, (22, 23)), (31, (32, 33)))
    compareOutputWithSequential(progStaged)(progSeq.filterUDT, progStaged.scalan.filterUDT, "filterUDT", in)
  }
  */
  test("test14aggregates") {
    val in = Array(1, 2, 3, 4, 5)
    compareOutputWithSequential(progStaged)(progSeq.aggregates, progStaged.scalan.aggregates, "aggregates", in)
    compareOutputWithSequential(progStagedU)(progSeq.aggregates, progStagedU.scalan.aggregates, "aggregates", in)
  }
  test("test15sortBy") {
    val in = Array((2, 1), (3, 2), (1, 3), (5, 4), (4, 5))
    compareOutputWithSequential(progStaged)(progSeq.sortBy, progStaged.scalan.sortBy, "sortBy", in)
//TODO: ArraySortBy is unsupported in CxxShptrCodegen
//    compareOutputWithSequential(progStagedU)(progSeq.sortBy, progStagedU.scalan.sortBy, "sortBy", in)
  }
  test("test15join") {
    val in = (Array((1, 1.1), (2, 2.2), (3, 3.3), (4, 4.4), (5, 5.5)), Array((0, 0.0), (2, 2.0), (4, 4.0), (6, 6.0)))
    compareOutputWithSequential(progStaged)(progSeq.joinMaps, progStaged.scalan.joinMaps, "joinMaps", in)
//TODO: lack of maps support in LMS C++ backend
//    compareOutputWithSequential(progStagedU)(progSeq.joinMaps, progStagedU.scalan.joinMaps, "joinMaps", in)
  }
  test("test16compoundMapKey") {
    val in = (Array((2, 1.0), (3, 2.0), (1, 3.0), (5, 4.0), (4, 5.0)), Array(1, 2, 3, 4, 5))
    compareOutputWithSequential(progStaged)(progSeq.compoundMapKey, progStaged.scalan.compoundMapKey, "compoundMapKey", in)
//TODO: lack of maps support in LMS C++ backend
//    compareOutputWithSequential(progStagedU)(progSeq.compoundMapKey, progStagedU.scalan.compoundMapKey, "compoundMapKey", in)
  }
  test("test17reduceMaps") {
    val in = (Array((1, 1.1), (2, 2.2), (3, 3.3), (4, 4.4), (5, 5.5)), Array((0, 0.0), (2, 2.0), (4, 4.0), (6, 6.0)))
    compareOutputWithSequential(progStaged)(progSeq.reduceMaps, progStaged.scalan.reduceMaps, "reduceMaps", in)
//TODO: lack of maps support in LMS C++ backend
//    compareOutputWithSequential(progStagedU)(progSeq.reduceMaps, progStagedU.scalan.reduceMaps, "reduceMaps", in)
  }
  test("test18groupByCount") {
    val in = Array((2, 1), (3, 2), (2, 5), (1, 3), (5, 4), (1, 3), (4, 5), (2, 4))
    compareOutputWithSequential(progStaged)(progSeq.groupByCount, progStaged.scalan.groupByCount, "groupByCount", in)
//TODO: lack of maps support in LMS C++ backend
//    compareOutputWithSequential(progStagedU)(progSeq.groupByCount, progStagedU.scalan.groupByCount, "groupByCount", in)
  }
  test("test19groupBySum") {
    val in = Array((2, 1), (3, 2), (2, 5), (1, 3), (5, 4), (1, 3), (4, 5), (2, 4))
    compareOutputWithSequential(progStaged)(progSeq.groupBySum, progStaged.scalan.groupBySum, "groupBySum", in)
//TODO: lack of maps support in LMS C++ backend
//    compareOutputWithSequential(progStagedU)(progSeq.groupBySum, progStagedU.scalan.groupBySum, "groupBySum", in)
  }
  /*
  test("test20filterCompoundPArray") { 
    val in = Array((11, (12, 13)), (21, (22, 23)), (31, (32, 33)))
    compareOutputWithSequential(progStaged)(progSeq.filterCompoundPArray, progStaged.scalan.filterCompoundPArray, "filterCompoundPArray", in)
  }
  */
  test("test21compoundMapValue") {
    val in = (Array("one", "two", "three"), Array((1, 1.1), (2, 2.2), (3, 3.3)))
    compareOutputWithSequential(progStaged)(progSeq.compoundMapValue, progStaged.scalan.compoundMapValue, "compoundMapValue", in)
//TODO: lack of maps support in LMS C++ backend
//    compareOutputWithSequential(progStagedU)(progSeq.compoundMapValue, progStagedU.scalan.compoundMapValue, "compoundMapValue", in)
  }
  test("test22fillArrayBuffer") {
    val in = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
    compareOutputWithSequential(progStaged)(progSeq.fillArrayBuffer, progStaged.scalan.fillArrayBuffer, "fillArrayBuffer", in)
    //fixme error http://10.122.85.33:81/scalan-lite/scalan-lite-public/issues/49
    //compareOutputWithSequential(progStagedU)(progSeq.fillArrayBuffer, progStagedU.scalan.fillArrayBuffer, "fillArrayBuffer", in)
  }
  test("test23unionMultiMaps") {
    val in = (Array((1, 1.1), (2, 2.2), (1, 3.3), (1, 4.4), (2, 5.5)), Array((0, 0.0), (2, 2.0), (1, 4.0), (1, 6.0)))
    compareOutputWithSequential(progStaged)(progSeq.unionMultiMaps, progStaged.scalan.unionMultiMaps, "unionMultiMaps", in)
//TODO: lack of maps support in LMS C++ backend
//    compareOutputWithSequential(progStagedU)(progSeq.unionMultiMaps, progStagedU.scalan.unionMultiMaps, "unionMultiMaps", in)
  }
  test("test33reuseTest") {
    val in = 5
    compareOutputWithSequential(progStaged)(progSeq.reuseTest, progStaged.scalan.reuseTest, "reuseTest", in)
    compareOutputWithSequential(progStagedU)(progSeq.reuseTest, progStagedU.scalan.reuseTest, "reuseTest", in)
  }
  test("test34arrayEmpty") {
    val in = 0
    val stgOut = getStagedOutput[Int,Array[Int]](progStaged)(progStaged.scalan.arrayEmpty, "arrayEmpty", in)
    val stgOutU = getStagedOutput[Int,Array[Int]](progStagedU)(progStagedU.scalan.arrayEmpty, "arrayEmpty", in)
    val seqOut:Array[Int] = progSeq.arrayEmpty(in)

    assert(stgOut.isEmpty, "stgOut.isEmpty")
    assert(stgOutU.isEmpty, "stgOutU.isEmpty")
    assert(seqOut.isEmpty, "seqOut.isEmpty")

//    compareOutputWithSequential(progStaged)(progSeq.arrayEmpty, progStaged.scalan.arrayEmpty, "arrayEmpty", in)
  }

  test("test35arrayReplicate") {
    val in = (3, 3.14)
    val res = Array(3.14, 3.14, 3.14)
    val stgOut = getStagedOutput[(Int,Double),Array[Double]](progStaged)(progStaged.scalan.arrayReplicate, "arrayReplicate", in)
    val stgOutU = getStagedOutput[(Int,Double),Array[Double]](progStagedU)(progStagedU.scalan.arrayReplicate, "arrayReplicate", in)
    val seqOut:Array[Double] = progSeq.arrayReplicate(in)

    assert(stgOut.sameElements( res ), "stgOut.sameElements( res )")
    assert(stgOutU.sameElements( res ), "stgOut.sameElements( res )")
    assert(seqOut.sameElements( res ), "seqOut.sameElements( res )")

//    compareOutputWithSequential(progStaged)(progSeq.arrayReplicate, progStaged.scalan.arrayReplicate, "arrayReplicate", in)
  }

  test("test36emptyNestedUnitArray") {
    // Wrong type is generated in SBT/TeamCity, right in IDEA
    pending
    val in = 3
    val seqOut = progSeq.emptyNestedUnitArray(in)
    println(seqOut)

    val stgOut = getStagedOutput(progStaged)(progStaged.scalan.emptyNestedUnitArray, "emptyNestedUnitArray", in)
    println(stgOut)
  }
  test("test37pairIf") {
    val in = (1, Array(1,2,3))
    compareOutputWithSequential(progStaged)(progSeq.pairIf, progStaged.scalan.pairIf, "pairIf", in)
  }
  test("test38ifSpecialize") {
    val in = Array(1,2,3)
    compareOutputWithSequential(progStaged)(progSeq.ifSpecialize, progStaged.scalan.ifSpecialize, "ifSpecialize", in)
  }
}
