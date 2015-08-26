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
    compareOutputWithSequential(progStaged, progSeq)(_.applyLambda2Array, "applyLambda2Array", in)
    compareOutputWithSequential(progStagedU, progSeq)(_.applyLambda2Array, "applyLambda2Array", in)
  }

  test ("convertPairCollectionSOA")  {
    val in = Array(Array((1,2.0),(2,3.0)), Array((3,4.0), (5,6.0)))
    compareOutputWithSequential(progStaged, progSeq)(_.convertPairCollectionSOA, "convertPairCollectionSOA", in)
    compareOutputWithSequential(progStagedU, progSeq)(_.convertPairCollectionSOA, "convertPairCollectionSOA", in)
  }

  test("mapPutContains") {
    val in = (314,3.14)
    compareOutputWithSequential(progStaged, progSeq)(_.mapPutContains, "mapPutContains", in)
//TODO: lack of maps support in LMS C++ backend
//    compareOutputWithSequential(progStagedU, progSeq)(_.mapPutContains, "mapPutContains", in)
  }
  test("mapAsSet") {
    val in = 314
    compareOutputWithSequential(progStaged, progSeq)(_.mapAsSet, "mapAsSet", in)
//TODO: lack of maps support in LMS C++ backend
//    compareOutputWithSequential(progStagedU, progSeq)(_.mapAsSet, "mapAsSet", in)
  }
  test("simpleArith") {
    val in = 2
    compareOutputWithSequential(progStaged, progSeq)(_.simpleArith, "simpleArith", in)
    compareOutputWithSequential(progStagedU, progSeq)(_.simpleArith, "simpleArith", in)
  }
  test("simpleArrGet") {
    val in = (Array(2,3), 1)
    compareOutputWithSequential(progStaged, progSeq)(_.simpleArrGet, "simpleArrGet", in)
    compareOutputWithSequential(progStagedU, progSeq)(_.simpleArrGet, "simpleArrGet", in)
  }
  test("simpleMap") {
    val in = Array(2,3)
    compareOutputWithSequential(progStaged, progSeq)(_.simpleMap, "simpleMap", in)
    compareOutputWithSequential(progStagedU, progSeq)(_.simpleMap, "simpleMap", in)
  }
  test("simpleMapNested") {
    val in = (Array(Array(2.0,3.0), Array(3.0,4.0)), 1)
    compareOutputWithSequential(progStaged, progSeq)(_.simpleMapNested, "simpleMapNested", in)
    compareOutputWithSequential(progStagedU, progSeq)(_.simpleMapNested, "simpleMapNested", in)
  }
  test("simpleZip") {
    val in = Array(2,3)
    compareOutputWithSequential(progStaged, progSeq)(_.simpleZip, "simpleZip", in)
    compareOutputWithSequential(progStagedU, progSeq)(_.simpleZip, "simpleZip", in)
  }
  test("simpleZipWith") {
    val in = Array(2,3)
    compareOutputWithSequential(progStaged, progSeq)(_.simpleZipWith, "simpleZipWith", in)
    compareOutputWithSequential(progStagedU, progSeq)(_.simpleZipWith, "simpleZipWith", in)
  }
  test("simpleReduce") {
    val in = Array(2,3)
    compareOutputWithSequential(progStaged, progSeq)(_.simpleReduce, "simpleReduce", in)
    compareOutputWithSequential(progStagedU, progSeq)(_.simpleReduce, "simpleReduce", in)
  }
  test("mvMul") {
    val in = (Array(Array(2,3), Array(4,5)), Array(6,7))
    compareOutputWithSequential(progStaged, progSeq)(_.mvMul, "mvMul", in)
    compareOutputWithSequential(progStagedU, progSeq)(_.mvMul, "mvMul", in)
  }
  test("simpleIf") {
    val in = (Array(2.0,3.0), 4.0)
    compareOutputWithSequential(progStaged, progSeq)(_.simpleIf, "simpleIf", in)
    compareOutputWithSequential(progStagedU, progSeq)(_.simpleIf, "simpleIf", in)
  }
  test("simpleSum") {
    //pending
    val in = 7
    // unclear why type parameters seem to be required here
    compareOutputWithSequential(progStaged, progSeq)[Int, (Either[Int, Unit], Either[Unit, Int])](_.simpleSum, "simpleSum", in)
//TODO: lack of Either[A,B] support in JNIExtractorOps
//    compareOutputWithSequential(progStagedU, progSeq)(_.simpleSum, "simpleSum", in)
  }
  test("sumOps") {
    val sum = Right[Unit, Int](7)
    compareOutputWithSequential(progStaged, progSeq)(_.sumOps, "sumOps", sum)
//TODO: lack of Either[A,B] support in JNIExtractorOps
//    compareOutputWithSequential(progStagedU, progSeq)(_.sumOps, "sumOps", sum)
  }
  test("optionOps") {
    val in = 7
    compareOutputWithSequential(progStaged, progSeq)(_.optionOps, "optionOps", in)
    compareOutputWithSequential(progStagedU, progSeq)(_.optionOps, "optionOps", in)
  }
  test("lambdaApply") {
    val x = 7
    val f = (_: Int) * 2
    compareOutputWithSequential(progStaged, progSeq)(_.lambdaApply, "lambdaApply", (x, f))
//TODO: can not pass lambda via JNI
//    compareOutputWithSequential(progStagedU, progSeq)(_.lambdaApply, "lambdaApply", (x, f))
  }
  test("lambdaConst") {
    val in = 7
    getStagedOutput(progStaged)(_.lambdaConst, "lambdaConst", in).isInstanceOf[Right[_, _]]
//TODO: lack of Either[A,B] support in JNIExtractorOps
//    getStagedOutput(progStagedU)(_.lambdaConst, "lambdaConst", in).isInstanceOf[Right[_, _]]
  }
  test("logicalOps") {
    val in = (true, false)
    compareOutputWithSequential(progStaged, progSeq)(_.logicalOps, "logicalOps", in)
    compareOutputWithSequential(progStagedU, progSeq)(_.logicalOps, "logicalOps", in)
  }
  test("test9unionMaps") {
    val in = (Array((1, 1.1), (2, 2.2), (3, 3.3), (4, 4.4), (5, 5.5)), Array((0, 0.0), (2, 2.0), (4, 4.0), (6, 6.0)))
    compareOutputWithSequential(progStaged, progSeq)(_.unionMaps, "unionMaps", in)
//TODO: lack of maps support in LMS C++ backend
//    compareOutputWithSequential(progStagedU, progSeq)(_.unionMaps, "unionMaps", in)
  }
  test("test10differenceMaps") {
    val in = (Array((1, 1.1), (2, 2.2), (3, 3.3), (4, 4.4), (5, 5.5)), Array((0, 0.0), (2, 2.0), (4, 4.0), (6, 6.0)))
    compareOutputWithSequential(progStaged, progSeq)(_.differenceMaps, "differenceMaps", in)
//TODO: lack of maps support in LMS C++ backend
//    compareOutputWithSequential(progStagedU, progSeq)(_.differenceMaps, "differenceMaps", in)
  }
  test("test11iterateMap") {
    val in = Array((1, 1.1), (2, 2.2), (3, 3.3))
    compareOutputWithSequential(progStaged, progSeq)(_.iterateMap, "iterateMap", in)
//TODO: lack of maps support in LMS C++ backend
//    compareOutputWithSequential(progStagedU, progSeq)(_.iterateMap, "iterateMap", in)
  }
  test("test12mapReduce") {
    val in = Array(1, 2, 1, 1, 2, 3, 4, 1, 5, 4, 3, 2, 5, 2, 1)
    compareOutputWithSequential(progStaged, progSeq)(_.mapReduceByKey, "mapReduce", in)
//TODO: lack of maps support in LMS C++ backend
//    compareOutputWithSequential(progStagedU, progSeq)(_.mapReduceByKey, "mapReduce", in)
  }
  test("test13filterCompound") {
    val in = Array((11, (12, 13)), (21, (22, 23)), (31, (32, 33)))
    compareOutputWithSequential(progStaged, progSeq)(_.filterCompound, "filterCompound", in)
    compareOutputWithSequential(progStagedU, progSeq)(_.filterCompound, "filterCompound", in)
  }
  /* Not working because of scalan/meta problem
  test("test13filterUDT") { 
    val in = Array((11, (12, 13)), (21, (22, 23)), (31, (32, 33)))
    compareOutputWithSequential(progStaged, progSeq)(_.filterUDT, "filterUDT", in)
  }
  */
  test("test14aggregates") {
    val in = Array(1, 2, 3, 4, 5)
    compareOutputWithSequential(progStaged, progSeq)(_.aggregates, "aggregates", in)
    compareOutputWithSequential(progStagedU, progSeq)(_.aggregates, "aggregates", in)
  }
  test("test15sortBy") {
    val in = Array((2, 1), (3, 2), (1, 3), (5, 4), (4, 5))
    compareOutputWithSequential(progStaged, progSeq)(_.sortBy, "sortBy", in)
//TODO: ArraySortBy is unsupported in CxxShptrCodegen
//    compareOutputWithSequential(progStagedU, progSeq)(_.sortBy, "sortBy", in)
  }
  test("test15join") {
    val in = (Array((1, 1.1), (2, 2.2), (3, 3.3), (4, 4.4), (5, 5.5)), Array((0, 0.0), (2, 2.0), (4, 4.0), (6, 6.0)))
    compareOutputWithSequential(progStaged, progSeq)(_.joinMaps, "joinMaps", in)
//TODO: lack of maps support in LMS C++ backend
//    compareOutputWithSequential(progStagedU, progSeq)(_.joinMaps, "joinMaps", in)
  }
  test("test16compoundMapKey") {
    val in = (Array((2, 1.0), (3, 2.0), (1, 3.0), (5, 4.0), (4, 5.0)), Array(1, 2, 3, 4, 5))
    compareOutputWithSequential(progStaged, progSeq)(_.compoundMapKey, "compoundMapKey", in)
//TODO: lack of maps support in LMS C++ backend
//    compareOutputWithSequential(progStagedU, progSeq)(_.compoundMapKey, "compoundMapKey", in)
  }
  test("test17reduceMaps") {
    val in = (Array((1, 1.1), (2, 2.2), (3, 3.3), (4, 4.4), (5, 5.5)), Array((0, 0.0), (2, 2.0), (4, 4.0), (6, 6.0)))
    compareOutputWithSequential(progStaged, progSeq)(_.reduceMaps, "reduceMaps", in)
//TODO: lack of maps support in LMS C++ backend
//    compareOutputWithSequential(progStagedU, progSeq)(_.reduceMaps, "reduceMaps", in)
  }
  test("test18groupByCount") {
    val in = Array((2, 1), (3, 2), (2, 5), (1, 3), (5, 4), (1, 3), (4, 5), (2, 4))
    compareOutputWithSequential(progStaged, progSeq)(_.groupByCount, "groupByCount", in)
//TODO: lack of maps support in LMS C++ backend
//    compareOutputWithSequential(progStagedU, progSeq)(_.groupByCount, "groupByCount", in)
  }
  test("test19groupBySum") {
    val in = Array((2, 1), (3, 2), (2, 5), (1, 3), (5, 4), (1, 3), (4, 5), (2, 4))
    compareOutputWithSequential(progStaged, progSeq)(_.groupBySum, "groupBySum", in)
//TODO: lack of maps support in LMS C++ backend
//    compareOutputWithSequential(progStagedU, progSeq)(_.groupBySum, "groupBySum", in)
  }
  /*
  test("test20filterCompoundPArray") { 
    val in = Array((11, (12, 13)), (21, (22, 23)), (31, (32, 33)))
    compareOutputWithSequential(progStaged, progSeq)(_.filterCompoundPArray, "filterCompoundPArray", in)
  }
  */
  test("test21compoundMapValue") {
    val in = (Array("one", "two", "three"), Array((1, 1.1), (2, 2.2), (3, 3.3)))
    compareOutputWithSequential(progStaged, progSeq)(_.compoundMapValue, "compoundMapValue", in)
//TODO: lack of maps support in LMS C++ backend
//    compareOutputWithSequential(progStagedU, progSeq)(_.compoundMapValue, "compoundMapValue", in)
  }
  test("test22fillArrayBuffer") {
    val in = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
    compareOutputWithSequential(progStaged, progSeq)(_.fillArrayBuffer, "fillArrayBuffer", in)
    //fixme error http://10.122.85.33:81/scalan-lite/scalan-lite-public/issues/49
    //compareOutputWithSequential(progStagedU, progSeq)(_.fillArrayBuffer, "fillArrayBuffer", in)
  }
  test("test23unionMultiMaps") {
    val in = (Array((1, 1.1), (2, 2.2), (1, 3.3), (1, 4.4), (2, 5.5)), Array((0, 0.0), (2, 2.0), (1, 4.0), (1, 6.0)))
    compareOutputWithSequential(progStaged, progSeq)(_.unionMultiMaps, "unionMultiMaps", in)
//TODO: lack of maps support in LMS C++ backend
//    compareOutputWithSequential(progStagedU, progSeq)(_.unionMultiMaps, "unionMultiMaps", in)
  }
  test("test33reuseTest") {
    val in = 5
    compareOutputWithSequential(progStaged, progSeq)(_.reuseTest, "reuseTest", in)
    compareOutputWithSequential(progStagedU, progSeq)(_.reuseTest, "reuseTest", in)
  }
  test("test34arrayEmpty") {
    val in = 0
    val stgOut = getStagedOutput(progStaged)(_.arrayEmpty, "arrayEmpty", in)
    val stgOutU = getStagedOutput(progStagedU)(_.arrayEmpty, "arrayEmpty", in)
    val seqOut:Array[Int] = progSeq.arrayEmpty(in)

    assert(stgOut.isEmpty, "stgOut.isEmpty")
    assert(stgOutU.isEmpty, "stgOutU.isEmpty")
    assert(seqOut.isEmpty, "seqOut.isEmpty")

//    compareOutputWithSequential(progStaged, progSeq)(_.arrayEmpty, "arrayEmpty", in)
  }

  test("test35arrayReplicate") {
    val in = (3, 3.14)
    val res = Array(3.14, 3.14, 3.14)
    val stgOut = getStagedOutput(progStaged)(_.arrayReplicate, "arrayReplicate", in)
    val stgOutU = getStagedOutput(progStagedU)(_.arrayReplicate, "arrayReplicate", in)
    val seqOut:Array[Double] = progSeq.arrayReplicate(in)

    assert(stgOut.sameElements( res ), "stgOut.sameElements( res )")
    assert(stgOutU.sameElements( res ), "stgOut.sameElements( res )")
    assert(seqOut.sameElements( res ), "seqOut.sameElements( res )")

//    compareOutputWithSequential(progStaged, progSeq)(_.arrayReplicate, "arrayReplicate", in)
  }

  test("test36emptyNestedUnitArray") {
    // Wrong type is generated in SBT/TeamCity, right in IDEA
    pending
    val in = 3
    val seqOut = progSeq.emptyNestedUnitArray(in)
    println(seqOut)

    val stgOut = getStagedOutput(progStaged)(_.emptyNestedUnitArray, "emptyNestedUnitArray", in)
    println(stgOut)
  }
  test("test37pairIf") {
    val in = (1, Array(1,2,3))
    compareOutputWithSequential(progStaged, progSeq)(_.pairIf, "pairIf", in)
  }
  test("test38ifSpecialize") {
    val in = Array(1,2,3)
    compareOutputWithSequential(progStaged, progSeq)(_.ifSpecialize, "ifSpecialize", in)
  }

  test("arrayUpdateMany") {
    val arr = Array(1,2,3)
    val idx = Array(0,2)
    val vls = Array(11, 33)
    compareOutputWithSequential(progStaged, progSeq)(_.arrayUpdateMany, "arrayUpdateMany", (arr,(idx,vls)))
    compareOutputWithSequential(progStagedU, progSeq)(_.arrayUpdateMany, "arrayUpdateManyU", (arr,(idx,vls)))
  }
}
