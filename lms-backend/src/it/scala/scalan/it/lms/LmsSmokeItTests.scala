package scalan
package it.lms

import scalan.collections.SimpleMapTests
import scalan.compilation.lms._
import scalan.compilation.lms.scalac.CommunityLmsCompilerScala
import scalan.compilation.lms.uni.{LmsBackendUni, LmsCompilerUni}
import scalan.it.smoke.CommunitySmokeItTests

class LmsSmokeItTests extends CommunitySmokeItTests with SimpleMapTests {
  trait Prog extends super.Prog with SimpleMapProg

  class ProgExp extends ProgCommunityExp with Prog

  val progSeq = new ProgCommunitySeq with Prog
  val progStaged = new CommunityLmsCompilerScala(new ProgExp) with CommunityBridge

  val progStagedU = new LmsCompilerUni(new ProgExp) with CommunityBridge with CommunityMethodMappingDSL

  val defaultCompilers = compilers(progStaged, progStagedU)
  val progStagedOnly = compilers(progStaged)

  test("applyLambda2Array") {
    //FIXME: applying lambda to Array don't compile in Uni compiler (see issue #50)
    val in = Array(1,2,3,4)
    compareOutputWithSequential(_.applyLambda2Array, "applyLambda2Array", progStagedOnly)(in)
  }

  test ("convertPairCollectionSOA")  {
    val in = Array(Array((1,2.0),(2,3.0)), Array((3,4.0), (5,6.0)))
    compareOutputWithSequential(_.convertPairCollectionSOA, "convertPairCollectionSOA")(in)
  }

  test("mapPutContains") {
    val in = (314,3.14)
    // TODO: lack of maps support in LMS C++ backend
    compareOutputWithSequential(_.mapPutContains, "mapPutContains", progStagedOnly)(in)
  }
  test("mapAsSet") {
    val in = 314
    compareOutputWithSequential(_.mapAsSet, "mapAsSet", progStagedOnly)(in)
//TODO: lack of maps support in LMS C++ backend
//    compareOutputWithSequential(progStagedU, progSeq)(_.mapAsSet, "mapAsSet")(in)
  }
  test("simpleArith") {
    val in = 2
    compareOutputWithSequential(_.simpleArith, "simpleArith")(in)
  }
  test("simpleArrGet") {
    val in = (Array(2,3), 1)
    compareOutputWithSequential(_.simpleArrGet, "simpleArrGet")(in)
  }
  test("simpleMap") {
    val in = Array(2,3)
    compareOutputWithSequential(_.simpleMap, "simpleMap")(in)
  }
  test("simpleMapNested") {
    val in = (Array(Array(2.0,3.0), Array(3.0,4.0)), 1)
    compareOutputWithSequential(_.simpleMapNested, "simpleMapNested")(in)
  }
  test("simpleZip") {
    val in = Array(2,3)
    compareOutputWithSequential(_.simpleZip, "simpleZip")(in)
  }
  test("simpleZipWith") {
    val in = Array(2,3)
    compareOutputWithSequential(_.simpleZipWith, "simpleZipWith")(in)
  }
  test("simpleReduce") {
    val in = Array(2,3)
    compareOutputWithSequential(_.simpleReduce, "simpleReduce")(in)
  }
  test("mvMul") {
    val in = (Array(Array(2,3), Array(4,5)), Array(6,7))
    compareOutputWithSequential(_.mvMul, "mvMul")(in)
  }
  test("simpleIf") {
    val in = (Array(2.0,3.0), 4.0)
    compareOutputWithSequential(_.simpleIf, "simpleIf")(in)
  }
  test("simpleSum") {
    //pending
    val in = 7
    compareOutputWithSequential((_: Prog).simpleSum, "simpleSum", progStagedOnly)(in)
//TODO: lack of Either[A,B] support in JNIExtractorOps
//    compareOutputWithSequential(progStagedU, progSeq)(_.simpleSum, "simpleSum")(in)
  }
  test("sumOps") {
    val sum = Right[Unit, Int](7)
    compareOutputWithSequential(_.sumOps, "sumOps", progStagedOnly)(sum)
//TODO: lack of Either[A,B] support in JNIExtractorOps
//    compareOutputWithSequential(progStagedU, progSeq)(_.sumOps, "sumOps", sum)
  }
  test("optionOps") {
    val in = 7
    compareOutputWithSequential(_.optionOps, "optionOps")(in)
  }
  test("lambdaApply") {
    val x = 7
    val f = (_: Int) * 2
    compareOutputWithSequential(_.lambdaApply, "lambdaApply", progStagedOnly)((x, f))
//TODO: can not pass lambda via JNI
//    compareOutputWithSequential(progStagedU, progSeq)(_.lambdaApply, "lambdaApply", (x, f))
  }
  test("lambdaConst") {
    val in = 7
    getStagedOutput((_: Prog).lambdaConst, "lambdaConst", progStagedOnly)(in).head.isInstanceOf[Right[_, _]]
//TODO: lack of Either[A,B] support in JNIExtractorOps
//    getStagedOutput(progStagedU)(_.lambdaConst, "lambdaConst")(in).isInstanceOf[Right[_, _]]
  }
  test("logicalOps") {
    val in = (true, false)
    compareOutputWithSequential(_.logicalOps, "logicalOps")(in)
  }
  test("test9unionMaps") {
    val in = (Array((1, 1.1), (2, 2.2), (3, 3.3), (4, 4.4), (5, 5.5)), Array((0, 0.0), (2, 2.0), (4, 4.0), (6, 6.0)))
    compareOutputWithSequential(_.unionMaps, "unionMaps", progStagedOnly)(in)
//TODO: lack of maps support in LMS C++ backend
//    compareOutputWithSequential(progStagedU, progSeq)(_.unionMaps, "unionMaps")(in)
  }
  test("test10differenceMaps") {
    val in = (Array((1, 1.1), (2, 2.2), (3, 3.3), (4, 4.4), (5, 5.5)), Array((0, 0.0), (2, 2.0), (4, 4.0), (6, 6.0)))
    compareOutputWithSequential(_.differenceMaps, "differenceMaps", progStagedOnly)(in)
//TODO: lack of maps support in LMS C++ backend
//    compareOutputWithSequential(progStagedU, progSeq)(_.differenceMaps, "differenceMaps")(in)
  }
  test("test11iterateMap") {
    val in = Array((1, 1.1), (2, 2.2), (3, 3.3))
    compareOutputWithSequential(_.iterateMap, "iterateMap", progStagedOnly)(in)
//TODO: lack of maps support in LMS C++ backend
//    compareOutputWithSequential(progStagedU, progSeq)(_.iterateMap, "iterateMap")(in)
  }
  test("test12mapReduce") {
    val in = Array(1, 2, 1, 1, 2, 3, 4, 1, 5, 4, 3, 2, 5, 2, 1)
    compareOutputWithSequential(_.mapReduceByKey, "mapReduce", progStagedOnly)(in)
//TODO: lack of maps support in LMS C++ backend
//    compareOutputWithSequential(progStagedU, progSeq)(_.mapReduceByKey, "mapReduce")(in)
  }
  test("test13filterCompound") {
    val in = Array((11, (12, 13)), (21, (22, 23)), (31, (32, 33)))
    compareOutputWithSequential(_.filterCompound, "filterCompound")(in)
  }
  /* Not working because of scalan/meta problem
  test("test13filterUDT") { 
    val in = Array((11, (12, 13)), (21, (22, 23)), (31, (32, 33)))
    compareOutputWithSequential(_.filterUDT, "filterUDT")(in)
  }
  */
  test("test14aggregates") {
    val in = Array(1, 2, 3, 4, 5)
    compareOutputWithSequential(_.aggregates, "aggregates", progStagedOnly)(in)
  }
  test("test15sortBy") {
    val in = Array((2, 1), (3, 2), (1, 3), (5, 4), (4, 5))
    compareOutputWithSequential(_.sortBy, "sortBy", progStagedOnly)(in)
//TODO: ArraySortBy is unsupported in CxxShptrCodegen
//    compareOutputWithSequential(progStagedU, progSeq)(_.sortBy, "sortBy")(in)
  }
  test("test15join") {
    val in = (Array((1, 1.1), (2, 2.2), (3, 3.3), (4, 4.4), (5, 5.5)), Array((0, 0.0), (2, 2.0), (4, 4.0), (6, 6.0)))
    compareOutputWithSequential(_.joinMaps, "joinMaps", progStagedOnly)(in)
//TODO: lack of maps support in LMS C++ backend
//    compareOutputWithSequential(progStagedU, progSeq)(_.joinMaps, "joinMaps")(in)
  }
  test("test16compoundMapKey") {
    val in = (Array((2, 1.0), (3, 2.0), (1, 3.0), (5, 4.0), (4, 5.0)), Array(1, 2, 3, 4, 5))
    compareOutputWithSequential(_.compoundMapKey, "compoundMapKey", progStagedOnly)(in)
//TODO: lack of maps support in LMS C++ backend
//    compareOutputWithSequential(progStagedU, progSeq)(_.compoundMapKey, "compoundMapKey")(in)
  }
  test("test17reduceMaps") {
    val in = (Array((1, 1.1), (2, 2.2), (3, 3.3), (4, 4.4), (5, 5.5)), Array((0, 0.0), (2, 2.0), (4, 4.0), (6, 6.0)))
    compareOutputWithSequential(_.reduceMaps, "reduceMaps", progStagedOnly)(in)
//TODO: lack of maps support in LMS C++ backend
//    compareOutputWithSequential(progStagedU, progSeq)(_.reduceMaps, "reduceMaps")(in)
  }
  test("test18groupByCount") {
    val in = Array((2, 1), (3, 2), (2, 5), (1, 3), (5, 4), (1, 3), (4, 5), (2, 4))
    compareOutputWithSequential(_.groupByCount, "groupByCount", progStagedOnly)(in)
//TODO: lack of maps support in LMS C++ backend
//    compareOutputWithSequential(progStagedU, progSeq)(_.groupByCount, "groupByCount")(in)
  }
  test("test19groupBySum") {
    val in = Array((2, 1), (3, 2), (2, 5), (1, 3), (5, 4), (1, 3), (4, 5), (2, 4))
    compareOutputWithSequential(_.groupBySum, "groupBySum", progStagedOnly)(in)
//TODO: lack of maps support in LMS C++ backend
//    compareOutputWithSequential(progStagedU, progSeq)(_.groupBySum, "groupBySum")(in)
  }
  /*
  test("test20filterCompoundPArray") { 
    val in = Array((11, (12, 13)), (21, (22, 23)), (31, (32, 33)))
    compareOutputWithSequential(_.filterCompoundPArray, "filterCompoundPArray", progStagedOnly)(in)
  }
  */
  test("test21compoundMapValue") {
    val in = (Array("one", "two", "three"), Array((1, 1.1), (2, 2.2), (3, 3.3)))
    compareOutputWithSequential(_.compoundMapValue, "compoundMapValue", progStagedOnly)(in)
//TODO: lack of maps support in LMS C++ backend
//    compareOutputWithSequential(progStagedU, progSeq)(_.compoundMapValue, "compoundMapValue")(in)
  }
  test("test22fillArrayBuffer") {
    val in = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
    compareOutputWithSequential(_.fillArrayBuffer, "fillArrayBuffer", progStagedOnly)(in)
    //fixme error http://10.122.85.33:81/scalan-lite/scalan-lite-public/issues/49
    //compareOutputWithSequential(progStagedU, progSeq)(_.fillArrayBuffer, "fillArrayBuffer")(in)
  }
  test("test23unionMultiMaps") {
    val in = (Array((1, 1.1), (2, 2.2), (1, 3.3), (1, 4.4), (2, 5.5)), Array((0, 0.0), (2, 2.0), (1, 4.0), (1, 6.0)))
    compareOutputWithSequential(_.unionMultiMaps, "unionMultiMaps", progStagedOnly)(in)
//TODO: lack of maps support in LMS C++ backend
//    compareOutputWithSequential(progStagedU, progSeq)(_.unionMultiMaps, "unionMultiMaps")(in)
  }
  test("test33reuseTest") {
    val in = 5
    compareOutputWithSequential(_.reuseTest, "reuseTest")(in)
  }
  test("test34arrayEmpty") {
    val in = 0
    val Seq(Seq(stgOut, stgOutU)) = getStagedOutput(_.arrayEmpty, "arrayEmpty")(in)
    val seqOut: Array[Int] = progSeq.arrayEmpty(in)

    assert(stgOut.isEmpty, "stgOut.isEmpty")
    assert(stgOutU.isEmpty, "stgOutU.isEmpty")
    assert(seqOut.isEmpty, "seqOut.isEmpty")

//    compareOutputWithSequential(_.arrayEmpty, "arrayEmpty", progStagedOnly)(in)
  }

  test("test35arrayReplicate") {
    val in = (3, 3.14)
    val res = Array(3.14, 3.14, 3.14)
    val Seq(Seq(stgOut, stgOutU)) = getStagedOutput(_.arrayReplicate, "arrayReplicate")(in)
    val seqOut: Array[Double] = progSeq.arrayReplicate(in)

    assert(stgOut.sameElements( res ), "stgOut.sameElements( res )")
    assert(stgOutU.sameElements( res ), "stgOut.sameElements( res )")
    assert(seqOut.sameElements( res ), "seqOut.sameElements( res )")

//    compareOutputWithSequential(_.arrayReplicate, "arrayReplicate", progStagedOnly)(in)
  }

  test("test36emptyNestedUnitArray") {
    // Wrong type is generated in SBT/TeamCity, right in IDEA
    pending
    val in = 3
    val seqOut = progSeq.emptyNestedUnitArray(in)
    println(seqOut)

    val stgOut = getStagedOutput(_.emptyNestedUnitArray, "emptyNestedUnitArray", progStagedOnly)(in)
    println(stgOut)
  }
  test("test37pairIf") {
    val in = (1, Array(1,2,3))
    compareOutputWithSequential(_.pairIf, "pairIf", progStagedOnly)(in)
  }
  test("test38ifSpecialize") {
    val in = Array(1,2,3)
    compareOutputWithSequential(_.ifSpecialize, "ifSpecialize", progStagedOnly)(in)
  }

  test("arrayUpdateMany") {
    val arr = Array(1,2,3)
    val idx = Array(0,2)
    val vls = Array(11, 33)
    compareOutputWithSequential(_.arrayUpdateMany, "arrayUpdateMany", progStagedOnly)((arr,(idx,vls)))
  }
}
