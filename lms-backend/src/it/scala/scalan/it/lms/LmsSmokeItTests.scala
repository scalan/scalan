package scalan
package it.lms

import scalan.collections.{SimpleMapProg, MapItTests}
import scalan.compilation.lms._
import scalan.compilation.lms.scalac.CommunityLmsCompilerScala
import scalan.compilation.lms.uni.{LmsBackendUni, LmsCompilerUni}
import scalan.it.smoke.{SmokeProg, SmokeItTests, CommunitySmokeItTests}

class LmsSmokeItTests extends SmokeItTests {
  class ProgExp extends ScalanCommunityDslExp with SmokeProg with JNIExtractorOpsExp

  val progStaged = new CommunityLmsCompilerScala(new ProgExp) with CommunityBridge

  val progStagedU = new LmsCompilerUni(new ProgExp) with CommunityBridge with CommunityMethodMappingDSL

  val defaultCompilers = compilers(progStaged, progStagedU)
  val progStagedOnly = compilers(progStaged)

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
    compareOutputWithSequential((_: SmokeProg).simpleSum, "simpleSum", progStagedOnly)(in)
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
    getStagedOutput((_: SmokeProg).lambdaConst, "lambdaConst", progStagedOnly)(in).head.isInstanceOf[Right[_, _]]
//TODO: lack of Either[A,B] support in JNIExtractorOps
//    getStagedOutput(progStagedU)(_.lambdaConst, "lambdaConst")(in).isInstanceOf[Right[_, _]]
  }
  test("logicalOps") {
    val in = (true, false)
    compareOutputWithSequential(_.logicalOps, "logicalOps")(in)
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
  /*
  test("test20filterCompoundPArray") { 
    val in = Array((11, (12, 13)), (21, (22, 23)), (31, (32, 33)))
    compareOutputWithSequential(_.filterCompoundPArray, "filterCompoundPArray", progStagedOnly)(in)
  }
  */
  test("test22fillArrayBuffer") {
    val in = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
    compareOutputWithSequential(_.fillArrayBuffer, "fillArrayBuffer", progStagedOnly)(in)
    //fixme error http://10.122.85.33:81/scalan-lite/scalan-lite-public/issues/49
    //compareOutputWithSequential(progStagedU, progSeq)(_.fillArrayBuffer, "fillArrayBuffer")(in)
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

  test("arrayUpdateMany") {
    val arr = Array(1,2,3)
    val idx = Array(0,2)
    val vls = Array(11, 33)
    compareOutputWithSequential(_.arrayUpdateMany, "arrayUpdateMany", progStagedOnly)((arr,(idx,vls)))
  }

  test("applyLambda2Array") {
    //FIXME: applying lambda to Array don't compile in Uni compiler (see issue #50)
    val in = Array(1,2,3,4)
    compareOutputWithSequential(_.applyLambda2Array, "applyLambda2Array", progStagedOnly)(in)
  }

  test("listRangeFrom0") {
    val in = 3
    compareOutputWithSequential(_.listRangeFrom0, "listRangeFrom0", progStagedOnly)(in)
  }

}
