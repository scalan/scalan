package scalan
package it.lms

import scalan.compilation.lms._
import scalan.compilation.lms.scalac.LmsCompilerScala
import scalan.compilation.lms.uni.LmsCompilerUni
import scalan.it.smoke.{SmokeItTests, SmokeProg}

class LmsSmokeItTests extends SmokeItTests {
  class ProgExp extends ScalanDslExp with SmokeProg with JNIExtractorOpsExp

  val progStaged = new LmsCompilerScala(new ProgExp)

  val progStagedU = new LmsCompilerUni(new ProgExp)

  val defaultCompilers = compilers(progStaged, progStagedU)
  val progStagedOnly = compilers(progStaged)

  test("simpleSum") {
    //TODO: lack of Either[A,B] support in JNIExtractorOps
    compareOutputWithStd((_: SmokeProg).simpleSum, progStagedOnly)(7)
  }
  test("sumOps") {
    //TODO: lack of Either[A,B] support in JNIExtractorOps
    compareOutputWithStd(_.sumOps, progStagedOnly)(Right[Unit, Int](7))
  }
  test("lambdaApply") {
    val x = 7
    val f = (_: Int) * 2
    compareOutputWithStd(_.lambdaApply, progStagedOnly)((x, f))
//TODO: can not pass lambda via JNI
  }
  test("lambdaConst") {
    assert(getStagedOutput((_: SmokeProg).lambdaConst, progStagedOnly)(7).head.head.isInstanceOf[Right[_, _]])
//TODO: lack of Either[A,B] support in JNIExtractorOps
  }
  test("aggregates") {
    compareOutputWithStd(_.aggregates)(Array(1, 2, 3, 4, 5))
  }
  test("sortBy") {
    val in = Array((2, 1), (3, 2), (1, 3), (5, 4), (4, 5))
    compareOutputWithStd(_.sortBy, progStagedOnly)(in)
//TODO: ArraySortBy is unsupported in CxxShptrCodegen
  }
  test("fillArrayBuffer") {
    val in = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
    compareOutputWithStd(_.fillArrayBuffer, progStagedOnly)(in)
    //fixme error http://10.122.85.33:81/scalan-lite/scalan-lite-public/issues/49
  }
  test("makeArrayBuffer") {
    val in = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
    compareOutputWithStd(_.makeArrayBuffer, progStagedOnly)(in)
  }
  test("emptyNestedUnitArray") {
    // Wrong type is generated in SBT/TeamCity, right in IDEA
    pending
    val in = 3
    val seqOut = progStd.emptyNestedUnitArray(in)
    println(seqOut)

    val Seq(Seq(stgOut)) = getStagedOutput(_.emptyNestedUnitArray, progStagedOnly)(in)
    println(stgOut)
  }
  test("pairIf") {
    compareOutputWithStd(_.pairIf, progStagedOnly)(Tuple2(1, Array(1,2,3)))
  }

  test("arrayUpdateMany") {
    val arr = Array(1,2,3)
    val idx = Array(0,2)
    val vls = Array(11, 33)
    compareOutputWithStd(_.arrayUpdateMany, progStagedOnly)((arr,(idx,vls)))
  }

  test("applyLambda2Array") {
    //FIXME: applying lambda to Array don't compile in Uni compiler (see issue #50)
    compareOutputWithStd(_.applyLambda2Array, progStagedOnly)(Array(1, 2, 3, 4))
  }

  test("listRangeFrom0") {
    compareOutputWithStd(_.listRangeFrom0, progStagedOnly)(3)
  }

  test("stringCompare") {
    val in = ("abc", "abc")
    compareOutputWithStd(_.stringCompare, progStagedOnly)(in)
  }

  test("stringMax") {
    val in = ("abc", "abc")
    compareOutputWithStd(_.stringMax, progStagedOnly)(in)
  }

  test("randoms") {
    val (i, d) = (3, 3.14)
    val in = (i, d)
    val Seq(Seq((ri, rd))) = getStagedOutput(_.randoms, progStagedOnly)(in)
    assert(ri >= 0 && ri < i)
    assert(rd >= 0.0 && rd <= d)
  }
}
