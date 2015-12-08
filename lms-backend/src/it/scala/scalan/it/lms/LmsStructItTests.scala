package scalan.it.lms

import scalan.common.SegmentsDslExp
import scalan.compilation.lms.CommunityBridge
import scalan.compilation.lms.cxx.LmsCompilerCxx
import scalan.compilation.lms.scalac.CommunityLmsCompilerScala
import scalan.compilation.lms.uni.LmsCompilerUni
import scalan.primitives.{StructItTests, StructExamples}
import scalan.{CommunityMethodMappingDSL, JNIExtractorOpsExp, ScalanCommunityDslExp}

class LmsStructItTests extends StructItTests {
  class ProgExp extends ScalanCommunityDslExp with StructExamples with SegmentsDslExp with JNIExtractorOpsExp

  val progStaged = new CommunityLmsCompilerScala(new ProgExp) with CommunityBridge with CommunityMethodMappingDSL

  val progStagedCxx = new LmsCompilerCxx(new ProgExp) with CommunityBridge with CommunityMethodMappingDSL

  val progStagedU = new LmsCompilerUni(new ProgExp) with CommunityBridge with CommunityMethodMappingDSL

  // TODO Packing StructElems not supported in JNIExtractorOpsExp yet
  val defaultCompilers = compilers(progStaged/*, progStagedU*/)

  val cxxOnly = compilers(progStagedCxx)

  import progSeq._

  test("Generate CPP source") {
    compileSource(s => s.t1.asInstanceOf[s.Rep[Int => Struct]], cxxOnly)

    compileSource(s => s.structIn.asInstanceOf[s.Rep[Struct => Int]], cxxOnly)

    compileSource(s => s.structInOut.asInstanceOf[s.Rep[Struct => Struct]], cxxOnly)
  }
}
