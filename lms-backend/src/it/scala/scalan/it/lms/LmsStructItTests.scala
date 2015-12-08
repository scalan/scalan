package scalan.it.lms

import scalan.common.SegmentsDslExp
import scalan.compilation.lms.{LmsStructsCompiler, CommunityBridge}
import scalan.compilation.lms.scalac.CommunityLmsCompilerScala
import scalan.compilation.lms.uni.LmsCompilerUni
import scalan.primitives.{StructItTests, StructExamples}
import scalan.{CommunityMethodMappingDSL, JNIExtractorOpsExp, ScalanCommunityDslExp}

class LmsStructItTests extends StructItTests {
  class ProgExp extends ScalanCommunityDslExp with StructExamples with SegmentsDslExp with JNIExtractorOpsExp

  val progStaged = new CommunityLmsCompilerScala(new ProgExp) with CommunityBridge with CommunityMethodMappingDSL

  val progStagedWrapping = new CommunityLmsCompilerScala(new ProgExp) with CommunityBridge with
                               CommunityMethodMappingDSL with LmsStructsCompiler[ProgExp]

  val progStagedU = new LmsCompilerUni(new ProgExp) with CommunityBridge with CommunityMethodMappingDSL

  // TODO Packing StructElems not supported in JNIExtractorOpsExp yet
  val defaultCompilers = compilers(progStaged, progStagedWrapping/*, progStagedU*/)
}
