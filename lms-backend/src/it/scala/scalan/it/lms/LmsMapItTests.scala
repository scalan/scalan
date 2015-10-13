package scalan.it.lms

import scalan.compilation.lms.CommunityBridge
import scalan.compilation.lms.uni.LmsCompilerUni
import scalan.{CommunityMethodMappingDSL, ScalanCommunityDslExp, JNIExtractorOpsExp}
import scalan.collections.{SimpleMapProg, MapItTests}
import scalan.compilation.lms.scalac.CommunityLmsCompilerScala

class LmsMapItTests extends MapItTests {
  class ProgExp extends ScalanCommunityDslExp with SimpleMapProg with JNIExtractorOpsExp

  val progStaged = new CommunityLmsCompilerScala(new ProgExp) with CommunityBridge

  val progStagedU = new LmsCompilerUni(new ProgExp) with CommunityBridge with CommunityMethodMappingDSL

  // TODO lack of maps support in LMS C++ backend
  val defaultCompilers = compilers(progStaged/*, progStagedU*/)
}
