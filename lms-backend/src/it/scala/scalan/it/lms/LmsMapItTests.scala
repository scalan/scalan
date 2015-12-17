package scalan.it.lms

import scalan.collections.{MapItTests, SimpleMapProg}
import scalan.compilation.lms.scalac.LmsCompilerScala
import scalan.compilation.lms.uni.LmsCompilerUni
import scalan.{JNIExtractorOpsExp, ScalanDslExp}

class LmsMapItTests extends MapItTests {
  class ProgExp extends ScalanDslExp with SimpleMapProg with JNIExtractorOpsExp

  val progStaged = new LmsCompilerScala(new ProgExp)

  val progStagedU = new LmsCompilerUni(new ProgExp)

  // TODO lack of maps support in LMS C++ backend
  val defaultCompilers = compilers(progStaged/*, progStagedU*/)
}
