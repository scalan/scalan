package scalan.compilation.kotlin

import scalan.ScalanDslExp
import scalan.common.{MetaTestsDslExp, SegmentsDslExp}
import scalan.primitives.{StructExamples, StructItTests}

class LuaStructItTests extends StructItTests {
  class ProgExp extends ScalanDslExp with SegmentsDslExp with StructExamples with MetaTestsDslExp

  val progStaged = new LuaCompiler(new ProgExp)

  val defaultCompilers = compilers(progStaged)
}
