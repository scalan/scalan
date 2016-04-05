package scalan.compilation.lua

import scalan.ScalanDslExp
import scalan.common.SegmentsDslExp
import scalan.primitives.{StructExamples, StructItTests}

class LuaStructItTests extends StructItTests {
  class ProgExp extends ScalanDslExp with SegmentsDslExp with StructExamples

  val progStaged = new LuaCompiler(new ProgExp)

  val defaultCompilers = compilers(progStaged)
}
