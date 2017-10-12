package scalan.compilation.kotlin

import scalan.ScalanDslExp
import scalan.it.smoke.{SmokeItTests, SmokeProg}

class LuaSmokeItTests extends SmokeItTests {
  class ProgExp extends ScalanDslExp with SmokeProg

  val progStaged = new LuaCompiler(new ProgExp)

  val defaultCompilers = compilers(progStaged)
}
