package scalan.collections

import scalan.compilation.lms.scalac.LmsCompilerScala

class LmsMultiMapItTests extends MultiMapItTests {
  // Maps not supported in LmsCompilerUni
  val progStaged = new LmsCompilerScala(new MultiMapExamplesExp)

  val defaultCompilers = compilers(progStaged)
}
