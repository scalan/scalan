package scalan.it.lms

import scalan.common.SegmentsDslExp
import scalan.compilation.lms.cxx.LmsCompilerCxx
import scalan.compilation.lms.scalac.LmsCompilerScala
import scalan.compilation.lms.uni.LmsCompilerUni
import scalan.primitives.{StructItTests, StructExamples}
import scalan.{JNIExtractorOpsExp, ScalanDslExp}

class LmsStructItTests extends StructItTests {
  class ProgExp extends ScalanDslExp with StructExamples with SegmentsDslExp with JNIExtractorOpsExp

  val progStaged = new LmsCompilerScala(new ProgExp)

  val progStagedCxx = new LmsCompilerCxx(new ProgExp)

  val progStagedU = new LmsCompilerUni(new ProgExp)

  val defaultCompilers = compilers(progStaged, progStagedU)

  val cxxOnly = compilers(progStagedCxx)

  import progStd._

  test("Generate CPP source") {
    compileSource(s => s.t1.asInstanceOf[s.Rep[Int => Struct]], cxxOnly)

    compileSource(s => s.singleFieldStructIn.asInstanceOf[s.Rep[Struct => Int]], cxxOnly)

    compileSource(s => s.structInOut.asInstanceOf[s.Rep[Struct => Struct]], cxxOnly)

    compileSource(_.structInside, cxxOnly)
  }
}
