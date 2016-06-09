package scalan.compilation

import scalan.{BaseNestedTests, JNIExtractorOpsExp, ScalanDslExp}

// Compile in SBT and copy class path from application.scala to src/main/resources/application.conf.
// This is needed due to https://github.com/sbt/sbt-buildinfo/issues/95
class KernelTests extends BaseNestedTests {

  describe("can create and launch simple kernel") {
    // see above for the reason
    pendingOnCI()

    val s = new ScalanDslExp with JNIExtractorOpsExp {}
    import s._
    val incFun: Rep[Array[Int] => Array[Int]] = fun { xs: Arr[Int] =>
      xs.map { x: Exp[Int] => x + 1 }
    }
    val kernelStore = KernelStore.open(s, prefix)

    it("Lua") {
      val kernel = kernelStore.createKernel("inc", KernelType.Lua, incFun)
      val in = Array(10)
      val res = kernel(in)
      assertResult(Array(11))(res)
    }

    describe("LMS") {
      it("Scala") {
        val kernel = kernelStore.createKernel("inc", KernelType.Scala, incFun)
        val in = Array(10)
        val res = kernel(in)
        assertResult(Array(11))(res)
      }

      it("C++") {
        val kernel = kernelStore.createKernel("inc", KernelType.Cpp, incFun)
        val in = Array(10)
        val res = kernel(in)
        assertResult(Array(11))(res)
      }

    }
  }

}
