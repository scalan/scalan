package scalan.collections

import scalan._
import scalan.compilation.DummyCompiler

/**
  * Created by dkolmakov on 12/31/15.
  */
class SimpleArrayBufferTests extends BaseCtxTests  {

  class Ctx(testName: String) extends TestCompilerContext(testName) {
    override val compiler = new DummyCompiler(new Scalan)
  }

  test("NonUnifiedEmptyArrayBuffer") {
    val ctx = new Ctx("NonUnifiedEmptyArrayBuffer") {
      import compiler.scalan._

      def test() = {
        val t = reifyEffects {
//          val arrayBuffer1 = ArrayBuffer.empty[Int]
//          val arrayBuffer2 = ArrayBuffer.empty[Int]
//          assert(arrayBuffer1 != arrayBuffer2)
        }
      }
      test()
    }
  }

}


