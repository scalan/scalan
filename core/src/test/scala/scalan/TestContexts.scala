package scalan

import java.lang.reflect.Method

import scalan.compilation.{GraphVizConfig, DummyCompilerWithPasses}
import scalan.util.FileUtil

trait TestContexts extends TestsUtil {
  abstract class TestContext(testName: String) extends ScalanCtxExp {
    override def isInvokeEnabled(d: Def[_], m: Method) = true
    override def shouldUnpack(e: Elem[_]) = true
    def emit(name: String, ss: Exp[_]*) =
      emitDepGraph(ss, FileUtil.file(prefix, testName, s"$name.dot"))(defaultGraphVizConfig)
  }

  abstract class TestCompilerContext(testName: String) extends TestContext(testName) with DummyCompilerWithPasses {
    def test[A,B](functionName: String, f: Exp[A => B]) = {
      buildExecutable(FileUtil.file(prefix, functionName), functionName, f, GraphVizConfig.default)(defaultCompilerConfig)
    }
  }
}

abstract class BaseCtxTests extends BaseTests with TestContexts

abstract class BaseShouldCtxTests extends BaseShouldTests with TestContexts