package scalan

import java.lang.reflect.Method

import org.scalatest.Suite

import scalan.compilation.{GraphVizConfig, Compiler}
import scalan.util.FileUtil

trait TestContexts extends TestsUtil {

  abstract class TestContext(testName: String) extends ScalanCtxExp {
    def this() = this(currentTestNameAsFileName)

    override def isInvokeEnabled(d: Def[_], m: Method) = true
    override def shouldUnpack(e: Elem[_]) = true
    def emit(name: String, ss: Exp[_]*): Unit =
      emitDepGraph(ss, FileUtil.file(prefix, testName, s"$name.dot"))(defaultGraphVizConfig)
    def emit(ss: Exp[_]*): Unit = emit(testName, ss: _*)
  }

  // TODO change API to use defaultCompilers here! See JNI_MsfItTests and others
  abstract class TestCompilerContext(testName: String) {
    def this() = this(currentTestNameAsFileName)

    val compiler: Compiler[_ <: ScalanExp]
    import compiler._

    def test[A,B](functionName: String, f: Exp[A => B]): CompilerOutput[A, B] = {
      buildExecutable(FileUtil.file(prefix, functionName), functionName, f, GraphVizConfig.default)(defaultCompilerConfig)
    }
    def test[A,B](f: Exp[A => B]): CompilerOutput[A, B] = test(testName, f)

    def emit(name: String, ss: Exp[_]*): Unit =
      scalan.emitDepGraph(ss, FileUtil.file(prefix, testName, s"$name.dot"))(scalan.defaultGraphVizConfig)
    def emit(ss: Exp[_]*): Unit = emit(testName, ss: _*)
  }
}

abstract class BaseCtxTests extends BaseTests with TestContexts

abstract class BaseNestedCtxTests extends BaseNestedTests with TestContexts

abstract class BaseShouldCtxTests extends BaseShouldTests with TestContexts