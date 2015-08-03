package scalan

import java.lang.reflect.Method

import org.scalatest.Suite

import scalan.compilation.{GraphVizConfig, Compiler}
import scalan.util.FileUtil

trait TestContexts extends Suite with TestsUtil {
  private val currentTestName = new ThreadLocal[String]

  override def withFixture(test: NoArgTest) = {
    currentTestName.set(test.name)
    val outcome = super.withFixture(test)
    currentTestName.set(null)
    outcome
  }

  abstract class TestContext(testName: String) extends ScalanCtxExp {
    def this() = this(defaultContextName)

    override def isInvokeEnabled(d: Def[_], m: Method) = true
    override def shouldUnpack(e: Elem[_]) = true
    def emit(name: String, ss: Exp[_]*): Unit =
      emitDepGraph(ss, FileUtil.file(prefix, testName, s"$name.dot"))(defaultGraphVizConfig)
    def emit(ss: Exp[_]*): Unit = emit(testName, ss: _*)
  }

  protected def defaultContextName: String = {
    val testName = currentTestName.get()
    assert(testName != null, "defaultContextName called outside a test")
    testName.replaceAll("""[ /\\]""", "_")
  }

  abstract class TestCompilerContext(testName: String) {
    def this() = this(defaultContextName)

    val compiler: Compiler
    import compiler._

    def test[A,B](functionName: String, f: Exp[A => B]): CompilerOutput[A, B] = {
      buildExecutable(FileUtil.file(prefix, functionName), functionName, f, GraphVizConfig.default)(defaultCompilerConfig)
    }
    def test[A,B](f: Exp[A => B]): CompilerOutput[A, B] = test(testName, f)
  }
}

abstract class BaseCtxTests extends BaseTests with TestContexts

abstract class BaseShouldCtxTests extends BaseShouldTests with TestContexts