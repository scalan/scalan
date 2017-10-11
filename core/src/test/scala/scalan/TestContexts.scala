package scalan

import java.lang.reflect.Method

import scalan.compilation.{GraphVizConfig, Compiler}
import scalan.util.FileUtil

trait TestContexts extends TestUtils {
  protected[this] def stage(scalan: Scalan)(testName: String, name: String, sfs: Seq[() => scalan.Sym]): Unit = {
    val directory = FileUtil.file(prefix, testName)
    implicit val graphVizConfig = scalan.defaultGraphVizConfig
    try {
      val ss = sfs.map(_.apply())
      scalan.emitDepGraph(ss, directory, name)
    } catch {
      case e: Exception =>
        val graphMsg = scalan.emitExceptionGraph(e, directory, name) match {
          case Some(graphFile) =>
            s"See ${graphFile.file.getAbsolutePath} for exception graph."
          case None =>
            s"No exception graph produced."
        }
        fail(s"Staging $name failed. $graphMsg", e)
    }
  }

  abstract class TestContext(testName: String) extends Scalan {
    def this() = this(currentTestNameAsFileName)

    override val invokeAll = true
    override def isInvokeEnabled(d: Def[_], m: Method) = invokeAll
    override def shouldUnpack(e: Elem[_]) = true

    // workaround for non-existence of by-name repeated parameters
    def emitF(name: String, sfs: (() => Sym)*): Unit = stage(this)(testName, name, sfs)
//    def emit(name: String, s1: => Sym): Unit = emitF(name, () => s1)
    def emit(name: String, ss: Sym*): Unit = {
      emitF(name, ss.map((s: Rep[_]) => () => s): _*)
    }
    def emit(s1: => Sym): Unit = emitF(testName, () => s1)
    def emit(s1: => Sym, s2: Sym*): Unit = {
      emitF(testName, Seq(() => s1) ++ s2.map((s: Rep[_]) => () => s): _*)
    }
  }

  // TODO change API to use defaultCompilers here! See JNI_MsfItTests and others
  abstract class TestCompilerContext(testName: String) {
    def this() = this(currentTestNameAsFileName)

    val compiler: Compiler[_ <: Scalan]
    import compiler._

    def test[A,B](functionName: String, f: => scalan.Exp[A => B]): CompilerOutput[A, B] = {
      buildExecutable(FileUtil.file(prefix + "/" + testName, functionName), functionName, f, GraphVizConfig.default)(defaultCompilerConfig)
    }
    def test[A,B](f: => scalan.Exp[A => B]): CompilerOutput[A, B] = test(testName, f)

    // workaround for non-existence of by-name repeated parameters
    def emitF(name: String, sfs: (() => scalan.Sym)*): Unit = stage(scalan)(testName, name, sfs)
    def emit(name: String, s1: => scalan.Sym): Unit = emitF(name, () => s1)
    def emit(name: String, s1: => scalan.Sym, s2: => scalan.Sym): Unit =
      emitF(name, () => s1, () => s2)
    def emit(name: String, s1: => scalan.Sym, s2: => scalan.Sym, s3: => scalan.Sym): Unit =
      emitF(name, () => s1, () => s2, () => s3)
    def emit(s1: => scalan.Sym): Unit = emitF(testName, () => s1)
    def emit(s1: => scalan.Sym, s2: => scalan.Sym): Unit =
      emitF(testName, () => s1, () => s2)
    def emit(s1: => scalan.Sym, s2: => scalan.Sym, s3: => scalan.Sym): Unit =
      emitF(testName, () => s1, () => s2, () => s3)
  }
}

abstract class BaseCtxTests extends BaseTests with TestContexts

abstract class BaseNestedCtxTests extends BaseNestedTests with TestContexts

abstract class BaseShouldCtxTests extends BaseShouldTests with TestContexts