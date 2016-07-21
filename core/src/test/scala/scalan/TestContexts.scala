package scalan

import java.lang.reflect.Method

import scalan.compilation.{GraphVizConfig, Compiler}
import scalan.util.FileUtil

trait TestContexts extends TestsUtil {
  protected[this] def stage(scalan: ScalanExp)(testName: String, name: String, sfs: Seq[() => scalan.Exp[_]]): Unit = {
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

  abstract class TestContext(testName: String) extends ScalanDslExp {
    def this() = this(currentTestNameAsFileName)

    override val invokeAll = true
    override def isInvokeEnabled(d: Def[_], m: Method) = invokeAll
    override def shouldUnpack(e: Elem[_]) = true

    // workaround for non-existence of by-name repeated parameters
    def emitF(name: String, sfs: (() => Exp[_])*): Unit = stage(this)(testName, name, sfs)
    def emit(name: String, s1: => Exp[_]): Unit = emitF(name, () => s1)
    def emit(name: String, s1: => Exp[_], s2: Exp[_]*): Unit = {
      emitF(name, () => s1)
      s2.foreach(s => emitF(name, () => s))
    }
    def emitMany(name: String, ss: Exp[_]*): Unit = {
      emitF(name, ss.map((s: Rep[_]) => () => s): _*)
    }
//    def emit(name: String, s1: => Exp[_], s2: => Exp[_], s3: => Exp[_]): Unit = emitF(name, () => s1, () => s2, () => s3)
    def emit(s1: => Exp[_]): Unit = emitF(testName, () => s1)
    def emit(s1: => Exp[_], s2: Exp[_]*): Unit = {
      emitF(testName, () => s1)
      s2.foreach(s => emitF(testName, () => s))
    }
//    def emit(s1: => Exp[_], s2: => Exp[_], s3: => Exp[_]): Unit = emitF(testName, () => s1, () => s2, () => s3)
  }

  // TODO change API to use defaultCompilers here! See JNI_MsfItTests and others
  abstract class TestCompilerContext(testName: String) {
    def this() = this(currentTestNameAsFileName)

    val compiler: Compiler[_ <: ScalanExp]
    import compiler._

    def test[A,B](functionName: String, f: => scalan.Exp[A => B]): CompilerOutput[A, B] = {
      buildExecutable(FileUtil.file(prefix + "/" + testName, functionName), functionName, f, GraphVizConfig.default)(defaultCompilerConfig)
    }
    def test[A,B](f: => scalan.Exp[A => B]): CompilerOutput[A, B] = test(testName, f)

    // workaround for non-existence of by-name repeated parameters
    def emitF(name: String, sfs: (() => scalan.Exp[_])*): Unit = stage(scalan)(testName, name, sfs)
    def emit(name: String, s1: => scalan.Exp[_]): Unit = emitF(name, () => s1)
    def emit(name: String, s1: => scalan.Exp[_], s2: => scalan.Exp[_]): Unit =
      emitF(name, () => s1, () => s2)
    def emit(name: String, s1: => scalan.Exp[_], s2: => scalan.Exp[_], s3: => scalan.Exp[_]): Unit =
      emitF(name, () => s1, () => s2, () => s3)
    def emit(s1: => scalan.Exp[_]): Unit = emitF(testName, () => s1)
    def emit(s1: => scalan.Exp[_], s2: => scalan.Exp[_]): Unit =
      emitF(testName, () => s1, () => s2)
    def emit(s1: => scalan.Exp[_], s2: => scalan.Exp[_], s3: => scalan.Exp[_]): Unit =
      emitF(testName, () => s1, () => s2, () => s3)
  }
}

abstract class BaseCtxTests extends BaseTests with TestContexts

abstract class BaseNestedCtxTests extends BaseNestedTests with TestContexts

abstract class BaseShouldCtxTests extends BaseShouldTests with TestContexts