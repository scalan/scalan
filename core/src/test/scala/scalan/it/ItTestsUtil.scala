package scalan.it

import org.scalatest.{Matchers, Suite}

import scalan._
import scalan.compilation.{GraphVizConfig, Compiler}
import scalan.util.FileUtil
import scalan.util.FileUtil.file

// extracted so it can be used with different suite styles
trait ItTestsUtil extends TestsUtil { self: Suite with Matchers =>
  override def testOutDir = "it-out"

  // can be overridden
  def graphVizConfig = GraphVizConfig.default

  def assertFileContentCheck(name: String): Unit =
    FileUtil.read(file(prefix, name)) should be(FileUtil.read(file(prefix, name + ".check")))

  // Note: deprecated API will be removed before next release (0.2.11 or 0.3.0)

  final class GetStagedOutput[S <: Scalan, Back <: Compiler[S with ScalanCtxExp]](val back: Back) {
    def apply[A, B](f: S => S#Rep[A => B], functionName: String, input: A, compilerConfig: back.CompilerConfig = back.defaultCompilerConfig): B = {
      val compiled = compileSource[S](back)(f, functionName, compilerConfig)
      back.execute(compiled, input)
    }
    @deprecated("Use the overload taking f: S => S#Rep[A => B] instead", "0.2.10")
    def apply[A, B](f: back.scalan.Exp[A => B], functionName: String, input: A): B =
      getStagedOutputConfig(back)(f, functionName, input, back.defaultCompilerConfig)
  }
  def getStagedOutput[S <: Scalan](back: Compiler[S with ScalanCtxExp]) = new GetStagedOutput[S, back.type](back)

  @deprecated("Use getStagedOutput with f: S => S#Rep[A => B] instead", "0.2.10")
  def getStagedOutputConfig[A, B](back: Compiler[_ <: ScalanCtxExp])(f: back.scalan.Exp[A => B], functionName: String, input: A, compilerConfig: back.CompilerConfig): B = {
    val compiled = compileSource(back)(f, functionName, compilerConfig)
    back.execute(compiled, input)
  }

  final class CompileSource[S <: Scalan, Back <: Compiler[S with ScalanCtxExp]](val back: Back) {
    def apply[A, B](f: S => S#Rep[A => B], functionName: String, compilerConfig: back.CompilerConfig = back.defaultCompilerConfig): back.CompilerOutput[A, B] = {
      back.buildExecutable(file(prefix, functionName), functionName, f(back.scalan).asInstanceOf[back.Exp[A => B]], graphVizConfig)(compilerConfig)
    }

    @deprecated("Use the overload taking f: S => S#Rep[A => B] instead", "0.2.10")
    def apply[A, B](f: back.scalan.Exp[A => B], functionName: String, compilerConfig: back.CompilerConfig) : back.CompilerOutput[A, B] = {
      back.buildExecutable(file(prefix, functionName), functionName, f, graphVizConfig)(compilerConfig)
    }
  }
  def compileSource[S <: Scalan](back: Compiler[S with ScalanCtxExp]) = new CompileSource[S, back.type](back)

  implicit def defaultComparator[A](expected: A, actual: A): Unit = {
    actual should equal(expected)
  }

  final class CompareOutputWithSequential[S <: Scalan, Back <: Compiler[S with ScalanCtxExp]](val back: Back, forth: S with ScalanCtxSeq) {
    def apply[A, B](f: S => S#Rep[A => B], functionName: String, input: A, compilerConfig: back.CompilerConfig = back.defaultCompilerConfig)
                   (implicit comparator: (B, B) => Unit) = {
      compareOutputWithExpected[S](back)(f(forth).asInstanceOf[A => B](input), f, functionName, input, compilerConfig)
    }
  }
  def compareOutputWithSequential[S <: Scalan](back: Compiler[S with ScalanCtxExp], forth: S with ScalanCtxSeq) = new CompareOutputWithSequential[S, back.type](back, forth)

  @deprecated("Use the overload taking f: S => S#Rep[A => B] instead", "0.2.10")
  def compareOutputWithSequential[A, B](back: Compiler[_ <: ScalanCtxExp])
                                       (fSeq: A => B, f: back.scalan.Exp[A => B], functionName: String, input: A)
                                       (implicit comparator: (B, B) => Unit) {
    compareOutputWithSequentialConfig(back)(fSeq, f, functionName, input, back.defaultCompilerConfig)
  }

  @deprecated("Use compareOutputWithSequential with f: S => S#Rep[A => B] instead", "0.2.10")
  def compareOutputWithSequentialConfig[A, B](back: Compiler[_ <: ScalanCtxExp])
                                             (fSeq: A => B, f: back.scalan.Exp[A => B], functionName: String, input: A, config: back.CompilerConfig)
                                             (implicit comparator: (B, B) => Unit) {
    compareOutputWithExpectedConfig(back)(fSeq(input), f, functionName, input, config)
  }

  final class CompareOutputWithExpected[S <: Scalan, Back <: Compiler[S with ScalanCtxExp]](val back: Back) {
    def apply[A, B](expected: B, f: S => S#Rep[A => B], functionName: String, input: A, compilerConfig: back.CompilerConfig = back.defaultCompilerConfig)
                   (implicit comparator: (B, B) => Unit) = {
      val actual = getStagedOutput[S](back)(f, functionName, input, compilerConfig)
      comparator(expected, actual)
    }

    @deprecated("Use the overload taking f: S => S#Rep[A => B] instead", "0.2.10")
    def apply[A, B](expected: B, f: back.scalan.Exp[A => B], functionName: String, input: A)
                   (implicit comparator: (B, B) => Unit) = {
      compareOutputWithExpectedConfig(back)(expected, f, functionName, input, back.defaultCompilerConfig)
    }
  }
  def compareOutputWithExpected[S <: Scalan](back: Compiler[S with ScalanCtxExp]) = new CompareOutputWithExpected[S, back.type](back)

  @deprecated("Use compareOutputWithExpected with f: S => S#Rep[A => B] instead", "0.2.10")
  def compareOutputWithExpectedConfig[A, B](back: Compiler[_ <: ScalanCtxExp])
                                           (expected: B, f: back.scalan.Exp[A => B], functionName: String, input: A, config: back.CompilerConfig)
                                           (implicit comparator: (B, B) => Unit) {
    val actual = getStagedOutputConfig(back)(f, functionName, input, config)
    comparator(expected, actual)
  }

  def untyped[S <: Scalan](f: S => S#Rep[_ => _]) = f.asInstanceOf[S => S#Rep[Any => Any]]
}
