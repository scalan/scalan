package scalan.it

import scalan._
import scalan.compilation.{GraphVizConfig, Compiler}
import scalan.util.FileUtil
import scalan.util.FileUtil.file

// extracted so it can be used with different suite styles
trait ItTestsUtil[Prog <: Scalan] extends TestsUtil {
  override def testOutDir = "it-out"

  // can be overridden
  def defaultGraphVizConfig = GraphVizConfig.default

  type ProgCompiler = Compiler[_ <: Prog with ScalanCtxExp]

  class CompilerWithConfig private (val compiler: ProgCompiler)(_config: ProgCompiler#CompilerConfig) {
    def config = _config.asInstanceOf[compiler.CompilerConfig]
  }
  object CompilerWithConfig {
    def apply(compiler: ProgCompiler)(config: compiler.CompilerConfig) = new CompilerWithConfig(compiler)(config)
  }

  implicit def compilerWithDefaultConfig(compiler: ProgCompiler): CompilerWithConfig =
    CompilerWithConfig(compiler)(compiler.defaultCompilerConfig)

  def cwc(compiler: ProgCompiler)(compilerConfig: compiler.CompilerConfig) =
    CompilerWithConfig(compiler)(compilerConfig)

  def defaultCompilers: Seq[CompilerWithConfig]
  val progSeq: Prog with ScalanSeq

  /** Utility method to be used when defining [[defaultCompilers]]. */
  def compilers(cs: CompilerWithConfig*) = cs

  def sourceDir(functionName: String) =
    file(prefix, functionName)

  def assertFileContentCheck(name: String): Unit =
    FileUtil.read(file(prefix, name)) should be(FileUtil.read(file(prefix, name + ".check")))

  def buildGraphs[A, B](f: Prog => Prog#Rep[A => B],
                        compilers: Seq[CompilerWithConfig] = defaultCompilers,
                        graphVizConfig: GraphVizConfig = defaultGraphVizConfig,
                        functionName: String = currentTestNameAsFileName) =
    defaultCompilers.foreach { cwc =>
      cwc.compiler.buildGraph(sourceDir(functionName), functionName,
        f(cwc.compiler.scalan).asInstanceOf[cwc.compiler.Exp[A => B]], graphVizConfig)(cwc.config)
    }

  def compileSource[A, B](f: Prog => Prog#Rep[A => B],
                          compilers: Seq[CompilerWithConfig] = defaultCompilers,
                          graphVizConfig: GraphVizConfig = defaultGraphVizConfig,
                          functionName: String = currentTestNameAsFileName) =
    defaultCompilers.map { cwc =>
      cwc.compiler.buildExecutable(sourceDir(functionName), functionName,
        f(cwc.compiler.scalan).asInstanceOf[cwc.compiler.Exp[A => B]], graphVizConfig)(cwc.config)
    }

  def getStagedOutput[A, B](f: Prog => Prog#Rep[A => B],
                            compilers: Seq[CompilerWithConfig] = defaultCompilers,
                            graphVizConfig: GraphVizConfig = defaultGraphVizConfig,
                            functionName: String = currentTestNameAsFileName)(inputs: A*) = {
    val compiled = compilers.map { cwc =>
      val fExp = f(cwc.compiler.scalan).asInstanceOf[cwc.compiler.Exp[A => B]]
      val out = cwc.compiler.buildExecutable(sourceDir(functionName), functionName, fExp, graphVizConfig)(cwc.config)
      (cwc.compiler, out)
    }

    inputs.map { input =>
      compiled.map { case (compiler, out) =>
        compiler.execute(out.asInstanceOf[compiler.CompilerOutput[A, B]], input)
      }
    }
  }

  def compareOutputWithSequential[A, B](f: Prog => Prog#Rep[A => B],
                                        compilers: Seq[CompilerWithConfig] = defaultCompilers,
                                        graphVizConfig: GraphVizConfig = defaultGraphVizConfig,
                                        functionName: String = currentTestNameAsFileName)(inputs: A*) = {
    val fSeq = f(progSeq).asInstanceOf[A => B]
    val inputsOutputs = inputs.map { x => (x, fSeq(x)) }
    compareOutputWithExpected(f, compilers, graphVizConfig, functionName)(inputsOutputs: _*)
  }

  def compareOutputWithExpected[A, B](f: Prog => Prog#Rep[A => B],
                                      compilers: Seq[CompilerWithConfig] = defaultCompilers,
                                      graphVizConfig: GraphVizConfig = defaultGraphVizConfig,
                                      functionName: String = currentTestNameAsFileName)(inputsOutputs: (A, B)*) = {
    val compiled = compilers.map { cwc =>
      val fExp = f(cwc.compiler.scalan).asInstanceOf[cwc.compiler.Exp[A => B]]
      val out = cwc.compiler.buildExecutable(sourceDir(functionName), functionName, fExp, graphVizConfig)(cwc.config)
      (cwc.compiler, out)
    }

    for {
      (input, expected) <- inputsOutputs
      (compiler, out) <- compiled
    } {
      val output = compiler.execute(out.asInstanceOf[compiler.CompilerOutput[A, B]], input)
      assert(output === expected, s"Compiler: $compiler, input: $input, expected: $expected, got: $output")
    }
  }

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
  // TODO still used in LmsMSTItTests
  // @deprecated("Use overload taking compilers instead", "0.2.11")
  def getStagedOutput[S <: Scalan](back: Compiler[S with ScalanCtxExp]) = new GetStagedOutput[S, back.type](back)

  @deprecated("Use getStagedOutput with f: S => S#Rep[A => B] instead", "0.2.10")
  def getStagedOutputConfig[A, B](back: Compiler[_ <: ScalanCtxExp])(f: back.scalan.Exp[A => B], functionName: String, input: A, compilerConfig: back.CompilerConfig): B = {
    val compiled = compileSource(back)(f, functionName, compilerConfig)
    back.execute(compiled, input)
  }

  final class CompileSource[S <: Scalan, Back <: Compiler[S with ScalanCtxExp]](val back: Back) {
    def apply[A, B](f: S => S#Rep[A => B], functionName: String, compilerConfig: back.CompilerConfig = back.defaultCompilerConfig): back.CompilerOutput[A, B] = {
      back.buildExecutable(sourceDir(functionName), functionName, f(back.scalan).asInstanceOf[back.Exp[A => B]], defaultGraphVizConfig)(compilerConfig)
    }

    @deprecated("Use the overload taking f: S => S#Rep[A => B] instead", "0.2.10")
    def apply[A, B](f: back.scalan.Exp[A => B], functionName: String, compilerConfig: back.CompilerConfig) : back.CompilerOutput[A, B] = {
      back.buildExecutable(sourceDir(functionName), functionName, f, defaultGraphVizConfig)(compilerConfig)
    }
  }

  // TODO still used in UniCompilerItTests
  // @deprecated("Use overload taking compilers instead", "0.2.11")
  def compileSource[S <: Scalan](back: Compiler[S with ScalanCtxExp]) = new CompileSource[S, back.type](back)

  implicit def defaultComparator[A](expected: A, actual: A): Unit = {
    actual should equal(expected)
  }

  @deprecated("Use overload taking compilers instead", "0.2.11")
  final class CompareOutputWithSequential[S <: Scalan, Back <: Compiler[S with ScalanCtxExp]](val back: Back, forth: S with ScalanCtxSeq) {
    def apply[A, B](f: S => S#Rep[A => B], functionName: String, input: A, compilerConfig: back.CompilerConfig = back.defaultCompilerConfig)
                   (implicit comparator: (B, B) => Unit) = {
      compareOutputWithExpected[S](back)(f(forth).asInstanceOf[A => B](input), f, functionName, input, compilerConfig)
    }
  }
  @deprecated("Use overload taking compilers instead", "0.2.11")
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

  @deprecated("Use overload taking compilers instead", "0.2.11")
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
  @deprecated("Use overload taking compilers instead", "0.2.11")
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
