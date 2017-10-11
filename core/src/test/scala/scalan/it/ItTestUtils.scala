package scalan.it

import scalan._
import scalan.compilation.{GraphVizConfig, Compiler}
import scalan.util.FileUtil
import scalan.util.FileUtil.file

// extracted so it can be used with different suite styles
trait ItTestUtils[Prog <: Scalan] extends TestUtils {
  override def testOutDir = "it-out"

  // can be overridden
  def defaultGraphVizConfig = GraphVizConfig.default

  type ProgCompiler = Compiler[_ <: Prog with Scalan]

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
  val progStd: Prog

  /** Utility method to be used when defining [[defaultCompilers]]. */
  def compilers(cs: CompilerWithConfig*) = cs

  private def sourceDir(functionName: String) =
    file(prefix, functionName)

  // gives each compiler a subfolder of sourceDir(functionName) to make them unique
  private def compilersWithSourceDirs(compilers: Seq[CompilerWithConfig], functionName: String, deleteBaseDir: Boolean = true) = {
    val baseDir = sourceDir(functionName)
    if (deleteBaseDir) {
      FileUtil.deleteIfExist(baseDir)
    }
    compilers match {
      case Seq(onlyCompiler) =>
        Seq(onlyCompiler -> baseDir)
      case _ => compilers.zipWithIndex.map {
        case (cwc, index) => (cwc, file(baseDir, s"${index + 1}_${cwc.compiler.name}"))
      }
    }
  }

  def assertFileContentCheck(name: String): Unit =
    FileUtil.read(file(prefix, name)) should be(FileUtil.read(file(prefix, name + ".check")))

  def buildGraphs[A, B](f: Prog => Prog#Rep[A => B],
                        compilers: Seq[CompilerWithConfig] = defaultCompilers,
                        graphVizConfig: GraphVizConfig = defaultGraphVizConfig,
                        functionName: String = currentTestNameAsFileName) =
    compilersWithSourceDirs(compilers, functionName).foreach { case (cwc, dir) =>
      cwc.compiler.buildGraph(dir, functionName,
        f(cwc.compiler.scalan).asInstanceOf[cwc.compiler.Exp[A => B]], graphVizConfig)(cwc.config)
    }

  def compileSource[A, B](f: Prog => Prog#Rep[A => B],
                          compilers: Seq[CompilerWithConfig] = defaultCompilers,
                          graphVizConfig: GraphVizConfig = defaultGraphVizConfig,
                          functionName: String = currentTestNameAsFileName) =
    compilersWithSourceDirs(compilers, functionName).map { case (cwc, dir) =>
      cwc.compiler.buildExecutable(dir, functionName,
        f(cwc.compiler.scalan).asInstanceOf[cwc.compiler.Exp[A => B]], graphVizConfig)(cwc.config)
    }

  def getStagedOutput[A, B](f: Prog => Prog#Rep[A => B],
                            compilers: Seq[CompilerWithConfig] = defaultCompilers,
                            graphVizConfig: GraphVizConfig = defaultGraphVizConfig,
                            functionName: String = currentTestNameAsFileName)(inputs: A*) = {
    val compiled = compilersWithSourceDirs(compilers, functionName).map { case (cwc, dir) =>
      val out = cwc.compiler.buildExecutable(dir, functionName,
        f(cwc.compiler.scalan).asInstanceOf[cwc.compiler.Exp[A => B]],
        graphVizConfig)(cwc.config)
      (cwc.compiler, out)
    }

    inputs.map { input =>
      compiled.map { case (compiler, out) =>
        compiler.execute(out.asInstanceOf[compiler.CompilerOutput[A, B]], input)
      }
    }
  }

  def compareOutputWithStd[A, B](f: Prog => Prog#Rep[A => B],
                                 compilers: Seq[CompilerWithConfig] = defaultCompilers,
                                 graphVizConfig: GraphVizConfig = defaultGraphVizConfig,
                                 functionName: String = currentTestNameAsFileName)(inputs: A*) = {
//    val fStd = f(progStd).asInstanceOf[A => B]
    val expectedOutputs = List()//inputs.map { x => (x, fStd(x)) }
    compareOutputWithExpected(f, compilers, graphVizConfig, functionName)(expectedOutputs: _*)
  }

  def compareOutputWithExpected[A, B](f: Prog => Prog#Rep[A => B],
                                      compilers: Seq[CompilerWithConfig] = defaultCompilers,
                                      graphVizConfig: GraphVizConfig = defaultGraphVizConfig,
                                      functionName: String = currentTestNameAsFileName)
                                     (expectedOutputs: (A, B)*) = {
    val compiled = compilersWithSourceDirs(compilers, functionName).map { case (cwc, dir) =>
      val out = cwc.compiler.buildExecutable(dir, functionName,
        f(cwc.compiler.scalan).asInstanceOf[cwc.compiler.Exp[A => B]], graphVizConfig)(cwc.config)
      (cwc.compiler, out)
    }

//    for {
//      (input, expected) <- expectedOutputs
//      (compiler, out_) <- compiled
//    } {
//      val out = out_.asInstanceOf[compiler.CompilerOutput[A, B]]
//      val output = compiler.execute(out, input)
//      assert(expected === output, s"Compiler: $compiler,\n input: $input,\n expected: $expected,\n got: $output")
//    }
  }
  // Note: deprecated API will be removed before next release (0.2.11 or 0.3.0)

  final class GetStagedOutput[S <: Scalan, Back <: Compiler[S with Scalan]](val back: Back) {
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
  def getStagedOutput[S <: Scalan](back: Compiler[S with Scalan]) = new GetStagedOutput[S, back.type](back)

  @deprecated("Use getStagedOutput with f: S => S#Rep[A => B] instead", "0.2.10")
  def getStagedOutputConfig[A, B](back: Compiler[_ <: Scalan])(f: back.scalan.Exp[A => B], functionName: String, input: A, compilerConfig: back.CompilerConfig): B = {
    val compiled = compileSource(back)(f, functionName, compilerConfig)
    back.execute(compiled, input)
  }

  final class CompileSource[S <: Scalan, Back <: Compiler[S with Scalan]](val back: Back) {
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
  def compileSource[S <: Scalan](back: Compiler[S with Scalan]) = new CompileSource[S, back.type](back)

  implicit def defaultComparator[A](expected: A, actual: A): Unit = {
    actual should equal(expected)
  }

  @deprecated("Use overload taking compilers instead", "0.2.11")
  final class CompareOutputWithSequential[S <: Scalan, Back <: Compiler[S with Scalan]](val back: Back, forth: S) {
    def apply[A, B](f: S => S#Rep[A => B], functionName: String, input: A, compilerConfig: back.CompilerConfig = back.defaultCompilerConfig)
                   (implicit comparator: (B, B) => Unit) = {
      compareOutputWithExpected[S](back)(f(forth).asInstanceOf[A => B](input), f, functionName, input, compilerConfig)
    }
  }
  @deprecated("Use overload taking compilers instead", "0.2.11")
  def compareOutputWithStd[S <: Scalan](back: Compiler[S with Scalan], forth: S) = new CompareOutputWithSequential[S, back.type](back, forth)

  @deprecated("Use the overload taking f: S => S#Rep[A => B] instead", "0.2.10")
  def compareOutputWithStd[A, B](back: Compiler[_ <: Scalan])
                                (fSeq: A => B, f: back.scalan.Exp[A => B], functionName: String, input: A)
                                (implicit comparator: (B, B) => Unit) {
    compareOutputWithSequentialConfig(back)(fSeq, f, functionName, input, back.defaultCompilerConfig)
  }

  @deprecated("Use compareOutputWithSequential with f: S => S#Rep[A => B] instead", "0.2.10")
  def compareOutputWithSequentialConfig[A, B](back: Compiler[_ <: Scalan])
                                             (fSeq: A => B, f: back.scalan.Exp[A => B], functionName: String, input: A, config: back.CompilerConfig)
                                             (implicit comparator: (B, B) => Unit) {
    compareOutputWithExpectedConfig(back)(fSeq(input), f, functionName, input, config)
  }

  @deprecated("Use overload taking compilers instead", "0.2.11")
  final class CompareOutputWithExpected[S <: Scalan, Back <: Compiler[S with Scalan]](val back: Back) {
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
  def compareOutputWithExpected[S <: Scalan](back: Compiler[S with Scalan]) = new CompareOutputWithExpected[S, back.type](back)

  @deprecated("Use compareOutputWithExpected with f: S => S#Rep[A => B] instead", "0.2.10")
  def compareOutputWithExpectedConfig[A, B](back: Compiler[_ <: Scalan])
                                           (expected: B, f: back.scalan.Exp[A => B], functionName: String, input: A, config: back.CompilerConfig)
                                           (implicit comparator: (B, B) => Unit) {
    val actual = getStagedOutputConfig(back)(f, functionName, input, config)
    comparator(expected, actual)
  }

  def untyped[S <: Scalan](f: S => S#Rep[_ => _]) = f.asInstanceOf[S => S#Rep[Any => Any]]
}
