package scalan.compilation

import java.io.File

import scalan.Scalan

/**
 * Trivial compiler used for tests
 */
class DummyCompiler[ScalanCake <: Scalan](_scalan: ScalanCake) extends Compiler(_scalan) {
  import scalan._

  type CompilerConfig = Unit

  def defaultCompilerConfig = ()

  type CustomCompilerOutput = Unit

  override def graphPasses(compilerConfig: CompilerConfig) = Seq.empty[PassBuilder[GraphPass]]

  protected def doBuildExecutable[A, B](sourcesDir: File, executableDir: File, functionName: String, graph: PGraph, graphVizConfig: GraphVizConfig)
                                       (compilerConfig: CompilerConfig, eInput: Elem[A], eOutput: Elem[B]) = ()

  protected def doExecute[A, B](compilerOutput: CompilerOutput[A, B], input: A): B =
    !!!("Dummy compiler doesn't produce an executable")
}

/**
 * Adds some passes to DummyCompiler
 */
class DummyCompilerWithPasses[ScalanCake <: Scalan](scalan: ScalanCake) extends DummyCompiler(scalan) {
  override def graphPasses(compilerConfig: CompilerConfig) = Seq(AllUnpackEnabler, AllInvokeEnabler)
}