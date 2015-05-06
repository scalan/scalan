package scalan.compilation

import java.io.File

import scalan.ScalanExp

/**
 * Trivial compiler used for tests
 */
trait DummyCompiler extends ScalanExp with Compiler with GraphVizExport {
  type CompilerConfig = Unit

  def defaultCompilerConfig = ()

  type CustomCompilerOutput = Unit

  override def graphPasses(compilerConfig: CompilerConfig) = Seq.empty[PGraph => GraphPass]

  protected def doBuildExecutable[A, B](sourcesDir: File, executableDir: File, functionName: String, graph: PGraph, graphVizConfig: GraphVizConfig)
                                       (compilerConfig: CompilerConfig, eInput: Elem[A], eOutput: Elem[B]) = ()

  protected def doExecute[A, B](compilerOutput: CompilerOutput[A, B], input: A): B =
    !!!("Dummy compiler doesn't produce an executable")
}

/**
 * Adds some passes to DummyCompiler
 */
trait DummyCompilerWithPasses extends DummyCompiler {
  override def graphPasses(compilerConfig: CompilerConfig) = Seq(AllUnpackEnabler, AllInvokeEnabler)
}