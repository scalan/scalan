package scalan.compilation

import java.io.File

import scalan.ScalanExp
import scalan.staged.BaseExp

trait Compiler extends BaseExp with Passes {
  self: ScalanExp with GraphVizExport =>

  type CompilerConfig

  def defaultCompilerConfig: CompilerConfig

  type CompilationOutput

  // see comment for buildInitialGraph
  // TODO sequence may depend on input or intermediate graphs, use a state monad instead
  def graphPasses(config: CompilerConfig): Seq[PGraph => GraphPass]

  // Can it return ProgramGraph[Ctx] for some other Ctx?
  // If so, may want to add Ctx as type argument or type member
  def buildInitialGraph[A, B](func: Exp[A => B])(config: CompilerConfig): PGraph =
    new PGraph(func)

  def buildGraph[A, B](sourcesDir: File, functionName: String, func: Exp[A => B], emitGraphs: Boolean)(config: CompilerConfig): PGraph = {
    val g0 = buildInitialGraph(func)(config)
    if (emitGraphs) {
      val dotFile = new File(sourcesDir, s"$functionName.dot")
      emitDepGraph(g0, dotFile)
    }

    val passes = graphPasses(config)

    val numPassesLength = passes.length.toString.length

    passes.zipWithIndex.foldLeft(g0) { case (graph, (passFunc, index)) =>
      val pass = passFunc(graph)
      val graph1 = pass(graph)

      if (emitGraphs) {
        val indexStr = (index + 1).toString
        val dotFileName = s"${functionName}_${"0" * (numPassesLength - indexStr.length) + indexStr}_${pass.name}.dot"
        emitDepGraph(graph1, new File(sourcesDir, dotFileName))
      }

      graph1
    }
  }

  def buildExecutable[A, B](sourcesDir: File, executableDir: File, functionName: String, func: Exp[A => B], emitGraphs: Boolean)
                           (implicit config: CompilerConfig): CompilationOutput = {
    sourcesDir.mkdirs()
    executableDir.mkdirs()
    val eFunc = func.elem
    val graph = buildGraph(sourcesDir, functionName, func, emitGraphs)(config)
    doBuildExecutable(sourcesDir, executableDir, functionName, graph, emitGraphs)(config, eFunc.eDom, eFunc.eRange)
  }

  def buildExecutable[A, B](sourcesAndExecutableDir: File, functionName: String, func: Exp[A => B], emitGraphs: Boolean)
                           (implicit config: CompilerConfig): CompilationOutput =
    buildExecutable(sourcesAndExecutableDir, sourcesAndExecutableDir, functionName, func, emitGraphs)(config)

  protected def doBuildExecutable[A, B](sourcesDir: File, executableDir: File, functionName: String, graph: PGraph, emitGraphs: Boolean)
                                       (config: CompilerConfig, eInput: Elem[A], eOutput: Elem[B]): CompilationOutput

  // func is passed to enable inference of B and to get types if needed
  def execute[A, B](compilationOutput: CompilationOutput, functionName: String, input: A, func: Exp[A => B])
                   (implicit config: CompilerConfig): B = {
    val eFunc = func.elem
    doExecute(compilationOutput, functionName, input)(config, eFunc.eDom, eFunc.eRange)
  }

  protected def doExecute[A, B](compilationOutput: CompilationOutput, functionName: String, input: A)
                               (config: CompilerConfig, eInput: Elem[A], eOutput: Elem[B]): B
}
