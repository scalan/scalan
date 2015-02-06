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
  def graphPasses(compilerConfig: CompilerConfig): Seq[PGraph => GraphPass]

  // Can it return ProgramGraph[Ctx] for some other Ctx?
  // If so, may want to add Ctx as type argument or type member
  def buildInitialGraph[A, B](func: Exp[A => B])(compilerConfig: CompilerConfig): PGraph =
    new PGraph(func)

  def buildGraph[A, B](sourcesDir: File, functionName: String, func: Exp[A => B], graphVizConfig: GraphVizConfig)(compilerConfig: CompilerConfig): PGraph = {
    val g0 = buildInitialGraph(func)(compilerConfig)
    val dotFile = new File(sourcesDir, s"$functionName.dot")
    emitDepGraph(g0, dotFile)(graphVizConfig)

    val passes = graphPasses(compilerConfig)

    val numPassesLength = passes.length.toString.length

    passes.zipWithIndex.foldLeft(g0) { case (graph, (passFunc, index)) =>
      val pass = passFunc(graph)
      val graph1 = pass(graph)

      val indexStr = (index + 1).toString
      val dotFileName = s"${functionName}_${"0" * (numPassesLength - indexStr.length) + indexStr}_${pass.name}.dot"
      emitDepGraph(graph1, new File(sourcesDir, dotFileName))(graphVizConfig)

      graph1
    }
  }

  def buildExecutable[A, B](sourcesDir: File, executableDir: File, functionName: String, func: Exp[A => B], graphVizConfig: GraphVizConfig)
                           (implicit compilerConfig: CompilerConfig): CompilationOutput = {
    sourcesDir.mkdirs()
    executableDir.mkdirs()
    val eFunc = func.elem
    val graph = buildGraph(sourcesDir, functionName, func, graphVizConfig)(compilerConfig)
    doBuildExecutable(sourcesDir, executableDir, functionName, graph, graphVizConfig)(compilerConfig, eFunc.eDom, eFunc.eRange)
  }

  def buildExecutable[A, B](sourcesAndExecutableDir: File, functionName: String, func: Exp[A => B], graphVizConfig: GraphVizConfig)
                           (implicit compilerConfig: CompilerConfig): CompilationOutput =
    buildExecutable(sourcesAndExecutableDir, sourcesAndExecutableDir, functionName, func, graphVizConfig)(compilerConfig)

  protected def doBuildExecutable[A, B](sourcesDir: File, executableDir: File, functionName: String, graph: PGraph, graphVizConfig: GraphVizConfig)
                                       (compilerConfig: CompilerConfig, eInput: Elem[A], eOutput: Elem[B]): CompilationOutput

  // func is passed to enable inference of B and to get types if needed
  def execute[A, B](compilationOutput: CompilationOutput, functionName: String, input: A, func: Exp[A => B])
                   (implicit compilerConfig: CompilerConfig): B = {
    val eFunc = func.elem
    doExecute(compilationOutput, functionName, input)(compilerConfig, eFunc.eDom, eFunc.eRange)
  }

  protected def doExecute[A, B](compilationOutput: CompilationOutput, functionName: String, input: A)
                               (compilerConfig: CompilerConfig, eInput: Elem[A], eOutput: Elem[B]): B
}
