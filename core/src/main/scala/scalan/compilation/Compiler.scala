package scalan.compilation

import java.io.File

import scalan.ScalanCtxExp

abstract class Compiler[+ScalanCake <: ScalanCtxExp](val scalan: ScalanCake) extends Passes {
  import scalan._

  type CompilerConfig

  def defaultCompilerConfig: CompilerConfig

  type CustomCompilerOutput

  case class CommonCompilerOutput[A, B](executableDir: File, name: String, eInput: Elem[A], eOutput: Elem[B])

  case class CompilerOutput[A, B](common: CommonCompilerOutput[A, B], custom: CustomCompilerOutput, config: CompilerConfig)

  // see comment for buildInitialGraph
  // TODO sequence may depend on input or intermediate graphs, use a state monad instead
  def graphPasses(compilerConfig: CompilerConfig): Seq[PGraph => GraphPass] = Seq()

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
      val graph1 = pass(graph).withoutContext

      val indexStr = (index + 1).toString
      val dotFileName = s"${functionName}_${"0" * (numPassesLength - indexStr.length) + indexStr}_${pass.name}.dot"
      emitDepGraph(graph1, new File(sourcesDir, dotFileName))(graphVizConfig)

      graph1
    }
  }

  def buildExecutable[A, B](sourcesDir: File, executableDir: File, functionName: String, func: Exp[A => B], graphVizConfig: GraphVizConfig)
                           (implicit compilerConfig: CompilerConfig): CompilerOutput[A, B] = {
    sourcesDir.mkdirs()
    executableDir.mkdirs()
    val graph = buildGraph(sourcesDir, functionName, func, graphVizConfig)(compilerConfig)
    val eFunc = func.elem
    val eInput = eFunc.eDom
    val eOutput = eFunc.eRange
    val customOutput = doBuildExecutable(sourcesDir, executableDir, functionName, graph, graphVizConfig)(compilerConfig, eInput, eOutput)
    val commonOutput = CommonCompilerOutput(executableDir, functionName, eInput, eOutput)
    CompilerOutput(commonOutput, customOutput, compilerConfig)
  }

  def buildExecutable[A, B](sourcesAndExecutableDir: File, functionName: String, func: Exp[A => B], graphVizConfig: GraphVizConfig)
                           (implicit compilerConfig: CompilerConfig): CompilerOutput[A, B] = {
    buildExecutable(sourcesAndExecutableDir, sourcesAndExecutableDir, functionName, func, graphVizConfig)(compilerConfig)
  }
  protected def doBuildExecutable[A, B](sourcesDir: File, executableDir: File, functionName: String, graph: PGraph, graphVizConfig: GraphVizConfig)
                                       (compilerConfig: CompilerConfig, eInput: Elem[A], eOutput: Elem[B]): CustomCompilerOutput

  // func is passed to enable inference of B and to get types if needed
  def execute[A, B](compilerOutput: CompilerOutput[A, B], input: A): B = {
    doExecute(compilerOutput, input)
  }

  protected def doExecute[A, B](compilerOutput: CompilerOutput[A, B], input: A): B
}
