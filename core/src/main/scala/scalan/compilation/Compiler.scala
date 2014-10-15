package scalan.compilation

import java.io.File

import scalan.ScalanExp
import scalan.staged.BaseExp

trait Compiler extends BaseExp with Passes {
  self: ScalanExp with GraphVizExport =>

  type Config

  def defaultConfig: Config

  // see comment for buildInitialGraph
  def graphPasses(config: Config): Seq[PGraph => GraphPass]

  // Can it return ProgramGraph[Ctx] for some other Ctx?
  // If so, may want to add Ctx as type argument or type member
  def buildInitialGraph[A, B](func: Exp[A => B])(config: Config): PGraph =
    new PGraph(func)

  def buildGraph[A, B](sourcesDir: File, functionName: String, func: Exp[A => B], emitGraphs: Boolean)(config: Config): PGraph = {
    val g0 = buildInitialGraph(func)(config)
    if (emitGraphs) {
      val dotFile = new File(sourcesDir, s"$functionName.dot")
      emitDepGraph(func, dotFile, false)
    }

    val passes = graphPasses(config)

    val numPassesLength = passes.length.toString.length

    passes.zipWithIndex.foldLeft(g0) { case (graph, (passFunc, index)) =>
      val pass = passFunc(graph)
      val graph1 = pass(graph)

      if (emitGraphs) {
        val indexStr = (index + 1).toString
        val dotFileName = s"${functionName}_${"0" * (numPassesLength - indexStr.length) + indexStr}_${pass.name}.dot"
        emitDepGraph(graph1.roots, new File(sourcesDir, dotFileName), false)
      }

      graph1
    }
  }

  def buildExecutable[A, B](sourcesDir: File, executableDir: File, functionName: String, func: Exp[A => B], emitGraphs: Boolean)
                           (implicit config: Config = defaultConfig) {
    sourcesDir.mkdirs()
    executableDir.mkdirs()
    val eFunc = func.elem
    val graph = buildGraph(sourcesDir, functionName, func, emitGraphs)(config)
    doBuildExecutable(sourcesDir, executableDir, functionName, graph, emitGraphs)(config, eFunc.eDom, eFunc.eRange)
  }

  def buildExecutable[A, B](sourcesAndExecutableDir: File, functionName: String, func: Exp[A => B], emitGraphs: Boolean)
                           (implicit config: Config = defaultConfig): Unit =
    buildExecutable(sourcesAndExecutableDir, sourcesAndExecutableDir, functionName, func, emitGraphs)(config)

  protected def doBuildExecutable[A, B](sourcesDir: File, executableDir: File, functionName: String, graph: PGraph, emitGraphs: Boolean)
                                       (config: Config, eInput: Elem[A], eOutput: Elem[B])

  // func is passed to enable inference of B and to get types if needed
  def execute[A, B](executableDir: File, functionName: String, input: A, func: Exp[A => B])
                   (implicit config: Config = defaultConfig): B = {
    val eFunc = func.elem
    doExecute(executableDir, functionName, input)(config, eFunc.eDom, eFunc.eRange)
  }

  protected def doExecute[A, B](executableDir: File, functionName: String, input: A)
                               (config: Config, eInput: Elem[A], eOutput: Elem[B]): B
}
