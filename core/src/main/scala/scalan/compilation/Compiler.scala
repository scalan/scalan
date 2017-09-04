package scalan.compilation

import java.io.File

import scala.collection.JavaConverters._

import com.typesafe.config.Config

import scalan.ScalanDsl
import scalan.util.ReflectionUtil

abstract class Compiler[+ScalanCake <: ScalanDsl](val scalan: ScalanCake) extends Passes {
  import scalan._

  type CompilerConfig
  // TODO implicit Configs[CompilerConfig], defaultCompilerConfig/compilerConfigFrom should work like GraphVizConfig

  def defaultCompilerConfig: CompilerConfig
  def compilerConfigFrom(config: Config) = config.entrySet.iterator.asScala.foldLeft(defaultCompilerConfig) {
    case (configAcc, entry) => modifyConfig(entry.getKey, entry.getValue, configAcc)
  }
  // override to handle specific keys this config cares about
  protected def modifyConfig(key: String, value: Any, config: CompilerConfig): CompilerConfig = config

  type CustomCompilerOutput

  case class CommonCompilerOutput[A, B](graph: PGraph, sourcesDirectory: File, name: String, eInput: Elem[A], eOutput: Elem[B])

  case class CompilerOutput[A, B](common: CommonCompilerOutput[A, B], custom: CustomCompilerOutput, config: CompilerConfig)

  // see comment for buildInitialGraph
  // TODO sequence may depend on input or intermediate graphs, use a state monad instead
  def graphPasses(compilerConfig: CompilerConfig): Seq[PassBuilder[GraphPass]] = Seq()
  def onFinishGraphPasses() = {
    scalan.resetTesters()
  }
  // Can it return ProgramGraph[Ctx] for some other Ctx?
  // If so, may want to add Ctx as type argument or type member
  protected def buildInitialGraph[A, B](func: Exp[A => B])(compilerConfig: CompilerConfig): PGraph = {
    new PGraph(func)
  }

  def buildGraph[A, B](sourcesDir: File, functionName: String, func: => Exp[A => B], graphVizConfig: GraphVizConfig)(compilerConfig: CompilerConfig): CommonCompilerOutput[A, B] = {
    // G is PGraph with some extra information
    def emittingGraph[G](fileName: String, passName: String, emitMetadata: Boolean, toGraph: G => PGraph)(mkGraph: => G): G = {
      val graphVizConfig1 = graphVizConfig.copy(emitMetadata = emitMetadata)
      try {
        val g = mkGraph
        emitDepGraph(toGraph(g), sourcesDir, fileName)(graphVizConfig1)
        g
      } catch {
        case e: Exception =>
          val graphMsg = scalan.emitExceptionGraph(e, sourcesDir, fileName)(graphVizConfig1) match {
            case Some(graphFile) =>
              s"See ${graphFile.file.getAbsolutePath} for exception graph."
            case None =>
              s"No exception graph produced."
          }
          throw new CompilationException(s"Staging $functionName failed at pass $passName. $graphMsg", e)
      }
    }

    val (initialGraph, eInput, eOutput) = emittingGraph[(PGraph, Elem[A], Elem[B])](functionName, "Initial graph generation", false, _._1) {
      val func0 = func
      val eFunc = func0.elem
      (buildInitialGraph(func0)(compilerConfig), eFunc.eDom, eFunc.eRange)
    }

    val passes = graphPasses(compilerConfig)

    val numPassesLength = passes.length.toString.length

    val finalGraph = passes.zipWithIndex.foldLeft(initialGraph) { case (graph, (passFunc, index)) =>
      val pass = passFunc(graph)

      val indexStr = (index + 1).toString
      val dotFileName = s"${functionName}_${"0" * (numPassesLength - indexStr.length) + indexStr}_${pass.name}"
      val dotFileNameAna = dotFileName + "_analyzed"

      emittingGraph[PGraph](dotFileName, pass.name, false, g => g) {
        scalan.beginPass(pass)
        pass.clearMarkings(graph)
        pass.backwardAnalyse(graph)
        val analyzed = emittingGraph[PGraph](dotFileNameAna, pass.name, true, g => g)(graph)
        val graph1 = pass(graph).withoutContext
        scalan.endPass(pass)
        graph1
      }
    }
    onFinishGraphPasses()

    CommonCompilerOutput(finalGraph, sourcesDir, functionName, eInput, eOutput)
  }

  def buildExecutable[A, B](sourcesDir: File, executableDir: File, functionName: String, func: => Exp[A => B], graphVizConfig: GraphVizConfig)
                           (implicit compilerConfig: CompilerConfig): CompilerOutput[A, B] = {
    sourcesDir.mkdirs()
    executableDir.mkdirs()
    val commonOutput = buildGraph(sourcesDir, functionName, func, graphVizConfig)(compilerConfig)
    val customOutput = doBuildExecutable(sourcesDir, executableDir, functionName, commonOutput.graph, graphVizConfig)(compilerConfig, commonOutput.eInput, commonOutput.eOutput)
    CompilerOutput(commonOutput, customOutput, compilerConfig)
  }

  def buildExecutable[A, B](sourcesAndExecutableDir: File, functionName: String, func: => Exp[A => B], graphVizConfig: GraphVizConfig)
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

  lazy val name = ReflectionUtil.namedSuperclass(getClass).getSimpleName
}

class CompilationException(message: String, cause: Exception) extends RuntimeException(message, cause)
