package scalan.compilation

import java.io.File

import scalan.ScalanExp
import scalan.staged.BaseExp

trait Backend extends BaseExp {
  self: ScalanExp =>

  type Config

  def defaultConfig: Config

  def buildExecutable[A, B](sourcesDir: File, executableDir: File, functionName: String, func: Exp[A => B], emitGraphs: Boolean)
                           (implicit config: Config = defaultConfig) {
    sourcesDir.mkdirs()
    executableDir.mkdirs()
    val eFunc = func.elem
    doBuildExecutable(sourcesDir, executableDir, functionName, func, emitGraphs)(config, eFunc.eDom, eFunc.eRange)
  }

  def buildExecutable[A, B](sourcesAndExecutableDir: File, functionName: String, func: Exp[A => B], emitGraphs: Boolean)
                           (implicit config: Config = defaultConfig): Unit =
    buildExecutable(sourcesAndExecutableDir, sourcesAndExecutableDir, functionName, func, emitGraphs)(config)

  protected def doBuildExecutable[A, B](sourcesDir: File, executableDir: File, functionName: String, func: Exp[A => B], emitGraphs: Boolean)
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
