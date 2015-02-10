package scalan
package compilation
package lms

import java.io.File

trait LmsCompiler extends Compiler { self: ScalanCtxExp =>

  def makeBridge: LmsBridge

  def emitSource[A, B](sourcesDir: File, extension: String, functionName: String, graph: PGraph, eInput: Elem[A], eOutput: Elem[B]): File = {
    val bridge = makeBridge
    val graph1 = graph.asInstanceOf[bridge.scalan.PGraph]
    val eInput1 = eInput.asInstanceOf[bridge.scalan.Elem[A]]
    val eOutput1 = eOutput.asInstanceOf[bridge.scalan.Elem[B]]
    bridge.emitSource(sourcesDir, extension, functionName, graph1, eInput1, eOutput1)
  }
}
