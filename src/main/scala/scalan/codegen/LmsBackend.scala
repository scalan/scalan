package scalan.codegen

import scalan.ScalanStaged

trait LmsBackend extends LangBackend { self: ScalanStaged =>
  def run(dir: String, fileName: String, func: Exp[_], emitGraphs: Boolean) = ???
}
