package scalan.codegen.lms

import scalan.ScalanStaged
import scalan.codegen.LangBackend

trait LmsBackend extends LangBackend { self: ScalanStaged =>
  def run(dir: String, fileName: String, func: Exp[_], emitGraphs: Boolean) = ???
}
