package scalan.compilation

import scalan.Scalan

abstract class ScalanCompiler[+ScalanCake <: Scalan, +Codegen <: FileCodegen[ScalanCake]](_scalan: ScalanCake)
    extends Compiler[ScalanCake](_scalan) {
  def codegen: Codegen
}
