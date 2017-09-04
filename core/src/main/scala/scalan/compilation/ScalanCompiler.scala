package scalan.compilation

import scalan.ScalanDsl

abstract class ScalanCompiler[+ScalanCake <: ScalanDsl, +Codegen <: BaseCodegen[ScalanCake]](_scalan: ScalanCake)
    extends Compiler[ScalanCake](_scalan) {
  def codegen: Codegen
}
