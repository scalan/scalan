package scalan.compilation

import scalan.ScalanDslExp

abstract class ScalanCompiler[+ScalanCake <: ScalanDslExp, +Codegen <: BaseCodegen[ScalanCake]](_scalan: ScalanCake)
    extends Compiler[ScalanCake](_scalan) {
  def codegen: Codegen
}
