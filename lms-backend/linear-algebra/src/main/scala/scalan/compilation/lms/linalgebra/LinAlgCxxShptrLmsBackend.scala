package scalan.compilation.lms.linalgebra

import scalan.compilation.lms.cxx.sharedptr.{CxxCoreCodegen, CoreCxxShptrLmsBackend}

class LinAlgCxxShptrLmsBackend extends CoreCxxShptrLmsBackend with VectorOpsExp { self =>
  override val codegen = new CxxCoreCodegen[self.type](self) with CxxShptrGenVectorOps {
    def mappings = self.mappings
  }
}
