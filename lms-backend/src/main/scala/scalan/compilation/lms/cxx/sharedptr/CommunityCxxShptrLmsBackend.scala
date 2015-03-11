package scalan.compilation.lms.cxx.sharedptr

import scalan.compilation.lms.CommunityLmsBackendBase
import scalan.compilation.lms.common.CxxShptrGenVectorOps

class CommunityCxxShptrLmsBackend  extends CoreCxxShptrLmsBackend with CommunityLmsBackendBase { self =>
  override val codegen = new Codegen with CxxShptrGenVectorOps/* with CxxShptrGenSystemOps*/ {
    override val IR: self.type = self
  }
}
