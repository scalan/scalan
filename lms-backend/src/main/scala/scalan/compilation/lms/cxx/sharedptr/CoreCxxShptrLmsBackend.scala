package scalan
package compilation
package lms
package cxx
package sharedptr

import scala.virtualization.lms.common._
import scalan.compilation.lms.common._

class CxxCodegen [BackendCake <: LmsBackendFacade with JNILmsOpsExp ](backend: BackendCake) extends BaseCodegen[BackendCake]
with CxxShptrCodegen
with CxxShptrGenPointer //it use PointerLmsOpsExp
with CxxShptrGenVectorOps
with CLikeGenEqual
with CLikeGenPrimitiveOps
with CxxShptrGenStruct
with CxxShptrGenFatArrayLoopsFusionOpt
with LoopFusionOpt
with CxxShptrGenCastingOps
with CxxShptrGenIfThenElseFat
with CLikeGenOrderingOps
with CLikeGenBooleanOps
with CxxShptrGenFunctions
//  with CxxShptrGenArrayOps
  with CxxShptrGenArrayOpsBoost
  with CxxShptrGenVariables
  with CxxShptrGenArrayBuilderOps
  with CxxShptrGenArrayOpsExt
  with CxxShptrGenRangeOps
  with CLikeGenWhile
  with CLikeGenNumericOps
  with CxxShptrGenListOps
  with CxxShptrGenLstOps
  with CxxShptrGenJNIExtractor
  with CxxShptrGenStringOps
  with CxxShptrGenEitherOps
  with CxxShptrGenIterableOps
  {
    override val IR: BackendCake = backend
    import IR._

  //def codeExtension: String = "cxx" -
  override val kernelFileExt = "cxx"


  override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true
  //
  //    override def hashCode(): Int = super.hashCode()
}


class CoreCxxShptrLmsBackend extends CoreLmsBackendBase with JNILmsOpsExp with PointerLmsOpsExp { self =>

  override val codegen = new CxxCodegen[self.type](self)  {}
}
