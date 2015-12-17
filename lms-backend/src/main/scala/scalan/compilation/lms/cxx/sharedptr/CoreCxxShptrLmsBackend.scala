package scalan
package compilation
package lms
package cxx
package sharedptr

import scala.lms.common._
import scalan.compilation.lms.common._

class CxxCoreCodegen[BackendCake <: LmsBackendFacade with JNILmsOpsExp with PointerLmsOpsExp](backend: BackendCake) extends BaseCodegen[BackendCake]
  with CxxShptrCodegen
  with CxxShptrGenPointer // uses PointerLmsOpsExp
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

  override val kernelFileExt = "cxx"


  override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true
  //
  //    override def hashCode(): Int = super.hashCode()
}

class CoreCxxShptrLmsBackend extends CoreLmsBackend with JNILmsOpsExp with PointerLmsOpsExp { self =>
  override val codegen = new CxxCoreCodegen[self.type](self)
}

class CommunityCxxShptrLmsBackend extends CoreCxxShptrLmsBackend with VectorOpsExp { self =>
  override val codegen = new CxxCoreCodegen[self.type](self) with CxxShptrGenVectorOps
}
