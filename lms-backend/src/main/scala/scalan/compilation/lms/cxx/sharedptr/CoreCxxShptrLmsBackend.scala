package scalan.compilation.lms.cxx.sharedptr

import scala.virtualization.lms.common._
import scalan.compilation.lms.CoreLmsBackendBase

class CoreCxxShptrLmsBackend extends CoreLmsBackendBase { self =>

  trait Codegen extends CxxShptrCodegen
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
  with CxxShptrGenArrayOps
  with CxxShptrGenVariables
  with CxxShptrGenArrayBuilderOps
  with CxxShptrGenRangeOps
  with CLikeGenWhile
  with CLikeGenNumericOps
  {
    override val IR: self.type = self

    override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true
//
//    override def hashCode(): Int = super.hashCode()
  }

  override val codegen = new Codegen {}
}
