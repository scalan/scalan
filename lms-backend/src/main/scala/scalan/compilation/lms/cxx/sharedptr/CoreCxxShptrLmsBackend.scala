package scalan.compilation.lms.cxx.sharedptr

import scala.virtualization.lms.common._
import scalan.compilation.lms.CoreLmsBackendBase
import scalan.compilation.lms.cxx._

class CoreCxxShptrLmsBackend extends CoreLmsBackendBase { self =>

  trait Codegen extends CxxShptrCodegen
  with CLikeGenEqual
  with CLikeGenArrayOps
  with CLikeGenPrimitiveOps
  with CxxShptrGenStruct
//  with CXXGenFatArrayLoopsFusionOpt
  with LoopFusionOpt
//  with CXXFatCodegen
//  with CXXGenCastingOps
//  with CXXGenIfThenElseFat
  with CLikeGenOrderingOps
  with CLikeGenBooleanOps
//  with CXXGenFunctions
  with CxxShptrGenArrayOps
//  with CXXGenVariables
//  with CXXGenArrayBuilderOps
//  with CXXGenRangeOps
  with CLikeGenWhile
  with CLikeGenNumericOps
  {
    override val IR: self.type = self

//    override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true
//
//    override def hashCode(): Int = super.hashCode()
  }

  override val codegen = new Codegen {}
}
