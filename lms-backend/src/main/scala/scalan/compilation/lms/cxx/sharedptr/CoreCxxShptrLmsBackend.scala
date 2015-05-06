package scalan
package compilation
package lms
package cxx
package sharedptr

import scala.virtualization.lms.common._
import scalan.compilation.lms.common.{CxxShptrGenEitherOps, CxxShptrGenJNIExtractor, CxxShptrGenLstOps, JNILmsOpsExp}

class CoreCxxShptrLmsBackend extends CoreLmsBackendBase with JNILmsOpsExp { self =>  //code for ArrayDotProdSparse defined in emitNode in CxxShptrGenVectorOps

  trait Codegen extends BaseCodegen[self.type]
  with CxxShptrCodegen
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
    override val IR: self.type = self

    override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true
//
//    override def hashCode(): Int = super.hashCode()
  }

  override val codegen = new Codegen {}
}
