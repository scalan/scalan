package scalan
package compilation
package lms
package cxx
package sharedptr

import java.util.HashMap

import scala.lms.common._
import scalan.compilation.lms.common._

class CxxCoreCodegen[BackendCake <: LmsBackendFacade with JNILmsOpsExp with CxxMethodCallOpsExp](backend: BackendCake) extends BaseCodegen[BackendCake]
  with CxxShptrCodegen
  with CLikeGenEqual
  with CLikeGenPrimitiveOps
  with CLikeGenMathOps
  with CLikeGenBooleanOps
  with CxxShptrGenStruct
  with CxxShptrGenFatArrayLoopsFusionOpt
  with LoopFusionOpt
  with CxxShptrGenCastingOps
  with CxxShptrGenIfThenElseFat
  with CLikeGenOrderingOps
  with CxxShptrGenOrderingOps
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
  with CxxShptrGenHashMapOps
  with CxxShptrGenHashMapOpsExt
  with CxxShptrGenMethodCallOps {
    override val IR: BackendCake = backend
    import IR._

  override val kernelFileExt = "cxx"

  override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true
}

class CoreCxxShptrLmsBackend extends CoreLmsBackend with JNILmsOpsExp with CxxMethodCallOpsExp { self =>
  override val codegen = new CxxCoreCodegen[self.type](self)

  override def map_keys[K: Manifest, V: Manifest](map: Exp[HashMap[K, V]]): Exp[Array[K]] = {
    hashmap_keys_array(map)
  }
}
