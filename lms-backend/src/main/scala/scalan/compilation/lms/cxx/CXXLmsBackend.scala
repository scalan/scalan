package scalan.compilation.lms.cxx

import java.io.PrintWriter

import scala.virtualization.lms.common._
import scalan.compilation.lms.{CoreLmsBackendBase, LmsBackend, LmsBackendFacade}

class CoreCXXLmsBackend extends CoreLmsBackendBase { self =>

  trait Codegen extends CLikeGenNumericOps
  with CLikeGenEqual
  with CLikeGenArrayOps
  with CLikeGenPrimitiveOps
  with CXXGenStruct
  with CXXGenFatArrayLoopsFusionOpt
  with LoopFusionOpt
  with CXXFatCodegen
  with CXXGenCastingOps
  with CXXGenIfThenElseFat
  with CLikeGenOrderingOps
  with CLikeGenBooleanOps
  with CXXCodegen
  {
    override val IR: self.type = self

    override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true
  }

  override val codegen = new Codegen {}
}
