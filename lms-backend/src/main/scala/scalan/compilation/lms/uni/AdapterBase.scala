package scalan.compilation.lms.uni

import scala.tools.cmd.gen.Codegen
import scalan.ScalanCtxExp
import scalan.compilation.lms.{LmsBackendFacade, BaseCodegen}
import scalan.compilation.lms.common.JNILmsOpsExp

/**
 * Created by adel on 6/8/15.
 */
abstract class AdapterBase [ScalanCake <: ScalanCtxExp](scalan: ScalanCake) {
  val ScalanIR: ScalanCake = scalan
  import ScalanIR._

  def adapt[A, B](func: Exp[A => B]): Exp[_]

}

