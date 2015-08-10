package scalan.compilation.lms.uni

import scalan.{JNIExtractorOpsExp, ScalanCtxExp}

/**
 * Created by adel on 6/8/15.
 */
class AdapterScala2Cxx[ScalanCake <: ScalanCtxExp with JNIExtractorOpsExp](sc: ScalanCake)
  extends AdapterBase [ScalanCake](sc){

  import scalanIR._

  def adapt[A, B](func: Exp[A => B]): Exp[_] = JNI_Wrap(func)

}
