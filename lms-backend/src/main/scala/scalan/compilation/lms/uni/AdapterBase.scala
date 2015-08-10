package scalan.compilation.lms.uni

import scalan.ScalanCtxExp

/**
 * Created by adel on 6/8/15.
 */
abstract class AdapterBase [ScalanCake <: ScalanCtxExp](val scalanIR: ScalanCake) {

  def adapt[A, B](func: scalanIR.Exp[A => B]): scalanIR.Exp[_]

}

