package scalan.compilation.lms.uni

import scalan.ScalanDslExp

/**
 * Created by adel on 6/8/15.
 */
abstract class AdapterBase [ScalanCake <: ScalanDslExp](val scalanIR: ScalanCake) {

  def adapt[A, B](func: scalanIR.Exp[A => B]): scalanIR.Exp[_]

}

