package scalan.compilation.lms.uni

import scalan.{JNIExtractorOpsExp, ScalanCtxExp}
import scalan.compilation.lms.{JNIBridge, LmsBackendFacade}
import scalan.compilation.lms.common.JNILmsOpsExp
import scalan.compilation.lms.cxx.sharedptr.CxxCodegen

/**
 * Created by adel on 6/8/15.
 */
class AdapterScala2Cxx[ScalanCake <: ScalanCtxExp with JNIExtractorOpsExp with JNIBridge](sc: ScalanCake)
  extends AdapterBase [ScalanCake](sc){

  override val ScalanIR: ScalanCake = sc
  import ScalanIR._

  def adapt[A, B](func: Exp[A => B]): Exp[_] = {
    func match {
      case Def(lam: Lambda[a, b]) => {
        implicit val eA = lam.eA.asElem[a]
        implicit val eB = lam.eB.asElem[b]

        val res = fun[JNIType[a], JNIType[b]] { arg: Rep[JNIType[a]] =>
          val data = JNI_Extract(arg)
          val res = lam.self(data)
          JNI_Pack(res) (eB) // (lam.eB???)
        } //(Lazy(eJNIA))
        res
      }
      case _ => !!! ("Error in AdapterScala2Cxx.adapt: function was not mached to 'Def(lam: Lambda[a, b])'")
    }

  }

}
