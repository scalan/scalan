package scalan.compilation.lms

import scalan.compilation.lms.common.{PointerLmsOps, PointerLmsOpsExp}
import scalan.ScalanDslExp
import scalan.pointers.PointerOpsExp

trait PointerBridge extends CoreBridge {
  override val scalan: ScalanDslExp with PointerOpsExp
  import scalan._

  val lms: CoreLmsBackend with PointerLmsOpsExp

  override protected def lmsMethodName(d: Def[_], primitiveName: String): String = d match {
    // for consistency; will be overridden in CoreBridge to "arrayPtr" otherwise
    case _: ArrayPtr[_] => "array_ptr"
    case _ => super.lmsMethodName(d, primitiveName)
  }

  registerElemClass[ScalarElem[_, _], PointerLmsOps#Scalar[_]]
  registerElemClass[PointerElem[_, _], PointerLmsOps#Scalar[_]]
}
