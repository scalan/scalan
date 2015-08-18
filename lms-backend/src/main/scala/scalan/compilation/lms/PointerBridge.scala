package scalan.compilation.lms

import scalan.compilation.lms.common.{PointerLmsOps, PointerLmsOpsExp}
import scalan.{ScalanCtxExp, PointerOpsExp}

trait PointerBridge extends CoreBridge {
  override val scalan: ScalanCtxExp with PointerOpsExp
  import scalan._

  val lms: CoreLmsBackend with PointerLmsOpsExp

  override protected def lmsMethodName(d: Def[_], primitiveName: String): String = d match {
    // for consistency; will be overridden in CoreBridge to "arrayPtr" otherwise
    case _: ArrayPtr[_] => "array_ptr"
    case _ => super.lmsMethodName(d, primitiveName)
  }

  override def createManifest[T](elem: Elem[T]): Manifest[_] = elem match {
    case ScalarElem(eItem) =>
      Manifest.classType(classOf[PointerLmsOps#Scalar[_]], createManifest(eItem))
    case PointerElem(eItem) =>
      Manifest.classType(classOf[PointerLmsOps#Pointer[_]], createManifest(eItem))
    case el =>
      super.createManifest(el)
  }
}
