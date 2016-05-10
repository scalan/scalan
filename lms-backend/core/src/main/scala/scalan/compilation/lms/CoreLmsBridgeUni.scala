package scalan.compilation.lms

import scalan.compilation.lms.common.{JNILmsOps, JNILmsOpsExp}
import scalan.primitives.AbstractStringsDslExp
import scalan.util.StringUtil
import scalan.{ScalanDslExp, JNIExtractorOpsExp}

trait JNILmsBridge extends CoreLmsBridge {
  override val scalan: ScalanDslExp with JNIExtractorOpsExp with AbstractStringsDslExp
  import scalan._

  val lms: CoreLmsBackend with JNILmsOpsExp

  registerElemClass[JNITypeElem[_], JNILmsOps#JNIType[_]]
  registerElemClass[JNIArrayElem[_], JNILmsOps#JNIArray[_]]

  override protected def lmsMethodName(d: Def[_], primitiveName: String): String = d match {
    case _: JNI_GetFieldID => "jni_get_field_id"
    case _: JNI_GetMethodID => "jni_get_method_id"
    case _ if primitiveName.startsWith("JNI_") =>
      val parts = primitiveName.stripPrefix("JNI_").split("(?<=.)(?=\\p{Lu})").map(StringUtil.lowerCaseFirst)
      "jni_" + parts.mkString("_")
    case _ => super.lmsMethodName(d, primitiveName)
  }

  override protected def transformDef[T](m: LmsMirror, g: AstGraph, sym: Exp[T], d: Def[T]) = d match {
    case res@ExpCString(Def(s: Const[_])) =>
      val exp = lms.JNIStringConst(s.x)
      m.addSym(sym, exp)
    case _ => super.transformDef(m, g, sym, d)
  }
}

trait CoreLmsBridgeUni extends CoreLmsBridge with JNILmsBridge