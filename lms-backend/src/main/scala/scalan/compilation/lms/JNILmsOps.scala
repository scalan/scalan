package scalan.compilation.lms

import scala.reflect.SourceContext
import scala.virtualization.lms.common._


/**
 * Created by zotov on 12/27/14.
 */
trait JNILmsOps extends Base {
  trait JNIType[T]
  trait JNIClass[T]
  trait JNIFieldID[T]
}

trait JNILmsOpsExp extends JNILmsOps with BaseExp {
  case class JNIStringConst(x: String) extends Exp[String]

  case class ExtractPrimitive[T](x: Rep[JNIType[T]]) extends Def[T]
  case class ExtractPrimitiveArray[T](x: Rep[JNIType[Array[T]]]) extends Def[Array[T]]
  case class GetArrayLength[T](x: Rep[JNIType[Array[T]]]) extends Def[Int]
  case class ExtractObjectArray[T](x: Rep[JNIType[Array[T]]]) extends Def[Array[JNIType[T]]]
  case class GetObjectArrayItem[T](x: Rep[Array[JNIType[T]]], i: Rep[Int]) extends Def[JNIType[T]]
  case class GetObjectClass[T](x: Rep[JNIType[T]]) extends Def[JNIClass[T]]
  case class GetFieldID[A, T](x: Rep[JNIClass[T]], fn: Rep[String], sig: Rep[String]) extends Def[JNIFieldID[A]]
  case class GetObjectFieldValue[A, T](fid: Rep[JNIFieldID[A]], x: Rep[JNIType[T]]) extends Def[JNIType[A]]
  case class GetPrimitiveFieldValue[A, T](fid: Rep[JNIFieldID[A]], x: Rep[JNIType[T]]) extends Def[A]

  def jni_extract_primitive[T: Manifest](x: Rep[JNIType[T]]): Rep[T] = ExtractPrimitive(x)
  def jni_extract_primitive_array[T: Manifest](x: Rep[JNIType[Array[T]]]): Rep[Array[T]] = ExtractPrimitiveArray(x)
  def jni_get_array_length[T: Manifest](x: Rep[JNIType[Array[T]]]): Rep[Int] = GetArrayLength(x)
  def jni_extract_object_array[T: Manifest](x: Rep[JNIType[Array[T]]]): Rep[Array[JNIType[T]]] = ExtractObjectArray(x)
  def jni_get_object_array_item[T: Manifest](x: Rep[Array[JNIType[T]]], i: Rep[Int]): Rep[JNIType[T]] = GetObjectArrayItem(x, i)
  def jni_get_object_class[T: Manifest](x: Rep[JNIType[T]]): Rep[JNIClass[T]] = GetObjectClass(x)
  def jni_get_field_id[A: Manifest, T: Manifest](x: Rep[JNIClass[T]], fn: Rep[String], sig: Rep[String]): Rep[JNIFieldID[A]] = GetFieldID[A,T](x, fn, sig)
  def jni_get_object_field_value[A: Manifest, T: Manifest](fid: Rep[JNIFieldID[A]], x: Rep[JNIType[T]]): Rep[JNIType[A]] = GetObjectFieldValue(fid, x)
  def jni_get_primitive_field_value[A: Manifest, T: Manifest](fid: Rep[JNIFieldID[A]], x: Rep[JNIType[T]]): Rep[A] = GetPrimitiveFieldValue(fid, x)

  override
  def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case GetObjectArrayItem(x, i) =>
      x.tp.typeArguments(0).typeArguments(0) match {
        case(mA: Manifest[a_t]) =>
          jni_get_object_array_item[a_t](f(x).asInstanceOf[Exp[Array[JNIType[a_t]]]], f(i))(mA)
      }
    case res@ExtractPrimitiveArray(x) =>
      x.tp.typeArguments(0).typeArguments(0) match {
        case(mA: Manifest[a_t]) =>
          jni_extract_primitive_array[a_t](f(x).asInstanceOf[Exp[JNIType[Array[a_t]]]])(mA)
      }
    case res@GetObjectClass(x) =>
      x.tp.typeArguments(0) match {
        case(mA: Manifest[a_t]) =>
          jni_get_object_class[a_t](f(x).asInstanceOf[Exp[JNIType[a_t]]])(mA)
      }
    case res@GetObjectFieldValue(fid, x) =>
      (fid.tp.typeArguments(0), x.tp.typeArguments(0)) match {
        case(mA: Manifest[a_t], mT: Manifest[t_t]) =>
          jni_get_object_field_value[a_t,t_t](f(fid).asInstanceOf[Exp[JNIFieldID[a_t]]], f(x).asInstanceOf[Exp[JNIType[t_t]]])(mA,mT)
      }
    case res@GetPrimitiveFieldValue(fid, x) =>
      (fid.tp.typeArguments(0), x.tp.typeArguments(0)) match {
        case (mA: Manifest[a_t], mT: Manifest[t_t]) =>
          jni_get_primitive_field_value[a_t,t_t](f(fid).asInstanceOf[Exp[JNIFieldID[a_t]]], f(x).asInstanceOf[Exp[JNIType[t_t]]])(mA,mT)
      }
    case res@GetFieldID(clazz, fn, sig) =>
      (res.tp.typeArguments(0), clazz.tp.typeArguments(0)) match {
        case (mA: Manifest[a_t], mT: Manifest[t_t]) =>
          jni_get_field_id[a_t,t_t](f(clazz).asInstanceOf[Exp[JNIClass[t_t]]], f(fn), f(sig))(mA,mT)
      }
    case _ =>
      super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}
