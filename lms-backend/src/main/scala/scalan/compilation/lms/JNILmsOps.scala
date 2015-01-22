package scalan.compilation.lms

import scala.reflect.SourceContext
import scala.virtualization.lms.common._


/**
 * Created by zotov on 12/27/14.
 */
trait JNILmsOps extends Base {
  trait JNIType[T]
  trait JNIClass
  trait JNIFieldID
  trait JNIArray[T]
}

trait JNILmsOpsExp extends JNILmsOps with BaseExp {
  case class JNIStringConst(x: String) extends Exp[String]

  case class FindClass[T](className: Rep[String]) extends Def[JNIClass]
  case class ExtractPrimitive[T](x: Rep[JNIType[T]]) extends Def[T]
  case class ExtractPrimitiveArray[T](x: Rep[JNIType[Array[T]]]) extends Def[JNIArray[T]]
  case class GetArrayLength[T](x: Rep[JNIType[Array[T]]]) extends Def[Int]
  case class ExtractObjectArray[T](x: Rep[JNIType[Array[T]]]) extends Def[Array[JNIType[T]]]
  case class GetObjectArrayItem[T](x: Rep[Array[JNIType[T]]], i: Rep[Int]) extends Def[JNIType[T]]
  case class GetObjectClass[T](x: Rep[JNIType[T]]) extends Def[JNIClass]
  case class GetFieldID(clazz: Rep[JNIClass], fn: Rep[String], sig: Rep[String]) extends Def[JNIFieldID]
  case class GetObjectFieldValue[A, T](fid: Rep[JNIFieldID], x: Rep[JNIType[T]]) extends Def[JNIType[A]]
  case class GetPrimitiveFieldValue[A: Manifest, T](fid: Rep[JNIFieldID], x: Rep[JNIType[T]]) extends Def[A] {
    val tp = manifest[A]
  }

  def jni_extract_primitive[T: Manifest](x: Rep[JNIType[T]]): Rep[T] = ExtractPrimitive(x)
  def jni_extract_primitive_array[T: Manifest](x: Rep[JNIType[Array[T]]]): Rep[JNIArray[T]] = ExtractPrimitiveArray(x)
  def jni_get_array_length[T: Manifest](x: Rep[JNIType[Array[T]]]): Rep[Int] = GetArrayLength(x)
  def jni_extract_object_array[T: Manifest](x: Rep[JNIType[Array[T]]]): Rep[Array[JNIType[T]]] = ExtractObjectArray(x)
  def jni_get_object_array_item[T: Manifest](x: Rep[Array[JNIType[T]]], i: Rep[Int]): Rep[JNIType[T]] = GetObjectArrayItem(x, i)
  def jni_get_object_class[T: Manifest](x: Rep[JNIType[T]]): Rep[JNIClass] = GetObjectClass(x)
  def jni_find_class(className: Rep[String]): Rep[JNIClass] = FindClass(className)
  def jni_get_field_id(clazz: Rep[JNIClass], fn: Rep[String], sig: Rep[String]): Rep[JNIFieldID] = GetFieldID(clazz, fn, sig)
  def jni_get_object_field_value[A: Manifest, T: Manifest](fid: Rep[JNIFieldID], x: Rep[JNIType[T]]): Rep[JNIType[A]] = GetObjectFieldValue[A,T](fid, x)
  def jni_get_primitive_field_value[A: Manifest, T: Manifest](fid: Rep[JNIFieldID], x: Rep[JNIType[T]]): Rep[A] = GetPrimitiveFieldValue[A,T](fid, x)

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
      (res.tp.typeArguments(0), x.tp.typeArguments(0)) match {
        case(mA: Manifest[a_t], mT: Manifest[t_t]) =>
          jni_get_object_field_value[a_t,t_t](f(fid), f(x).asInstanceOf[Exp[JNIType[t_t]]])(mA,mT)
      }
    case res@GetPrimitiveFieldValue(fid, x) =>
      (res.tp, x.tp.typeArguments(0)) match {
        case (mA: Manifest[a_t], mT: Manifest[t_t]) =>
          jni_get_primitive_field_value[a_t,t_t](f(fid), f(x).asInstanceOf[Exp[JNIType[t_t]]])(mA,mT)
      }
    case res@GetFieldID(clazz, fn, sig) =>
      jni_get_field_id(f(clazz), f(fn), f(sig))
    case _ =>
      super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}
