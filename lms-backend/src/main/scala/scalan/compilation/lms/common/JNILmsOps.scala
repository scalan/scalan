package scalan.compilation.lms.common

import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.GenerationFailedException
import scalan.compilation.lms.cxx.CXXCodegen


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

trait CXXGenJNIExtractor extends CXXCodegen {
  val IR: JNILmsOpsExp
  import IR._

  trait JObject[T]
  def jObjectManifest[T: Manifest]( m: Manifest[T]) = Manifest.classType(classOf[JObject[_]], m)

  override def traverseStm(stm: Stm): Unit = {
    stm match {
      case TP(sym,rhs) =>
        if (sym.tp.runtimeClass.isArray && sym.tp.typeArguments(0) <:< Manifest.AnyVal) // array of primitives should be moved
          moveableSyms += sym
        else if (sym.tp.runtimeClass == classOf[JNIArray[_]])
          moveableSyms += sym
        else if (syms(rhs).find(moveableSyms.contains) != None) // derived from moveable
          moveableSyms += sym
      case _ =>
        ()
    }
    super.traverseStm(stm)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case FindClass(className) =>
      emitValDef(quote(sym), norefManifest(sym.tp), s"env->FindClass(${quote(className)})")
    case ExtractPrimitive(x) =>
      emitValDef(quote(sym), norefManifest(sym.tp), s"static_cast<${remap(norefManifest(sym.tp))}>(${quote(x)})")
    case ExtractPrimitiveArray(x) =>
      emitValDef(quote(sym), norefManifest(sym.tp), s"${remap(sym.tp)}(env, ${quote(x)})")
    case GetArrayLength(x) =>
      emitValDef(quote(sym), sym.tp, s"env->GetArrayLength(${quote(x)})")
    case ExtractObjectArray(x) =>
      emitValDef(quote(sym), norefManifest(sym.tp), s"${quote(x)} /*ExtractObjectArray: sym.tp=${sym.tp}*/")
    case GetObjectArrayItem(x, i) =>
      emitValDef(quote(sym), norefManifest(sym.tp), s"static_cast<${remapObject(sym.tp)}>(env->GetObjectArrayElement(${quote(x)}, ${quote(i)}))")
    case GetObjectClass(x) =>
      emitValDef(quote(sym), norefManifest(sym.tp), s"env->GetObjectClass(${quote(x)})")
    case GetFieldID(clazz, fn, sig) =>
      emitValDef(quote(sym), norefManifest(sym.tp), s"env->GetFieldID(${quote(clazz)}, ${quote(fn)}, ${quote(sig)})")
    case GetObjectFieldValue(fid, x) =>
      emitValDef(quote(sym), norefManifest(jObjectManifest(sym.tp)), s"static_cast<${remap(jObjectManifest(sym.tp))}>(env->GetObjectField(${quote(x)}, ${quote(fid)})) /*GetObjectField: sym.tp=${sym.tp}*/")
    case res@GetPrimitiveFieldValue(fid, x) =>
      val funName = s"Get${res.tp.toString}Field"
      emitValDef(quote(sym), norefManifest(sym.tp), s"static_cast<${remap(norefManifest(sym.tp))}>(env->${funName}(${quote(x)}, ${quote(fid)}))")
    case _ =>
      super.emitNode(sym, rhs)
  }

  override def quote(x: Exp[Any]) = {
    x match {
      case JNIStringConst(str) =>
        "\""+str.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n")+"\""
      case _ =>
        super.quote(x)
    }
  }

  override def remap[A](m: Manifest[A]): String = {
    m.runtimeClass match {
      case c if c == classOf[JNIArray[_]] =>
        val ts = remap(m.typeArguments(0))
        s"jni_array<${ts}>"
      case c if c == classOf[JNIClass] => "jclass"
      case c if c == classOf[JNIFieldID] => "jfieldID"
      case c if c == classOf[JObject[_]] => remapObject(m.typeArguments(0))
      case c if c.isArray =>
        m.typeArguments(0) match {
          case mItem if mItem.runtimeClass == classOf[JNIType[_]] => remapSimpleType( mItem.typeArguments(0) ) + "Array"
          case _ => super.remap(m)
        }
      case c if c == classOf[JNIType[_]] =>
        m.typeArguments(0) match {
          case mT if mT.runtimeClass.isArray => remapSimpleType( mT.typeArguments(0) ) + "Array"
          case mT => remapSimpleType(mT)
        }
      case _ =>
        super.remap(m)
    }
  }

  private def remapSimpleType[A](m: Manifest[A]): String = m match {
    case Manifest.Byte => "jbyte"
    case Manifest.Int => "jint"
    case Manifest.Double => "jdouble"
    case _ if m <:< Manifest.AnyRef => "jobject"
    case _ =>
      throw new GenerationFailedException(s"CXXGenJNIExtractor.remapSimpleType(m) : Type ${m} cannot be remapped.")
  }

  private def remapObject[A](m: Manifest[A]): String = m match {
    case _ if m.runtimeClass == classOf[JNIType[_]] => m.typeArguments(0) match {
      case mT if mT <:< Manifest.AnyVal => "jobject"
      case _ => remap(m)
    }
    case _ if m.runtimeClass.isArray => m.typeArguments(0) match {
      case mItem if mItem.runtimeClass == classOf[JNIType[_]] =>
        remap(m)
      case _ =>
        throw new GenerationFailedException(s"CXXGenJNIExtractor.remapObject(m) : Type ${m} cannot be remapped to jobject.")
    }
    case _ =>
      throw new GenerationFailedException(s"CXXGenJNIExtractor.remapObject(m) : Type ${m} cannot be remapped to jobject.")
  }
}
