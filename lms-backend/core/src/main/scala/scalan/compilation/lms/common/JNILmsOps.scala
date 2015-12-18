package scalan.compilation.lms.common

import org.objectweb.asm.Type

import scala.reflect.{RefinedManifest, SourceContext}
import scala.lms.common._
import scala.lms.internal.{GenerationFailedException, GenericCodegen}
import scalan.compilation.lms.ManifestUtil
import scalan.compilation.lms.arrays.ArrayLoopsExp
import scalan.compilation.lms.cxx.sharedptr.CxxShptrCodegen

trait JNILmsOps extends Base {
  trait JNIType[T]
  trait JNIClass
  trait JNIFieldID
  trait JNIMethodID
  trait JNIArray[T]
}

trait JNILmsOpsExp extends JNILmsOps with LoopsFatExp with ArrayLoopsExp with StructExp with ManifestUtil {
  case class JNIStringConst(x: String) extends Exp[String]

  case class JNIArrayElem[T](x: Rep[JNIArray[T]], y: Block[T]) extends Def[JNIArray[T]]
  case class JNIArrayIfElem[T](x: Rep[JNIArray[T]], c: Exp[Boolean], y: Block[T]) extends Def[JNIType[Array[T]]]

  case class JArrayElem[T](x: Rep[JNIType[Array[T]]], y: Block[JNIType[T]]) extends Def[JNIType[Array[T]]]
  case class JArrayIfElem[T](x: Rep[JNIType[Array[T]]], c: Exp[Boolean], y: Block[JNIType[T]]) extends Def[JNIType[Array[T]]]

  case class FindClass[T](className: Rep[String]) extends Def[JNIClass]

  case class BoxPrimitive[T: Manifest](x: Rep[JNIType[T]]) extends Def[JNIType[T]] {
    require(manifest[T].isPrimitive, "(" + manifest[T] + " <:< " + Manifest.AnyVal + ") isn't true")
  }

  case class ExtractPrimitive[T](x: Rep[JNIType[T]]) extends Def[T]
  case class ExtractPrimitiveArray[T](x: Rep[JNIType[Array[T]]]) extends Def[JNIArray[T]]
  case class GetArrayLength[T](x: Rep[JNIType[Array[T]]]) extends Def[Int]
  case class ExtractObjectArray[T](x: Rep[JNIType[Array[T]]]) extends Def[Array[JNIType[T]]]
  case class GetObjectArrayItem[T](x: Rep[Array[JNIType[T]]], i: Rep[Int]) extends Def[JNIType[T]]
  case class GetObjectClass[T](x: Rep[JNIType[T]]) extends Def[JNIClass]
  case class GetFieldID(clazz: Rep[JNIClass], fn: Rep[String], sig: Rep[String]) extends Def[JNIFieldID]
  case class GetMethodID(clazz: Rep[JNIClass], mn: Rep[String], sig: Rep[String]) extends Def[JNIMethodID]
  case class GetObjectFieldValue[A, T](fid: Rep[JNIFieldID], x: Rep[JNIType[T]]) extends Def[JNIType[A]]
  case class GetPrimitiveFieldValue[A: Manifest, T](fid: Rep[JNIFieldID], x: Rep[JNIType[T]]) extends Def[A] {
    val tp = manifest[A]
  }

  case class NewPrimitiveArray[A: Manifest](len: Rep[Int]) extends Def[JNIType[Array[A]]] {
    require(manifest[A].isPrimitive, "(" + manifest[A] + " <:< " + Manifest.AnyVal + ") isn't true")
    val mA = manifest[A]
  }

  case class NewObjectArray[A: Manifest](len: Rep[Int], clazz: Rep[JNIClass]) extends Def[JNIType[Array[A]]] {
    require(manifest[A].isClass, "(" + manifest[A] + " <:< " + Manifest.AnyRef + ") isn't true")
    val mA = manifest[A]
  }

  case class ReturnFirstArg[A: Manifest](jArray: Rep[JNIType[A]], ignore: Rep[Any]*) extends Def[JNIType[A]]
  case class NewPrimitive[T: Manifest](x: Rep[T]) extends Def[JNIType[T]]
  case class NewObject[T: Manifest](clazz: Rep[JNIClass], mid: Rep[JNIMethodID], args: Rep[Any]*) extends Def[JNIType[T]]
  case class CallObjectMethod[R: Manifest, A: Manifest](x: Rep[JNIType[A]], mid: Rep[JNIMethodID], args: Rep[Any]*) extends Def[JNIType[R]]

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case JNIArrayElem(x,y) =>
      effectSyms(y)
    case JArrayElem(x,y) =>
      effectSyms(y)
    case _ =>
      super.boundSyms(e)
  }

  def jni_box_primitive[A: Manifest](x: Rep[JNIType[A]]): Rep[JNIType[A]] = BoxPrimitive[A](x)
  def jni_new_primitive[A: Manifest](x: Rep[A]): Rep[JNIType[A]] = NewPrimitive[A](x)
  def jni_new_primitive_array[A: Manifest](len: Rep[Int]): Rep[JNIType[Array[A]]] = NewPrimitiveArray[A](len)
  def jni_new_primitive_array_var[A: Manifest](len: Rep[Int]): Rep[JNIType[Array[A]]] = reflectMutable(NewPrimitiveArray[A](len))
  def jni_new_object_array[A: Manifest](len: Rep[Int], clazz: Rep[JNIClass]): Rep[JNIType[Array[A]]] = NewObjectArray[A](len, clazz)
  def jni_new_object_array_var[A: Manifest](len: Rep[Int], clazz: Rep[JNIClass]): Rep[JNIType[Array[A]]] = reflectMutable(NewObjectArray[A](len, clazz))
  def jni_extract_primitive[T: Manifest](x: Rep[JNIType[T]]): Rep[T] = ExtractPrimitive(x)
  def jni_extract_primitive_array[T: Manifest](x: Rep[JNIType[Array[T]]]): Rep[JNIArray[T]] = ExtractPrimitiveArray(x)
  def jni_extract_primitive_array_var[T: Manifest](x: Rep[JNIType[Array[T]]]): Rep[JNIArray[T]] = reflectMutable(ExtractPrimitiveArray(x))
  def jni_get_array_length[T: Manifest](x: Rep[JNIType[Array[T]]]): Rep[Int] = GetArrayLength(x)
  def jni_extract_object_array[T: Manifest](x: Rep[JNIType[Array[T]]]): Rep[Array[JNIType[T]]] = ExtractObjectArray(x)
  def jni_get_object_array_item[T: Manifest](x: Rep[Array[JNIType[T]]], i: Rep[Int]): Rep[JNIType[T]] = GetObjectArrayItem(x, i)
  def jni_get_object_class[T: Manifest](x: Rep[JNIType[T]]): Rep[JNIClass] = GetObjectClass(x)
  def jni_find_class(className: Rep[String]): Rep[JNIClass] = FindClass(className)
  def jni_get_field_id(clazz: Rep[JNIClass], fn: Rep[String], sig: Rep[String]): Rep[JNIFieldID] = GetFieldID(clazz, fn, sig)
  def jni_get_method_id(clazz: Rep[JNIClass], mn: Rep[String], sig: Rep[String]): Rep[JNIMethodID] = GetMethodID(clazz, mn, sig)
  def jni_get_object_field_value[A: Manifest, T: Manifest](fid: Rep[JNIFieldID], x: Rep[JNIType[T]]): Rep[JNIType[A]] = GetObjectFieldValue[A,T](fid, x)
  def jni_call_object_method[R: Manifest, A: Manifest](x: Rep[JNIType[A]], mid: Rep[JNIMethodID], args: Rep[Any]*): Rep[JNIType[R]] = CallObjectMethod[R,A](x,mid,args:_*)
  def jni_get_primitive_field_value[A: Manifest, T: Manifest](fid: Rep[JNIFieldID], x: Rep[JNIType[T]]): Rep[A] = GetPrimitiveFieldValue[A,T](fid, x)
  def jni_return_first_arg[A: Manifest](ret: Rep[JNIType[A]], ignore: Rep[Any]): Rep[JNIType[A]] = ReturnFirstArg[A](ret, ignore)

  def jni_map_primitive_array[A: Manifest, B: Manifest](a: Exp[Array[A]], f: Rep[A] => Rep[B]): Exp[JNIType[Array[B]]] = {
    val jArray = jni_new_primitive_array_var[B](a.length)
    val jniArray = jni_extract_primitive_array_var(jArray)
    val f1 = {i:Rep[Int] => f(a.at(i))}
    val x = fresh[Int]
    val y = reifyEffects(f1(x))
    val jnia = simpleLoop(a.length, x, JNIArrayElem(jniArray,y))
    ReturnFirstArg(jArray, jnia)
  }

  private def manifestToTypeDescriptor(m: Manifest[_]) = Type.getDescriptor(m.runtimeClass)

  def jni_map_object_array[A, B](a: Exp[Array[A]], f: Rep[A] => Rep[JNIType[B]])(implicit mA: Manifest[A], mB: Manifest[B]): Exp[JNIType[Array[B]]] = {
    val mA1 = if (mA.runtimeClass == classOf[JNIArray[_]])
      mA.typeArguments.head.arrayManifest
    else
      mA
    val clazzName = manifestToTypeDescriptor(mA1)
    val jniclazz = jni_find_class(JNIStringConst(clazzName))
    val jArray = jni_new_object_array_var[B](a.length, jniclazz)
    val f1 = {i:Rep[Int] => f(a.at(i))}
    val x = fresh[Int]
    val y = reifyEffects(f1(x))
    val res = simpleLoop(a.length, x, JArrayElem(jArray,y))
    ReturnFirstArg(jArray, res)
  }

  def jni_new_object[T: Manifest](clazz: Rep[JNIClass], mid: Rep[JNIMethodID], args: Rep[Any]*): Rep[JNIType[T]] = {
    NewObject[T](clazz, mid, args:_*)
  }

  private def jniClassForStruct(m: Manifest[_]) = {
    val className = structName(m)
    jni_find_class(JNIStringConst(className))
  }

  def jni_new_struct[A](m: RefinedManifest[A], args: Rep[Any]*): Rep[JNIType[A]] = {
    val jniClass = jniClassForStruct(m)
    val argClassDescriptors = args.map { arg =>
      val unJniType = arg.tp.typeArguments.head
      manifestToTypeDescriptor(unJniType)
    }
    val methodDescriptor = argClassDescriptors.mkString("(", "", ")V")
    val jniMethodID = jni_get_method_id(jniClass, JNIStringConst("<init>"), JNIStringConst(methodDescriptor))
    jni_new_object(jniClass, jniMethodID, args: _*)(m)
  }

  def jni_get_struct_field_value[A, S](struct: Rep[JNIType[S]], fieldName: String)(implicit mA: Manifest[A]) = {
    implicit val mS = struct.tp.typeArguments.head.asInstanceOf[Manifest[S]]
    val jniClass = jniClassForStruct(mS)
    val fieldID = jni_get_field_id(jniClass, JNIStringConst(fieldName), JNIStringConst(manifestToTypeDescriptor(mA)))
    if (mA.isPrimitive)
      jni_get_primitive_field_value[A, S](fieldID, struct)
    else
      jni_get_object_field_value[A, S](fieldID, struct)
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case Reflect(SimpleLoop(s,v,body: JNIArrayElem[A]), u, es) =>
      reflectMirrored(Reflect(SimpleLoop(f(s),f(v).asInstanceOf[Sym[Int]],mirrorFatDef(body,f)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(SimpleLoop(s,v,body: JArrayElem[A]), u, es) =>
      reflectMirrored(Reflect(SimpleLoop(f(s),f(v).asInstanceOf[Sym[Int]],mirrorFatDef(body,f)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case res@Reflect(ExtractPrimitiveArray(x), u, es) =>
      reflectMirrored(Reflect(ExtractPrimitiveArray(f(x)), mapOver(f, u), f(es)))(mtype(manifest[A]), pos)
    case res@Reflect(arr@NewObjectArray(len, clazz), u, es) =>
      reflectMirrored(Reflect(NewObjectArray(f(len), f(clazz))(arr.mA), mapOver(f, u), f(es)))(mtype(manifest[A]), pos)
    case res@Reflect(arr@NewPrimitiveArray(len), u, es) =>
      reflectMirrored(Reflect(NewPrimitiveArray(f(len))(arr.mA), mapOver(f, u), f(es)))(mtype(manifest[A]), pos)
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

    case res@ReturnFirstArg(ret, jniArray) =>
      ret.tp.typeArguments(0) match {
        case mA: Manifest[a_t] =>
          jni_return_first_arg[a_t]( f(ret).asInstanceOf[Exp[JNIType[a_t]]], f(jniArray) )(mA)
      }
    case res@NewPrimitiveArray(len) =>
      res.tp.typeArguments(0).typeArguments(0) match {
        case mA: Manifest[a_t] =>
          jni_new_primitive_array[a_t](f(len))(mA)
      }
    case res@NewObjectArray(len, clazz) =>
      res.tp.typeArguments(0).typeArguments(0) match {
        case mA: Manifest[a_t] =>
          jni_new_object_array[a_t](f(len), f(clazz))(mA)
      }
    case res@NewPrimitive(x) =>
      x.tp match {
        case mA: Manifest[a_t] =>
          jni_new_primitive[a_t](f(x).asInstanceOf[Exp[a_t]])(mA)
      }
    case res@NewObject(clazz,mid,args@_*) =>
      res.tp.typeArguments(0) match {
        case mT: Manifest[t_t] =>
          val _args = for(arg <- args) yield f(arg)
          jni_new_object[t_t](f(clazz), f(mid), _args:_*)(mT)
      }
    case res@CallObjectMethod(x,mid,args@_*) =>
      (res.tp.typeArguments(0),x.tp.typeArguments(0)) match {
        case (mR: Manifest[r_t], mA: Manifest[a_t]) =>
          val _args = for(arg <- args) yield f(arg)
          jni_call_object_method[r_t,a_t](f(x).asInstanceOf[Exp[JNIType[a_t]]], f(mid), _args:_*)(mR,mA)
      }
    case res@BoxPrimitive(x) =>
      x.tp.typeArguments(0) match {
        case mT: Manifest[t_t] =>
          jni_box_primitive[t_t](f(x).asInstanceOf[Exp[JNIType[t_t]]])(mT)
      }
      case res@GetArrayLength(xs) =>
        xs.tp.typeArguments(0).typeArguments(0) match {
          case mT: Manifest[t_t] =>
            jni_get_array_length[t_t](f(xs).asInstanceOf[Exp[JNIType[Array[t_t]]]])(mT)
        }
    case res@ExtractObjectArray(xs) =>
      xs.tp.typeArguments(0).typeArguments(0) match {
        case mT: Manifest[t_t] =>
          jni_extract_object_array[t_t](f(xs).asInstanceOf[Exp[JNIType[Array[t_t]]]])(mT)
      }
    case _ =>
      super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

  override def mirrorDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = {
    (e match {
      case ReturnFirstArg(ret, ignore) =>
        ReturnFirstArg(f(ret),f(ignore))
      case _ =>
        super.mirrorDef(e, f)
    }).asInstanceOf[Def[A]]
  }

  override def mirrorFatDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = (e match {
    case JNIArrayElem(x,y) => JNIArrayElem(f(x),f(y))
//    case ReduceElem(y) => ReduceElem(f(y))
    case JNIArrayIfElem(x,c,y) => JNIArrayIfElem(f(x),f(c),f(y))

    case JArrayElem(x,y) => JArrayElem(f(x),f(y))
    case JArrayIfElem(x,c,y) => JArrayIfElem(f(x),f(c),f(y))
//    case ReduceIfElem(c,y) => ReduceIfElem(f(c),f(y))
    case _ => super.mirrorFatDef(e,f)
  }).asInstanceOf[Def[A]]
}

trait JNIExtractorOpsCxxGenBase extends GenericCodegen with ManifestUtil {
  val IR: JNILmsOpsExp
  import IR._

  trait JObject[T]
  def jobjectManifest[T: Manifest]( m: Manifest[T]) = Manifest.classType(classOf[JObject[_]], m)

  override def remap[A](m: Manifest[A]): String = {
    m.runtimeClass match {
      case c if c == classOf[JNIArray[_]] =>
        val t = m.typeArguments(0)
        src"jni_array<$t>"
      case c if c == classOf[JNIClass] => "jclass"
      case c if c == classOf[JNIFieldID] => "jfieldID"
      case c if c == classOf[JNIMethodID] => "jmethodID"
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
    case Manifest.Boolean => "jboolean"
    case Manifest.Byte => "jbyte"
    case Manifest.Char => "jchar"
    case Manifest.Double => "jdouble"
    case Manifest.Float => "jfloat"
    case Manifest.Int => "jint"
    case Manifest.Long => "jlong"
    case Manifest.Short => "jshort"
    // case Manifest.Unit => "void" // TODO is this needed?
    case _ if m == manifest[String] => "jstring"
    case _ => "jobject"
  }

  protected def remapObject[A](m: Manifest[A]): String = m match {
    case _ if m.runtimeClass == classOf[JNIType[_]] => m.typeArguments(0) match {
      case mT if mT.isPrimitive => "jobject"
      case _ => remap(m)
    }
    case _ if m.runtimeClass.isArray => m.typeArguments(0) match {
      case mItem if mItem.runtimeClass == classOf[JNIType[_]] =>
        remap(m)
      case _ =>
        throw new GenerationFailedException(s"JNIExtractorOpsCxxGenBase.remapObject(m) : Type ${m} cannot be remapped to jobject.")
    }
    case _ =>
      throw new GenerationFailedException(s"JNIExtractorOpsCxxGenBase.remapObject(m) : Type ${m} cannot be remapped to jobject.")
  }
}

trait CxxShptrGenJNIExtractor extends CxxShptrCodegen with JNIExtractorOpsCxxGenBase {
  val IR: JNILmsOpsExp
  import IR._

  headerFiles ++= Seq("jni-array-wrapper.hpp")

  override def toShptrManifest(m: Manifest[_]): Manifest[_] = {
    m.runtimeClass match {
      case c if c == classOf[JNIType[_]] => m
      case c if c.isArray && m.typeArguments(0).runtimeClass == classOf[JNIType[_]] =>
        m
      case _ =>
        super.toShptrManifest(m)
    }
  }

  override protected def doNotWrap(m: Manifest[_]) =
    // JNIArray[_] intentionally not included
    m.isOneOf(classOf[JNIType[_]], classOf[JObject[_]], classOf[JNIClass], classOf[JNIFieldID], classOf[JNIMethodID]) ||
      super.doNotWrap(m)

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case res@NewPrimitive(x) =>
      emitValDef(sym, src"static_cast<${x.tp}>($x)")
    case res@NewObject(clazz, mid, args@_*) =>
      emitValDef(quote(sym), jobjectManifest(sym.tp), src"env->NewObject($clazz, ${mid +: args})")
    case res@CallObjectMethod(x, mid, args@_*) =>
      // TODO is static_cast useful here and in GetObjectFieldValue?
      emitValDef(quote(sym),jobjectManifest(sym.tp), src"static_cast<${jobjectManifest(sym.tp)}>(env->CallObjectMethod($x, ${mid +: args}))")
    case res@ReturnFirstArg(jArray, _) =>
      emitValDef(sym, src"$jArray")
    case res@NewPrimitiveArray(len) =>
      // toString avoids remapping the JVM type name to C++
      emitValDef(sym, src"env->New${res.mA.toString}Array($len)")
    case res@NewObjectArray(len, clazz) =>
      emitValDef(sym, src"env->NewObjectArray($len, $clazz, nullptr)")
    case FindClass(className) =>
      emitValDef(sym, src"env->FindClass($className)")
    case ExtractPrimitive(x) =>
      emitValDef(sym, src"static_cast<${sym.tp}>($x)")
    case ExtractPrimitiveArray(x) =>
      emitConstruct(sym, "env", src"$x")
    case Reflect(ExtractPrimitiveArray(x),_,_) =>
      emitConstruct(sym, "env", src"$x")
    case GetArrayLength(x) =>
      emitValDef(sym, src"env->GetArrayLength($x)")
    case ExtractObjectArray(x) =>
      emitValDef(sym, src"$x")
    case GetObjectArrayItem(x, i) =>
      emitValDef(sym, src"static_cast<${sym.tp}>(env->GetObjectArrayElement($x, $i))")
    case GetObjectClass(x) =>
      emitValDef(sym, src"env->GetObjectClass($x)")
    case GetFieldID(clazz, fn, sig) =>
      emitValDef(sym, src"env->GetFieldID($clazz, $fn, $sig)")
    case GetMethodID(clazz, mn, sig) =>
      emitValDef(sym, src"env->GetMethodID($clazz, $mn, $sig)")
    case GetObjectFieldValue(fid, x) =>
      emitValDef(quote(sym), jobjectManifest(sym.tp), src"static_cast<${jobjectManifest(sym.tp)}>(env->GetObjectField($x, $fid))")
    case res@GetPrimitiveFieldValue(fid, x) =>
      // toString avoids remapping the JVM type name to C++
      emitValDef(sym, src"static_cast<${sym.tp}>(env->Get${res.tp.toString}Field($x, $fid))")
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
}
