package scalan.compilation.lms.common

import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.epfl.test7.ArrayLoopsExp
import scala.virtualization.lms.internal.GenerationFailedException
import scalan.compilation.lms.cxx.CXXCodegen


trait JNILmsOps extends Base {
  trait JNIType[T]
  trait JNIClass
  trait JNIFieldID
  trait JNIMethodID
  trait JNIArray[T]
}

trait JNILmsOpsExp extends JNILmsOps with LoopsFatExp with ArrayLoopsExp with BaseExp {
  case class JNIStringConst(x: String) extends Exp[String]

  case class JNIArrayElem[T](x: Rep[JNIArray[T]], y: Block[T]) extends Def[JNIArray[T]]
  case class JNIArrayIfElem[T](x: Rep[JNIArray[T]], c: Exp[Boolean], y: Block[T]) extends Def[JNIType[Array[T]]]

  case class JArrayElem[T](x: Rep[JNIType[Array[T]]], y: Block[JNIType[T]]) extends Def[JNIType[Array[T]]]
  case class JArrayIfElem[T](x: Rep[JNIType[Array[T]]], c: Exp[Boolean], y: Block[JNIType[T]]) extends Def[JNIType[Array[T]]]

  case class FindClass[T](className: Rep[String]) extends Def[JNIClass]

  case class BoxPrimitive[T: Manifest](x: Rep[JNIType[T]]) extends Def[JNIType[T]] {
    require( (manifest[T] <:< Manifest.AnyVal), "(" + manifest[T] + " <:< " + Manifest.AnyVal + ") isn't true")
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
    require( (manifest[A] <:< Manifest.AnyVal), "(" + manifest[A] + " <:< " + Manifest.AnyVal + ") isn't true")
    val mA = manifest[A]
  }

  case class NewObjectArray[A: Manifest](len: Rep[Int], clazz: Rep[JNIClass]) extends Def[JNIType[Array[A]]] {
    require( (manifest[A] <:< Manifest.AnyRef), "(" + manifest[A] + " <:< " + Manifest.AnyRef + ") isn't true")
    val mA = manifest[A]
  }

  case class ReturnFirstArg[A: Manifest](jArray: Rep[JNIType[A]], ignore: Rep[Any]*) extends Def[JNIType[A]]
  case class NewPrimitive[T: Manifest](x: Rep[T]) extends Def[JNIType[T]]
  case class NewObject[T: Manifest](clazz: Rep[JNIClass], mid: Rep[JNIMethodID], args: Rep[Any]*) extends Def[JNIType[T]]
  case class CallObjectMethod[R: Manifest, A: Manifest](x: Rep[JNIType[A]], mid: Rep[JNIMethodID], args: Rep[Any]*) extends Def[JNIType[R]]

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case JNIArrayElem(x,y) =>
      effectSyms(y)
    case _ =>
      super.boundSyms(e)
  }

  def jni_box_primitive[A: Manifest](x: Rep[JNIType[A]]): Rep[JNIType[A]] = BoxPrimitive[A](x)
  def jni_new_primitive[A: Manifest](x: Rep[A]): Rep[JNIType[A]] = NewPrimitive[A](x)
  def jni_new_primitive_array[A: Manifest](len: Rep[Int]): Rep[JNIType[Array[A]]] = NewPrimitiveArray[A](len)
  def jni_new_object_array[A: Manifest](len: Rep[Int], clazz: Rep[JNIClass]): Rep[JNIType[Array[A]]] = NewObjectArray[A](len, clazz)
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

  def jni_map_array[A: Manifest, B: Manifest](a: Exp[Array[A]], f: Rep[A] => Rep[B]): Exp[JNIType[Array[B]]] = {
    val jArray = jni_new_primitive_array[B](a.length)
    val jniArray = jni_extract_primitive_array_var(jArray)
    val f1 = {i:Rep[Int] => f(a.at(i))}
    val x = fresh[Int]
    val y = reifyEffects(f1(x))
    val jnia = simpleLoop(a.length, x, JNIArrayElem(jniArray,y))
    ReturnFirstArg(jArray, jnia)
  }

  def jni_map_object_array[A: Manifest, B: Manifest](a: Exp[Array[A]], f: Rep[A] => Rep[JNIType[B]]): Exp[JNIType[Array[B]]] = {
    val clazzName = org.objectweb.asm.Type.getType(manifest[A].runtimeClass).getDescriptor
    val clazz = jni_find_class(JNIStringConst(clazzName))
    val jArray = jni_new_object_array[B](a.length, clazz)
    val f1 = {i:Rep[Int] => f(a.at(i))}
    val x = fresh[Int]
    val y = reifyEffects(f1(x))
    val res = simpleLoop(a.length, x, JArrayElem(jArray,y))
    ReturnFirstArg(jArray, res)
  }

  def jni_new_object[T: Manifest](clazz: Rep[JNIClass], mid: Rep[JNIMethodID], args: Rep[Any]*): Rep[JNIType[T]] = {
    NewObject[T](clazz, mid, args:_*)
  }

  override
  def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case Reflect(SimpleLoop(s,v,body: JNIArrayElem[A]), u, es) =>
      reflectMirrored(Reflect(SimpleLoop(f(s),f(v).asInstanceOf[Sym[Int]],mirrorFatDef(body,f)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case res@Reflect(ExtractPrimitiveArray(x), u, es) =>
      reflectMirrored(Reflect(ExtractPrimitiveArray(f(x)), mapOver(f, u), f(es)))(mtype(manifest[A]), pos)
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
    case _ =>
      super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

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

trait CXXGenJNIExtractor extends CXXCodegen {
  val IR: JNILmsOpsExp
  import IR._

  trait JObject[T]
  def jobjectManifest[T: Manifest]( m: Manifest[T]) = Manifest.classType(classOf[JObject[_]], m)

  override def traverseStm(stm: Stm): Unit = {
    stm match {
      case TP(sym,rhs) => rhs match {
        case ExtractPrimitiveArray(_) =>
          moveableSyms += sym
        case _ =>
          ()
      }
      case _ =>
        ()
    }
    super.traverseStm(stm)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case res@NewPrimitive(x) =>
      emitValDef(sym, s"static_cast<${remap(x.tp)}>(${quote(x)})")
    case res@NewObject(clazz, mid, args@_*) =>
      val sargs = if(args.isEmpty) "" else ", " + args.map(quote).mkString(", ")
      emitValDef(quote(sym), jobjectManifest(sym.tp), s"env->NewObject(${quote(clazz)},${quote(mid)}${sargs})")
    case res@CallObjectMethod(x, mid, args@_*) =>
      val sargs = if(args.isEmpty) "" else ", " + args.map(quote).mkString(", ")
      emitValDef(quote(sym),jobjectManifest(sym.tp), s"static_cast<${remap(jobjectManifest(sym.tp))}>(env->CallObjectMethod(${quote(x)},${quote(mid)}${sargs}))")
    case res@ReturnFirstArg(jArray, _) =>
      emitValDef(sym, s"${quote(jArray)}")
    case res@NewPrimitiveArray(len) =>
      emitValDef(sym, s"env->New${res.mA.toString}Array(${quote(len)})")
    case res@NewObjectArray(len, clazz) =>
      emitValDef(sym, s"env->NewObjectArray(${quote(len)}, ${quote(clazz)}, nullptr)")
    case FindClass(className) =>
      emitValDef(sym, s"env->FindClass(${quote(className)})")
    case ExtractPrimitive(x) =>
      emitValDef(sym, s"static_cast<${remap(norefManifest(sym.tp))}>(${quote(x)})")
    case ExtractPrimitiveArray(x) =>
      emitValDef(sym, s"${remap(sym.tp)}(env, ${quote(x)})")
    case Reflect(ExtractPrimitiveArray(x),_,_) =>
      emitConstruct(sym, "env", s"${quote(x)}")
    case GetArrayLength(x) =>
      emitValDef(sym, s"env->GetArrayLength(${quote(x)})")
    case ExtractObjectArray(x) =>
      emitValDef(sym, s"${quote(x)} /*ExtractObjectArray: sym.tp=${sym.tp}*/")
    case GetObjectArrayItem(x, i) =>
      emitValDef(sym, s"static_cast<${remapObject(sym.tp)}>(env->GetObjectArrayElement(${quote(x)}, ${quote(i)}))")
    case GetObjectClass(x) =>
      emitValDef(sym, s"env->GetObjectClass(${quote(x)})")
    case GetFieldID(clazz, fn, sig) =>
      emitValDef(sym, s"env->GetFieldID(${quote(clazz)}, ${quote(fn)}, ${quote(sig)})")
    case GetMethodID(clazz, mn, sig) =>
      emitValDef(sym, s"env->GetMethodID(${quote(clazz)}, ${quote(mn)}, ${quote(sig)})")
    case GetObjectFieldValue(fid, x) =>
      emitValDef(quote(sym), jobjectManifest(sym.tp), s"static_cast<${remap(jobjectManifest(sym.tp))}>(env->GetObjectField(${quote(x)}, ${quote(fid)})) /*GetObjectField: sym.tp=${sym.tp}*/")
    case res@GetPrimitiveFieldValue(fid, x) =>
      val funName = s"Get${res.tp.toString}Field"
      emitValDef(sym, s"static_cast<${remap(norefManifest(sym.tp))}>(env->${funName}(${quote(x)}, ${quote(fid)}))")
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
