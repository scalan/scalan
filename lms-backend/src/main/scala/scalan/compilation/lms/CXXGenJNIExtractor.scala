package scalan.compilation.lms

import scala.virtualization.lms.internal.{GenerationFailedException, FatExpressions}

/**
 * Created by zotov on 12/27/14.
 */
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
        else if (syms(rhs).find(moveableSyms.contains) != None) // derived from moveable
          moveableSyms += sym
      case _ =>
        ()
    }
    super.traverseStm(stm)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
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
      case GetPrimitiveFieldValue(fid, x) =>
        val funName = s"Get${fid.tp.typeArguments(0).toString}Field"
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
      case c if c == classOf[JNIClass[_]] => "jclass"
      case c if c == classOf[JNIFieldID[_]] => "jfieldID"
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
