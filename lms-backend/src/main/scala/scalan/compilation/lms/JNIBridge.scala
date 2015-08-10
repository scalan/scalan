package scalan.compilation.lms

import scalan.compilation.language.MethodMappingDSL
import scalan.compilation.lms.common.{JNILmsOps, JNILmsOpsExp}
import scalan.primitives.AbstractStringsDslExp
import scalan.{ScalanCtxExp, JNIExtractorOpsExp}

trait JNIBridge extends CoreBridge {
  override val scalan: ScalanCtxExp with JNIExtractorOpsExp with AbstractStringsDslExp
  import scalan._

  val lms: CoreLmsBackend with JNILmsOpsExp

  override def createManifest[T]: PartialFunction[Elem[T], Manifest[_]] = {
    case el: JNITypeElem[_] =>
      Manifest.classType(classOf[JNILmsOps#JNIType[_]], createManifest(el.eT))
    case el: JNIArrayElem[arr_t] =>
      el.eItem match {
        case ei: Elem[a_t] =>
          val mItem = createManifest(ei)
          Manifest.classType(classOf[JNILmsOps#JNIArray[a_t]], mItem)
      }
    case el =>
      super.createManifest(el)
  }

  override def transformDef[T](m: LmsMirror, g: AstGraph, sym: Exp[T], d: Def[T]) = d match {
    case res@ExpCString(Def(s: Const[_])) =>
      val exp = lms.JNIStringConst(s.x)
      m.addSym(sym, exp)
    case res@JNI_ExtractPrimitive(x) =>
      x.elem match {
        case (jnie: JNITypeElem[jni_a_t]) =>
          createManifest(jnie.eT) match {
            case (mA: Manifest[a_t]) =>
              val _x = m.symMirror[lms.JNIType[a_t]](x)
              val exp = lms.jni_extract_primitive[a_t](_x)(mA)
              m.addSym(sym, exp)
          }
      }
    case res@JNI_ExtractPrimitiveArray(x) =>
      x.elem match {
        case (jnie: JNITypeElem[jni_arr_a_t]) =>
          createManifest(jnie.eT) match {
            case (mA: Manifest[arr_a_t]) =>
              mA.typeArguments(0) match {
                case(mItem: Manifest[a_t]) =>
                  val _x = m.symMirror[lms.JNIType[Array[a_t]]](x)
                  val exp = lms.jni_extract_primitive_array(_x)(mItem)
                  m.addSym(sym, exp)
              }
          }
      }
    case res@JNI_GetArrayLength(x) =>
      x.elem match {
        case (jnie: JNITypeElem[jni_arr_a_t]) =>
          createManifest(jnie.eT) match {
            case (mA: Manifest[arr_a_t]) =>
              mA.typeArguments(0) match {
                case (mItem: Manifest[a_t]) =>
                  val _x = m.symMirror[lms.JNIType[Array[a_t]]](x)
                  val exp = lms.jni_get_array_length(_x)(mItem)
                  m.addSym(sym, exp)
              }
          }
      }
    case res@JNI_ExtractObjectArray(x) =>
      x.elem match {
        case (jnie: JNITypeElem[jni_arr_a_t]) =>
          createManifest(jnie.eT) match {
            case (mA: Manifest[arr_a_t]) =>
              mA.typeArguments(0) match {
                case mItem: Manifest[a_t] =>
                  val _x = m.symMirror[lms.JNIType[Array[a_t]]](x)
                  val exp = lms.jni_extract_object_array(_x)(mItem)
                  m.addSym(sym, exp)
              }
          }
      }
    case res@JNI_GetObjectArrayItem(x, i) =>
      x.elem match {
        case arrel: ArrayElem[arr_jni_a_t] =>
          arrel.eItem match {
            case (jnie: JNITypeElem[jni_a_t]) =>
              createManifest(jnie.eT) match {
                case (mA: Manifest[a_t]) =>
                  val _x = m.symMirror[Array[lms.JNIType[a_t]]](x)
                  val _i = m.symMirror[Int](i)
                  val exp = lms.jni_get_object_array_item(_x, _i)(mA)
                  m.addSym(sym, exp)
              }
          }
      }
    case res@JNI_GetObjectClass(x) =>
      x.elem match {
        case (jnie: JNITypeElem[jni_a_t]) =>
          createManifest(jnie.eT) match {
            case (mA: Manifest[a_t]) =>
              val _x = m.symMirror[lms.JNIType[a_t]](x)
              val exp = lms.jni_get_object_class(_x)(mA)
              m.addSym(sym, exp)
          }
      }
    case res@JNI_FindClass(x) =>
      val _x = m.symMirror[String](x)
      val exp = lms.jni_find_class(_x)
      m.addSym(sym, exp)
    case res@JNI_GetFieldID(clazz, fn, sig) =>
      val _clazz = m.symMirror[lms.JNIClass](clazz)
      val _fn = m.symMirror[String](fn)
      val _sig = m.symMirror[String](sig)
      val exp = lms.jni_get_field_id(_clazz, _fn, _sig)
      m.addSym(sym, exp)
    case res@JNI_GetObjectFieldValue(fid, x) =>
      (res.selfType, x.elem) match {
        case (jnife: JNITypeElem[jni_A_t], jnie: JNITypeElem[jni_T_t]) =>
          val mA = createManifest(jnife.eT)
          val mT = createManifest(jnie.eT)
          (mA,mT) match {
            case (ma: Manifest[a_t], mt: Manifest[t_t]) =>
              val _x = m.symMirror[lms.JNIType[t_t]](x)
              val _fid = m.symMirror[lms.JNIFieldID](fid)
              val exp = lms.jni_get_object_field_value[a_t,t_t](_fid, _x)(ma,mt)
              m.addSym(sym, exp)
          }
      }
    case res@JNI_GetPrimitiveFieldValue(fid, x) =>
      (res.selfType, x.elem) match {
        case (rese: Element[jnifid_A_t], jnie: JNITypeElem[jni_T_t]) =>
          val mA = createManifest(rese)
          val mT = createManifest(jnie.eT)
          (mA,mT) match {
            case (ma: Manifest[a_t], mt: Manifest[t_t]) =>
              val _x = m.symMirror[lms.JNIType[t_t]](x)
              val _fid = m.symMirror[lms.JNIFieldID](fid)
              val exp = lms.jni_get_primitive_field_value[a_t,t_t](_fid, _x)(ma,mt)
              m.addSym(sym, exp)
          }
      }
    case res@JNI_GetMethodID(clazz, mn, sig) =>
      val _clazz = m.symMirror[lms.JNIClass](clazz)
      val _mn = m.symMirror[String](mn)
      val _sig = m.symMirror[String](sig)
      val exp = lms.jni_get_method_id(_clazz, _mn, _sig)
      m.addSym(sym, exp)
    case res@JNI_MapPrimitiveArray(x, fSym@Def(f: Lambda[_, _])) =>
      (res.selfType, x.elem) match {
        case (rese: JNITypeElem[jniarr_t], xe: ArrayElem[_]) =>
          rese.eT match {
            case be: ArrayElem[_] =>
              (createManifest(xe.eItem),createManifest(be.eItem)) match {
                case (mA: Manifest[a], mB: Manifest[b]) =>
                  val _x = m.symMirror[Array[a]](x)
                  val _f = m.mirrorLambda[a, b](f.asInstanceOf[Lambda[a, b]])
                  val exp = lms.jni_map_array[a, b](_x, _f)(mA, mB)
                  m.addSym(sym, exp).addFunc(fSym, _f)
              }
          }
      }
    case res@JNI_MapObjectArray(x, fSym@Def(f: Lambda[_, _])) =>
      (res.selfType, x.elem) match {
        case (rese: JNITypeElem[_], xe: ArrayElem[_]) =>
          rese.eT match {
            case be: ArrayElem[_] =>
              (createManifest(xe.eItem),createManifest(be.eItem)) match {
                case (mA: Manifest[a], mB: Manifest[b]) =>
                  val _x = m.symMirror[Array[a]](x)
                  val _f = m.mirrorLambda[a, lms.JNIType[b]](f.asInstanceOf[Lambda[a, lms.JNIType[b]]])
                  val exp = lms.jni_map_object_array[a, b](_x, _f)(mA, mB)
                  m.addSym(sym, exp).addFunc(fSym, _f)
              }
          }
      }
    case res@JNI_NewPrimitive(x) =>
      createManifest(x.elem) match {
        case mA: Manifest[a_t] =>
          val _x = m.symMirror[a_t](x)
          val exp = lms.jni_new_primitive[a_t](_x)(mA)
          m.addSym(sym, exp)
      }
    case res@JNI_NewObject(clazz,mid,args@_*) => res.selfType match {
      case el: JNITypeElem[_] =>
        createManifest(el.eT) match {
          case mA: Manifest[a_t] =>
            val _clazz = m.symMirror[lms.JNIClass](clazz)
            val _mid = m.symMirror[lms.JNIMethodID](mid)
            val _args = args.map(m.symMirrorUntyped)
            val exp = lms.jni_new_object[a_t](_clazz,_mid,_args:_*)(mA)
            m.addSym(sym, exp)
        }
    }
    case res@JNI_CallObjectMethod(x,mid,args@_*) => (res.selfType, x.elem) match {
      case (resel: JNITypeElem[_], xel: JNITypeElem[_]) =>
        (createManifest(resel.eT), createManifest(xel.eT)) match {
          case (mR: Manifest[r_t], mA: Manifest[a_t]) =>
            val _x = m.symMirror[lms.JNIType[a_t]](x)
            val _mid = m.symMirror[lms.JNIMethodID](mid)
            val _args = args.map(m.symMirrorUntyped)
            val exp = lms.jni_call_object_method[r_t,a_t](_x,_mid,_args:_*)(mR,mA)
            m.addSym(sym, exp)
        }
    }
    case _ => super.transformDef(m, g, sym, d)
  }
}
