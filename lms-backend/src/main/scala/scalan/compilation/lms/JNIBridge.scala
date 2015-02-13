package scalan.compilation.lms

import scalan.compilation.lms.common.JNILmsOpsExp
import scalan.primitives.AbstractStringsDslExp
import scalan.{JNIExtractorOpsExp, ScalanExp}

trait JNIBridge[A, B] extends LmsBridge[A, B] {

  // `LmsCompiler` mixed just to provide `createManifest` function
  val scalan: ScalanExp with JNIExtractorOpsExp with AbstractStringsDslExp with LmsCompiler
  val lms: CommunityLmsBackendBase with JNILmsOpsExp

  abstract override def defTransformer[T](m: Mirror, g: scalan.AstGraph, e: scalan.TableEntry[T]) = {
    jniDefTransformer(m, g, e) orElse super.defTransformer(m, g, e)
  }

  def jniDefTransformer[T](m: Mirror, g: scalan.AstGraph, e: scalan.TableEntry[T]): DefTransformer = {
    val (exps, symMirr, funcMirr) = m
    val sym = e.sym
    val tt: DefTransformer = {
      case res@scalan.ExpCString(scalan.Def(s: scalan.Const[_])) =>
        val exp = lms.JNIStringConst(s.x)
        (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
      case res@scalan.JNI_ExtractPrimitive(x) =>
        x.elem match {
          case (jnie: scalan.JNITypeElem[jni_a_t]) =>
            scalan.createManifest(jnie.tElem) match {
              case (mA: Manifest[a_t]) =>
                val _x = symMirr(x).asInstanceOf[lms.Exp[lms.JNIType[a_t]]]
                val exp = lms.jni_extract_primitive[a_t](_x)(mA)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
            }
        }
      case res@scalan.JNI_ExtractPrimitiveArray(x) =>
        x.elem match {
          case (jnie: scalan.JNITypeElem[jni_arr_a_t]) =>
            scalan.createManifest(jnie.tElem) match {
            case (mA: Manifest[arr_a_t]) =>
              mA.typeArguments(0) match {
                case(mItem: Manifest[a_t]) =>
                  val _x = symMirr(x).asInstanceOf[lms.Exp[lms.JNIType[Array[a_t]]]]
                  val exp = lms.jni_extract_primitive_array(_x)(mItem)
                  (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
        }
      case res@scalan.JNI_GetArrayLength(x) =>
        x.elem match {
          case (jnie: scalan.JNITypeElem[jni_arr_a_t]) =>
            scalan.createManifest(jnie.tElem) match {
            case (mA: Manifest[arr_a_t]) =>
              mA.typeArguments(0) match {
                case (mItem: Manifest[a_t]) =>
                  val _x = symMirr(x).asInstanceOf[lms.Exp[lms.JNIType[Array[a_t]]]]
                  val exp = lms.jni_get_array_length(_x)(mItem)
                  (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
        }
      case res@scalan.JNI_ExtractObjectArray(x) =>
        x.elem match {
          case (jnie: scalan.JNITypeElem[jni_arr_a_t]) =>
            implicit val m = scalan.createManifest(jnie.tElem)
            m match {
              case (mA: Manifest[arr_a_t]) =>
                mA.typeArguments(0) match {
                  case mItem: Manifest[a_t] =>
                    val _x = symMirr(x).asInstanceOf[lms.Exp[lms.JNIType[Array[a_t]]]]
                    val exp = lms.jni_extract_object_array(_x)(mItem)
                    (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
                }
            }
        }
      case res@scalan.JNI_GetObjectArrayItem(x, i) =>
        x.elem match {
          case arrel: scalan.ArrayElem[arr_jni_a_t] =>
            arrel.eItem match {
              case (jnie: scalan.JNITypeElem[jni_a_t]) =>
                implicit val m = scalan.createManifest(jnie.tElem)
                m match {
                  case (mA: Manifest[a_t]) =>
                    val _x = symMirr(x).asInstanceOf[lms.Exp[Array[lms.JNIType[a_t]]]]
                    val _i = symMirr(i).asInstanceOf[lms.Exp[Int]]
                    val exp = lms.jni_get_object_array_item(_x, _i)(mA)
                    (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
                }
            }
        }
      case res@scalan.JNI_GetObjectClass(x) =>
        x.elem match {
          case (jnie: scalan.JNITypeElem[jni_a_t]) =>
            implicit val m = scalan.createManifest(jnie.tElem)
            m match {
              case (mA: Manifest[a_t]) =>
                val _x = symMirr(x).asInstanceOf[lms.Exp[lms.JNIType[a_t]]]
                val exp = lms.jni_get_object_class(_x)(mA)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
            }
        }
      case res@scalan.JNI_FindClass(x) =>
        val _x = symMirr(x).asInstanceOf[lms.Exp[String]]
        val exp = lms.jni_find_class(_x)
        (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
      case res@scalan.JNI_GetFieldID(clazz, fn, sig) =>
        val _clazz = symMirr(clazz).asInstanceOf[lms.Exp[ lms.JNIClass] ]
        val _fn = symMirr(fn).asInstanceOf[lms.Exp[String]]
        val _sig = symMirr(sig).asInstanceOf[lms.Exp[String]]
        val exp = lms.jni_get_field_id(_clazz, _fn, _sig)
        (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
      case res@scalan.JNI_GetObjectFieldValue(fid, x) =>
        (res.selfType, x.elem) match {
          case (jnife: scalan.JNITypeElem[jni_A_t], jnie: scalan.JNITypeElem[jni_T_t]) =>
            val mA = scalan.createManifest(jnife.tElem)
            val mT = scalan.createManifest(jnie.tElem)
            (mA,mT) match {
              case (ma: Manifest[a_t], mt: Manifest[t_t]) =>
                val _x = symMirr(x).asInstanceOf[lms.Exp[lms.JNIType[t_t]]]
                val _fid = symMirr(fid).asInstanceOf[lms.Exp[lms.JNIFieldID]]
                val exp = lms.jni_get_object_field_value[a_t,t_t](_fid, _x)(ma,mt)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
            }
        }
      case res@scalan.JNI_GetPrimitiveFieldValue(fid, x) =>
        (res.selfType, x.elem) match {
          case (rese: scalan.Element[jnifid_A_t], jnie: scalan.JNITypeElem[jni_T_t]) =>
            val mA = scalan.createManifest(rese)
            val mT = scalan.createManifest(jnie.tElem)
            (mA,mT) match {
              case (ma: Manifest[a_t], mt: Manifest[t_t]) =>
                val _x = symMirr(x).asInstanceOf[lms.Exp[lms.JNIType[t_t]]]
                val _fid = symMirr(fid).asInstanceOf[lms.Exp[lms.JNIFieldID]]
                val exp = lms.jni_get_primitive_field_value[a_t,t_t](_fid, _x)(ma,mt)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
            }
        }
      case res@scalan.JNI_GetMethodID(clazz, mn, sig) =>
        val _clazz = symMirr(clazz).asInstanceOf[lms.Exp[ lms.JNIClass] ]
        val _mn = symMirr(mn).asInstanceOf[lms.Exp[String]]
        val _sig = symMirr(sig).asInstanceOf[lms.Exp[String]]
        val exp = lms.jni_get_method_id(_clazz, _mn, _sig)
        (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
      case res@scalan.JNI_Map(x, fSym@scalan.Def(f: scalan.Lambda[_, _])) =>
        (res.selfType, x.elem) match {
          case (rese: scalan.JNITypeElem[jniarr_t], xe: scalan.ArrayElem[_]) =>
            rese.tElem match {
              case be: scalan.ArrayElem[_] =>
                (scalan.createManifest(xe.eItem),scalan.createManifest(be.eItem)) match {
                  case (mA: Manifest[a], mB: Manifest[b]) =>
                    val _x = symMirr(x).asInstanceOf[lms.Exp[Array[a]]]
                    val _f = mirrorLambdaToLmsFunc[a, b](m)(f.asInstanceOf[scalan.Lambda[a, b]])
                    val exp = lms.jni_map_array[a, b](_x, _f)(mA, mB)
                    (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((fSym, _f)))
                }
            }
        }
      case res@scalan.JNI_MapObjectArray(x, fSym@scalan.Def(f: scalan.Lambda[_, _])) =>
        (res.selfType, x.elem) match {
          case (rese: scalan.JNITypeElem[_], xe: scalan.ArrayElem[_]) =>
            rese.tElem match {
              case be: scalan.ArrayElem[_] =>
                (scalan.createManifest(xe.eItem),scalan.createManifest(be.eItem)) match {
                  case (mA: Manifest[a], mB: Manifest[b]) =>
                    val _x = symMirr(x).asInstanceOf[lms.Exp[Array[a]]]
                    val _f = mirrorLambdaToLmsFunc[a, lms.JNIType[b]](m)(f.asInstanceOf[scalan.Lambda[a, lms.JNIType[b]]])
                    val exp = lms.jni_map_object_array[a, b](_x, _f)(mA, mB)
                    (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((fSym, _f)))
                }
            }
        }
      case res@scalan.JNI_NewPrimitive(x) =>
        scalan.createManifest(x.elem) match {
          case mA: Manifest[a_t] =>
            val _x = symMirr(x).asInstanceOf[lms.Exp[a_t]]
            val exp = lms.jni_new_primitive[a_t](_x)(mA)
            (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
        }
      case res@scalan.JNI_NewObject(clazz,mid,args) => res.selfType match {
        case el: scalan.JNITypeElem[_] =>
          scalan.createManifest(el.tElem) match {
            case mA: Manifest[a_t] =>
              val _clazz = symMirr(clazz).asInstanceOf[lms.Exp[lms.JNIClass]]
              val _mid = symMirr(mid).asInstanceOf[lms.Exp[lms.JNIMethodID]]
              val _args = for( arg <- args ) yield symMirr(arg)
              val exp = lms.jni_new_object[a_t](_clazz,_mid,_args)(mA)
              (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
          }
      }
      case res@scalan.JNI_CallObjectMethod(x,mid,args) => (res.selfType, x.elem) match {
        case (resel: scalan.JNITypeElem[_], xel: scalan.JNITypeElem[_]) =>
          (scalan.createManifest(resel.tElem), scalan.createManifest(xel.tElem)) match {
            case (mR: Manifest[r_t], mA: Manifest[a_t]) =>
              val _x = symMirr(x).asInstanceOf[lms.Exp[lms.JNIType[a_t]]]
              val _mid = symMirr(mid).asInstanceOf[lms.Exp[lms.JNIMethodID]]
              val _args = for( arg <- args ) yield symMirr(arg)
              val exp = lms.jni_call_object_method[r_t,a_t](_x,_mid,_args)(mR,mA)
              (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
          }
      }
    }
    tt
  }
}
