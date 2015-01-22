package scalan.compilation.lms

import scalan.{JNIExtractorOpsExp, ScalanExp}
import scalan.linalgebra.VectorsDslExp

trait JNIBridge[A, B] extends LmsBridge[A, B] { self: LmsBridge[A, B] =>

  // `LmsCompiler` mixed just to provide `createManifest` function
  val scalan: ScalanExp with JNIExtractorOpsExp with LmsCompiler

  abstract override def defTransformer[T](m: Mirror, g: scalan.AstGraph, e: scalan.TableEntry[T]) = {
    jniDefTransformer(m, g, e) orElse super.defTransformer(m, g, e)
  }

  def jniDefTransformer[T](m: Mirror, g: scalan.AstGraph, e: scalan.TableEntry[T]): DefTransformer = {
    val (exps, symMirr, funcMirr) = m
    val sym = e.sym
    val tt: DefTransformer = {
      case p@scalan.Tup(a,b) =>
        val mA = scalan.createManifest(a.elem)
        val mB = scalan.createManifest(b.elem)
        (mA,mB) match {
          case (ma: Manifest[a_t], mb: Manifest[b_t]) =>
            val _a = symMirr(a).asInstanceOf[lms.Exp[a_t]]
            val _b = symMirr(b).asInstanceOf[lms.Exp[b_t]]
            val exp = lms.tuple(_a, _b)(ma,mb)
            (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
        }
      case res@scalan.JNIStringConst(_) =>
        val exp = lms.JNIStringConst(res.str)
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
    }
    tt
  }
}
