package scalan.compilation.lms

import scalan.compilation.language.MethodMapping
import scalan.compilation.lms.common.{JNILmsOps, JNILmsOpsExp}
import scalan.primitives.AbstractStringsDslExp
import scalan.{ScalanCtxExp, JNIExtractorOpsExp}

trait JNIBridge extends CoreBridge { self: ScalanCtxExp with MethodMapping with JNIExtractorOpsExp with AbstractStringsDslExp =>

  val lms: CoreLmsBackendBase with JNILmsOpsExp

  override def createManifest[T]: PartialFunction[Elem[T], Manifest[_]] = {
    case el: JNITypeElem[_] =>
      Manifest.classType(classOf[JNILmsOps#JNIType[_]], createManifest(el.tElem))
    case el: JNIArrayElem[arr_t] =>
      el.eItem match {
        case ei: Elem[a_t] =>
          val mItem = createManifest(ei)
          Manifest.classType(classOf[JNILmsOps#JNIArray[a_t]], mItem)
      }
    case el =>
      super.createManifest(el)
  }

  abstract override def defTransformer[T](m: LmsMirror, g: AstGraph, e: TableEntry[T]) = {
    jniDefTransformer(m, g, e) orElse super.defTransformer(m, g, e)
  }

  def jniDefTransformer[T](m: LmsMirror, g: AstGraph, e: TableEntry[T]): DefTransformer = {
    val (exps, symMirr, funcMirr) = m
    val sym = e.sym
    val tt: DefTransformer = {
      case res@ExpCString(Def(s: Const[_])) =>
        val exp = lms.JNIStringConst(s.x)
        (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
      case res@JNI_ExtractPrimitive(x) =>
        x.elem match {
          case (jnie: JNITypeElem[jni_a_t]) =>
            createManifest(jnie.tElem) match {
              case (mA: Manifest[a_t]) =>
                val _x = symMirr(x).asInstanceOf[lms.Exp[lms.JNIType[a_t]]]
                val exp = lms.jni_extract_primitive[a_t](_x)(mA)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
            }
        }
      case res@JNI_ExtractPrimitiveArray(x) =>
        x.elem match {
          case (jnie: JNITypeElem[jni_arr_a_t]) =>
            createManifest(jnie.tElem) match {
            case (mA: Manifest[arr_a_t]) =>
              mA.typeArguments(0) match {
                case(mItem: Manifest[a_t]) =>
                  val _x = symMirr(x).asInstanceOf[lms.Exp[lms.JNIType[Array[a_t]]]]
                  val exp = lms.jni_extract_primitive_array(_x)(mItem)
                  (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
        }
      case res@JNI_GetArrayLength(x) =>
        x.elem match {
          case (jnie: JNITypeElem[jni_arr_a_t]) =>
            createManifest(jnie.tElem) match {
            case (mA: Manifest[arr_a_t]) =>
              mA.typeArguments(0) match {
                case (mItem: Manifest[a_t]) =>
                  val _x = symMirr(x).asInstanceOf[lms.Exp[lms.JNIType[Array[a_t]]]]
                  val exp = lms.jni_get_array_length(_x)(mItem)
                  (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
              }
            }
        }
      case res@JNI_ExtractObjectArray(x) =>
        x.elem match {
          case (jnie: JNITypeElem[jni_arr_a_t]) =>
            implicit val m = createManifest(jnie.tElem)
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
      case res@JNI_GetObjectArrayItem(x, i) =>
        x.elem match {
          case arrel: ArrayElem[arr_jni_a_t] =>
            arrel.eItem match {
              case (jnie: JNITypeElem[jni_a_t]) =>
                implicit val m = createManifest(jnie.tElem)
                m match {
                  case (mA: Manifest[a_t]) =>
                    val _x = symMirr(x).asInstanceOf[lms.Exp[Array[lms.JNIType[a_t]]]]
                    val _i = symMirr(i).asInstanceOf[lms.Exp[Int]]
                    val exp = lms.jni_get_object_array_item(_x, _i)(mA)
                    (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
                }
            }
        }
      case res@JNI_GetObjectClass(x) =>
        x.elem match {
          case (jnie: JNITypeElem[jni_a_t]) =>
            implicit val m = createManifest(jnie.tElem)
            m match {
              case (mA: Manifest[a_t]) =>
                val _x = symMirr(x).asInstanceOf[lms.Exp[lms.JNIType[a_t]]]
                val exp = lms.jni_get_object_class(_x)(mA)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
            }
        }
      case res@JNI_FindClass(x) =>
        val _x = symMirr(x).asInstanceOf[lms.Exp[String]]
        val exp = lms.jni_find_class(_x)
        (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
      case res@JNI_GetFieldID(clazz, fn, sig) =>
        val _clazz = symMirr(clazz).asInstanceOf[lms.Exp[ lms.JNIClass] ]
        val _fn = symMirr(fn).asInstanceOf[lms.Exp[String]]
        val _sig = symMirr(sig).asInstanceOf[lms.Exp[String]]
        val exp = lms.jni_get_field_id(_clazz, _fn, _sig)
        (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
      case res@JNI_GetObjectFieldValue(fid, x) =>
        (res.selfType, x.elem) match {
          case (jnife: JNITypeElem[jni_A_t], jnie: JNITypeElem[jni_T_t]) =>
            val mA = createManifest(jnife.tElem)
            val mT = createManifest(jnie.tElem)
            (mA,mT) match {
              case (ma: Manifest[a_t], mt: Manifest[t_t]) =>
                val _x = symMirr(x).asInstanceOf[lms.Exp[lms.JNIType[t_t]]]
                val _fid = symMirr(fid).asInstanceOf[lms.Exp[lms.JNIFieldID]]
                val exp = lms.jni_get_object_field_value[a_t,t_t](_fid, _x)(ma,mt)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
            }
        }
      case res@JNI_GetPrimitiveFieldValue(fid, x) =>
        (res.selfType, x.elem) match {
          case (rese: Element[jnifid_A_t], jnie: JNITypeElem[jni_T_t]) =>
            val mA = createManifest(rese)
            val mT = createManifest(jnie.tElem)
            (mA,mT) match {
              case (ma: Manifest[a_t], mt: Manifest[t_t]) =>
                val _x = symMirr(x).asInstanceOf[lms.Exp[lms.JNIType[t_t]]]
                val _fid = symMirr(fid).asInstanceOf[lms.Exp[lms.JNIFieldID]]
                val exp = lms.jni_get_primitive_field_value[a_t,t_t](_fid, _x)(ma,mt)
                (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
            }
        }
      case res@JNI_GetMethodID(clazz, mn, sig) =>
        val _clazz = symMirr(clazz).asInstanceOf[lms.Exp[ lms.JNIClass] ]
        val _mn = symMirr(mn).asInstanceOf[lms.Exp[String]]
        val _sig = symMirr(sig).asInstanceOf[lms.Exp[String]]
        val exp = lms.jni_get_method_id(_clazz, _mn, _sig)
        (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
      case res@JNI_MapPrimitiveArray(x, fSym@Def(f: Lambda[_, _])) =>
        (res.selfType, x.elem) match {
          case (rese: JNITypeElem[jniarr_t], xe: ArrayElem[_]) =>
            rese.tElem match {
              case be: ArrayElem[_] =>
                (createManifest(xe.eItem),createManifest(be.eItem)) match {
                  case (mA: Manifest[a], mB: Manifest[b]) =>
                    val _x = symMirr(x).asInstanceOf[lms.Exp[Array[a]]]
                    val _f = mirrorLambdaToLmsFunc[a, b](m)(f.asInstanceOf[Lambda[a, b]])
                    val exp = lms.jni_map_array[a, b](_x, _f)(mA, mB)
                    (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((fSym, _f)))
                }
            }
        }
      case res@JNI_MapObjectArray(x, fSym@Def(f: Lambda[_, _])) =>
        (res.selfType, x.elem) match {
          case (rese: JNITypeElem[_], xe: ArrayElem[_]) =>
            rese.tElem match {
              case be: ArrayElem[_] =>
                (createManifest(xe.eItem),createManifest(be.eItem)) match {
                  case (mA: Manifest[a], mB: Manifest[b]) =>
                    val _x = symMirr(x).asInstanceOf[lms.Exp[Array[a]]]
                    val _f = mirrorLambdaToLmsFunc[a, lms.JNIType[b]](m)(f.asInstanceOf[Lambda[a, lms.JNIType[b]]])
                    val exp = lms.jni_map_object_array[a, b](_x, _f)(mA, mB)
                    (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr + ((fSym, _f)))
                }
            }
        }
      case res@JNI_NewPrimitive(x) =>
        createManifest(x.elem) match {
          case mA: Manifest[a_t] =>
            val _x = symMirr(x).asInstanceOf[lms.Exp[a_t]]
            val exp = lms.jni_new_primitive[a_t](_x)(mA)
            (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
        }
      case res@JNI_NewObject(clazz,mid,args@_*) => res.selfType match {
        case el: JNITypeElem[_] =>
          createManifest(el.tElem) match {
            case mA: Manifest[a_t] =>
              val _clazz = symMirr(clazz).asInstanceOf[lms.Exp[lms.JNIClass]]
              val _mid = symMirr(mid).asInstanceOf[lms.Exp[lms.JNIMethodID]]
              val _args = for( arg <- args ) yield symMirr(arg)
              val exp = lms.jni_new_object[a_t](_clazz,_mid,_args:_*)(mA)
              (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
          }
      }
      case res@JNI_CallObjectMethod(x,mid,args@_*) => (res.selfType, x.elem) match {
        case (resel: JNITypeElem[_], xel: JNITypeElem[_]) =>
          (createManifest(resel.tElem), createManifest(xel.tElem)) match {
            case (mR: Manifest[r_t], mA: Manifest[a_t]) =>
              val _x = symMirr(x).asInstanceOf[lms.Exp[lms.JNIType[a_t]]]
              val _mid = symMirr(mid).asInstanceOf[lms.Exp[lms.JNIMethodID]]
              val _args = for( arg <- args ) yield symMirr(arg)
              val exp = lms.jni_call_object_method[r_t,a_t](_x,_mid,_args:_*)(mR,mA)
              (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
          }
      }
    }
    tt
  }
}
