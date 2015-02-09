package scalan

import scala.reflect.runtime.universe._
import scalan.common.Default
import scalan.primitives.{AbstractStringsDslExp, AbstractStringsDslSeq, AbstractStringsDsl}

/**
 * Created by zotov on 12/9/14.
 */
trait JNIExtractorOps { self: Scalan with AbstractStringsDsl =>

  class JNIType[T: Elem]
  class JNIClass
  class JNIFieldID

  private implicit val z:Default[JNIClass] = scalan.common.Default.defaultVal[JNIClass](null.asInstanceOf[JNIClass])
  case object JNIClassElem extends BaseElem[JNIClass]

  private implicit val z1:Default[JNIFieldID] = scalan.common.Default.defaultVal[JNIFieldID](null.asInstanceOf[JNIFieldID])
  case object JNIFieldIDElem extends BaseElem[JNIFieldID]

  case class JNITypeElem[T: Elem]() extends Elem[JNIType[T]] {
    val tElem = element[T]
    override val tag = {
      implicit val ttag = element[T].tag
      weakTypeTag[JNIType[T]]
    }

    override def isEntityType: Boolean = element[T].isEntityType

    lazy val getDefaultRep = null.asInstanceOf[Rep[JNIType[T]]]
  }

//  case class JNIFieldIDElem[T: Elem]() extends Elem[JNIFieldID[T]] {
//    val tElem = element[T]
//    override val tag = {
//      implicit val ttag = element[T].tag
//      weakTypeTag[JNIFieldID[T]]
//    }
//
//    override def isEntityType: Boolean = element[T].isEntityType
//
//    lazy val defaultRep = scalan.common.Default.defaultVal[Rep[JNIFieldID[T]]](null.asInstanceOf[JNIFieldID[T]])
//  }

  case class JNIArrayElem[A](eItem: Elem[A]) extends ArrayElem[A] {
    override def isEntityType = eItem.isEntityType

    lazy val tag = {
      implicit val tag1 = eItem.tag
      weakTypeTag[Array[A]]
    }
    lazy val getDefaultRep: Rep[Array[A]] = null.asInstanceOf[Rep[Array[A]]]
  }

  implicit def JNITypeElement[T: Elem]: Elem[JNIType[T]] = new JNITypeElem[T]
  implicit def JNIClassElement: Elem[JNIClass] = JNIClassElem
  implicit def JNIFieldIDElement: Elem[JNIFieldID] = JNIFieldIDElem

  def JNI_Extract[I: Elem](x: Rep[JNIType[I]]): Rep[I]
}

trait JNIExtractorOpsSeq extends JNIExtractorOps { self: ScalanSeq with AbstractStringsDslSeq =>
  def JNI_Extract[I: Elem](x: Rep[JNIType[I]]): Rep[I] = ???
}

trait JNIExtractorOpsExp extends JNIExtractorOps { self: ScalanExp with AbstractStringsDslExp =>

  private def find_class_of_obj[T: Elem](x: Rep[JNIType[T]]): Rep[JNIClass] = x.elem match {
      case jnie: JNITypeElem[_] =>
        val className = jnie.tElem.classTag.runtimeClass.getCanonicalName.replaceAllLiterally(".","/")
        JNI_FindClass(CString(className))
    }

  private def unbox[A: Elem](x: Rep[JNIType[A]]): Rep[A] = {
    val clazz = find_class_of_obj(x)

    val fn = "value"
    val sig = x.elem match {
      case (jnie: JNITypeElem[_]) =>
        org.objectweb.asm.Type.getType(jnie.tElem.classTag.runtimeClass).getDescriptor
    }
    val fid = JNI_GetFieldID(clazz, CString(fn), CString(sig))

    JNI_GetPrimitiveFieldValue[A,A](fid, x)
  }

  private def get_field[A: Elem, T: Elem](fn: String, x: Rep[JNIType[T]]): Rep[A] = {
    val clazz = find_class_of_obj(x)

    val sig = x.elem match {
      case (jnie: JNITypeElem[_]) =>
        org.objectweb.asm.Type.getType(jnie.tElem.classTag.runtimeClass.getField(fn).getType).getDescriptor
    }

    val fid = JNI_GetFieldID(clazz, CString(fn), CString(sig))

    element[A] match {
      case elem if !(elem <:< AnyRefElement) =>
        unbox(JNI_GetObjectFieldValue[A,T](fid, x))
      case _ =>
        JNI_Extract( JNI_GetObjectFieldValue[A,T](fid, x) )
    }
  }

  def JNI_Extract[I: Elem](x: Rep[JNIType[I]]): Rep[I] = {
    element[JNIType[I]].asInstanceOf[JNITypeElem[I]].tElem match {
          case elem if !(elem <:< AnyRefElement) =>
            JNI_ExtractPrimitive[I] (x)

          case (pe: PairElem[a,b]) =>
            implicit val ae = pe.eFst
            implicit val be = pe.eSnd

            var f1 = get_field("_1", x)(ae, pe)
            var f2 = get_field("_2", x)(be, pe)

            Pair(f1, f2)
          case (earr: ArrayElem[a]) =>
            implicit val ea = earr.eItem
            ea match {
              case elem if !(elem <:< AnyRefElement) =>
                JNI_ExtractPrimitiveArray(x)
              case _ =>
                val arr = JNI_ExtractObjectArray(x)
                val len = JNI_GetArrayLength(x)
                val res = SArray.rangeFrom0(len).map( { i => JNI_Extract( JNI_GetObjectArrayItem(arr, i) ) } )
                res.asRep[I]
            }
          case elem =>
            ???(s"Don't know how to extract: elem = ${elem}")
//        }
    }
  }

  case class JNI_FindClass(className: Rep[CString]) extends Def[JNIClass] {
    override def selfType = element[JNIClass]
    override def uniqueOpId = "JNI_FindClass"
    override def mirror(t: Transformer) = JNI_FindClass(t(className))
  }

  case class JNI_GetObjectClass[T: Elem](x: Rep[JNIType[T]]) extends Def[JNIClass] {
    override def selfType = element[JNIClass]
    override def uniqueOpId = "JNI_GetObjectClass"
    override def mirror(t: Transformer) = JNI_GetObjectClass(t(x))
  }

  case class JNI_GetFieldID(clazz: Rep[JNIClass], fname: Rep[CString], sig: Rep[CString]) extends Def[JNIFieldID] {
    override def selfType = element[JNIFieldID]
    override def uniqueOpId = "JNI_GetFieldID"
    override def mirror(t: Transformer) = JNI_GetFieldID(t(clazz), t(fname), t(sig))
  }

  case class JNI_GetObjectFieldValue[A: Elem, T: Elem](fid: Rep[JNIFieldID], x: Rep[JNIType[T]]) extends Def[JNIType[A]] {
    override def selfType = element[JNIType[A]]
    override def uniqueOpId = "JNI_GetObjectFieldValue"
    override def mirror(t: Transformer) = JNI_GetObjectFieldValue[A,T](t(fid), t(x))
  }

  case class JNI_GetPrimitiveFieldValue[A: Elem,T: Elem](fid: Rep[JNIFieldID], tup: Rep[JNIType[T]]) extends Def[A] {
    require( !(element[A] <:< AnyRefElement), "!(" + element[A] + " <:< " + AnyRefElement + ") isn't true")
    override def selfType = element[A]
    override def uniqueOpId = "JNI_GetPrimitiveFieldValue"
    override def mirror(t: Transformer) = JNI_GetPrimitiveFieldValue[A,T](t(fid), t(tup))
  }

  case class JNI_ExtractPrimitive[A: Elem](x: Rep[JNIType[A]]) extends Def[A] {
    require( !(element[A] <:< AnyRefElement), "!(" + element[A] + " <:< " + AnyRefElement + ") isn't true")
    override def selfType = element[A]
    override def uniqueOpId = "JNI_ExtractPrimitive"
    override def mirror(t: Transformer) = JNI_ExtractPrimitive[A](t(x))
  }

  case class JNI_GetArrayLength[A: Elem](arr: Rep[JNIType[Array[A]]]) extends Def[Int] {
    override def selfType = IntElement
    override def uniqueOpId = "JNI_GetArrayLength"
    override def mirror(t: Transformer) = JNI_GetArrayLength(t(arr))
  }

  case class JNI_ExtractObjectArray[A: Elem](x: Rep[JNIType[Array[A]]]) extends Def[Array[JNIType[A]]] {
    require( element[A] <:< AnyRefElement, element[A] + " <:< " + AnyRefElement + " isn't true")
    override def selfType = element[Array[JNIType[A]]]
    override def uniqueOpId = "JNI_ExtractObjectArray"
    override def mirror(t: Transformer) = JNI_ExtractObjectArray(t(x))
  }

  case class JNI_GetObjectArrayItem[A: Elem](arr: Rep[Array[JNIType[A]]], i: Rep[Int] ) extends Def[JNIType[A]] {
    require( element[A] <:< AnyRefElement, element[A] + " <:< " + AnyRefElement + " isn't true")
    override def selfType = element[JNIType[A]]
    override def uniqueOpId = "JNI_GetObjectArrayItem"
    override def mirror(t: Transformer) = JNI_GetObjectArrayItem(t(arr), t(i))
  }

  case class JNI_ExtractPrimitiveArray[A: Elem](x: Rep[JNIType[Array[A]]]) extends Def[Array[A]] {
    require( !(element[A] <:< AnyRefElement), "!(" + element[A] + " <:< " + AnyRefElement + ") isn't true")
    override def selfType = JNIArrayElem(element[A])
    override def uniqueOpId = "JNI_ExtractPrimitiveArray"
    override def mirror(t: Transformer) = JNI_ExtractPrimitiveArray[A](t(x))
  }
}
