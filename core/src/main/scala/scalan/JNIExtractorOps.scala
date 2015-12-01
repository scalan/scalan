package scalan

import scala.reflect.runtime.universe._
import scalan.common.Default
import scalan.primitives.{AbstractStringsDslExp, AbstractStringsDslSeq, AbstractStringsDsl}

/**
 * Created by zotov on 12/9/14.
 */
trait JNIExtractorOps extends Base { self: Scalan with AbstractStringsDsl =>

  class JNIType[T]
  class JNIClass
  class JNIFieldID
  class JNIMethodID

  private implicit val z:Default[JNIClass] = scalan.common.Default.defaultVal[JNIClass](null.asInstanceOf[JNIClass])
  case object JNIClassElem extends BaseElem[JNIClass]

  private implicit val z1:Default[JNIFieldID] = scalan.common.Default.defaultVal[JNIFieldID](null.asInstanceOf[JNIFieldID])
  case object JNIFieldIDElem extends BaseElem[JNIFieldID]

  private implicit val z2:Default[JNIMethodID] = scalan.common.Default.defaultVal[JNIMethodID](null.asInstanceOf[JNIMethodID])
  case object JNIMethodIDElem extends BaseElem[JNIMethodID]

  case class JNITypeElem[T](eT: Elem[T]) extends Elem[JNIType[T]] {
    override val tag = {
      implicit val ttag = eT.tag
      weakTypeTag[JNIType[T]]
    }

    override def isEntityType: Boolean = eT.isEntityType

    lazy val getDefaultRep = null.asInstanceOf[Rep[JNIType[T]]]
  }

//  case class JNIFieldIDElem[T](eT: Elem[T]) extends Elem[JNIFieldID[T]] {
//    override val tag = {
//      implicit val ttag = element[T].tag
//      weakTypeTag[JNIFieldID[T]]
//    }
//
//    override def isEntityType: Boolean = element[T].isEntityType
//
//    lazy val defaultRep = scalan.common.Default.defaultVal[Rep[JNIFieldID[T]]](null.asInstanceOf[JNIFieldID[T]])
//  }

  case class JNIArrayElem[A](override val eItem: Elem[A]) extends ArrayElem[A]()(eItem) {
    def parent: Option[Elem[_]] = Some(arrayElement(eItem))
    override def isEntityType = eItem.isEntityType
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eItem))
    }
    override def getName = "JNI-" + cleanUpTypeName(tag.tpe)

    lazy val tag = {
      implicit val tag1 = eItem.tag
      weakTypeTag[Array[A]]
    }
    lazy val getDefaultRep: Rep[Array[A]] = null.asInstanceOf[Rep[Array[A]]]

    override def canEqual(other: Any) = other.isInstanceOf[JNIArrayElem[_]]
  }

  implicit def JNITypeElement[T: Elem]: Elem[JNIType[T]] = cachedElem[JNITypeElem[T]](element[T])
  implicit def JNIClassElement: Elem[JNIClass] = JNIClassElem
  implicit def JNIFieldIDElement: Elem[JNIFieldID] = JNIFieldIDElem
  implicit def JNIMethodIDElement: Elem[JNIMethodID] = JNIMethodIDElem
  def jniArrayElem[A](eItem: Elem[A]) = cachedElem[JNIArrayElem[A]](eItem)

  def JNI_Extract[I](x: Rep[JNIType[I]]): Rep[I]
  def JNI_Pack[T](x: Rep[T]): Rep[JNIType[T]]

  def JNI_Wrap[A, B](f: Rep[A => B]): Rep[JNIType[A] => JNIType[B]]
}

trait JNIExtractorOpsSeq extends JNIExtractorOps { self: ScalanSeq with AbstractStringsDslSeq =>
  def JNI_Extract[I](x: Rep[JNIType[I]]): Rep[I] = ???
  def JNI_Pack[T](x: Rep[T]): Rep[JNIType[T]] = ???
  def JNI_Wrap[A, B](f: Rep[A => B]) =
    x => JNI_Pack(f(JNI_Extract(x)))
}

trait JNIExtractorOpsExp extends JNIExtractorOps { self: ScalanExp with AbstractStringsDslExp =>

  private def find_class[T](clazz: Class[T]): Rep[JNIClass] = {
    val className = clazz.getCanonicalName.replaceAllLiterally(".","/")
    JNI_FindClass(CString(className))
  }

  private def find_class_of_obj[T: Elem](x: Rep[JNIType[T]]): Rep[JNIClass] = x.elem match {
      case jnie: JNITypeElem[_] =>
        val clazz = jnie.eT match {
          case el if el <:< AnyRefElement =>
            el.runtimeClass
          case el =>
            anyval_boxed_class(el)
        }
        find_class(clazz)
    }

  private def unbox[A: Elem](x: Rep[JNIType[A]]): Rep[A] = {
    val clazz = find_class_of_obj(x)

    val fn = "value"
    val sig = x.elem match {
      case (jnie: JNITypeElem[_]) =>
        org.objectweb.asm.Type.getType(jnie.eT.runtimeClass).getDescriptor
    }
    val fid = JNI_GetFieldID(clazz, CString(fn), CString(sig))

    JNI_GetPrimitiveFieldValue[A,A](fid, x)
  }

  private def get_method_id[T: Elem](x: Rep[JNIType[T]], mn: String, args: Rep[Any]*): Rep[JNIMethodID] = {
    val clazz = find_class_of_obj(x)
    val sig = x.elem match {
      case (jnie: JNITypeElem[_]) =>
        val argclass = args.map({arg => arg.elem.runtimeClass})
        org.objectweb.asm.Type.getMethodDescriptor(jnie.eT.runtimeClass.getMethod(mn, argclass:_*))
    }

    JNI_GetMethodID(clazz, CString(mn), CString(sig))
  }

  private def call_primitive_method[A: Elem, T: Elem](x: Rep[JNIType[T]], mn: String, args: Rep[Any]*): Rep[A] = {
    val mid = get_method_id(x, mn, args:_*)
    JNI_Extract( JNI_CallPrimitiveMethod[A,T](mid, x, args:_*) )
  }

  private def call_object_method[A: Elem, T: Elem](x: Rep[JNIType[T]], mn: String, args: Rep[Any]*): Rep[JNIType[A]] = {
    val mid = get_method_id(x, mn, args:_*)
    JNI_CallObjectMethod[A,T](x, mid, args:_*)
  }

  def JNI_Extract[I](x: Rep[JNIType[I]]): Rep[I] = {
    implicit val eI = x.elem.asInstanceOf[JNITypeElem[I]].eT
    eI match {
          case elem if !(elem <:< AnyRefElement) =>
            JNI_ExtractPrimitive[I](x)

          case (pe: PairElem[a,b]) =>
            implicit val ae = pe.eFst
            implicit val be = pe.eSnd

            val a1 = if( !(ae <:< AnyRefElement) )
              unbox( call_object_method(x, "_1")(ae,pe) )(ae)
            else
              JNI_Extract( call_object_method(x, "_1")(ae,pe) )

            val b1 = if( !(be <:< AnyRefElement) )
              unbox( call_object_method(x, "_2")(be,pe) )(be)
            else
              JNI_Extract( call_object_method(x, "_2")(be,pe) )

            Pair(a1, b1)
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
            ???(s"Don't know how to extract: elem = ${elem}", x)
//        }
    }
  }

  private def anyval_boxed_class[T]: PartialFunction[Elem[T],Class[_]] = {
    case BooleanElement => classOf[java.lang.Boolean]
    case ByteElement => classOf[java.lang.Byte]
    case ShortElement => classOf[java.lang.Short]
    case IntElement => classOf[java.lang.Integer]
    case LongElement => classOf[java.lang.Long]
    case FloatElement => classOf[java.lang.Float]
    case DoubleElement => classOf[java.lang.Double]
    case CharElement => classOf[java.lang.Character]
  }

  private def box[T: Elem](x: Rep[JNIType[T]]): Rep[JNIType[T]] = {
    element[T] match {
      case el if el <:< AnyRefElement =>
        x
      case el =>
        val jniclazz = find_class(anyval_boxed_class(el))
        val mn = "<init>"
        val argdescr = org.objectweb.asm.Type.getType(el.runtimeClass).getDescriptor
        val sig = s"(${argdescr})V"

        val mid = JNI_GetMethodID(jniclazz, CString(mn), CString(sig))

        JNI_NewObject[T](jniclazz, mid, x)
    }
  }

  private def make_pair[A: Elem, B: Elem](a: Rep[JNIType[A]], b: Rep[JNIType[B]]): Rep[JNIType[(A,B)]] = {
    val clazz = find_class(classOf[(A,B)])
    val mn = "<init>"
    val sig = s"(Ljava/lang/Object;Ljava/lang/Object;)V"

    val mid = JNI_GetMethodID(clazz, CString(mn), CString(sig))
    JNI_NewObject[(A,B)](clazz, mid, a, b)
  }


  def JNI_Pack[T](x: Rep[T]): Rep[JNIType[T]] = {
    x.elem match {
      case el: PairElem[a,b] =>
        val p = x.asInstanceOf[Rep[(a,b)]]
        implicit val eA = el.eFst
        implicit val eB = el.eSnd
        make_pair[a,b]( box( JNI_Pack[a](p._1) ), box( JNI_Pack[b](p._2) ) )
      case el: ArrayElem[a] =>
        implicit val eA = el.eItem
        el.eItem match {
          case eI if eI <:< AnyRefElement =>
            JNI_MapObjectArray[a,a](x, JNI_Pack(_: Rep[a]))
          case _ =>
            JNI_MapPrimitiveArray[a,a](x, identityFun[a])
        }
      case el: BaseElem[_] =>
        el match {
          case e if e.tag.tpe <:< TypeTag.AnyVal.tpe =>
            JNI_NewPrimitive(x)(el)
          case _ =>
            ???(s"Don't know how to pack: elem = ${el}", x)
        }
      case elem =>
        ???(s"Don't know how to pack: elem = ${elem}", x)
    }
  }

  def JNI_Wrap[A, B](f: Rep[A => B]) = {
    implicit val eA = f.elem.eDom
    implicit val eB = f.elem.eRange

    fun[JNIType[A], JNIType[B]] { x =>
      val unpackedX = JNI_Extract(x)
      val unpackedY = f(unpackedX)
      JNI_Pack(unpackedY)
    }
  }

  private[this] def isPrimitive(e: Elem[_]) = !(e <:< AnyRefElement)
  private[this] def isObject(e: Elem[_]) = e <:< AnyRefElement

  case class JNI_NewObject[T](clazz: Rep[JNIClass], mid: Rep[JNIMethodID], args: Rep[JNIType[_]]*)(implicit val eT: Elem[T]) extends BaseDef[JNIType[T]]
  case class JNI_NewPrimitive[T](x: Rep[T])(implicit val eT: Elem[T]) extends BaseDef[JNIType[T]] {
    require(isPrimitive(eT))
  }

  case class JNI_MapPrimitiveArray[A, B](x: Rep[Array[A]], f: Rep[A => B])(implicit val eA: Elem[A], val eB: Elem[B]) extends BaseDef[JNIType[Array[B]]] {
    require(isPrimitive(eA))
  }

  case class JNI_MapObjectArray[A, B](x: Rep[Array[A]], f: Rep[A => JNIType[B]])(implicit val eA: Elem[A], val eB: Elem[B]) extends BaseDef[JNIType[Array[B]]] {
    require(isObject(eA))
  }

  case class JNI_FindClass(className: Rep[CString]) extends BaseDef[JNIClass]

  case class JNI_GetObjectClass[T](x: Rep[JNIType[T]])(implicit val eT: Elem[T]) extends BaseDef[JNIClass]

  case class JNI_GetFieldID(clazz: Rep[JNIClass], fname: Rep[CString], sig: Rep[CString]) extends BaseDef[JNIFieldID]

  case class JNI_GetMethodID(clazz: Rep[JNIClass], mname: Rep[CString], sig: Rep[CString]) extends BaseDef[JNIMethodID]

  case class JNI_GetObjectFieldValue[A, T](fid: Rep[JNIFieldID], x: Rep[JNIType[T]])(implicit val eA: Elem[A], val eT: Elem[T]) extends BaseDef[JNIType[A]]

  case class JNI_GetPrimitiveFieldValue[A, T](fid: Rep[JNIFieldID], tup: Rep[JNIType[T]])(implicit val eA: Elem[A], val eT: Elem[T]) extends BaseDef[A] {
    require(isPrimitive(eA))
  }

  case class JNI_CallObjectMethod[A, T](x: Rep[JNIType[T]], mid: Rep[JNIMethodID], args: Rep[Any]*)(implicit val eA: Elem[A], val eT: Elem[T]) extends BaseDef[JNIType[A]]

  case class JNI_CallPrimitiveMethod[A, T](mid: Rep[JNIMethodID], x: Rep[JNIType[T]], args: Rep[Any]*)(implicit val eA: Elem[A], val eT: Elem[T]) extends BaseDef[JNIType[A]] {
    require(isPrimitive(eA))
  }

  case class JNI_ExtractPrimitive[A](x: Rep[JNIType[A]])(implicit val eA: Elem[A]) extends BaseDef[A] {
    require(isPrimitive(eA))
  }

  case class JNI_GetArrayLength[A](arr: Rep[JNIType[Array[A]]])(implicit val eA: Elem[A]) extends BaseDef[Int]

  case class JNI_ExtractObjectArray[A](x: Rep[JNIType[Array[A]]])(implicit val eA: Elem[A]) extends BaseDef[Array[JNIType[A]]] {
    require(isObject(eA))
  }

  case class JNI_GetObjectArrayItem[A](arr: Rep[Array[JNIType[A]]], i: Rep[Int])(implicit val eA: Elem[A]) extends BaseDef[JNIType[A]] {
    require(isObject(eA))
  }

  case class JNI_ExtractPrimitiveArray[A](x: Rep[JNIType[Array[A]]])(implicit val eA: Elem[A]) extends Def[Array[A]] {
    require(isPrimitive(eA))
    override def selfType = jniArrayElem(element[A])
  }

  case class JNI_NewPrimitiveArray[A](len: Rep[Int])(implicit val eA: Elem[A]) extends BaseDef[JNIType[Array[A]]] {
    require(isPrimitive(eA))
  }
}
