package scalan

import org.objectweb.asm.{Type => TypeDescriptors}

import scala.reflect.runtime.universe._
import scalan.primitives.{AbstractStringsDsl, AbstractStringsDslExp, AbstractStringsDslStd}

/**
 * Created by zotov on 12/9/14.
 */
trait JNIExtractorOps extends Base { self: Scalan with AbstractStringsDsl =>

  class JNIType[T]
  class JNIClass
  class JNIFieldID
  class JNIMethodID

  case object JNIClassElem extends BaseElem[JNIClass](null)

  case object JNIFieldIDElem extends BaseElem[JNIFieldID](null)

  case object JNIMethodIDElem extends BaseElem[JNIMethodID](null)

  case class JNITypeElem[T](eT: Elem[T]) extends Elem[JNIType[T]] {
    override val tag = {
      implicit val ttag = eT.tag
      weakTypeTag[JNIType[T]]
    }

    override def isEntityType: Boolean = eT.isEntityType

    lazy val getDefaultRep = null.asInstanceOf[Rep[JNIType[T]]]

    lazy val typeArgs = TypeArgs("T" -> eT)
  }

  case class JNIArrayElem[A](override val eItem: Elem[A]) extends ArrayElem[A]()(eItem) {
    def parent: Option[Elem[_]] = Some(arrayElement(eItem))
    override def isEntityType = eItem.isEntityType
    override lazy val typeArgs = TypeArgs("A" -> eItem)
    override def getName(f: TypeDesc => String) = s"JNI-${super.getName(f)}"

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

trait JNIExtractorOpsStd extends JNIExtractorOps { self: ScalanStd with AbstractStringsDslStd =>
  def JNI_Extract[I](x: Rep[JNIType[I]]): Rep[I] = ???
  def JNI_Pack[T](x: Rep[T]): Rep[JNIType[T]] = ???
  def JNI_Wrap[A, B](f: Rep[A => B]) =
    x => JNI_Pack(f(JNI_Extract(x)))
}

trait JNIExtractorOpsExp extends JNIExtractorOps { self: ScalanExp with AbstractStringsDslExp =>

  private def elemToDescriptor(e: Elem[_]) = TypeDescriptors.getDescriptor(e.runtimeClass)

  private def find_class[T](clazz: Class[T]): Rep[JNIClass] = {
    val className = clazz.getCanonicalName.replaceAllLiterally(".","/")
    JNI_FindClass(CString(className))
  }

  private def find_boxed_class[T: Elem]: Rep[JNIClass] = find_class(boxed_class(element[T]))

  private def unbox[A: Elem](x: Rep[JNIType[A]]): Rep[A] = {
    val clazz = find_boxed_class[A]

    val sig = elemToDescriptor(element[A])
    val fid = JNI_GetFieldID(clazz, CString("value"), CString(sig))

    JNI_GetPrimitiveFieldValue[A,A](fid, x)
  }

  private def get_method_id[A, T: Elem](methodName: String, returnClass: Class[_], argClasses: Class[_]*): Rep[JNIMethodID] = {
    val clazz = find_boxed_class[T]
    val sig = argClasses.map(TypeDescriptors.getDescriptor).mkString("(", "", ")") + TypeDescriptors.getDescriptor(returnClass)

    JNI_GetMethodID(clazz, CString(methodName), CString(sig))
  }

  private def call_object_method[A: Elem, T: Elem](x: Rep[JNIType[T]], methodName: String, args: (Class[_], Rep[Any])*): Rep[JNIType[A]] =
    call_object_method[A, T](x, methodName, element[A].runtimeClass, args: _*)

  private def call_object_method[A: Elem, T: Elem](x: Rep[JNIType[T]], methodName: String, returnClass: Class[_], args: (Class[_], Rep[Any])*): Rep[JNIType[A]] = {
    val methodId = get_method_id[A, T](methodName, returnClass, args.map(_._1): _*)
    JNI_CallObjectMethod[A, T](x, methodId, args.map(_._2): _*)
  }

  def JNI_Extract[I](x: Rep[JNIType[I]]): Rep[I] = {
    implicit val eI = x.elem.asInstanceOf[JNITypeElem[I]].eT

    eI match {
      case elem if isPrimitive(elem) =>
        JNI_ExtractPrimitive[I](x)

      case pe: PairElem[a, b] =>
        implicit val ae = pe.eFst
        implicit val be = pe.eSnd

        def extract_pair_field[A](name: String)(implicit e: Elem[A]) = {
          // _1 and _2 JVM method return type is Object
          val jniMethodCallResult = call_object_method(x, name, classOf[Object])(e, eI)
          if (isPrimitive(e))
            unbox(jniMethodCallResult)(e)
          else
            JNI_Extract(jniMethodCallResult)
        }

        val a1 = extract_pair_field("_1")(ae)
        val b1 = extract_pair_field("_2")(be)

        Pair(a1, b1)

      case se: StructElem[a] =>
        // a == I
        type S = a with Struct
        val s = x.asRep[JNIType[S]]

        val fields = se.fields.map { case (name, fieldElem: Elem[f]) =>
          (name, JNI_Extract(JNI_GetStructFieldValue[f, S](s, name)(fieldElem)))
        }
        struct(se.structTag.asInstanceOf[StructTag[S]], fields).asRep[I]

      case earr: ArrayElem[a] =>
        implicit val ea = earr.eItem
        ea match {
          case _ if isPrimitive(ea) =>
            JNI_ExtractPrimitiveArray(x)
          case _ =>
            val arr = JNI_ExtractObjectArray(x)
            val len = JNI_GetArrayLength(x)
            val res = SArray.tabulate(len) { i => JNI_Extract(JNI_GetObjectArrayItem(arr, i)) }
            res.asRep[I]
        }
      case elem =>
        ???(s"Don't know how to extract: elem = ${elem}", x)
    }
  }

  private def boxed_class(e: Elem[_]): Class[_] = e match {
    case BooleanElement => classOf[java.lang.Boolean]
    case ByteElement => classOf[java.lang.Byte]
    case ShortElement => classOf[java.lang.Short]
    case IntElement => classOf[java.lang.Integer]
    case LongElement => classOf[java.lang.Long]
    case FloatElement => classOf[java.lang.Float]
    case DoubleElement => classOf[java.lang.Double]
    case CharElement => classOf[java.lang.Character]
    case _ => e.runtimeClass
  }

  private def construct[A: Elem](clazz: Class[_], args: (Class[_], Rep[JNIType[_]])*): Rep[JNIType[A]] = {
    val jniClazz = find_class(clazz)
    val (argClasses, argValues) = args.unzip
    val signature = argClasses.map(TypeDescriptors.getDescriptor).mkString("(", "", ")V")
    val constructorId = JNI_GetMethodID(jniClazz, CString("<init>"), CString(signature))

    JNI_NewObject[A](jniClazz, constructorId, argValues: _*)
  }

  private def box[T](x: Rep[JNIType[T]])(implicit eT: Elem[T]): Rep[JNIType[T]] = eT match {
    case _ if isPrimitive(eT) =>
      construct[T](boxed_class(eT), (eT.runtimeClass, x))
    case _ =>
      x
  }

  private def make_pair[A: Elem, B: Elem](a: Rep[JNIType[A]], b: Rep[JNIType[B]]): Rep[JNIType[(A,B)]] = {
    construct(classOf[(_, _)], (classOf[Object], box(a)), (classOf[Object], box(b)))
  }

  def JNI_Pack[T](x: Rep[T]): Rep[JNIType[T]] = {
    x.elem match {
      case el: PairElem[a, b] =>
        val p = x.asInstanceOf[Rep[(a, b)]]
        implicit val eA = el.eFst
        implicit val eB = el.eSnd
        make_pair[a, b](JNI_Pack[a](p._1), JNI_Pack[b](p._2))
      case se: StructElem[a] =>
        val s = x.asRep[a with Struct]
        val fields = se.fields.map {
          case (name, _) => JNI_Pack(field(s, name))
        }
        reifyObject(JNI_NewStruct(se.asInstanceOf[StructElem[a with Struct]], fields: _*)).asRep[JNIType[T]]
      case el: ArrayElem[a] =>
        implicit val eA = el.eItem
        eA match {
          case _ if isPrimitive(eA) =>
            JNI_MapPrimitiveArray[a, a](x, identityFun[a])
          case _ =>
            JNI_MapObjectArray[a, a](x, JNI_Pack(_: Rep[a]))
        }
      case el: BaseElem[_] if isPrimitive(el) =>
        JNI_NewPrimitive(x)(el)
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

  private[this] def isPrimitive(e: Elem[_]) = e match {
    case BooleanElement => true
    case ByteElement => true
    case ShortElement => true
    case IntElement => true
    case LongElement => true
    case FloatElement => true
    case DoubleElement => true
    case CharElement => true
    case _ => false
  }
  private[this] def isObject(e: Elem[_]) = !isPrimitive(e)

  case class JNI_NewObject[T](clazz: Rep[JNIClass], mid: Rep[JNIMethodID], args: Rep[JNIType[_]]*)(implicit val eT: Elem[T]) extends BaseDef[JNIType[T]]
  case class JNI_NewPrimitive[T](x: Rep[T])(implicit val eT: Elem[T]) extends BaseDef[JNIType[T]] {
    require(isPrimitive(eT))
  }
  // Separate from JNI_NewObject because we don't know the name of the class which implements the structure here.
  // This has to be handled in lms-backend, see JNILmsOps.jni_new_struct.
  // Note LMS plans to move structName from StructsExp to codegen, in which case we could override it and remove
  // this class
  case class JNI_NewStruct[T <: Struct](se: StructElem[T], args: Rep[JNIType[_]]*) extends BaseDef[JNIType[T]]()(JNITypeElement(se))

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

  // exists for the same reason as JNI_NewStruct
  case class JNI_GetStructFieldValue[A, T <: Struct](struct: Rep[JNIType[T]], fieldName: String)(implicit val eA: Elem[A]) extends BaseDef[JNIType[A]]

  case class JNI_CallObjectMethod[A, T](x: Rep[JNIType[T]], mid: Rep[JNIMethodID], args: Rep[Any]*)(implicit val eA: Elem[A], val eT: Elem[T]) extends BaseDef[JNIType[A]]

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
