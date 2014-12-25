package scalan

import scala.reflect.runtime.universe._
/**
 * Created by zotov on 12/9/14.
 */
trait JNIExtractorOps { self: Scalan =>

  class JNIType[T: Elem]

  class JNIElem[T: Elem] extends Element[JNIType[T]] {
    val tElem = element[T]
    override val tag = {
      implicit val ttag = element[T].tag
      typeTag[JNIType[T]]
    }

    override def isEntityType: Boolean = element[T].isEntityType

    lazy val defaultRep = scalan.common.Default.defaultVal[Rep[JNIType[T]]](null.asInstanceOf[JNIType[T]])
  }

  implicit def JNIElement[T: Elem]: Element[JNIType[T]] = new JNIElem[T]

  def JNI_ExtractPrimitive[I <: AnyVal : Elem](env: Rep[AnyRef], x: Rep[JNIType[I]]): Rep[I] = ???
  def JNI_ExtractObject[I: Elem](env: Rep[AnyRef], x: Rep[JNIType[AnyRef]]): Rep[I] = ???
}

trait JNIExtractorOpsSeq extends JNIExtractorOps { self: ScalanSeq =>
  override
  def JNI_ExtractPrimitive[I <: AnyVal : Elem](env: Rep[AnyRef], x: Rep[JNIType[I]]): Rep[I] = x.asInstanceOf[I]
  override
  def JNI_ExtractObject[I: Elem](env: Rep[AnyRef], x: Rep[JNIType[AnyRef]]): Rep[I] = x.asInstanceOf[I]
}

trait JNIExtractorOpsExp extends JNIExtractorOps { self: ScalanExp =>

  private def __JNI_Unbox[A: Elem](env: Rep[AnyRef], x: Rep[JNIType[AnyRef]]): Rep[JNIType[A]] = {
    val clazz = JNI_GetObjectClass(env, x)
    val fid = JNI_GetFieldID[A](env, clazz, "value")

    JNI_Unbox[A](env, fid, x)
  }

  private def __JNI_GetTupleField(env: Rep[AnyRef], fn: Rep[String], tup: Rep[JNIType[AnyRef]]): Rep[JNIType[AnyRef]] = {
    implicit val ear = AnyRefElement
    val clazz = JNI_GetObjectClass(env, tup)
    val fid = JNI_GetFieldID[AnyRef](env, clazz, fn)

    JNI_GetObjectField(env, fid, tup)
  }

  private def isPrimitiveElement[A: Elem]: Boolean = {
    element[A] match {
      case DoubleElement => true
      case IntElement => true
      case _ => false
    }
  }

  override
  def JNI_ExtractPrimitive[I <: AnyVal : Elem](env: Rep[AnyRef], x: Rep[JNIType[I]]): Rep[I] = {
    element[I] match {
      case (de@DoubleElement) =>
        JNI_ExtractDouble[I](env, x.asRep[JNIType[Double]])
      case (de@IntElement) =>
        JNI_ExtractInt[I](env, x.asRep[JNIType[Int]])
    }
  }

  override
  def JNI_ExtractObject[I: Elem](env: Rep[AnyRef], x: Rep[JNIType[AnyRef]]): Rep[I] = {
    element[I] match {
      case (pe: PairElem[a,b]) =>
        implicit val ae = pe.eFst
        implicit val be = pe.eSnd

        var f1 = __JNI_GetTupleField(env, "_1", x)
        var f2 = __JNI_GetTupleField(env, "_2", x)

        if( isPrimitiveElement[a] )
          f1 = (__JNI_Unbox(env, f1)(ae)).asRep[JNIType[AnyRef]] //TODO: yz: I don't know how to reuse JNIEtractPrimitive in this place

        if( isPrimitiveElement[b] )
          f2 = (__JNI_Unbox(env, f2)(ae)).asRep[JNIType[AnyRef]]

        Pair(JNI_ExtractObject(env, f1)(ae), JNI_ExtractObject(env, f2)(be))

      case (de@DoubleElement) =>
        JNI_ExtractDouble[I](env, x.asRep[JNIType[Double]])
      case (de@IntElement) =>
        JNI_ExtractInt[I](env, x.asRep[JNIType[Int]])

      case (earr: ArrayElem[a]) =>
        implicit val ea = earr.eItem
        ea match {
          case ( el@DoubleElement ) =>
            JNI_ExtractDoubleArray[a](env, x)
          case ( el@IntElement ) =>
            JNI_ExtractIntArray[a](env, x)
          case _ =>
            implicit val anyrefel = AnyRefElement
            val arr = JNI_ExtractObjectArray(env, x)
            val len = JNI_GetArrayLength(env, x)
            val res = Array.rangeFrom0(len).map( { i => JNI_ExtractObject( env, JNI_GetArrayItem(env, arr, i) )(ea) } )
            res.asRep[I]
        }
      case elem =>
        ???(s"Don't know how to extract: elem = ${elem}")
    }
  }

  case class JNI_GetObjectClass(env: Rep[AnyRef], x: Rep[JNIType[AnyRef]]) extends Def[JNIType[AnyRef]] {
    implicit val anyrefel = AnyRefElement
    override def selfType = element[JNIType[AnyRef]]
    override def uniqueOpId = "JNI_GetObjectClass"
    override def mirror(t: Transformer) = JNI_GetObjectClass(t(env), t(x))
  }

  case class JNI_GetFieldID[A: Elem](env: Rep[AnyRef], clazz: Rep[JNIType[AnyRef]], fname: Rep[String]) extends Def[JNIType[AnyRef]] {
    def fieldType = element[A]
    implicit val anyrefel = AnyRefElement
    override def selfType = element[JNIType[AnyRef]]
    override def uniqueOpId = "JNI_GetFieldID"
    override def mirror(t: Transformer) = JNI_GetFieldID[A](t(env), t(clazz), t(fname))
  }

  case class JNI_GetObjectField(env: Rep[AnyRef], fid: Rep[JNIType[AnyRef]], tup: Rep[JNIType[AnyRef]]) extends Def[JNIType[AnyRef]] {
    implicit val anyrefel = AnyRefElement
    override def selfType = element[JNIType[AnyRef]]
    override def uniqueOpId = "JNI_GetObjectField"
    override def mirror(t: Transformer) = JNI_GetObjectField(t(env), t(fid), t(tup))
  }

  case class JNI_Unbox[A: Elem](env: Rep[AnyRef], fid: Rep[JNIType[AnyRef]], x: Rep[JNIType[AnyRef]]) extends Def[JNIType[A]] {
    override def selfType = element[JNIType[A]]
    override def uniqueOpId = "JNI_Unbox"
    override def mirror(t: Transformer) = JNI_Unbox[A](t(env), t(fid), t(x))
  }

  case class JNI_ExtractDouble[A: Elem](env: Rep[AnyRef], x: Rep[JNIType[Double]]) extends Def[A] {
    override def selfType = element[A]
    override def uniqueOpId = "JNI_ExtractDouble"
    override def mirror(t: Transformer) = JNI_ExtractDouble[A](t(env), t(x))
  }

  case class JNI_ExtractInt[A: Elem](env: Rep[AnyRef], x: Rep[JNIType[Int]]) extends Def[A] {
    override def selfType = element[A]
    override def uniqueOpId = "JNI_ExtractInt"
    override def mirror(t: Transformer) = JNI_ExtractInt[A](t(env), t(x))
  }

  case class JNI_GetArrayLength(env: Rep[AnyRef], arr: Rep[JNIType[AnyRef]]) extends Def[Int] {
    override def selfType = IntElement
    override def uniqueOpId = "JNI_GetArrayLength"
    override def mirror(t: Transformer) = JNI_GetArrayLength(t(env), t(arr))
  }

  case class JNI_ExtractObjectArray(env: Rep[AnyRef], x: Rep[JNIType[AnyRef]]) extends Def[Array[JNIType[AnyRef]]] {
    implicit val e = AnyRefElement
    override def selfType = element[Array[JNIType[AnyRef]]]
    override def uniqueOpId = "JNI_ExtractObjectArray"
    override def mirror(t: Transformer) = JNI_ExtractObjectArray(t(env), t(x))
  }

  case class JNI_GetArrayItem(env: Rep[AnyRef], arr: Rep[Array[JNIType[AnyRef]]], i: Rep[Int] ) extends Def[JNIType[AnyRef]] {
    implicit val e = AnyRefElement
    override def selfType = element[JNIType[AnyRef]]
    override def uniqueOpId = "JNI_GetArrayItem"
    override def mirror(t: Transformer) = JNI_GetArrayItem(t(env), t(arr), t(i))
  }

  case class JNI_ExtractDoubleArray[A: Elem](env: Rep[AnyRef], x: Rep[JNIType[AnyRef]]) extends Def[Array[A]] {
    override def selfType = element[Array[A]]
    override def uniqueOpId = "JNI_ExtractDoubleArray"
    override def mirror(t: Transformer) = JNI_ExtractDoubleArray[A](t(env), t(x))
  }

  case class JNI_ExtractIntArray[A: Elem](env: Rep[AnyRef], x: Rep[JNIType[AnyRef]]) extends Def[Array[A]] {
    override def selfType = element[Array[A]]
    override def uniqueOpId = "JNI_ExtractIntArray"
    override def mirror(t: Transformer) = JNI_ExtractIntArray[A](t(env), t(x))
  }
}
