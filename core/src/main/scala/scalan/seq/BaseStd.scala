package scalan.seq

import scala.language.implicitConversions
import scala.runtime.{BoxedUnit, ScalaRunTime}
import scalan.{Base, ScalanStd}

trait BaseStd extends Base { self: ScalanStd =>
  type Rep[+A] = A

  override protected def stagingExceptionMessage(message: String, syms: Seq[Rep[_]]) = message

  override def toRep[A](x: A)(implicit eA: Elem[A]) = x
  def valueFromRep[A](x: Rep[A]): A = x

  override def def_unapply[A](e: Rep[A]): Option[Def[A]] = e match {
    case e: Def[A @unchecked] => Some(e)
    case _ => None
  }

  override def reifyObject[A](x: Def[A]): Rep[A] = x.asInstanceOf[A]

  override def repDef_getElem[T <: Def[_]](x: Rep[T]): Elem[T] = x.selfType.asElem[T]

  // TODO should there be a way to register new classes for DateElem, etc?
  // Should be obsoleted by #72
  private val classToElemMapping: Map[Class[_], Elem[_]] = Map(
    classOf[java.lang.Integer]   -> IntElement,
    classOf[java.lang.Boolean]   -> BooleanElement,
    classOf[java.lang.Byte]      -> ByteElement,
    classOf[java.lang.Short]     -> ShortElement,
    classOf[java.lang.Long]      -> LongElement,
    classOf[java.lang.Float]     -> FloatElement,
    classOf[java.lang.Double]    -> DoubleElement,
    classOf[java.lang.Character] -> CharElement,
    classOf[BoxedUnit]           -> UnitElement,
    classOf[String]              -> StringElement
  )

  private def classToElem(clazz: Class[_]): Elem[_] = {
    if (clazz.isArray) {
      val cItem = clazz.getComponentType
      arrayElement(classToElem(cItem))
    } else {
      val clazz1 = ScalaRunTime.box(clazz)
      classToElemMapping.getOrElse(clazz1, !!!(s"No way to determine element for $clazz"))
    }
  }

  override def rep_getElem[T](x: Rep[T]): Elem[T] = {
    val res: Elem[_] = x match {
      case d: Def[_] =>
        d.selfType
      case (a, b) =>
        pairElement(rep_getElem(a), rep_getElem(b))
      case _: Either[_, _] =>
        !!!("Can't determine the element of other side of Either in standard context")
      case _: Function1[_, _] =>
        !!!("Can't determine input element of a function in standard context")
      case _ =>
        classToElem(x.getClass)
    }
    res.asElem[T]
  }
}
