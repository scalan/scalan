package scalan.seq

import scala.language.implicitConversions
import scalan.{Base, ScalanSeq}

trait BaseSeq extends Base { self: ScalanSeq =>
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

  object Classes {
    val IntClass = classOf[Int]
    val BooleanClass = classOf[Boolean]
    val ByteClass = classOf[Byte]
    val ShortClass = classOf[Short]
    val LongClass = classOf[Long]
    val FloatClass = classOf[Float]
    val DoubleClass = classOf[Double]
    val UnitClass = classOf[Unit]
    val StringClass = classOf[String]
    val CharClass = classOf[Char]
  }
  import Classes._

  val classToElemMapping: Map[Class[_], Elem[_]] = Map(
    IntClass     -> IntElement,
    BooleanClass -> BooleanElement,
    ByteClass    -> ByteElement,
    ShortClass   -> ShortElement,
    LongClass    -> LongElement,
    FloatClass   -> FloatElement,
    DoubleClass  -> DoubleElement,
    UnitClass    -> UnitElement,
    StringClass  -> StringElement,
    CharClass    -> CharElement
  )

  def classToElem[T](clazz: Class[T]): Elem[T] = {
    val res = if (clazz.isArray) {
      val cItem = clazz.getComponentType
      arrayElement(classToElem(cItem))
    } else {
      val opt = classToElemMapping.get(clazz)
      opt match {
        case Some(c) => c
        case None => !!!(s"Don't know how to get element for $clazz")
      }
    }
    res.asElem[T]
  }

  override def rep_getElem[T](x: Rep[T]): Elem[T] = classToElem(x.getClass.asInstanceOf[Class[T]])
}
