package scalan

trait Elems {

  trait Dummy

  abstract class Elem[A] extends Serializable with Dummy

  class BaseElem[A] extends Elem[A]

  implicit val BooleanElement: Elem[Boolean] = new BaseElem[Boolean]
  implicit val ByteElement: Elem[Byte] = new BaseElem[Byte]
  implicit val ShortElement: Elem[Short] = new BaseElem[Short]
  implicit val IntElement: Elem[Int] = new BaseElem[Int]
  implicit val LongElement: Elem[Long] = new BaseElem[Long]
  implicit val FloatElement: Elem[Float] = new BaseElem[Float]
  implicit val DoubleElement: Elem[Double] = new BaseElem[Double]
  implicit val UnitElement: Elem[Unit] = new BaseElem[Unit]
  implicit val StringElement: Elem[String] = new BaseElem[String]
  implicit val CharElement: Elem[Char] = new BaseElem[Char]
}

trait GoodMatch { self: Elems =>

  private def boxed_class(e: Elem[_]): Class[_] = e match {
    case BooleanElement => classOf[java.lang.Boolean]
    case ByteElement => classOf[java.lang.Byte]
    case ShortElement => classOf[java.lang.Short]
    case IntElement => classOf[java.lang.Integer]
    case LongElement => classOf[java.lang.Long]
    case FloatElement => classOf[java.lang.Float]
    case DoubleElement => classOf[java.lang.Double]
    case CharElement => classOf[java.lang.Character]
    case _ => ???
  }

}

abstract class BadMatch[+A <: Elems](scalan: A) {
  import scalan._

  protected def toLuaValue(x: Any, eX: Elem[_]): String = eX match {
    case UnitElement => ""
    //    case BooleanElement => ""
    //    case IntElement => ""
    //    case DoubleElement => ""
    //    case LongElement => ""
    //    case FloatElement => ""
    //    case StringElement => ""
    //    case el: ArrayElem[_] => ""
    //    case PairElem(eFst, eSnd) => ""
    //    case StructElem(_, fields) => ""
    case _ => ???
  }

  // should check type before conversion?
  protected def fromLuaValue[B](lv: Any, eA: Elem[B]): B = (eA match {
    case UnitElement => ()
    //    case BooleanElement => lv
    //    case IntElement => lv
    //    case DoubleElement => lv
    //    case LongElement => lv
    //    case FloatElement => lv
    //    case StringElement => lv
    //    case el: ArrayElem[_] => lv
    //    case PairElem(eFst, eSnd) => lv
    //    case StructElem(tag, elemFields) => lv
    //    case _ => !!!(s"Can't convert LuaValue $lv to JVM value of type ${eA.name}")
  }).asInstanceOf[B]

}
