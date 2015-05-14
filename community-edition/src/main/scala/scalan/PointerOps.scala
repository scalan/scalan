package scalan

import scala.reflect.runtime.universe._

trait PointerOps { self: Scalan =>
  class Pointer[A: Elem]
  class Scalar[A: Elem]

  def nullPtr[A: Elem]: Rep[Pointer[A]]

  case class PointerElem[A: Elem](eItem: Elem[A]) extends Elem[Pointer[A]] {
    override val tag = {
      implicit val ttag = eItem.tag
      weakTypeTag[Pointer[A]]
    }
    override def isEntityType: Boolean = eItem.isEntityType
    protected def getDefaultRep = nullPtr[A]
  }
  implicit def PointerElement[A: Elem]: Elem[Pointer[A]] = new PointerElem[A](element[A])

  case class ScalarElem[A: Elem](eItem: Elem[A]) extends Elem[Scalar[A]] {
    override val tag = {
      implicit val ttag = eItem.tag
      weakTypeTag[Scalar[A]]
    }
    override def isEntityType: Boolean = eItem.isEntityType
    protected def getDefaultRep = eItem.defaultRepValue.asInstanceOf[Rep[Scalar[A]]]
  }
  implicit def ScalarElement[A: Elem]: Elem[Scalar[A]] = new ScalarElem[A](element[A])
}

trait PointerOpsExp extends PointerOps { self: ScalanExp =>

  case class NullPtr[A: Elem] extends Def[Pointer[A]] {
    override def selfType = element[Pointer[A]]
    override def uniqueOpId = "NullPtr"
    override def mirror(t: Transformer) = NullPtr[A]
  }
  def nullPtr[A: Elem]: Exp[Pointer[A]] = NullPtr[A]

  // type Scalar for case when Exp[A] is a value and no pointer can be applied
  case class CreateScalar[A: Elem](source: Exp[A]) extends Def[Scalar[A]] {
    override def selfType = element[Scalar[A]]
    override def uniqueOpId = "Scalar"
    override def mirror(t: Transformer) = CreateScalar[A](t(source))
  }
  def createScalar[A: Elem](source: Exp[A]): Exp[Scalar[A]] = CreateScalar[A](source)

  case class PtrScalar[A: Elem](xScalar: Exp[Scalar[A]]) extends Def[Pointer[A]] {
    override def selfType = element[Pointer[A]]
    override def uniqueOpId = "PtrScalar"
    override def mirror(t: Transformer) = PtrScalar[A](t(xScalar))
  }
  def ptrScalar[A: Elem](xScalar: Exp[Scalar[A]]): Exp[Pointer[A]] = PtrScalar(xScalar)

}


