package scalan

import scala.reflect.runtime.universe._

trait PointerOps { self: Scalan =>
  class Pointer[A: Elem]
  class Scalar[A: Elem]

  def nullPtr[A: Elem]: Rep[Pointer[A]]

  case class PointerElem[A: Elem, To <: Pointer[A]](eItem: Elem[A]) extends EntityElem[To] {
    def parent: Option[Elem[_]] = None
    override def entityDef = !!!("not supported")
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eItem))
    }
    override lazy val tag = {
      implicit val ttag = eItem.tag
      weakTypeTag[Pointer[A]].asInstanceOf[WeakTypeTag[To]]
    }
    def convertPointer(x: Rep[Pointer[A]]): Rep[To] = x.asRep[To]
    override def isEntityType: Boolean = eItem.isEntityType
    protected def getDefaultRep = convertPointer(nullPtr[A])
  }
  implicit def PointerElement[A](implicit eItem: Elem[A]): Elem[Pointer[A]] =
    new PointerElem[A, Pointer[A]](eItem)
  
  case class ScalarElem[A: Elem, To <: Scalar[A]](eItem: Elem[A]) extends EntityElem[To] {
    def parent: Option[Elem[_]] = None
    override def entityDef = !!!("not supported")
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eItem))
    }
    override lazy val tag = {
      implicit val ttag = eItem.tag
      weakTypeTag[Scalar[A]].asInstanceOf[WeakTypeTag[To]]
    }
    def convertScalar(x: Rep[Scalar[A]]): Rep[To] = x.asRep[To]
    override def isEntityType: Boolean = eItem.isEntityType
    protected def getDefaultRep = convertScalar(eItem.defaultRepValue.asRep[Scalar[A]])
  }
  implicit def ScalarElement[A](implicit eItem: Elem[A]): Elem[Scalar[A]] =
    new ScalarElem[A, Scalar[A]](eItem)
}

trait PointerOpsExp extends PointerOps { self: ScalanExp =>

  case class NullPtr[A: Elem]() extends Def[Pointer[A]] {
    override def selfType = element[Pointer[A]]
    override def mirror(t: Transformer) = NullPtr[A]()
  }
  def nullPtr[A: Elem]: Exp[Pointer[A]] = NullPtr[A]()

  // type Scalar for case when Exp[A] is a value and no pointer can be applied
  case class CreateScalar[A: Elem](source: Exp[A]) extends Def[Scalar[A]] {
    override def selfType = element[Scalar[A]]
    override def mirror(t: Transformer) = CreateScalar[A](t(source))
  }

  case class ScalarPtr[A: Elem](xScalar: Exp[Scalar[A]]) extends Def[Pointer[A]] {
    override def selfType = element[Pointer[A]]
    override def mirror(t: Transformer) = ScalarPtr[A](t(xScalar))
  }
  def scalarPtr[A: Elem](source: Exp[A]): Exp[Pointer[A]] = {
    source.elem match {
      case be: BaseElem[_] => ScalarPtr(CreateScalar(source))
      case _ => !!!(s"not allowed to make scalar pointer for non-BaseElem: ${source.elem}")
    }
  }

  case class ArrayPtr[A: Elem](xs: Exp[Array[A]]) extends Def[Pointer[A]] {
    override def selfType = element[Pointer[A]]
    override def mirror(t: Transformer) = ArrayPtr(t(xs))
  }
  def arrayPtr[A: Elem](xs: Exp[Array[A]]): Exp[Pointer[A]] = ArrayPtr(xs)

}


