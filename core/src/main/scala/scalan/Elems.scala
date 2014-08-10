package scalan

import scalan.common.Default
import Default._
import scalan.common.Lazy
import scalan.staged.BaseExp
import annotation.implicitNotFound
import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.runtime.universe._
import scala.reflect.ClassTag

trait Elems extends Base { self: Scalan =>


  type Elem[A] = Element[A] // typeclass witnessing that type A can be an element of other data type (i.e. belongs to Family)
  type LElem[A] = Lazy[Elem[A]] // lazy element

  @implicitNotFound(msg = "No Element available for ${A}.")
  abstract class Element[A] extends Serializable {
    def isEntityType: Boolean
    def tag: TypeTag[A]
    final def classTag: ClassTag[A] = TagImplicits.typeTagToClassTag(tag)
    def defaultRep: Default[Rep[A]]
    def defaultRepValue = defaultRep.value
    lazy val name = tag.tpe.toString.
      replaceAll("[A-Za-z0-9_.]*this.", "").
      replace("scala.math.Numeric$", "").
      replace("scala.", "").
      replaceAll("""[^# \[\],>]*[#$]""", "")

    override def toString = s"${getClass.getSimpleName}[$name]"
    override def equals(other: Any) = other match {
      case e: Element[_] => tag == e.tag
      case _ => false
    }
    override def hashCode = tag.hashCode
  }

  def element[A](implicit ea: Elem[A]): Elem[A] = ea

  implicit class ElemForSomeExtension(e: Elem[_]) {
    def asElem[T]: Elem[T] = e.asInstanceOf[Elem[T]]
  }

  class BaseElem[A](implicit val tag: TypeTag[A], z: Default[A]) extends Element[A] with Serializable {
    lazy val defaultRep = defaultVal(toRep(z.value)(this))
    override def isEntityType = false
  }

  case class PairElem[A, B](implicit eFst: Elem[A], eSnd: Elem[B]) extends Element[(A, B)] {
    override def isEntityType = eFst.isEntityType || eSnd.isEntityType
    lazy val tag = {
      implicit val tA = eFst.tag
      implicit val tB = eSnd.tag
      typeTag[(A, B)]
    }
    lazy val defaultRep = defaultVal(Pair(eFst.defaultRepValue, eSnd.defaultRepValue))
  }

  case class SumElem[A, B](implicit eLeft: Elem[A], eRight: Elem[B]) extends Element[(A | B)] {
    override def isEntityType = eLeft.isEntityType || eRight.isEntityType
    lazy val tag = {
      implicit val tA = eLeft.tag
      implicit val tB = eRight.tag
      typeTag[A | B]
    }
    lazy val defaultRep = defaultVal(toLeftSum[A, B](eLeft.defaultRepValue)(eRight))
  }

  case class FuncElem[A, B](implicit eDom: Elem[A], eRange: Elem[B]) extends Element[A => B] {
    override def isEntityType = eDom.isEntityType || eRange.isEntityType
    lazy val tag = {
      implicit val tA = eDom.tag
      implicit val tB = eRange.tag
      typeTag[A => B]
    }
    lazy val defaultRep = defaultVal(fun(funcRepDefault[A, B].value))
  }

  val AnyRefElement: Elem[AnyRef] = new BaseElem[AnyRef]()(typeTag[AnyRef], Default.OfAnyRef)
  implicit val BoolElement: Elem[Boolean] = new BaseElem[Boolean]
  implicit val ByteElement: Elem[Byte] = new BaseElem[Byte]
  implicit val IntElement: Elem[Int] = new BaseElem[Int]
  implicit val FloatElement: Elem[Float] = new BaseElem[Float]
  implicit val DoubleElement: Elem[Double] = new BaseElem[Double]
  implicit val UnitElement: Elem[Unit] = new BaseElem[Unit]
  implicit val StringElement: Elem[String] = new BaseElem[String]

  implicit def pairElement[A, B](implicit ea: Elem[A], eb: Elem[B]): Elem[(A, B)] = new PairElem[A, B]
  implicit def sumElement[A, B](implicit ea: Elem[A], eb: Elem[B]): Elem[(A | B)] = new SumElem[A, B]
  implicit def funcElement[A, B](implicit ea: Elem[A], eb: Elem[B]): Elem[A => B] = new FuncElem[A, B]
  ///implicit def elemElement[A](implicit ea: Elem[A]): Elem[Elem[A]]

  implicit def PairElemExtensions[A, B](eAB: Elem[(A, B)]): PairElem[A, B] = eAB.asInstanceOf[PairElem[A, B]]
  implicit def SumElemExtensions[A, B](eAB: Elem[(A | B)]): SumElem[A, B] = eAB.asInstanceOf[SumElem[A, B]]
  implicit def FuncElemExtensions[A, B](eAB: Elem[A => B]): FuncElem[A, B] = eAB.asInstanceOf[FuncElem[A, B]]
  //  implicit def ElemElemExtensions[A](eeA: Elem[Elem[A]]): ElemElem[A] = eeA.asInstanceOf[ElemElem[A]]

  implicit def toLazyElem[A](implicit eA: Elem[A]): LElem[A] = Lazy(eA)

  implicit def funcRepDefault[A, B: Elem]: Default[Rep[A] => Rep[B]] = {
    implicit val zB = element[B].defaultRep
    Default.OfFunction1[Rep[A], Rep[B]](zB)
  }

  object TagImplicits {
    implicit def elemToClassTag[A](implicit elem: Element[A]): ClassTag[A] = elem.classTag
    implicit def typeTagToClassTag[A](implicit tag: TypeTag[A]): ClassTag[A] = ClassTag(tag.mirror.runtimeClass(tag.tpe))    
  }

  //  implicit def elemElement[A](implicit elema: Elem[A]): Elem[Elem[A]] =
  //    new ElemElem[A](elema)
}

trait ElemsSeq extends Elems with Scalan { self: ScalanSeq =>

}

trait ElemsExp extends Elems
  with BaseExp
  with Scalan { self: ScalanStaged =>

  def withElemOf[A, R](x: Rep[A])(block: Elem[A] => R) = block(x.elem)
  def withResultElem[A, B, R](f: Rep[A => B])(block: Elem[B] => R) = block(withElemOf(f) { e => e.eRange })

  //  override def toRep[A](x: A)(implicit eA: Elem[A]) = eA match {
  //    case ee: ElemElem[a] => ElemDef[a](x)
  //    case _ => super.toRep(x)(eA)
  //  }
}
