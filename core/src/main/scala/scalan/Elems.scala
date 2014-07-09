package scalan

import scalan.common.Default
import Default._
import scalan.common.Lazy
import scalan.staged.BaseExp
import annotation.implicitNotFound
import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.runtime.universe._
import scala.reflect.ClassTag
import scala.Predef._

trait Elems extends Base { self: Scalan =>


  type Elem[A] = Element[A] // typeclass witnessing that type A can be an element of other data type (i.e. belongs to Family)
  type LElem[A] = Lazy[Elem[A]] // lazy element

  @implicitNotFound(msg = "No Element available for ${A}.")
  abstract class Element[A] extends Serializable {
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
  }

  class PairElem[A, B](implicit val ea: Elem[A], val eb: Elem[B]) extends Element[(A, B)] {
    lazy val tag = {
      implicit val tA = ea.tag
      implicit val tB = eb.tag
      typeTag[(A, B)]
    }
    lazy val defaultRep = defaultVal(Pair(ea.defaultRepValue, eb.defaultRepValue))
  }

  class SumElem[A, B](implicit val ea: Elem[A], val eb: Elem[B]) extends Element[(A | B)] {
    lazy val tag = {
      implicit val tA = ea.tag
      implicit val tB = eb.tag
      typeTag[A | B]
    }
    lazy val defaultRep = defaultVal(toLeftSum[A, B](ea.defaultRepValue)(eb))
  }

  class FuncElem[A, B](implicit val ea: Elem[A], val eb: Elem[B]) extends Element[A => B] {
    lazy val tag = {
      implicit val tA = ea.tag
      implicit val tB = eb.tag
      typeTag[A => B]
    }
    lazy val defaultRep = defaultVal(fun(funcRepDefault[A, B].value))
  }

  class ArrayElem[A](implicit val ea: Elem[A]) extends Element[Array[A]] {
    lazy val tag = {
      implicit val tag1 = ea.tag
      implicitly[TypeTag[Array[A]]]
    }
    lazy val defaultRep: Default[Rep[Array[A]]] = arrayRepDefault[A]
  }

  implicit val boolElement: Elem[Boolean] = new BaseElem[Boolean]
  implicit val intElement: Elem[Int] = new BaseElem[Int]
  implicit val floatElement: Elem[Float] = new BaseElem[Float]
  implicit val doubleElement: Elem[Double] = new BaseElem[Double]
  implicit val unitElement: Elem[Unit] = new BaseElem[Unit]
  implicit val stringElement: Elem[String] = new BaseElem[String]
  implicit def arrayElement[A](implicit eA: Elem[A]): Elem[Array[A]] = new ArrayElem[A]

  implicit def pairElement[A, B](implicit ea: Elem[A], eb: Elem[B]): Elem[(A, B)] = new PairElem[A, B]
  implicit def sumElement[A, B](implicit ea: Elem[A], eb: Elem[B]): Elem[(A | B)] = new SumElem[A, B]
  implicit def funcElement[A, B](implicit ea: Elem[A], eb: Elem[B]): Elem[A => B] = new FuncElem[A, B]
  ///implicit def elemElement[A](implicit ea: Elem[A]): Elem[Elem[A]]

  implicit def PairElemExtensions[A, B](eAB: Elem[(A, B)]): PairElem[A, B] = eAB.asInstanceOf[PairElem[A, B]]
  implicit def SumElemExtensions[A, B](eAB: Elem[(A | B)]): SumElem[A, B] = eAB.asInstanceOf[SumElem[A, B]]
  implicit def FuncElemExtensions[A, B](eAB: Elem[A => B]): FuncElem[A, B] = eAB.asInstanceOf[FuncElem[A, B]]
  implicit def ArrayElemExtensions[A](eArr: Elem[Array[A]]): ArrayElem[A] = eArr.asInstanceOf[ArrayElem[A]]
  //  implicit def ElemElemExtensions[A](eeA: Elem[Elem[A]]): ElemElem[A] = eeA.asInstanceOf[ElemElem[A]]

  implicit def toLazyElem[A](implicit eA: Elem[A]): LElem[A] = Lazy(eA)

  implicit def arrayRepDefault[A](implicit e: Elem[A]): Default[Rep[Array[A]]] = {
    implicit val aCT = e.classTag
    defaultVal[Rep[Array[A]]](scala.Array.empty[A])
  }
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
  def withResultElem[A, B, R](f: Rep[A => B])(block: Elem[B] => R) = block(withElemOf(f) { e => e.eb })

  //  override def toRep[A](x: A)(implicit eA: Elem[A]) = eA match {
  //    case ee: ElemElem[a] => ElemDef[a](x)
  //    case _ => super.toRep(x)(eA)
  //  }
}
