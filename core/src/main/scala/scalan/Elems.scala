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
  trait Element[A] {
    def tag: TypeTag[A]
    final def classTag: ClassTag[A] = TagImplicits.typeTagToClassTag(tag)
    def defaultRep: Default[Rep[A]]
    def defaultRepValue = defaultRep.value
    def name = tag.toString

    lazy val prettyName = name.
      replace("scala.math.Numeric$", "").
      replace("scala.", "").
      replaceAll("""scalan[^$]*\$""", "")

    override def toString = s"${getClass.getSimpleName}[$name]"
    override def equals(other: Any) = other match {
      case e: Element[_] => tag == e.tag
      case _ => false
    }
  }

  def element[A](implicit ea: Elem[A]): Elem[A] = ea

  implicit class ElemForSomeExtension(e: Elem[_]) {
    def asElem[T]: Elem[T] = e.asInstanceOf[Elem[T]]
  }

  abstract class UnitElem extends Element[Unit] {
    val tag = typeTag[Unit]
  }
  abstract class BaseElem[A](implicit val tag: TypeTag[A]) extends Element[A] {
  }
  abstract class PairElem[A, B](val ea: Elem[A], val eb: Elem[B]) extends Element[(A, B)] {
    lazy val tag = {
      implicit val tA = ea.tag
      implicit val tB = eb.tag
      typeTag[(A, B)]
    }
  }
  abstract class SumElem[A, B](val ea: Elem[A], val eb: Elem[B]) extends Element[(A | B)] {
    lazy val tag = {
      implicit val tA = ea.tag
      implicit val tB = eb.tag
      typeTag[A | B]
    }
  }
  abstract class FuncElem[A, B](val ea: Elem[A], val eb: Elem[B]) extends Element[A => B] {
    lazy val tag = {
      implicit val tA = ea.tag
      implicit val tB = eb.tag
      typeTag[A => B]
    }
  }
  abstract class ArrayElem[A](val ea: Elem[A]) extends Element[Array[A]] {
    lazy val tag = {
      implicit val tag1 = ea.tag
      implicitly[TypeTag[Array[A]]]
    }
  }

  //  abstract class ElemElem[A](val ea: Elem[A]) extends Element[Elem[A]]

  implicit val boolElement: Elem[Boolean]
  implicit val intElement: Elem[Int]
  implicit val floatElement: Elem[Float]
  implicit val unitElement: Elem[Unit]
  implicit val stringElement: Elem[String]
  implicit def arrayElement[A](implicit eA: Elem[A]): Elem[Array[A]]

  implicit def pairElement[A, B](implicit ea: Elem[A], eb: Elem[B]): Elem[(A, B)]
  implicit def sumElement[A, B](implicit ea: Elem[A], eb: Elem[B]): Elem[(A | B)]
  implicit def funcElement[A, B](implicit ea: Elem[A], eb: Elem[B]): Elem[A => B]
  ///implicit def elemElement[A](implicit ea: Elem[A]): Elem[Elem[A]]

  implicit def PairElemExtensions[A, B](eAB: Elem[(A, B)]): PairElem[A, B] = eAB.asInstanceOf[PairElem[A, B]]
  implicit def SumElemExtensions[A, B](eAB: Elem[(A | B)]): SumElem[A, B] = eAB.asInstanceOf[SumElem[A, B]]
  implicit def FuncElemExtensions[A, B](eAB: Elem[A => B]): FuncElem[A, B] = eAB.asInstanceOf[FuncElem[A, B]]
  implicit def UnitElemExtensions(eu: Elem[Unit]): UnitElem = eu.asInstanceOf[UnitElem]
  implicit def ArrayElemExtensions[A](eArr: Elem[Array[A]]): ArrayElem[A] = eArr.asInstanceOf[ArrayElem[A]]
  //  implicit def ElemElemExtensions[A](eeA: Elem[Elem[A]]): ElemElem[A] = eeA.asInstanceOf[ElemElem[A]]

  implicit def toLazyElem[A](implicit eA: Elem[A]): LElem[A] = Lazy(eA)

//  implicit lazy val IntRepDefault: Default[Rep[Int]] = defaultVal[Rep[Int]](0)
//  implicit lazy val FloatRepDefault: Default[Rep[Float]] = defaultVal[Rep[Float]](0f)
//  implicit lazy val BooleanRepDefault: Default[Rep[Boolean]] = defaultVal[Rep[Boolean]](false)
//  implicit lazy val StringRepDefault: Default[Rep[String]] = defaultVal[Rep[String]]("")
  implicit def arrayRepDefault[A](implicit e: Elem[Array[A]]): Default[Rep[Array[A]]] = {
    implicit val aCT = e.ea.classTag
    defaultVal[Rep[Array[A]]](Array.empty[A])
  }
  implicit def funcRepDefault[A, B: Elem]: Default[Rep[A] => Rep[B]] = {
    implicit val zB = element[B].defaultRep
    Default.OfFunction1[Rep[A], Rep[B]](zB)
  }

  object TagImplicits {
    implicit def elemToClassTag[A](implicit elem: Element[A]): ClassTag[A] = elem.classTag
    implicit def typeTagToClassTag[A](implicit tag: TypeTag[A]): ClassTag[A] = ClassTag(tag.mirror.runtimeClass(tag.tpe))    
  }
}

trait ElemsSeq extends Elems with Scalan { self: ScalanSeq =>

  override implicit lazy val boolElement: Elem[Boolean] =
    new SeqBaseElement[Boolean]()

  override implicit lazy val intElement: Elem[Int] =
    new SeqBaseElement[Int]()

  override implicit lazy val floatElement: Elem[Float] =
    new SeqBaseElement[Float]()

  override implicit lazy val stringElement: Elem[String] =
    new SeqBaseElement[String]()

  implicit def arrayElement[A](implicit eA: Elem[A]): Elem[Array[A]] = {
    new ArrayElem[A](eA) with SeqElement[Array[A]] {
      lazy val defaultRep = Default.OfArray(eA.classTag)
    }
  }

  override implicit lazy val unitElement: Elem[Unit] = new SeqUnitElement

  trait SeqElement[A] extends Element[A] {
  }

  class SeqBaseElement[A](implicit val defaultRep: Default[A], tag: TypeTag[A] /*, val desc: Desc[A]*/ )
    extends BaseElem[A]()(tag) with SeqElement[A] {
  }

  class SeqUnitElement extends UnitElem with SeqElement[Unit] {
    def defaultRep = Default.OfUnit
  }

  override implicit def pairElement[A, B](implicit elema: Elem[A], elemb: Elem[B]): Elem[(A, B)] =
    new PairElem[A, B](elema, elemb) with SeqElement[(A, B)] {
      lazy val defaultRep = defaultVal((ea.defaultRepValue, eb.defaultRepValue))
    }

  override implicit def sumElement[A, B](implicit elema: Elem[A], elemb: Elem[B]): Elem[(A | B)] =
    new SumElem[A, B](elema, elemb) with SeqElement[(A | B)] {
      lazy val defaultRep = defaultVal[A | B](scala.Left(ea.defaultRepValue))
    }

  implicit def funcElement[A, B](implicit elema: Elem[A], elemb: Elem[B]): Elem[A => B] =
    new FuncElem[A, B](elema, elemb) with SeqElement[A => B] {
      val defaultRep = Default.OfFunction1[A, B](elemb.defaultRep)
    }

  //  override implicit def elemElement[A](implicit elema: Elem[A]): Elem[Elem[A]] =
  //    new ElemElem[A](elema) with SeqElement[Elem[A]] {
  //      lazy val m: Manifest[Elem[A]] = Manifest.classType(classOf[Elem[A]], ea.manifest)
  //      def defaultOf = Common.defaultVal(descriptor[A])
  //      def manifest: Manifest[Elem[A]] = m
  //    }

  //-------------- Stagin of descriptors ---------------

}

trait ElemsExp extends Elems
  with BaseExp
  with Scalan { self: ScalanStaged =>

  def withElemOf[A, R](x: Rep[A])(block: Elem[A] => R) = block(x.elem)
  def withResultElem[A, B, R](f: Rep[A => B])(block: Elem[B] => R) = block(withElemOf(f) { e => e.eb })

  override implicit lazy val boolElement: Elem[Boolean] =
    new StagedBaseElement[Boolean]()

  override implicit lazy val intElement: Elem[Int] =
    new StagedBaseElement[Int]()

  override implicit lazy val floatElement: Elem[Float] =
    new StagedBaseElement[Float]()

  override implicit lazy val stringElement: Elem[String] =
    new StagedBaseElement[String]()

  implicit def arrayElement[A](implicit eA: Elem[A]): Elem[Array[A]] = {
    new ArrayElem[A](eA) with StagedElement[Array[A]] {
      lazy val defaultRep: Default[Rep[Array[A]]] = arrayRepDefault[A]
    }
  }

  override implicit lazy val unitElement: Elem[Unit] = new StagedUnitElement

  trait StagedElement[A] extends Element[A] {
  }

  class StagedBaseElement[A]( /*, val desc: Desc[A]*/ )(implicit z: Default[A], tag: TypeTag[A])
    extends BaseElem[A]()(tag) with StagedElement[A] {
    override lazy val defaultRep = defaultVal(toRep(z.value)(this))
  }

  class StagedUnitElement extends UnitElem with StagedElement[Unit] {
    lazy val defaultRep = defaultVal[Rep[Unit]](toRep(())(this))
  }

  override implicit def pairElement[A, B](implicit elema: Elem[A], elemb: Elem[B]): Elem[(A, B)] =
    new PairElem[A, B](elema, elemb) with StagedElement[(A, B)] {
      lazy val defaultRep = defaultVal(Pair(ea.defaultRepValue, eb.defaultRepValue))
    }

  override implicit def sumElement[A, B](implicit elema: Elem[A], elemb: Elem[B]): Elem[(A | B)] =
    new SumElem[A, B](elema, elemb) with StagedElement[(A | B)] {
      lazy val defaultRep = defaultVal(toLeftSum[A, B](ea.defaultRepValue))
    }

  override implicit def funcElement[A, B](implicit elema: Elem[A], elemb: Elem[B]): Elem[A => B] =
    new FuncElem[A, B](elema, elemb) with StagedElement[A => B] {
      lazy val defaultRep = defaultVal(fun(funcRepDefault[A, B].value))
    }

  //  override implicit def elemElement[A](implicit elema: Elem[A]): Elem[Elem[A]] =
  //    new ElemElem[A](elema) with StagedElement[Elem[A]] {
  //      lazy val m: Manifest[Elem[A]] = Manifest.classType(classOf[Elem[A]], ea.manifest)
  //      def defaultOf = Common.defaultVal(descriptor[A])
  //      def manifest: Manifest[Elem[A]] = m
  //    }

  //  override def toRep[A](x: A)(implicit eA: Elem[A]) = eA match {
  //    case ee: ElemElem[a] => ElemDef[a](x)
  //    case _ => super.toRep(x)(eA)
  //  }

}
