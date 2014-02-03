package scalan

import scala.text._
import Document._
import scalan.common.Monoid._
import scalan.common._
import Common._
import annotation.implicitNotFound
import scalan.staged.BaseExp
import scala.language.{implicitConversions}
import scala.annotation.unchecked.uncheckedVariance

trait Elems extends Base { self: Scalan =>
  type Elem[A] = Element[A]      // typeclass vitnessing that type A can be an element of other data type (i.e. belongs to Family)
  type LElem[A] = () => Elem[A]  // lazy element

  @implicitNotFound(msg = "No Element available for ${A}.")
  trait Element[A] {
    def manifest: Manifest[A]
    def defaultOf: DefaultOf[Rep[A]]
    def name = manifest.toString
  }

  def element[A](implicit ea: Elem[A]): Elem[A] = ea

  implicit class ElemForSomeExtension(e: Elem[_]) {
    def asElem[T]: Elem[T] = e.asInstanceOf[Elem[T]]
  }

  abstract class UnitElem extends Element[Unit] {
  }
  abstract class BaseElem[A] extends Element[A] {
  }
  abstract class PairElem[A,B](val ea: Elem[A], val eb: Elem[B]) extends Element[(A,B)] {
  }
  abstract class SumElem [A,B](val ea: Elem[A], val eb: Elem[B]) extends Element[(A|B)] {
  }
  abstract class FuncElem[A,B](val ea: Elem[A], val eb: Elem[B]) extends Element[A => B]
  abstract class ArrayElem[A](val ea: Elem[A]) extends Element[Array[A]]

//  abstract class ElemElem[A](val ea: Elem[A]) extends Element[Elem[A]]

  implicit val boolElement:  Elem[Boolean]
  implicit val intElement:   Elem[Int]
  implicit val floatElement: Elem[Float]
  implicit val unitElement:  Elem[Unit]
  implicit val stringElement:  Elem[String]
  implicit def arrayElement[A](implicit eA: Elem[A]): Elem[Array[A]]

  implicit def pairElement[A,B](implicit ea: Elem[A], eb: Elem[B]): Elem[(A,B)]
  implicit def sumElement [A,B](implicit ea: Elem[A], eb: Elem[B]): Elem[(A|B)]
  implicit def funcElement[A,B](implicit ea: Elem[A], eb: Elem[B]): Elem[A => B]
  ///implicit def elemElement[A](implicit ea: Elem[A]): Elem[Elem[A]]

  implicit def PairElemExtensions[A,B](eAB: Elem[(A,B)]): PairElem[A,B] = eAB.asInstanceOf[PairElem[A,B]]
  implicit def SumElemExtensions[A,B](eAB: Elem[(A|B)]): SumElem[A,B] = eAB.asInstanceOf[SumElem[A,B]]
  implicit def FuncElemExtensions[A,B](eAB: Elem[A=>B]): FuncElem[A,B] = eAB.asInstanceOf[FuncElem[A,B]]
  implicit def UnitElemExtensions(eu: Elem[Unit]): UnitElem = eu.asInstanceOf[UnitElem]
  implicit def ArrayElemExtensions[A](eArr: Elem[Array[A]]): ArrayElem[A] = eArr.asInstanceOf[ArrayElem[A]]
  //  implicit def ElemElemExtensions[A](eeA: Elem[Elem[A]]): ElemElem[A] = eeA.asInstanceOf[ElemElem[A]]

  implicit def toLazyElem[A](implicit eA: Elem[A]): LElem[A] = () => eA

  implicit lazy val IntRepDefaultOf: DefaultOf[Rep[Int]]               = defaultVal[Rep[Int]](0)
  implicit lazy val FloatRepDefaultOf: DefaultOf[Rep[Float]]           = defaultVal[Rep[Float]](0f)
  implicit lazy val BooleanRepDefaultOf: DefaultOf[Rep[Boolean]]       = defaultVal[Rep[Boolean]](false)
  implicit lazy val StringRepDefaultOf: DefaultOf[Rep[String]]         = defaultVal[Rep[String]]("")
  implicit def arrayRepDefaultOf[A](implicit e: Elem[Array[A]]): DefaultOf[Rep[Array[A]]] = {
    implicit val mA = e.ea.manifest
    defaultVal[Rep[Array[A]]](Array.empty[A])
  }
  implicit def funcRepDefaultOf[A,B:Elem]: DefaultOf[Rep[A] => Rep[B]] = {
    implicit val zB = element[B].defaultOf
    Defaults.Function1ABDefaultOf[Rep[A],Rep[B]](zB)
  }

  object implicitManifests {
    implicit def elemToManifest[A:Elem] = element[A].manifest
  }
}

trait ElemsSeq extends Elems with Scalan { self: ScalanSeq =>

  override implicit lazy val boolElement: Elem[Boolean] =
    new SeqBaseElement[Boolean]()(Defaults.BooleanDefaultOf, manifest[Boolean]/*, descriptor[Boolean]*/)

  override implicit lazy val intElement: Elem[Int] =
    new SeqBaseElement[Int]()(Defaults.IntDefaultOf, manifest[Int]/*, descriptor[Int]*/)

  override implicit lazy val floatElement: Elem[Float] =
    new SeqBaseElement[Float]()(Defaults.FloatDefaultOf, manifest[Float]/*, descriptor[Float]*/)

  override implicit lazy val stringElement: Elem[String] =
    new SeqBaseElement[String]()(Defaults.StringDefaultOf, manifest[String]/*, descriptor[String]*/)

  implicit def arrayElement[A](implicit eA: Elem[A]): Elem[Array[A]] = {
    implicit val mA = eA.manifest
    new ArrayElem[A](eA) with SeqElement[Array[A]] {
      private val m = Predef.manifest[Array[A]]
      private val z = Defaults.ArrayDefaultOf(mA)
      def manifest: Manifest[Array[A]] = m
      def defaultOf = z
    }
  }

  override implicit lazy val unitElement: Elem[Unit] = new SeqUnitElement

  trait SeqElement[A] extends Element[A] {
  }

  class SeqBaseElement[A](implicit override val defaultOf: DefaultOf[A], val manifest: Manifest[A]/*, val desc: Desc[A]*/)
    extends BaseElem[A] with SeqElement[A] {
  }

  class SeqUnitElement extends UnitElem with SeqElement[Unit] {
    private val m = scala.Predef.manifest[Unit]
    private val z = Defaults.UnitDefaultOf
    implicit def manifest = m
    def defaultOf = z
  }

  override implicit def pairElement[A, B](implicit elema: Elem[A], elemb: Elem[B]): Elem[(A, B)] =
    new PairElem[A, B](elema, elemb) with SeqElement[(A, B)] {
      private val m = Manifest.classType(classOf[(A, B)], ea.manifest, eb.manifest)
      private val z = Common.defaultVal((ea.defaultOf.value, eb.defaultOf.value))
      def manifest: Manifest[(A, B)] = m
      def defaultOf = z
    }

  override implicit def sumElement[A, B](implicit elema: Elem[A], elemb: Elem[B]): Elem[(A | B)] =
    new SumElem[A, B](elema, elemb) with SeqElement[(A | B)] {
      lazy val m: Manifest[(A|B)] = Manifest.classType(classOf[(A|B)], ea.manifest, eb.manifest)
      def defaultOf = Common.defaultVal(scala.Left(ea.defaultOf.value))
      def manifest: Manifest[(A|B)] = m
    }

  implicit def funcElement[A,B](implicit elema: Elem[A], elemb: Elem[B]): Elem[A => B] =
    new FuncElem[A, B](elema, elemb) with SeqElement[A => B] {
      private val m = Manifest.classType(classOf[A=>B], ea.manifest, eb.manifest)
      private val z = funcRepDefaultOf[A,B]

      def manifest: Manifest[A=>B] = m
      def defaultOf = z
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

  def withElemOf[A,R](x: Rep[A])(block: Elem[A] => R) = block(x.elem)
  def withResultElem[A,B,R](f: Rep[A=>B])(block: Elem[B] => R) = block(withElemOf(f){ e => e.eb })

  override implicit lazy val boolElement: Elem[Boolean] =
    new StagedBaseElement[Boolean](BooleanRepDefaultOf, manifest[Boolean]/*, descriptor[Boolean]*/)

  override implicit lazy val intElement: Elem[Int] =
    new StagedBaseElement[Int](IntRepDefaultOf, manifest[Int]/*, descriptor[Int]*/)

  override implicit lazy val floatElement: Elem[Float] =
    new StagedBaseElement[Float](FloatRepDefaultOf, manifest[Float]/*, descriptor[Float]*/)

  override implicit lazy val stringElement: Elem[String] =
    new StagedBaseElement[String](StringRepDefaultOf, manifest[String]/*, descriptor[String]*/)

  implicit def arrayElement[A](implicit eA: Elem[A]): Elem[Array[A]] = {
    implicit val mA = eA.manifest
    //new StagedBaseElement[Array[A]](arrayRepDefaultOf, manifest[Array[A]] /*, descriptor[Array[A]]*/)
    new ArrayElem[A](eA) with StagedElement[Array[A]] {
      private val m = Predef.manifest[Array[A]]
      private val z = arrayRepDefaultOf(this)
      def manifest = m
      def defaultOf = z
    }
  }
//  implicit def arrayElement[A](implicit eA: Elem[A]): Elem[Array[A]] =
//    new ArrayElem[A](eA) with StagedElement[Array[A]] {
//      import implicitManifests._
//      private val m = Predef.manifest[Array[A]]
//      private val z = arrayRepDefaultOf
//      def manifest = m
//      def defaultOf = z
//    }

  override implicit lazy val unitElement: Elem[Unit] = new StagedUnitElement

  trait StagedElement[A] extends Element[A] {
  }

  class StagedBaseElement[A](z: => DefaultOf[Rep[A]], val manifest: Manifest[A]/*, val desc: Desc[A]*/)
    extends BaseElem[A] with StagedElement[A] {
    override lazy val defaultOf = z
  }

  class StagedUnitElement extends UnitElem with StagedElement[Unit] {
    private val m = scala.Predef.manifest[Unit]
    private lazy val z = defaultVal[Rep[Unit]](toRep(()))
    def manifest = m
    def defaultOf = z
  }

  override implicit def pairElement[A, B](implicit elema: Elem[A], elemb: Elem[B]): Elem[(A, B)] =
    new PairElem[A, B](elema, elemb) with StagedElement[(A, B)] {
      private val m = Manifest.classType(classOf[(A, B)], ea.manifest, eb.manifest)
      private lazy val z = defaultVal(Pair(ea.defaultOf.value, eb.defaultOf.value))
      def manifest: Manifest[(A, B)] = m
      def defaultOf = z
    }

  override implicit def sumElement[A, B](implicit elema: Elem[A], elemb: Elem[B]): Elem[(A | B)] =
    new SumElem[A, B](elema, elemb) with StagedElement[(A | B)] {
      lazy val m: Manifest[(A | B)] = Manifest.classType(classOf[(A | B)], ea.manifest, eb.manifest)
      def defaultOf = defaultVal(toLeftSum[A,B](ea.defaultOf.value))
      def manifest: Manifest[(A | B)] = m
    }

  override implicit def funcElement[A, B](implicit elema: Elem[A], elemb: Elem[B]): Elem[A => B] =
    new FuncElem[A, B](elema, elemb) with StagedElement[A => B] {
      lazy val m: Manifest[A => B] = Manifest.classType(classOf[A => B], ea.manifest, eb.manifest)
      def manifest = m
      lazy val z: DefaultOf[Rep[A=>B]] = defaultVal(fun(funcRepDefaultOf[A,B].value))
      implicit def defaultOf = z
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
