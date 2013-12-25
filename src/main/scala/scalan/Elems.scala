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
  type Elem[A] = Element[A]    // typeclass of type descriptors
  type E[A] = Rep[Elem[A]]

  implicit val boolElement:  Elem[Boolean]
  implicit val intElement:   Elem[Int]
  implicit val floatElement: Elem[Float]
  implicit val unitElement:  Elem[Unit]
  implicit val stringElement:  Elem[String]
  implicit def arrayElement[A: Manifest]: Elem[Array[A]]

  implicit def pairElement[A,B](implicit ea: Elem[A], eb: Elem[B]): Elem[(A,B)]
  implicit def sumElement [A,B](implicit ea: Elem[A], eb: Elem[B]): Elem[(A|B)]
  implicit def funcElement[A,B](implicit ea: Elem[A], eb: Elem[B]): Elem[A => B]
  implicit def elemElement[A](implicit ea: Elem[A]): Elem[Elem[A]]

  @implicitNotFound(msg = "No Element available for ${A}.")
  trait Element[A] {
    def manifest: Manifest[A @uncheckedVariance]
    def defaultOf: DefaultOf[Rep[A]]
    def name = manifest.toString
  }

  def element[A](implicit ea:Elem[A]): Elem[A] = ea

  abstract class UnitElem extends Element[Unit] {
  }
  abstract class BaseElem[A] extends Element[A] {
  }
  abstract class PairElem[A,B](val ea: Elem[A], val eb: Elem[B]) extends Element[(A,B)] {
  }
  abstract class SumElem [A,B](val ea: Elem[A], val eb: Elem[B]) extends Element[(A|B)] {
  }
  abstract class FuncElem[A,B](val ea: Elem[A], val eb: Elem[B]) extends Element[A => B]
  abstract class ElemElem[A](val ea: Elem[A]) extends Element[Elem[A]]

  implicit def PairElemExtensions[A,B](eAB: Elem[(A,B)]): PairElem[A,B] = eAB.asInstanceOf[PairElem[A,B]]
  implicit def SumElemExtensions[A,B](eAB: Elem[(A|B)]): SumElem[A,B] = eAB.asInstanceOf[SumElem[A,B]]
  implicit def FuncElemExtensions[A,B](eAB: Elem[A=>B]): FuncElem[A,B] = eAB.asInstanceOf[FuncElem[A,B]]
  implicit def ElemElemExtensions[A](eeA: Elem[Elem[A]]): ElemElem[A] = eeA.asInstanceOf[ElemElem[A]]
  implicit def UnitElemExtensions(eu: Elem[Unit]): UnitElem = eu.asInstanceOf[UnitElem]

  implicit lazy val IntRepDefaultOf          = defaultVal[Rep[Int]](0)
  implicit lazy val FloatRepDefaultOf        = defaultVal[Rep[Float]](0f)
  implicit lazy val BooleanRepDefaultOf      = defaultVal[Rep[Boolean]](false)
  implicit lazy val StringRepDefaultOf       = defaultVal[Rep[String]]("")
  implicit def arrayRepDefaultOf[A:Manifest] = defaultVal[Rep[Array[A]]](Array.empty[A])

}

trait ElemsSeq extends Elems with Scalan { self: ScalanSeq =>

  override implicit lazy val boolElement: Elem[Boolean] =
    new SeqBaseElement[Boolean]()(Defaults.BooleanDefaultOf, manifest[Boolean])

  override implicit lazy val intElement: Elem[Int] =
    new SeqBaseElement[Int]()(Defaults.IntDefaultOf, manifest[Int])

  override implicit lazy val floatElement: Elem[Float] =
    new SeqBaseElement[Float]()(Defaults.FloatDefaultOf, manifest[Float])

  override implicit lazy val stringElement: Elem[String] =
    new SeqBaseElement[String]()(Defaults.StringDefaultOf, manifest[String])

  implicit def arrayElement[A](implicit m: Manifest[A]): Elem[Array[A]] =
    new SeqBaseElement[Array[A]]()(Defaults.ArrayDefaultOf(m), manifest[Array[A]])

  override implicit lazy val unitElement: Elem[Unit] = new SeqUnitElement

  trait SeqElement[A] extends Element[A] {
  }

  class SeqBaseElement[A](implicit override val defaultOf: DefaultOf[A], m: Manifest[A])
    extends BaseElem[A] with SeqElement[A] {
    def manifest: Manifest[A @uncheckedVariance] = m
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
      lazy val boolElem = element[Boolean]
      lazy val m: Manifest[(A|B)] = Manifest.classType(classOf[(A|B)], ea.manifest, eb.manifest)
      def defaultOf = Common.defaultVal(scala.Left(ea.defaultOf.value))
      def manifest: Manifest[(A|B)] = m
    }

  implicit def funcElement[A,B](implicit elema: Elem[A], elemb: Elem[B]): Elem[A => B] =
    new FuncElem[A, B](elema, elemb) with SeqElement[A => B] {
      private val m = Manifest.classType(classOf[A=>B], ea.manifest, eb.manifest)
      private val z = {
        implicit val zA = ea.defaultOf; implicit val zB = eb.defaultOf
        Defaults.Function1ABDefaultOf[Rep[A],Rep[B]](zB)
      }

      def manifest: Manifest[A=>B] = m
      def defaultOf = z
    }

  override implicit def elemElement[A](implicit elema: Elem[A]): Elem[Elem[A]] =
    new ElemElem[A](elema) with SeqElement[Elem[A]] {
      lazy val m: Manifest[Elem[A]] = Manifest.classType(classOf[Elem[A]], ea.manifest)
      def defaultOf = Common.defaultVal(element[A])
      def manifest: Manifest[Elem[A]] = m
    }

  //-------------- Stagin of elements ---------------


}

trait ElemsExp extends Elems
                            with BaseExp
                            with Scalan { self: ScalanStaged =>

  override implicit lazy val boolElement: Elem[Boolean] =
    new StagedBaseElement[Boolean](BooleanRepDefaultOf, manifest[Boolean])

  override implicit lazy val intElement: Elem[Int] =
    new StagedBaseElement[Int](IntRepDefaultOf, manifest[Int])

  override implicit lazy val floatElement: Elem[Float] =
    new StagedBaseElement[Float](FloatRepDefaultOf, manifest[Float])

  override implicit lazy val stringElement: Elem[String] =
    new StagedBaseElement[String](StringRepDefaultOf, manifest[String])

  implicit def arrayElement[A](implicit m: Manifest[A]): Elem[Array[A]] =
    new StagedBaseElement[Array[A]](arrayRepDefaultOf, manifest[Array[A]]) {
      //override def toRep(x: Array[A]) = Const(x) //ArrayConst(Const(x))
    }

  override implicit lazy val unitElement: Elem[Unit] = new StagedUnitElement

  trait StagedElement[A] extends Element[A] {
    //def toRep(x: A): Rep[A] = { implicit val eA = this; Const(x) }
  }

  class StagedBaseElement[A](z: => DefaultOf[Rep[A]], m: Manifest[A])
    extends BaseElem[A] with StagedElement[A] {
    override lazy val defaultOf = z
    def manifest: Manifest[A] = m
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
      //override def toRep(x: (A|B)): Rep[(A|B)] = x fold(l => Left[A, B](self.toRep(l)), r => Right[A, B](self.toRep(r)))
    }

  override implicit def funcElement[A, B](implicit elema: Elem[A], elemb: Elem[B]): Elem[A => B] =
    new FuncElem[A, B](elema, elemb) with StagedElement[A => B] {
      lazy val m: Manifest[A => B] = Manifest.classType(classOf[A => B], ea.manifest, eb.manifest)

      def manifest = m

      lazy val z: DefaultOf[Rep[A=>B]] = defaultVal[Rep[A=>B]](fun { x => eb.defaultOf.value })
      implicit def defaultOf = z
    }

  override implicit def elemElement[A](implicit elema: Elem[A]): Elem[Elem[A]] =
    new ElemElem[A](elema) with StagedElement[Elem[A]] {
      lazy val m: Manifest[Elem[A]] = Manifest.classType(classOf[Elem[A]], ea.manifest)
      def defaultOf = Common.defaultVal(element[A])
      def manifest: Manifest[Elem[A]] = m
    }

//  override def toRep[A](x: A)(implicit eA: Elem[A]) = eA match {
//    case ee: ElemElem[a] => ElemDef[a](x)
//    case _ => super.toRep(x)(eA)
//  }

}
