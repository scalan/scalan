package scalan

import scala.text._
import Document._
import scalan.common.Monoid._
import scalan.common._
import Common._
import annotation.implicitNotFound
import scalan.staged.BaseExp
import scala.language.{implicitConversions}

trait TypeDescriptors extends Base { self: Scalan =>
  type Elem[A] = Element[A]    // typeclass of type descriptors

  implicit val boolElement:  Elem[Boolean]
  implicit val intElement:   Elem[Int]
  implicit val floatElement: Elem[Float]
  implicit val unitElement:  Elem[Unit]
  implicit val stringElement:  Elem[String]
  implicit def arrayElement[A: Manifest]: Elem[Array[A]]

  implicit def pairElement[A,B](implicit ea: Elem[A], eb: Elem[B]): Elem[(A,B)]
  implicit def sumElement [A,B](implicit ea: Elem[A], eb: Elem[B]): Elem[(A|B)]
  implicit def funcElement[A,B](implicit ea: Elem[A], eb: Elem[B]): Elem[A => B]

  def elem2Zero[A](implicit ea: Elem[A]): Zero[Rep[A]] = ea.zero

  @implicitNotFound(msg = "No Element available for ${A}.")
  trait Element[A] {
    def manifest: Manifest[A]
    def zero: Zero[Rep[A]]
    def name = manifest.toString
    def defaultOf = mzero[Rep[A]](zero)
    def toRep(x: A): Rep[A]
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

  implicit def PairElemExtensions[A,B](eAB: Elem[(A,B)]): PairElem[A,B] = eAB.asInstanceOf[PairElem[A,B]]
  implicit def SumElemExtensions[A,B](eAB: Elem[(A|B)]): SumElem[A,B] = eAB.asInstanceOf[SumElem[A,B]]
  implicit def FuncElemExtensions[A,B](eAB: Elem[A=>B]): FuncElem[A,B] = eAB.asInstanceOf[FuncElem[A,B]]
  implicit def UnitElemExtensions(eu: Elem[Unit]): UnitElem = eu.asInstanceOf[UnitElem]
}

trait TypeDescriptorsSeq extends TypeDescriptors with Scalan { self: ScalanSeq =>

  override implicit lazy val boolElement: Elem[Boolean] =
    new SeqBaseElement[Boolean]()(Zero.BooleanZero, manifest[Boolean])

  override implicit lazy val intElement: Elem[Int] =
    new SeqBaseElement[Int]()(Zero.IntZero, manifest[Int])

  override implicit lazy val floatElement: Elem[Float] =
    new SeqBaseElement[Float]()(Zero.FloatZero, manifest[Float])

  override implicit lazy val stringElement: Elem[String] =
    new SeqBaseElement[String]()(Zero.StringZero, manifest[String])

  implicit def arrayElement[A](implicit m: Manifest[A]): Elem[Array[A]] =
    new SeqBaseElement[Array[A]]()(Zero.ArrayZero(m), manifest[Array[A]])

  override implicit lazy val unitElement: Elem[Unit] = new SeqUnitElement

  trait SeqElement[A] extends Element[A] {
    def toRep(x: A): Rep[A] = x
  }

  class SeqBaseElement[A](implicit override val zero: Zero[A], m: Manifest[A])
    extends BaseElem[A] with SeqElement[A] {
    def manifest: Manifest[A] = m
  }

  class SeqUnitElement extends UnitElem with SeqElement[Unit] {
    private val m = scala.Predef.manifest[Unit]
    private val z = Zero.UnitZero
    implicit def manifest = m
    def zero = z
  }

  override implicit def pairElement[A, B](implicit elema: Elem[A], elemb: Elem[B]): Elem[(A, B)] =
    new PairElem[A, B](elema, elemb) with SeqElement[(A, B)] {
      private val m = Manifest.classType(classOf[(A, B)], ea.manifest, eb.manifest)
      private val z = Common.zero((ea.defaultOf, eb.defaultOf))

      def manifest: Manifest[(A, B)] = m
      def zero = z
    }

  override implicit def sumElement[A, B](implicit elema: Elem[A], elemb: Elem[B]): Elem[(A | B)] =
    new SumElem[A, B](elema, elemb) with SeqElement[(A | B)] {
      lazy val boolElem = element[Boolean]
      lazy val m: Manifest[(A|B)] = Manifest.classType(classOf[(A|B)], ea.manifest, eb.manifest)
      def zero = Common.zero(scala.Left(ea.defaultOf))
      def manifest: Manifest[(A|B)] = m
    }

  implicit def funcElement[A,B](implicit elema: Elem[A], elemb: Elem[B]): Elem[A => B] =
    new FuncElem[A, B](elema, elemb) with SeqElement[A => B] {
      //implicit val elemAB = this
      private val m = Manifest.classType(classOf[A=>B], ea.manifest, eb.manifest)
      private val z = {
        implicit val zA = ea.zero; implicit val zB = eb.zero
        //implicitly[Zero[Rep[A=>B]]]
        Zero.Function1ABZero[Rep[A],Rep[B]](zB)
      }

      def manifest: Manifest[A=>B] = m
      def zero = z
    }

}

trait TypeDescriptorsExp extends TypeDescriptors
                            with BaseExp
                            with Scalan { self: ScalanStaged =>

  override implicit lazy val boolElement: Elem[Boolean] =
    new StagedBaseElement[Boolean](BooleanRepFalse, manifest[Boolean])

  override implicit lazy val intElement: Elem[Int] =
    new StagedBaseElement[Int](IntRepZero, manifest[Int])

  override implicit lazy val floatElement: Elem[Float] =
    new StagedBaseElement[Float](FloatRepZero, manifest[Float])

  override implicit lazy val stringElement: Elem[String] =
    new StagedBaseElement[String](StringRepZero, manifest[String])

  implicit def arrayElement[A](implicit m: Manifest[A]): Elem[Array[A]] =
    new StagedBaseElement[Array[A]](arrayRepZero, manifest[Array[A]]) {
      override def toRep(x: Array[A]) = Const(x) //ArrayConst(Const(x))
    }

  override implicit lazy val unitElement: Elem[Unit] = new StagedUnitElement

  trait StagedElement[A] extends Element[A] {
    def toRep(x: A): Rep[A] = { implicit val eA = this; Const(x) }
  }

  class StagedBaseElement[A](z: => Zero[Rep[A]], m: Manifest[A])
    extends BaseElem[A] with StagedElement[A] {
    override lazy val zero = z
    def manifest: Manifest[A] = m
  }

  class StagedUnitElement extends UnitElem with StagedElement[Unit] {
    private val m = scala.Predef.manifest[Unit]
    private lazy val z: Zero[Rep[Unit]] = Common.zero(toRep(()))
    def manifest = m
    def zero = z
  }

  override implicit def pairElement[A, B](implicit elema: Elem[A], elemb: Elem[B]): Elem[(A, B)] =
    new PairElem[A, B](elema, elemb) with StagedElement[(A, B)] {
      private val m = Manifest.classType(classOf[(A, B)], ea.manifest, eb.manifest)
      private lazy val z = Common.zero(Pair(ea.defaultOf, eb.defaultOf))

      def manifest: Manifest[(A, B)] = m

      def zero = z

      override def toRep(x: (A,B)): Rep[(A,B)] = Pair(ea.toRep(x._1), eb.toRep(x._2))
    }

  override implicit def sumElement[A, B](implicit elema: Elem[A], elemb: Elem[B]): Elem[(A | B)] =
    new SumElem[A, B](elema, elemb) with StagedElement[(A | B)] {
      lazy val m: Manifest[(A | B)] = Manifest.classType(classOf[(A | B)], ea.manifest, eb.manifest)

      def zero = Common.zero(toLeftSum[A,B](ea.defaultOf))

      def manifest: Manifest[(A | B)] = m

      override def toRep(x: (A|B)): Rep[(A|B)] = x fold(l => Left[A, B](self.toRep(l)), r => Right[A, B](self.toRep(r)))
    }

  override implicit def funcElement[A, B](implicit elema: Elem[A], elemb: Elem[B]): Elem[A => B] =
    new FuncElem[A, B](elema, elemb) with StagedElement[A => B] {
      lazy val m: Manifest[A => B] = Manifest.classType(classOf[A => B], ea.manifest, eb.manifest)

      def manifest = m

      lazy val z: Zero[Rep[A=>B]] = Common.zero[Rep[A=>B]](fun { x => eb.zero.zero })
      implicit def zero = z
    }

}
