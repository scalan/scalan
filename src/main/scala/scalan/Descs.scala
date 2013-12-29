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

trait Descs extends Base { self: Scalan =>
  type Desc[A] = TypeDescriptor[A]    // typeclass vitnessing that A belong to Scalan's type family

  @implicitNotFound(msg = "No TypeDescriptor available for ${A}.")
  trait TypeDescriptor[A] {
    def getElem(x: Rep[A]): Elem[A]
  }

  def descriptor[A](implicit da: Desc[A]): Desc[A] = da

  abstract class UnitDesc extends TypeDescriptor[Unit] {
  }
  abstract class BaseDesc[A] extends TypeDescriptor[A] {
  }
  abstract class PairDesc[A,B](val ea: Desc[A], val eb: Desc[B]) extends TypeDescriptor[(A,B)] {
  }
  abstract class SumDesc [A,B](val ea: Elem[A], val eb: Elem[B]) extends TypeDescriptor[(A|B)] {
  }
  abstract class FuncDesc[A,B](val ea: Desc[A], val eb: Desc[B]) extends TypeDescriptor[A => B]
//  abstract class DescDesc[A](val ea: Desc[A]) extends TypeDescriptor[Desc[A]]

  implicit val boolTypeDescriptor:  Desc[Boolean]
  implicit val intTypeDescriptor:   Desc[Int]
  implicit val floatTypeDescriptor: Desc[Float]
  implicit val unitTypeDescriptor:  Desc[Unit]
  implicit val stringTypeDescriptor:  Desc[String]
  implicit def arrayTypeDescriptor[A: Manifest]: Desc[Array[A]]

  implicit def pairTypeDescriptor[A,B](implicit ea: Desc[A], eb: Desc[B]): Desc[(A,B)]
  implicit def sumTypeDescriptor [A,B](implicit ea: Elem[A], eb: Elem[B]): Desc[(A|B)]
  implicit def funcTypeDescriptor[A,B](implicit ea: Desc[A], eb: Desc[B]): Desc[A => B]
//  implicit def elemTypeDescriptor[A](implicit ea: Desc[A]): Desc[Desc[A]]

  implicit def PairDescExtensions[A,B](eAB: Desc[(A,B)]): PairDesc[A,B] = eAB.asInstanceOf[PairDesc[A,B]]
  implicit def SumDescExtensions[A,B](eAB: Desc[(A|B)]): SumDesc[A,B] = eAB.asInstanceOf[SumDesc[A,B]]
  implicit def FuncDescExtensions[A,B](eAB: Desc[A=>B]): FuncDesc[A,B] = eAB.asInstanceOf[FuncDesc[A,B]]
  implicit def UnitDescExtensions(eu: Desc[Unit]): UnitDesc = eu.asInstanceOf[UnitDesc]
//  implicit def DescDescExtensions[A](eeA: Desc[Desc[A]]): DescDesc[A] = eeA.asInstanceOf[DescDesc[A]]
}

trait DescsSeq extends Descs with Scalan { self: ScalanSeq =>

  trait SeqTypeDescriptor[A] extends TypeDescriptor[A] {
  }

  abstract class SeqBaseTypeDescriptor[A] extends BaseDesc[A] with SeqTypeDescriptor[A]

  class SeqUnitTypeDescriptor extends UnitDesc with SeqTypeDescriptor[Unit] {
    def getElem(x: Unit) = element[Unit]
  }
  class SeqArrayTypeDescriptor[A:Manifest] extends BaseDesc[Array[A]] with SeqTypeDescriptor[Array[A]] {
    def getElem(x: Array[A]) = element[Array[A]]
  }

  override implicit lazy val boolTypeDescriptor: Desc[Boolean] =  new SeqBaseTypeDescriptor[Boolean] {
    def getElem(x: Boolean) = element[Boolean]
  }
  override implicit lazy val intTypeDescriptor: Desc[Int] = new SeqBaseTypeDescriptor[Int] {
    def getElem(x: Int) = element[Int]
  }
  override implicit lazy val floatTypeDescriptor: Desc[Float] = new SeqBaseTypeDescriptor[Float] {
    def getElem(x: Float) = element[Float]
  }
  override implicit lazy val stringTypeDescriptor: Desc[String] = new SeqBaseTypeDescriptor[String] {
    def getElem(x: String) = element[String]
  }
  override implicit def arrayTypeDescriptor[A](implicit m: Manifest[A]): Desc[Array[A]] = new SeqArrayTypeDescriptor[A]
  override implicit lazy val unitTypeDescriptor: Desc[Unit] = new SeqUnitTypeDescriptor

  override implicit def pairTypeDescriptor[A, B](implicit elema: Desc[A], elemb: Desc[B]): Desc[(A, B)] =
    new PairDesc[A, B](elema, elemb) with SeqTypeDescriptor[(A, B)] {
      def getElem(x: (A, B)) = pairElement(elema.getElem(x._1), elemb.getElem(x._2))
    }

  override implicit def sumTypeDescriptor[A, B](implicit elema: Elem[A], elemb: Elem[B]): Desc[(A | B)] =
    new SumDesc[A, B](elema, elemb) with SeqTypeDescriptor[(A | B)] {
      def getElem(x: (A|B)) = element[A | B]
    }

  implicit def funcTypeDescriptor[A,B](implicit elema: Desc[A], elemb: Desc[B]): Desc[A => B] =
    new FuncDesc[A, B](elema, elemb) with SeqTypeDescriptor[A => B] {
      def getElem(f: A=>B) =  { ???
//        val default = elema.defaultOf.value
//        val res = f(default)
//        implicit val dB = elemb.getElem(res)
//        descriptor[A => B]
      }
    }

}

trait DescsExp extends Descs
                            with BaseExp
                            with Scalan { self: ScalanStaged =>

  trait ExpTypeDescriptor[A] extends TypeDescriptor[A] {
  }

  abstract class ExpBaseTypeDescriptor[A] extends BaseDesc[A] with ExpTypeDescriptor[A]

  class ExpUnitTypeDescriptor extends UnitDesc with ExpTypeDescriptor[Unit] {
    def getElem(x: Rep[Unit]) = element[Unit]
  }
  class ExpArrayTypeDescriptor[A:Manifest] extends BaseDesc[Array[A]] with ExpTypeDescriptor[Array[A]] {
    def getElem(x: Rep[Array[A]]) = element[Array[A]]
  }

  override implicit lazy val boolTypeDescriptor: Desc[Boolean] =  new ExpBaseTypeDescriptor[Boolean] {
    def getElem(x: Rep[Boolean]) = element[Boolean]
  }
  override implicit lazy val intTypeDescriptor: Desc[Int] = new ExpBaseTypeDescriptor[Int] {
    def getElem(x: Rep[Int]) = element[Int]
  }
  override implicit lazy val floatTypeDescriptor: Desc[Float] = new ExpBaseTypeDescriptor[Float] {
    def getElem(x: Rep[Float]) = element[Float]
  }
  override implicit lazy val stringTypeDescriptor: Desc[String] = new ExpBaseTypeDescriptor[String] {
    def getElem(x: Rep[String]) = element[String]
  }
  override implicit def arrayTypeDescriptor[A](implicit m: Manifest[A]): Desc[Array[A]] = new ExpArrayTypeDescriptor[A]
  override implicit lazy val unitTypeDescriptor: Desc[Unit] = new ExpUnitTypeDescriptor

  override implicit def pairTypeDescriptor[A, B](implicit elema: Desc[A], elemb: Desc[B]): Desc[(A, B)] =
    new PairDesc[A, B](elema, elemb) with ExpTypeDescriptor[(A, B)] {
      def getElem(x: Rep[(A, B)]) = pairElement(elema.getElem(x._1), elemb.getElem(x._2))
    }

  override implicit def sumTypeDescriptor[A, B](implicit elema: Elem[A], elemb: Elem[B]): Desc[(A | B)] =
    new SumDesc[A, B](elema, elemb) with ExpTypeDescriptor[(A | B)] {
      def getElem(x: Rep[(A|B)]) = element[A | B]
    }

  implicit def funcTypeDescriptor[A,B](implicit elema: Desc[A], elemb: Desc[B]): Desc[A => B] =
    new FuncDesc[A, B](elema, elemb) with ExpTypeDescriptor[A => B] {
      def getElem(f: Rep[A=>B]) = { ???
//        val default = elema.defaultOf.value
//        val res = f(default)
//        implicit val dB = elemb.getElem(res)
//        descriptor[A => B]
      }
    }
}
