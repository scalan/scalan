package scalan.types

import scala.annotation.implicitNotFound
import scalan.common.Default
import scalan._
import scala.reflect.runtime.universe._

trait TypesOps { scalan: TypesDsl =>

  trait TypeOps[A] extends Type[A] {
    def tag: TypeTag[A] = typeTagFromString(typeCode).asInstanceOf[TypeTag[A]]
    lazy val defaultOf: Default[Rep[A]] = Default.defaultVal(defaultValue)
  }
  trait TypeCompanion extends TypeFamily1[Type] {
    def defaultOf[A](implicit ea: Elem[A]): Default[Rep[Type[A]]] = ea match {
      case baseE: BaseElem[a] => BaseType.defaultOf[a](baseE)
      case pairE: PairElem[a,b] => Tuple2Type.defaultOf[a,b](pairE.ea, pairE.eb)
      case _ => ???
    }
  }

  //-------------------------------  BaseType ----------------------------------
  trait BaseTypeOps[A] extends TypeOps[A] { }
  trait BaseTypeCompanion extends ConcreteClass1[BaseType] {
    def defaultOf[A](implicit ea: Elem[A]) = Default.defaultVal(BaseType(getBaseTypeCode(ea), ea.defaultRepValue))
  }

  //-------------------------------  Tuple2Type ----------------------------------
  trait Tuple2TypeOps[A,B] extends TypeOps[(A,B)] {
    implicit def e1: Elem[A]
    implicit def e2: Elem[B]
    def eA = element[(A,B)]
    def tyA: Ty[A]
    def tyB: Ty[B]
    def typeCode = ???
    def defaultValue = Pair(tyA.defaultValue, tyB.defaultValue)
  }
  trait Tuple2TypeCompanion extends ConcreteClass2[Tuple2Type] {
    def defaultOf[A,B](implicit ea: Elem[A], eb: Elem[B]) = {
      val tyA = Type.defaultOf[A].value
      val tyB = Type.defaultOf[B].value
      Default.defaultVal(Tuple2Type(tyA, tyB))
    }
  }


  def typeTagFromString(typeCode: Rep[String]): TypeTag[_] = ???
  def getBaseTypeCode[A](e: Elem[A]): Rep[String] = ???
}

trait TypesDsl extends ScalanDsl with TypesAbs with TypesOps { }

trait TypesDslSeq extends TypesDsl with TypesSeq with ScalanSeqImplementation

trait TypesDslExp extends TypesDsl with TypesExp with ScalanStaged