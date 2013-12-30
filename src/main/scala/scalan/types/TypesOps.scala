package scalan.types

import scala.annotation.implicitNotFound
import scalan.common.{Common, DefaultOf}
import scalan._

trait TypesOps { scalan: TypesDsl =>

  trait TypeOps[A] extends Type[A] {
    def manifest: Manifest[A] = manifestFromString(typeCode).asInstanceOf[Manifest[A]]
    def defaultOf: DefaultOf[Rep[A]] = Common.defaultVal(defaultValue)
  }
  trait TypeCompanion extends TypeFamily1[Type] {
    import Common._
    def defaultOf[A](implicit ea: Elem[A]): DefaultOf[Rep[Type[A]]] = ea match {
      case baseE: BaseElem[a] => BaseType.defaultOf[a](baseE)
      case pairE: PairElem[a,b] => Tuple2Type.defaultOf[a,b](pairE.ea, pairE.eb)
      case _ => ???
    }
  }

  //-------------------------------  BaseType ----------------------------------
  trait BaseTypeOps[A] extends TypeOps[A] { }
  trait BaseTypeCompanion extends ConcreteClass1[BaseType] {
    import Common._
    def defaultOf[A](implicit ea: Elem[A]) = defaultVal(BaseType(getBaseTypeCode(ea), ea.defaultOf.value))
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
    import Common._
    def defaultOf[A,B](implicit ea: Elem[A], eb: Elem[B]) = {
      val tyA = Type.defaultOf[A].value
      val tyB = Type.defaultOf[B].value
      defaultVal(Tuple2Type(tyA, tyB))
    }
  }


  def manifestFromString(typeCode: Rep[String]): Manifest[_] = ???
  def getBaseTypeCode[A](e: Elem[A]): Rep[String] = ???
}

trait TypesDsl extends ScalanDsl with TypesAbs with TypesOps { }

trait TypesDslSeq extends TypesDsl with TypesSeq with ScalanSeqImplementation

trait TypesDslExp extends TypesDsl with TypesExp with ScalanStaged