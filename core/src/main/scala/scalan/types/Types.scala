package scalan.types

import scalan.common.Default
import scalan._
import scala.reflect.runtime.universe._

trait Types extends Base { self: TypesDsl =>

  type Ty[A] = Rep[Type[A]]
  trait Type[A] extends Reifiable[Type[A]] {
    implicit def eA: Elem[A]
    def typeCode: Rep[String]
    def defaultValue: Rep[A]
    def tag: TypeTag[A] = typeTagFromString(typeCode).asInstanceOf[TypeTag[A]]
    lazy val defaultOf: Default[Rep[A]] = Default.defaultVal(defaultValue)
  }

  trait TypeCompanion extends TypeFamily1[Type] {
    def defaultOf[A](implicit ea: Elem[A]): Default[Rep[Type[A]]] = ea match {
      case baseE: BaseElem[a] => BaseType.defaultOf[a](baseE)
      case pairE: PairElem[a, b] => Tuple2Type.defaultOf[a, b](pairE.eFst, pairE.eSnd)
      case _ => ???
    }
  }

  abstract class BaseType[A](
    val typeCode: Rep[String],
    val defaultValue: Rep[A])(implicit eA: Elem[A]) extends Type[A]

  trait BaseTypeCompanion extends ConcreteClass1[BaseType] with TypeCompanion {
    override def defaultOf[A](implicit ea: Elem[A]) = Default.defaultVal(BaseType(getBaseTypeCode(ea), ea.defaultRepValue))
  }

  abstract class Tuple2Type[A, B](val tyA: Rep[Type[A]], val tyB: Rep[Type[B]])(implicit val e1: Elem[A], val e2: Elem[B]) extends Type[(A, B)] {
    def eA = element[(A,B)]
    def typeCode = ???
    def defaultValue = Pair(tyA.defaultValue, tyB.defaultValue)  }

  trait Tuple2TypeCompanion extends ConcreteClass2[Tuple2Type] with TypeCompanion {
    def defaultOf[A,B](implicit ea: Elem[A], eb: Elem[B]) = {
      val tyA = Type.defaultOf[A].value
      val tyB = Type.defaultOf[B].value
      Default.defaultVal(Tuple2Type(tyA, tyB))
    }
  }

  implicit def defaultTypeElement[A: Elem]: Elem[Type[A]] = element[A] match {
    case _: BaseElem[_] => element[BaseType[A]].asElem[Type[A]]
    case pe: PairElem[a, b] =>
      implicit val ea = pe.eFst
      implicit val eb = pe.eSnd
      element[Tuple2Type[a, b]].asElem[Type[A]]
    case _ => ???
  }
  
  def typeTagFromString(typeCode: Rep[String]): TypeTag[_] = ???
  def getBaseTypeCode[A](e: Elem[A]): Rep[String] = ???
}


trait TypesDsl extends ScalanDsl with impl.TypesAbs with Types

trait TypesDslSeq extends TypesDsl with impl.TypesSeq with ScalanSeqImplementation

trait TypesDslExp extends TypesDsl with impl.TypesExp with ScalanStaged