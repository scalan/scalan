package scalan.types

import scala.annotation.implicitNotFound
import scala.annotation.unchecked.uncheckedVariance
import scalan.common.Default
import scalan._
import scala.reflect.runtime.universe._

trait Types extends Base with TypesOps { self: TypesDsl =>

  type Ty[A] = Rep[Type[A]]
  trait Type[A] extends UserType[Type[A]] {
    implicit def eA: Elem[A]
    def typeCode: Rep[String]
    def defaultValue: Rep[A]
    def tag: TypeTag[A]
    def defaultOf: Default[Rep[A]]
  }
  trait TypeCompanion extends TypeFamily1[Type] {}

  abstract class BaseType[A](
      val typeCode: Rep[String],
      val defaultValue: Rep[A])(implicit eA: Elem[A])
    extends Type[A]
       with BaseTypeOps[A]
  trait BaseTypeCompanion extends ConcreteClass1[BaseType] {}

  abstract class Tuple2Type[A,B](val tyA: Rep[Type[A]], val tyB: Rep[Type[B]])(implicit val e1: Elem[A], val e2: Elem[B])
    extends Type[(A,B)]
    with Tuple2TypeOps[A,B]
  trait Tuple2TypeCompanion extends ConcreteClass2[Tuple2Type] {}

}
