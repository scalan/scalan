package scalan

import scala.annotation.implicitNotFound
import scala.annotation.unchecked.uncheckedVariance
import scalan.common.{DefaultOf, Common}
import Common._
import scala.language.implicitConversions

trait Types extends Base with TypesOps { self: TypesDsl =>

  type Ty[A] = Rep[Type[A]]
  trait Type[A] extends UserType[Type[A]] {
    implicit def eA: Elem[A]
    def typeCode: Rep[String]
    def defaultValue: Rep[A]
    def manifest: Manifest[A]
    def defaultOf: DefaultOf[Rep[A]]
  }

  abstract class TypeImpl[A](
      val typeCode: Rep[String],
      val defaultValue: Rep[A])(implicit eA: Elem[A])
    extends Type[A]
       with TypeImplOps[A] { self: TypeImplOps[A] =>
  }

}


