package scalan.types

import scala.annotation.implicitNotFound
import scalan.common.{Common, DefaultOf}
import scalan._

trait TypesOps { scalan: TypesDsl =>

  trait TypeOps[A] extends Type[A] {
    def manifest: Manifest[A] = manifestFromString(typeCode).asInstanceOf[Manifest[A]]
    def defaultOf: DefaultOf[Rep[A]] = Common.defaultVal(defaultValue)
    //def name = manifest.toString
  }
  trait BaseTypeOps[A] extends TypeOps[A] { }
  trait BaseTypeCompanion {
  }
  trait Tuple2TypeOps[A,B] extends TypeOps[(A,B)] {
    implicit def e1: Elem[A]
    implicit def e2: Elem[B]
    def eA = element[(A,B)]
    def tyA: Ty[A]
    def tyB: Ty[B]


    def typeCode = ???

    def defaultValue = ???
  }
  trait Tuple2TypeCompanion {
  }


  def manifestFromString(typeCode: Rep[String]): Manifest[_] = ???
}

trait TypesDsl extends ScalanDsl with TypesAbs with TypesOps { }

trait TypesDslSeq extends TypesDsl with TypesSeq with ScalanSeqImplementation

trait TypesDslExp extends TypesDsl with TypesExp with ScalanStaged