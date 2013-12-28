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
  trait Tuple2TypeOps[A,B] extends TypeOps[(A,B)] { }
  trait Tuple2TypeCompanion {
  }

  abstract class Tuple2Type[A,B](val tyA: Ty[A], val tyB: Ty[B])
    extends Type[(A,B)]
    with Tuple2TypeOps[A,B] { self: Tuple2TypeOps[A,B] =>
  }

  def manifestFromString(typeCode: Rep[String]): Manifest[_] = ???
}

trait TypesDsl extends ScalanDsl with TypesAbs with TypesOps { }

trait TypesDslSeq extends TypesDsl with TypesSeq with ScalanSeqImplementation

trait TypesDslExp extends TypesDsl with TypesExp with ScalanStaged