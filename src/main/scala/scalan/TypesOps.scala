package scalan

import scala.annotation.implicitNotFound
import scalan.common.{Common, DefaultOf}

trait TypesOps { scalan: TypesDsl =>

  @implicitNotFound(msg = "No Type available for ${A}.")
  trait TypeOps[A] extends Type[A] {
    def manifest: Manifest[A] = manifestFromString(typeCode).asInstanceOf[Manifest[A]]
    def defaultOf: DefaultOf[Rep[A]] = Common.defaultVal(defaultValue)
    //def name = manifest.toString
  }
  trait TypeImplOps[A] extends TypeOps[A] { }
  trait TypeImplCompanion {
  }
  def manifestFromString(typeCode: Rep[String]): Manifest[_] = ???
}

trait TypesDsl extends ScalanDsl with TypesAbs with TypesOps { }

trait TypesDslSeq extends TypesDsl with TypesSeq with ScalanSeqImplementation

trait TypesDslExp extends TypesDsl with TypesExp with ScalanStaged