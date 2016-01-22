package scalan.primitives

import scalan._
import scalan.common._
import scala.reflect.runtime.universe._

trait StructKeys extends ViewsDsl with Entities  { self: StructsDsl with Scalan =>

  type SKey[S <: Struct] = Rep[StructKey[S]]
  trait StructKey[Schema <: Struct] extends Def[StructKey[Schema]] {
    def eSchema: Elem[Schema]
    def index: Rep[Int]
    def name: Rep[String]
  }
  abstract class IndexStructKey[Schema <: Struct]
      (val index: Rep[Int])
      (implicit val eSchema: Elem[Schema]) extends StructKey[Schema] {
    def name: Rep[String] = {
      val i = index.asValue
      eSchema.fieldNames(i)
    }
    override def toString = s"${eSchema.fieldsString}[$index]"
  }

  abstract class NameStructKey[Schema <: Struct]
      (val name: Rep[String])
      (implicit val eSchema: Elem[Schema]) extends StructKey[Schema] {
    def index: Rep[Int] = {
      val n = name.asValue
      eSchema.findFieldIndex(n)
    }
    override def toString = s"${eSchema.fieldsString}.$name"
  }

}

trait StructKeysDsl extends impl.StructKeysAbs {self: StructsDsl with Scalan =>
  type KSet = Rep[KeySet]
  trait KeySet {
    def keys: Seq[String]
  }
  class KeySetCompanion {
    def apply(names: Seq[String]) = keyset_create(names)
  }
  val KeySet: KeySetCompanion = new KeySetCompanion

  case class KeySetSeq(keys: Seq[String]) extends KeySet

  implicit class KeySetOps(ks: Rep[KeySet]) {
    //    def apply(i: Rep[Int]) = keyset_getAt(ks, i)
  }
  class KeySetElem extends BaseElem[KeySet]()(weakTypeTag[KeySet], Default.defaultVal(KeySetSeq(Seq())))
  implicit val KeySetElement: Elem[KeySet] = new KeySetElem
  def keyset_create(keys: Seq[String]): Rep[KeySet]
  //  def keyset_getAt(ks: KSet, i: Rep[Int]): Rep[StructKey]


}

trait StructKeysDslStd extends impl.StructKeysStd {self: StructsDsl with ScalanStd =>
  def keyset_create(keys: Seq[String]): Rep[KeySet] = KeySetSeq(keys)
}

trait StructKeysDslExp extends impl.StructKeysExp {self: StructsDsl with ScalanExp =>
  def keyset_create(keys: Seq[String]): Rep[KeySet] = KeySetDef(keys)
  case class KeySetDef(keys: Seq[String]) extends BaseDef[KeySet] {
    override def toString = s"KeySet(${keys.mkString(",")})"
  }
}

