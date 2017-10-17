package scalan.primitives

import scalan._

trait StructKeys extends ViewsModule with Entities  { self: Structs with Scalan =>

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

trait StructKeysModule extends impl.StructKeysDefs {self: Structs with Scalan =>
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
  class KeySetElem extends BaseElem[KeySet](KeySetSeq(Seq()))
  implicit val KeySetElement: Elem[KeySet] = new KeySetElem
  def keyset_create(keys: Seq[String]): Rep[KeySet] = KeySetDef(keys)
  case class KeySetDef(keys: Seq[String]) extends BaseDef[KeySet] {
    override def toString = s"KeySet(${keys.mkString(",")})"
  }
}

