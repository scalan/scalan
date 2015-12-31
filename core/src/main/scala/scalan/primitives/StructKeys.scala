package scalan.primitives

import scalan._
import scalan.common._
import scala.reflect.runtime.universe._

trait StructKeys extends ViewsDsl with Entities  { self: StructsDsl with Scalan =>

  type SKey = Rep[StructKey]
  trait StructKey extends Def[StructKey] {
    def keys: Rep[KeySet]
    def index: Rep[Int]
  }
  abstract class IndexStructKey(val keys: Rep[KeySet], val index: Rep[Int]) extends StructKey {
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

trait StructKeysDslSeq extends impl.StructKeysSeq {self: StructsDsl with ScalanSeq =>
  def keyset_create(keys: Seq[String]): Rep[KeySet] = KeySetSeq(keys)
}

trait StructKeysDslExp extends impl.StructKeysExp {self: StructsDsl with ScalanExp =>
  def keyset_create(keys: Seq[String]): Rep[KeySet] = KeySetDef(keys)
  case class KeySetDef(keys: Seq[String]) extends BaseDef[KeySet] {
    override def toString = s"KeySet(${keys.mkString(",")})"
  }
}

