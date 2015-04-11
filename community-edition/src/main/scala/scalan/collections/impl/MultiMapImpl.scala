package scalan.collections
package impl

import scalan._
import scalan.common.Default
import scala.reflect.runtime.universe._
import scala.reflect._
import scalan.common.Default

// Abs -----------------------------------
trait MultiMapsAbs extends Scalan with MultiMaps {
  self: ScalanCommunityDsl =>
  // single proxy for each type family
  implicit def proxyMMultiMap[K, V](p: Rep[MMultiMap[K, V]]): MMultiMap[K, V] = {
    proxyOps[MMultiMap[K, V]](p)(classTag[MMultiMap[K, V]])
  }

  class MMultiMapElem[K, V, To <: MMultiMap[K, V]](implicit val elemKey: Elem[K], val elemValue: Elem[V])
    extends EntityElem[To] {
    override def isEntityType = true
    override def tag = {
      implicit val tagK = elemKey.tag
      implicit val tagV = elemValue.tag
      weakTypeTag[MMultiMap[K, V]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Reifiable[_]]) = convertMMultiMap(x.asRep[MMultiMap[K, V]])
    def convertMMultiMap(x : Rep[MMultiMap[K, V]]): Rep[To] = {
      assert(x.selfType1.isInstanceOf[MMultiMapElem[_,_,_]])
      x.asRep[To]
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def mMultiMapElement[K, V](implicit elemKey: Elem[K], elemValue: Elem[V]) =
    new MMultiMapElem[K, V, MMultiMap[K, V]]()(elemKey, elemValue)

  trait MMultiMapCompanionElem extends CompanionElem[MMultiMapCompanionAbs]
  implicit lazy val MMultiMapCompanionElem: MMultiMapCompanionElem = new MMultiMapCompanionElem {
    lazy val tag = weakTypeTag[MMultiMapCompanionAbs]
    protected def getDefaultRep = MMultiMap
  }

  abstract class MMultiMapCompanionAbs extends CompanionBase[MMultiMapCompanionAbs] with MMultiMapCompanion {
    override def toString = "MMultiMap"
  }
  def MMultiMap: Rep[MMultiMapCompanionAbs]
  implicit def proxyMMultiMapCompanion(p: Rep[MMultiMapCompanion]): MMultiMapCompanion = {
    proxyOps[MMultiMapCompanion](p)
  }

  // elem for concrete class
  class HashMMultiMapElem[K, V](val iso: Iso[HashMMultiMapData[K, V], HashMMultiMap[K, V]])(implicit elemKey: Elem[K], elemValue: Elem[V])
    extends MMultiMapElem[K, V, HashMMultiMap[K, V]]
    with ViewElem[HashMMultiMapData[K, V], HashMMultiMap[K, V]] {
    override def convertMMultiMap(x: Rep[MMultiMap[K, V]]) = HashMMultiMap(x.map)
    override def getDefaultRep = super[ViewElem].getDefaultRep
    override lazy val tag = super[ViewElem].tag
  }

  // state representation type
  type HashMMultiMapData[K, V] = MMap[K,ArrayBuffer[V]]

  // 3) Iso for concrete class
  class HashMMultiMapIso[K, V](implicit elemKey: Elem[K], elemValue: Elem[V])
    extends Iso[HashMMultiMapData[K, V], HashMMultiMap[K, V]] {
    override def from(p: Rep[HashMMultiMap[K, V]]) =
      unmkHashMMultiMap(p) match {
        case Some((map)) => map
        case None => !!!
      }
    override def to(p: Rep[MMap[K,ArrayBuffer[V]]]) = {
      val map = p
      HashMMultiMap(map)
    }
    lazy val tag = {
      implicit val tagK = elemKey.tag
      implicit val tagV = elemValue.tag
      weakTypeTag[HashMMultiMap[K, V]]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[HashMMultiMap[K, V]]](HashMMultiMap(element[MMap[K,ArrayBuffer[V]]].defaultRepValue))
    lazy val eTo = new HashMMultiMapElem[K, V](this)
  }
  // 4) constructor and deconstructor
  abstract class HashMMultiMapCompanionAbs extends CompanionBase[HashMMultiMapCompanionAbs] with HashMMultiMapCompanion {
    override def toString = "HashMMultiMap"

    def apply[K, V](map: Rep[MMap[K,ArrayBuffer[V]]])(implicit elemKey: Elem[K], elemValue: Elem[V]): Rep[HashMMultiMap[K, V]] =
      mkHashMMultiMap(map)
    def unapply[K:Elem, V:Elem](p: Rep[HashMMultiMap[K, V]]) = unmkHashMMultiMap(p)
  }
  def HashMMultiMap: Rep[HashMMultiMapCompanionAbs]
  implicit def proxyHashMMultiMapCompanion(p: Rep[HashMMultiMapCompanionAbs]): HashMMultiMapCompanionAbs = {
    proxyOps[HashMMultiMapCompanionAbs](p)
  }

  class HashMMultiMapCompanionElem extends CompanionElem[HashMMultiMapCompanionAbs] {
    lazy val tag = weakTypeTag[HashMMultiMapCompanionAbs]
    protected def getDefaultRep = HashMMultiMap
  }
  implicit lazy val HashMMultiMapCompanionElem: HashMMultiMapCompanionElem = new HashMMultiMapCompanionElem

  implicit def proxyHashMMultiMap[K, V](p: Rep[HashMMultiMap[K, V]]): HashMMultiMap[K, V] =
    proxyOps[HashMMultiMap[K, V]](p)

  implicit class ExtendedHashMMultiMap[K, V](p: Rep[HashMMultiMap[K, V]])(implicit elemKey: Elem[K], elemValue: Elem[V]) {
    def toData: Rep[HashMMultiMapData[K, V]] = isoHashMMultiMap(elemKey, elemValue).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoHashMMultiMap[K, V](implicit elemKey: Elem[K], elemValue: Elem[V]): Iso[HashMMultiMapData[K, V], HashMMultiMap[K, V]] =
    new HashMMultiMapIso[K, V]

  // 6) smart constructor and deconstructor
  def mkHashMMultiMap[K, V](map: Rep[MMap[K,ArrayBuffer[V]]])(implicit elemKey: Elem[K], elemValue: Elem[V]): Rep[HashMMultiMap[K, V]]
  def unmkHashMMultiMap[K:Elem, V:Elem](p: Rep[HashMMultiMap[K, V]]): Option[(Rep[MMap[K,ArrayBuffer[V]]])]
}

// Seq -----------------------------------
trait MultiMapsSeq extends MultiMapsDsl with ScalanSeq {
  self: ScalanCommunityDslSeq =>
  lazy val MMultiMap: Rep[MMultiMapCompanionAbs] = new MMultiMapCompanionAbs with UserTypeSeq[MMultiMapCompanionAbs, MMultiMapCompanionAbs] {
    lazy val selfType = element[MMultiMapCompanionAbs]
  }

  case class SeqHashMMultiMap[K, V]
      (override val map: Rep[MMap[K,ArrayBuffer[V]]])
      (implicit elemKey: Elem[K], elemValue: Elem[V])
    extends HashMMultiMap[K, V](map)
        with UserTypeSeq[MMultiMap[K,V], HashMMultiMap[K, V]] {
    lazy val selfType = element[HashMMultiMap[K, V]].asInstanceOf[Elem[MMultiMap[K,V]]]
  }
  lazy val HashMMultiMap = new HashMMultiMapCompanionAbs with UserTypeSeq[HashMMultiMapCompanionAbs, HashMMultiMapCompanionAbs] {
    lazy val selfType = element[HashMMultiMapCompanionAbs]
  }

  def mkHashMMultiMap[K, V]
      (map: Rep[MMap[K,ArrayBuffer[V]]])(implicit elemKey: Elem[K], elemValue: Elem[V]): Rep[HashMMultiMap[K, V]] =
      new SeqHashMMultiMap[K, V](map)
  def unmkHashMMultiMap[K:Elem, V:Elem](p: Rep[HashMMultiMap[K, V]]) =
    Some((p.map))
}

// Exp -----------------------------------
trait MultiMapsExp extends MultiMapsDsl with ScalanExp {
  self: ScalanCommunityDslExp =>
  lazy val MMultiMap: Rep[MMultiMapCompanionAbs] = new MMultiMapCompanionAbs with UserTypeDef[MMultiMapCompanionAbs, MMultiMapCompanionAbs] {
    lazy val selfType = element[MMultiMapCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  case class ExpHashMMultiMap[K, V]
      (override val map: Rep[MMap[K,ArrayBuffer[V]]])
      (implicit elemKey: Elem[K], elemValue: Elem[V])
    extends HashMMultiMap[K, V](map) with UserTypeDef[MMultiMap[K,V], HashMMultiMap[K, V]] {
    lazy val selfType = element[HashMMultiMap[K, V]].asInstanceOf[Elem[MMultiMap[K,V]]]
    override def mirror(t: Transformer) = ExpHashMMultiMap[K, V](t(map))
  }

  lazy val HashMMultiMap: Rep[HashMMultiMapCompanionAbs] = new HashMMultiMapCompanionAbs with UserTypeDef[HashMMultiMapCompanionAbs, HashMMultiMapCompanionAbs] {
    lazy val selfType = element[HashMMultiMapCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object HashMMultiMapMethods {
    object union {
      def unapply(d: Def[_]): Option[(Rep[HashMMultiMap[K, V]], Rep[MMultiMap[K,V]]) forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(that, _*), _) if receiver.elem.isInstanceOf[HashMMultiMapElem[_, _]] && method.getName == "union" =>
          Some((receiver, that)).asInstanceOf[Option[(Rep[HashMMultiMap[K, V]], Rep[MMultiMap[K,V]]) forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[HashMMultiMap[K, V]], Rep[MMultiMap[K,V]]) forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object toMap {
      def unapply(d: Def[_]): Option[Rep[HashMMultiMap[K, V]] forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[HashMMultiMapElem[_, _]] && method.getName == "toMap" =>
          Some(receiver).asInstanceOf[Option[Rep[HashMMultiMap[K, V]] forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[HashMMultiMap[K, V]] forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object contains {
      def unapply(d: Def[_]): Option[(Rep[HashMMultiMap[K, V]], Rep[K]) forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(key, _*), _) if receiver.elem.isInstanceOf[HashMMultiMapElem[_, _]] && method.getName == "contains" =>
          Some((receiver, key)).asInstanceOf[Option[(Rep[HashMMultiMap[K, V]], Rep[K]) forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[HashMMultiMap[K, V]], Rep[K]) forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[HashMMultiMap[K, V]], Rep[K]) forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(key, _*), _) if receiver.elem.isInstanceOf[HashMMultiMapElem[_, _]] && method.getName == "apply" =>
          Some((receiver, key)).asInstanceOf[Option[(Rep[HashMMultiMap[K, V]], Rep[K]) forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[HashMMultiMap[K, V]], Rep[K]) forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object applyIfBy {
      def unapply(d: Def[_]): Option[(Rep[HashMMultiMap[K, V]], Rep[K], Rep[ArrayBuffer[V] => T], Rep[Unit => T]) forSome {type K; type V; type T}] = d match {
        case MethodCall(receiver, method, Seq(key, exists, otherwise, _*), _) if receiver.elem.isInstanceOf[HashMMultiMapElem[_, _]] && method.getName == "applyIfBy" =>
          Some((receiver, key, exists, otherwise)).asInstanceOf[Option[(Rep[HashMMultiMap[K, V]], Rep[K], Rep[ArrayBuffer[V] => T], Rep[Unit => T]) forSome {type K; type V; type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[HashMMultiMap[K, V]], Rep[K], Rep[ArrayBuffer[V] => T], Rep[Unit => T]) forSome {type K; type V; type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object add {
      def unapply(d: Def[_]): Option[(Rep[HashMMultiMap[K, V]], Rep[K], Rep[V]) forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(key, value, _*), _) if receiver.elem.isInstanceOf[HashMMultiMapElem[_, _]] && method.getName == "add" =>
          Some((receiver, key, value)).asInstanceOf[Option[(Rep[HashMMultiMap[K, V]], Rep[K], Rep[V]) forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[HashMMultiMap[K, V]], Rep[K], Rep[V]) forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object addAll {
      def unapply(d: Def[_]): Option[(Rep[HashMMultiMap[K, V]], Rep[K], Rep[ArrayBuffer[V]]) forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(key, value, _*), _) if receiver.elem.isInstanceOf[HashMMultiMapElem[_, _]] && method.getName == "addAll" =>
          Some((receiver, key, value)).asInstanceOf[Option[(Rep[HashMMultiMap[K, V]], Rep[K], Rep[ArrayBuffer[V]]) forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[HashMMultiMap[K, V]], Rep[K], Rep[ArrayBuffer[V]]) forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduceBy {
      def unapply(d: Def[_]): Option[(Rep[HashMMultiMap[K, V]], Rep[Array[V] => T]) forSome {type K; type V; type T}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[HashMMultiMapElem[_, _]] && method.getName == "reduceBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[HashMMultiMap[K, V]], Rep[Array[V] => T]) forSome {type K; type V; type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[HashMMultiMap[K, V]], Rep[Array[V] => T]) forSome {type K; type V; type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object keys {
      def unapply(d: Def[_]): Option[Rep[HashMMultiMap[K, V]] forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[HashMMultiMapElem[_, _]] && method.getName == "keys" =>
          Some(receiver).asInstanceOf[Option[Rep[HashMMultiMap[K, V]] forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[HashMMultiMap[K, V]] forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object values {
      def unapply(d: Def[_]): Option[Rep[HashMMultiMap[K, V]] forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[HashMMultiMapElem[_, _]] && method.getName == "values" =>
          Some(receiver).asInstanceOf[Option[Rep[HashMMultiMap[K, V]] forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[HashMMultiMap[K, V]] forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object toArray {
      def unapply(d: Def[_]): Option[Rep[HashMMultiMap[K, V]] forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[HashMMultiMapElem[_, _]] && method.getName == "toArray" =>
          Some(receiver).asInstanceOf[Option[Rep[HashMMultiMap[K, V]] forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[HashMMultiMap[K, V]] forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object size {
      def unapply(d: Def[_]): Option[Rep[HashMMultiMap[K, V]] forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[HashMMultiMapElem[_, _]] && method.getName == "size" =>
          Some(receiver).asInstanceOf[Option[Rep[HashMMultiMap[K, V]] forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[HashMMultiMap[K, V]] forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object HashMMultiMapCompanionMethods {
    object empty {
      def unapply(d: Def[_]): Option[Unit forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[HashMMultiMapCompanionElem] && method.getName == "empty" =>
          Some(()).asInstanceOf[Option[Unit forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Unit forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object make {
      def unapply(d: Def[_]): Option[Rep[String] forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(name, _*), _) if receiver.elem.isInstanceOf[HashMMultiMapCompanionElem] && method.getName == "make" =>
          Some(name).asInstanceOf[Option[Rep[String] forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[String] forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromArray {
      def unapply(d: Def[_]): Option[Arr[(K, V)] forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(arr, _*), _) if receiver.elem.isInstanceOf[HashMMultiMapCompanionElem] && method.getName == "fromArray" =>
          Some(arr).asInstanceOf[Option[Arr[(K, V)] forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Arr[(K, V)] forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkHashMMultiMap[K, V]
    (map: Rep[MMap[K,ArrayBuffer[V]]])(implicit elemKey: Elem[K], elemValue: Elem[V]): Rep[HashMMultiMap[K, V]] =
    new ExpHashMMultiMap[K, V](map)
  def unmkHashMMultiMap[K:Elem, V:Elem](p: Rep[HashMMultiMap[K, V]]) =
    Some((p.map))

  object MMultiMapMethods {
    object map {
      def unapply(d: Def[_]): Option[Rep[MMultiMap[K, V]] forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MMultiMapElem[_, _, _]] && method.getName == "map" =>
          Some(receiver).asInstanceOf[Option[Rep[MMultiMap[K, V]] forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[MMultiMap[K, V]] forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object union {
      def unapply(d: Def[_]): Option[(Rep[MMultiMap[K, V]], Rep[MMultiMap[K,V]]) forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(that, _*), _) if receiver.elem.isInstanceOf[MMultiMapElem[_, _, _]] && method.getName == "union" =>
          Some((receiver, that)).asInstanceOf[Option[(Rep[MMultiMap[K, V]], Rep[MMultiMap[K,V]]) forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[MMultiMap[K, V]], Rep[MMultiMap[K,V]]) forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object isEmpty {
      def unapply(d: Def[_]): Option[Rep[MMultiMap[K, V]] forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MMultiMapElem[_, _, _]] && method.getName == "isEmpty" =>
          Some(receiver).asInstanceOf[Option[Rep[MMultiMap[K, V]] forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[MMultiMap[K, V]] forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object contains {
      def unapply(d: Def[_]): Option[(Rep[MMultiMap[K, V]], Rep[K]) forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(key, _*), _) if receiver.elem.isInstanceOf[MMultiMapElem[_, _, _]] && method.getName == "contains" =>
          Some((receiver, key)).asInstanceOf[Option[(Rep[MMultiMap[K, V]], Rep[K]) forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[MMultiMap[K, V]], Rep[K]) forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[MMultiMap[K, V]], Rep[K]) forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(key, _*), _) if receiver.elem.isInstanceOf[MMultiMapElem[_, _, _]] && method.getName == "apply" =>
          Some((receiver, key)).asInstanceOf[Option[(Rep[MMultiMap[K, V]], Rep[K]) forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[MMultiMap[K, V]], Rep[K]) forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object applyIfBy {
      def unapply(d: Def[_]): Option[(Rep[MMultiMap[K, V]], Rep[K], Rep[ArrayBuffer[V] => T], Rep[Unit => T]) forSome {type K; type V; type T}] = d match {
        case MethodCall(receiver, method, Seq(key, exists, otherwise, _*), _) if receiver.elem.isInstanceOf[MMultiMapElem[_, _, _]] && method.getName == "applyIfBy" =>
          Some((receiver, key, exists, otherwise)).asInstanceOf[Option[(Rep[MMultiMap[K, V]], Rep[K], Rep[ArrayBuffer[V] => T], Rep[Unit => T]) forSome {type K; type V; type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[MMultiMap[K, V]], Rep[K], Rep[ArrayBuffer[V] => T], Rep[Unit => T]) forSome {type K; type V; type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object add {
      def unapply(d: Def[_]): Option[(Rep[MMultiMap[K, V]], Rep[K], Rep[V]) forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(key, value, _*), _) if receiver.elem.isInstanceOf[MMultiMapElem[_, _, _]] && method.getName == "add" =>
          Some((receiver, key, value)).asInstanceOf[Option[(Rep[MMultiMap[K, V]], Rep[K], Rep[V]) forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[MMultiMap[K, V]], Rep[K], Rep[V]) forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object addAll {
      def unapply(d: Def[_]): Option[(Rep[MMultiMap[K, V]], Rep[K], Rep[ArrayBuffer[V]]) forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(key, value, _*), _) if receiver.elem.isInstanceOf[MMultiMapElem[_, _, _]] && method.getName == "addAll" =>
          Some((receiver, key, value)).asInstanceOf[Option[(Rep[MMultiMap[K, V]], Rep[K], Rep[ArrayBuffer[V]]) forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[MMultiMap[K, V]], Rep[K], Rep[ArrayBuffer[V]]) forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduceBy {
      def unapply(d: Def[_]): Option[(Rep[MMultiMap[K, V]], Rep[Array[V] => T]) forSome {type K; type V; type T}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[MMultiMapElem[_, _, _]] && method.getName == "reduceBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[MMultiMap[K, V]], Rep[Array[V] => T]) forSome {type K; type V; type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[MMultiMap[K, V]], Rep[Array[V] => T]) forSome {type K; type V; type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object keys {
      def unapply(d: Def[_]): Option[Rep[MMultiMap[K, V]] forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MMultiMapElem[_, _, _]] && method.getName == "keys" =>
          Some(receiver).asInstanceOf[Option[Rep[MMultiMap[K, V]] forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[MMultiMap[K, V]] forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object values {
      def unapply(d: Def[_]): Option[Rep[MMultiMap[K, V]] forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MMultiMapElem[_, _, _]] && method.getName == "values" =>
          Some(receiver).asInstanceOf[Option[Rep[MMultiMap[K, V]] forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[MMultiMap[K, V]] forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object toArray {
      def unapply(d: Def[_]): Option[Rep[MMultiMap[K, V]] forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MMultiMapElem[_, _, _]] && method.getName == "toArray" =>
          Some(receiver).asInstanceOf[Option[Rep[MMultiMap[K, V]] forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[MMultiMap[K, V]] forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object size {
      def unapply(d: Def[_]): Option[Rep[MMultiMap[K, V]] forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MMultiMapElem[_, _, _]] && method.getName == "size" =>
          Some(receiver).asInstanceOf[Option[Rep[MMultiMap[K, V]] forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[MMultiMap[K, V]] forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object toMap {
      def unapply(d: Def[_]): Option[Rep[MMultiMap[K, V]] forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MMultiMapElem[_, _, _]] && method.getName == "toMap" =>
          Some(receiver).asInstanceOf[Option[Rep[MMultiMap[K, V]] forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[MMultiMap[K, V]] forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object MMultiMapCompanionMethods {
    object empty {
      def unapply(d: Def[_]): Option[Unit forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MMultiMapCompanionElem] && method.getName == "empty" =>
          Some(()).asInstanceOf[Option[Unit forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Unit forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object make {
      def unapply(d: Def[_]): Option[Rep[String] forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(name, _*), _) if receiver.elem.isInstanceOf[MMultiMapCompanionElem] && method.getName == "make" =>
          Some(name).asInstanceOf[Option[Rep[String] forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[String] forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromArray {
      def unapply(d: Def[_]): Option[Arr[(K, V)] forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(a, _*), _) if receiver.elem.isInstanceOf[MMultiMapCompanionElem] && method.getName == "fromArray" =>
          Some(a).asInstanceOf[Option[Arr[(K, V)] forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Arr[(K, V)] forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromMap {
      def unapply(d: Def[_]): Option[Rep[MMap[K,ArrayBuffer[V]]] forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(map, _*), _) if receiver.elem.isInstanceOf[MMultiMapCompanionElem] && method.getName == "fromMap" =>
          Some(map).asInstanceOf[Option[Rep[MMap[K,ArrayBuffer[V]]] forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[MMap[K,ArrayBuffer[V]]] forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}
