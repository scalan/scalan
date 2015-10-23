package scalan.collections

import scalan._
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait MultiMapsAbs extends MultiMaps with scalan.Scalan {
  self: ScalanCommunityDsl =>

  // single proxy for each type family
  implicit def proxyMMultiMap[K, V](p: Rep[MMultiMap[K, V]]): MMultiMap[K, V] = {
    proxyOps[MMultiMap[K, V]](p)(scala.reflect.classTag[MMultiMap[K, V]])
  }

  // familyElem
  class MMultiMapElem[K, V, To <: MMultiMap[K, V]](implicit _elemKey: Elem[K], _elemValue: Elem[V])
    extends EntityElem[To] {
    def elemKey = _elemKey
    def elemValue = _elemValue
    lazy val parent: Option[Elem[_]] = None
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("K" -> Left(elemKey), "V" -> Left(elemValue))
    }
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagK = elemKey.tag
      implicit val tagV = elemValue.tag
      weakTypeTag[MMultiMap[K, V]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[MMultiMap[K, V]] => convertMMultiMap(x) }
      tryConvert(element[MMultiMap[K, V]], this, x, conv)
    }

    def convertMMultiMap(x: Rep[MMultiMap[K, V]]): Rep[To] = {
      x.selfType1 match {
        case _: MMultiMapElem[_, _, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have MMultiMapElem[_, _, _], but got $e")
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def mMultiMapElement[K, V](implicit elemKey: Elem[K], elemValue: Elem[V]): Elem[MMultiMap[K, V]] =
    cachedElem[MMultiMapElem[K, V, MMultiMap[K, V]]](elemKey, elemValue)

  implicit case object MMultiMapCompanionElem extends CompanionElem[MMultiMapCompanionAbs] {
    lazy val tag = weakTypeTag[MMultiMapCompanionAbs]
    protected def getDefaultRep = MMultiMap
  }

  abstract class MMultiMapCompanionAbs extends CompanionDef[MMultiMapCompanionAbs] with MMultiMapCompanion {
    def selfType = MMultiMapCompanionElem
    override def toString = "MMultiMap"
  }
  def MMultiMap: Rep[MMultiMapCompanionAbs]
  implicit def proxyMMultiMapCompanion(p: Rep[MMultiMapCompanion]): MMultiMapCompanion =
    proxyOps[MMultiMapCompanion](p)

  abstract class AbsHashMMultiMap[K, V]
      (map: Rep[MMap[K, ArrayBuffer[V]]])(implicit elemKey: Elem[K], elemValue: Elem[V])
    extends HashMMultiMap[K, V](map) with Def[HashMMultiMap[K, V]] {
    lazy val selfType = element[HashMMultiMap[K, V]]
  }
  // elem for concrete class
  class HashMMultiMapElem[K, V](val iso: Iso[HashMMultiMapData[K, V], HashMMultiMap[K, V]])(implicit elemKey: Elem[K], elemValue: Elem[V])
    extends MMultiMapElem[K, V, HashMMultiMap[K, V]]
    with ConcreteElem[HashMMultiMapData[K, V], HashMMultiMap[K, V]] {
    override lazy val parent: Option[Elem[_]] = Some(mMultiMapElement(element[K], element[V]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("K" -> Left(elemKey), "V" -> Left(elemValue))
    }

    override def convertMMultiMap(x: Rep[MMultiMap[K, V]]) = HashMMultiMap(x.map)
    override def getDefaultRep = HashMMultiMap(element[MMap[K, ArrayBuffer[V]]].defaultRepValue)
    override lazy val tag = {
      implicit val tagK = elemKey.tag
      implicit val tagV = elemValue.tag
      weakTypeTag[HashMMultiMap[K, V]]
    }
  }

  // state representation type
  type HashMMultiMapData[K, V] = MMap[K, ArrayBuffer[V]]

  // 3) Iso for concrete class
  class HashMMultiMapIso[K, V](implicit elemKey: Elem[K], elemValue: Elem[V])
    extends Iso[HashMMultiMapData[K, V], HashMMultiMap[K, V]] {
    override def from(p: Rep[HashMMultiMap[K, V]]) =
      p.map
    override def to(p: Rep[MMap[K, ArrayBuffer[V]]]) = {
      val map = p
      HashMMultiMap(map)
    }
    lazy val eTo = new HashMMultiMapElem[K, V](this)
  }
  // 4) constructor and deconstructor
  class HashMMultiMapCompanionAbs extends CompanionDef[HashMMultiMapCompanionAbs] with HashMMultiMapCompanion {
    def selfType = HashMMultiMapCompanionElem
    override def toString = "HashMMultiMap"

    def apply[K, V](map: Rep[MMap[K, ArrayBuffer[V]]])(implicit elemKey: Elem[K], elemValue: Elem[V]): Rep[HashMMultiMap[K, V]] =
      mkHashMMultiMap(map)
  }
  object HashMMultiMapMatcher {
    def unapply[K, V](p: Rep[MMultiMap[K, V]]) = unmkHashMMultiMap(p)
  }
  lazy val HashMMultiMap: Rep[HashMMultiMapCompanionAbs] = new HashMMultiMapCompanionAbs
  implicit def proxyHashMMultiMapCompanion(p: Rep[HashMMultiMapCompanionAbs]): HashMMultiMapCompanionAbs = {
    proxyOps[HashMMultiMapCompanionAbs](p)
  }

  implicit case object HashMMultiMapCompanionElem extends CompanionElem[HashMMultiMapCompanionAbs] {
    lazy val tag = weakTypeTag[HashMMultiMapCompanionAbs]
    protected def getDefaultRep = HashMMultiMap
  }

  implicit def proxyHashMMultiMap[K, V](p: Rep[HashMMultiMap[K, V]]): HashMMultiMap[K, V] =
    proxyOps[HashMMultiMap[K, V]](p)

  implicit class ExtendedHashMMultiMap[K, V](p: Rep[HashMMultiMap[K, V]])(implicit elemKey: Elem[K], elemValue: Elem[V]) {
    def toData: Rep[HashMMultiMapData[K, V]] = isoHashMMultiMap(elemKey, elemValue).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoHashMMultiMap[K, V](implicit elemKey: Elem[K], elemValue: Elem[V]): Iso[HashMMultiMapData[K, V], HashMMultiMap[K, V]] =
    cachedIso[HashMMultiMapIso[K, V]](elemKey, elemValue)

  // 6) smart constructor and deconstructor
  def mkHashMMultiMap[K, V](map: Rep[MMap[K, ArrayBuffer[V]]])(implicit elemKey: Elem[K], elemValue: Elem[V]): Rep[HashMMultiMap[K, V]]
  def unmkHashMMultiMap[K, V](p: Rep[MMultiMap[K, V]]): Option[(Rep[MMap[K, ArrayBuffer[V]]])]

  registerModule(scalan.meta.ScalanCodegen.loadModule(MultiMaps_Module.dump))
}

// Seq -----------------------------------
trait MultiMapsSeq extends MultiMapsDsl with scalan.ScalanSeq {
  self: ScalanCommunityDslSeq =>
  lazy val MMultiMap: Rep[MMultiMapCompanionAbs] = new MMultiMapCompanionAbs {
  }

  case class SeqHashMMultiMap[K, V]
      (override val map: Rep[MMap[K, ArrayBuffer[V]]])(implicit elemKey: Elem[K], elemValue: Elem[V])
    extends AbsHashMMultiMap[K, V](map) {
  }

  def mkHashMMultiMap[K, V]
    (map: Rep[MMap[K, ArrayBuffer[V]]])(implicit elemKey: Elem[K], elemValue: Elem[V]): Rep[HashMMultiMap[K, V]] =
    new SeqHashMMultiMap[K, V](map)
  def unmkHashMMultiMap[K, V](p: Rep[MMultiMap[K, V]]) = p match {
    case p: HashMMultiMap[K, V] @unchecked =>
      Some((p.map))
    case _ => None
  }
}

// Exp -----------------------------------
trait MultiMapsExp extends MultiMapsDsl with scalan.ScalanExp {
  self: ScalanCommunityDslExp =>
  lazy val MMultiMap: Rep[MMultiMapCompanionAbs] = new MMultiMapCompanionAbs {
  }

  case class ExpHashMMultiMap[K, V]
      (override val map: Rep[MMap[K, ArrayBuffer[V]]])(implicit elemKey: Elem[K], elemValue: Elem[V])
    extends AbsHashMMultiMap[K, V](map)

  object HashMMultiMapMethods {
    object union {
      def unapply(d: Def[_]): Option[(Rep[HashMMultiMap[K, V]], Rep[MMultiMap[K, V]]) forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(that, _*), _) if receiver.elem.isInstanceOf[HashMMultiMapElem[_, _]] && method.getName == "union" =>
          Some((receiver, that)).asInstanceOf[Option[(Rep[HashMMultiMap[K, V]], Rep[MMultiMap[K, V]]) forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[HashMMultiMap[K, V]], Rep[MMultiMap[K, V]]) forSome {type K; type V}] = exp match {
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
        case MethodCall(receiver, method, _, _) if receiver.elem == HashMMultiMapCompanionElem && method.getName == "empty" =>
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
        case MethodCall(receiver, method, Seq(name, _*), _) if receiver.elem == HashMMultiMapCompanionElem && method.getName == "make" =>
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
        case MethodCall(receiver, method, Seq(arr, _*), _) if receiver.elem == HashMMultiMapCompanionElem && method.getName == "fromArray" =>
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
    (map: Rep[MMap[K, ArrayBuffer[V]]])(implicit elemKey: Elem[K], elemValue: Elem[V]): Rep[HashMMultiMap[K, V]] =
    new ExpHashMMultiMap[K, V](map)
  def unmkHashMMultiMap[K, V](p: Rep[MMultiMap[K, V]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: HashMMultiMapElem[K, V] @unchecked =>
      Some((p.asRep[HashMMultiMap[K, V]].map))
    case _ =>
      None
  }

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
      def unapply(d: Def[_]): Option[(Rep[MMultiMap[K, V]], Rep[MMultiMap[K, V]]) forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(that, _*), _) if receiver.elem.isInstanceOf[MMultiMapElem[_, _, _]] && method.getName == "union" =>
          Some((receiver, that)).asInstanceOf[Option[(Rep[MMultiMap[K, V]], Rep[MMultiMap[K, V]]) forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[MMultiMap[K, V]], Rep[MMultiMap[K, V]]) forSome {type K; type V}] = exp match {
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
        case MethodCall(receiver, method, _, _) if receiver.elem == MMultiMapCompanionElem && method.getName == "empty" =>
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
        case MethodCall(receiver, method, Seq(name, _*), _) if receiver.elem == MMultiMapCompanionElem && method.getName == "make" =>
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
        case MethodCall(receiver, method, Seq(a, _*), _) if receiver.elem == MMultiMapCompanionElem && method.getName == "fromArray" =>
          Some(a).asInstanceOf[Option[Arr[(K, V)] forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Arr[(K, V)] forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromMap {
      def unapply(d: Def[_]): Option[Rep[MMap[K, ArrayBuffer[V]]] forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(map, _*), _) if receiver.elem == MMultiMapCompanionElem && method.getName == "fromMap" =>
          Some(map).asInstanceOf[Option[Rep[MMap[K, ArrayBuffer[V]]] forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[MMap[K, ArrayBuffer[V]]] forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}

object MultiMaps_Module {
  val packageName = "scalan.collections"
  val name = "MultiMaps"
  val dump = "H4sIAAAAAAAAALVWTWwbRRQer+M6a4e0RCioSOAQGRAI7JBLDzlUievwUzuJslGFTIU0Xo+dLbOzm5lxtMuhB45wQ1wR6r03Lpy4ISEOnBAgceZUyqECegLxZvbHu46dViD2MNqZefN+vu97s3v3PioJjl4UNqaYNVwiccPS79tC1q02k44Mu95gTMk1Mvxw9Uu7y3aEgS720IVjLK4J2kNm9NIO/PTdIicdZGJmEyE9LiR6vqMjNG2PUmJLx2NNx3XHEvcpaXYcIbc6aKHvDcITdBsVOuiS7TGbE0msFsVCEBGvLxKVkZPOTT0P9/1JDNZUVTQzVRxx7EhIH2JciuwPiW+FzGOhK9FynNq+r9ICm7Lj+h6XSYgyuDv2Bsl0gWFYQCudW/gUNyHEqGlJ7rARnKz62H4fj8gemCjzBUhYEDo8Cn09L3ZQRZATAOgt16d6JfARQsDApk6iMcGnkeLTUPjULcIdTJ0PsNo84F4QougpFBEKfHDx6iNcJB5Imw3qH920331oVV1DHQ5UKmVd4QVwVJujBk0F4PjN4SfiwRt3rhio0kMVR2z3heTYllnKY7SqmDFP6pxTADEfAVvr89jSUbbBZkoSpu25PmbgKYZyCXiiju1IZazWlmJ25kBflj5JTAuBX0jrXZtTr9ZNC1N6cO/yay/82n7HQEY+hAkuLRA+T5xKZHa7YyqdLvZj/2q8KFHh+gRkNb2hp2owg8lYPiedFJiX7v02+HoD3TRSOOPoj8cguCiJn36ofv/yVQMt9rTedyke9QBR0abE3ectj8keWvROCY92yqeYqreZjJYHZIih6BjnLEBFAEiitbmd6ROF3pbugkICQDUS8p7HSH33oP6n9e2nd5VOOVqKdqJW/du58tfPy0OpJSxR0Y0hB3SL0OB5+Be6j2Kkus05DnfGwyHhU4azuZrmrRIlZ3kueXL9gfPenY+lZqgQ5K+L/f4t6M8tfe65c8hKrq0/ehvG75d//NxAJnDSdyQUWt94zGb7HxsI5VFabsVXthbZZn7ziTexOE47IwNnYrCSbrayWWZAL6dDDdhezTnMnalNaH06k8UzhURi2kiiMgGtXydhKhGl/ZkSyaR6xoupvNzAdEzO83NWQbMwqaUN/Ow8TWgGVg87T9H7V78yUOltVBpCX4oOKvW9MRsk1ML3U5JA7iRrhTy1QCXm2E2p1M8amsCXb4+9mQZni8pU/fqUPIqg5PzKf7sps6LQpo2ZYatKv7vYdWi4OS/87Js4J7c58oyg8Ked1TKRZkP3L7BV49HEJjY0UwwhyVg0kz8AESPD0focQVlxiwM7tx9+tvfKd1/8or9yFXVZwAXM0v+giXyCKcJWIn+AjDtm8HcF/zeZ7KEh1D2iM/8HeR4yhG8KAAA="
}
}

