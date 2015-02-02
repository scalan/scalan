package scalan.collections
package impl

import scalan._
import scalan.common.Default
import scala.reflect.runtime.universe._
import scalan.common.Default

// Abs -----------------------------------
trait MultiMapsAbs extends Scalan with MultiMaps
{ self: MultiMapsDsl =>
  // single proxy for each type family
  implicit def proxyMultiMap[K, V](p: Rep[MultiMap[K, V]]): MultiMap[K, V] =
    proxyOps[MultiMap[K, V]](p)



  abstract class MultiMapElem[K, V, From, To <: MultiMap[K, V]](iso: Iso[From, To]) extends ViewElem[From, To]()(iso)

  trait MultiMapCompanionElem extends CompanionElem[MultiMapCompanionAbs]
  implicit lazy val MultiMapCompanionElem: MultiMapCompanionElem = new MultiMapCompanionElem {
    lazy val tag = typeTag[MultiMapCompanionAbs]
    protected def getDefaultRep = MultiMap
  }

  abstract class MultiMapCompanionAbs extends CompanionBase[MultiMapCompanionAbs] with MultiMapCompanion {
    override def toString = "MultiMap"
    
  }
  def MultiMap: Rep[MultiMapCompanionAbs]
  implicit def proxyMultiMapCompanion(p: Rep[MultiMapCompanion]): MultiMapCompanion = {
    proxyOps[MultiMapCompanion](p)
  }

  //default wrapper implementation
  
  // elem for concrete class
  class HashMultiMapElem[K, V](iso: Iso[HashMultiMapData[K, V], HashMultiMap[K, V]]) extends MultiMapElem[K, V, HashMultiMapData[K, V], HashMultiMap[K, V]](iso)

  // state representation type
  type HashMultiMapData[K, V] = PMap[K,ArrayBuffer[V]]

  // 3) Iso for concrete class
  class HashMultiMapIso[K, V](implicit elemKey: Elem[K], elemValue: Elem[V])
    extends Iso[HashMultiMapData[K, V], HashMultiMap[K, V]] {
    override def from(p: Rep[HashMultiMap[K, V]]) =
      unmkHashMultiMap(p) match {
        case Some((map)) => map
        case None => !!!
      }
    override def to(p: Rep[PMap[K,ArrayBuffer[V]]]) = {
      val map = p
      HashMultiMap(map)
    }
    lazy val tag = {
      weakTypeTag[HashMultiMap[K, V]]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[HashMultiMap[K, V]]](HashMultiMap(element[PMap[K,ArrayBuffer[V]]].defaultRepValue))
    lazy val eTo = new HashMultiMapElem[K, V](this)
  }
  // 4) constructor and deconstructor
  abstract class HashMultiMapCompanionAbs extends CompanionBase[HashMultiMapCompanionAbs] with HashMultiMapCompanion {
    override def toString = "HashMultiMap"

    def apply[K, V](map: Rep[PMap[K,ArrayBuffer[V]]])(implicit elemKey: Elem[K], elemValue: Elem[V]): Rep[HashMultiMap[K, V]] =
      mkHashMultiMap(map)
    def unapply[K:Elem, V:Elem](p: Rep[HashMultiMap[K, V]]) = unmkHashMultiMap(p)
  }
  def HashMultiMap: Rep[HashMultiMapCompanionAbs]
  implicit def proxyHashMultiMapCompanion(p: Rep[HashMultiMapCompanionAbs]): HashMultiMapCompanionAbs = {
    proxyOps[HashMultiMapCompanionAbs](p)
  }

  class HashMultiMapCompanionElem extends CompanionElem[HashMultiMapCompanionAbs] {
    lazy val tag = typeTag[HashMultiMapCompanionAbs]
    protected def getDefaultRep = HashMultiMap
  }
  implicit lazy val HashMultiMapCompanionElem: HashMultiMapCompanionElem = new HashMultiMapCompanionElem

  implicit def proxyHashMultiMap[K, V](p: Rep[HashMultiMap[K, V]]): HashMultiMap[K, V] =
    proxyOps[HashMultiMap[K, V]](p)

  implicit class ExtendedHashMultiMap[K, V](p: Rep[HashMultiMap[K, V]])(implicit elemKey: Elem[K], elemValue: Elem[V]) {
    def toData: Rep[HashMultiMapData[K, V]] = isoHashMultiMap(elemKey, elemValue).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoHashMultiMap[K, V](implicit elemKey: Elem[K], elemValue: Elem[V]): Iso[HashMultiMapData[K, V], HashMultiMap[K, V]] =
    new HashMultiMapIso[K, V]

  // 6) smart constructor and deconstructor
  def mkHashMultiMap[K, V](map: Rep[PMap[K,ArrayBuffer[V]]])(implicit elemKey: Elem[K], elemValue: Elem[V]): Rep[HashMultiMap[K, V]]
  def unmkHashMultiMap[K:Elem, V:Elem](p: Rep[HashMultiMap[K, V]]): Option[(Rep[PMap[K,ArrayBuffer[V]]])]
}

// Seq -----------------------------------
trait MultiMapsSeq extends MultiMapsAbs with MultiMapsDsl with ScalanSeq
{ self: MultiMapsDslSeq =>
  lazy val MultiMap: Rep[MultiMapCompanionAbs] = new MultiMapCompanionAbs with UserTypeSeq[MultiMapCompanionAbs, MultiMapCompanionAbs] {
    lazy val selfType = element[MultiMapCompanionAbs]
    
  }

  

  

  case class SeqHashMultiMap[K, V]
      (override val map: Rep[PMap[K,ArrayBuffer[V]]])
      (implicit elemKey: Elem[K], elemValue: Elem[V])
    extends HashMultiMap[K, V](map)
        with UserTypeSeq[MultiMap[K,V], HashMultiMap[K, V]] {
    lazy val selfType = element[HashMultiMap[K, V]].asInstanceOf[Elem[MultiMap[K,V]]]
    
  }
  lazy val HashMultiMap = new HashMultiMapCompanionAbs with UserTypeSeq[HashMultiMapCompanionAbs, HashMultiMapCompanionAbs] {
    lazy val selfType = element[HashMultiMapCompanionAbs]
  }

  def mkHashMultiMap[K, V]
      (map: Rep[PMap[K,ArrayBuffer[V]]])(implicit elemKey: Elem[K], elemValue: Elem[V]) =
      new SeqHashMultiMap[K, V](map)
  def unmkHashMultiMap[K:Elem, V:Elem](p: Rep[HashMultiMap[K, V]]) =
    Some((p.map))
}

// Exp -----------------------------------
trait MultiMapsExp extends MultiMapsAbs with MultiMapsDsl with ScalanExp
{ self: MultiMapsDslExp =>
  lazy val MultiMap: Rep[MultiMapCompanionAbs] = new MultiMapCompanionAbs with UserTypeDef[MultiMapCompanionAbs, MultiMapCompanionAbs] {
    lazy val selfType = element[MultiMapCompanionAbs]
    override def mirror(t: Transformer) = this
  }



  case class ExpHashMultiMap[K, V]
      (override val map: Rep[PMap[K,ArrayBuffer[V]]])
      (implicit elemKey: Elem[K], elemValue: Elem[V])
    extends HashMultiMap[K, V](map) with UserTypeDef[MultiMap[K,V], HashMultiMap[K, V]] {
    lazy val selfType = element[HashMultiMap[K, V]].asInstanceOf[Elem[MultiMap[K,V]]]
    override def mirror(t: Transformer) = ExpHashMultiMap[K, V](t(map))
  }

  lazy val HashMultiMap: Rep[HashMultiMapCompanionAbs] = new HashMultiMapCompanionAbs with UserTypeDef[HashMultiMapCompanionAbs, HashMultiMapCompanionAbs] {
    lazy val selfType = element[HashMultiMapCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object HashMultiMapMethods {
    object union {
      def unapply(d: Def[_]): Option[(Rep[HashMultiMap[K, V]], Rep[MultiMap[K,V]]) forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(that, _*)) if receiver.elem.isInstanceOf[HashMultiMapElem[_, _]] && method.getName == "union" =>
          Some((receiver, that)).asInstanceOf[Option[(Rep[HashMultiMap[K, V]], Rep[MultiMap[K,V]]) forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[HashMultiMap[K, V]], Rep[MultiMap[K,V]]) forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object toMap {
      def unapply(d: Def[_]): Option[Rep[HashMultiMap[K, V]] forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, _) if receiver.elem.isInstanceOf[HashMultiMapElem[_, _]] && method.getName == "toMap" =>
          Some(receiver).asInstanceOf[Option[Rep[HashMultiMap[K, V]] forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[HashMultiMap[K, V]] forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object contains {
      def unapply(d: Def[_]): Option[(Rep[HashMultiMap[K, V]], Rep[K]) forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(key, _*)) if receiver.elem.isInstanceOf[HashMultiMapElem[_, _]] && method.getName == "contains" =>
          Some((receiver, key)).asInstanceOf[Option[(Rep[HashMultiMap[K, V]], Rep[K]) forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[HashMultiMap[K, V]], Rep[K]) forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[HashMultiMap[K, V]], Rep[K]) forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(key, _*)) if receiver.elem.isInstanceOf[HashMultiMapElem[_, _]] && method.getName == "apply" =>
          Some((receiver, key)).asInstanceOf[Option[(Rep[HashMultiMap[K, V]], Rep[K]) forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[HashMultiMap[K, V]], Rep[K]) forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object applyIfBy {
      def unapply(d: Def[_]): Option[(Rep[HashMultiMap[K, V]], Rep[K], Rep[ArrayBuffer[V] => T], Rep[Unit => T]) forSome {type K; type V; type T}] = d match {
        case MethodCall(receiver, method, Seq(key, exists, otherwise, _*)) if receiver.elem.isInstanceOf[HashMultiMapElem[_, _]] && method.getName == "applyIfBy" =>
          Some((receiver, key, exists, otherwise)).asInstanceOf[Option[(Rep[HashMultiMap[K, V]], Rep[K], Rep[ArrayBuffer[V] => T], Rep[Unit => T]) forSome {type K; type V; type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[HashMultiMap[K, V]], Rep[K], Rep[ArrayBuffer[V] => T], Rep[Unit => T]) forSome {type K; type V; type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object add {
      def unapply(d: Def[_]): Option[(Rep[HashMultiMap[K, V]], Rep[K], Rep[V]) forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(key, value, _*)) if receiver.elem.isInstanceOf[HashMultiMapElem[_, _]] && method.getName == "add" =>
          Some((receiver, key, value)).asInstanceOf[Option[(Rep[HashMultiMap[K, V]], Rep[K], Rep[V]) forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[HashMultiMap[K, V]], Rep[K], Rep[V]) forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object addAll {
      def unapply(d: Def[_]): Option[(Rep[HashMultiMap[K, V]], Rep[K], Rep[ArrayBuffer[V]]) forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(key, value, _*)) if receiver.elem.isInstanceOf[HashMultiMapElem[_, _]] && method.getName == "addAll" =>
          Some((receiver, key, value)).asInstanceOf[Option[(Rep[HashMultiMap[K, V]], Rep[K], Rep[ArrayBuffer[V]]) forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[HashMultiMap[K, V]], Rep[K], Rep[ArrayBuffer[V]]) forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `reduceBy`: Method's return type PM[K,T] is not a Rep

    // WARNING: Cannot generate matcher for method `keys`: Method's return type Arr[K] is not a Rep

    // WARNING: Cannot generate matcher for method `values`: Method's return type Arr[ArrayBuffer[V]] is not a Rep

    // WARNING: Cannot generate matcher for method `toArray`: Method's return type Arr[(K,ArrayBuffer[V])] is not a Rep

    object size {
      def unapply(d: Def[_]): Option[Rep[HashMultiMap[K, V]] forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, _) if receiver.elem.isInstanceOf[HashMultiMapElem[_, _]] && method.getName == "size" =>
          Some(receiver).asInstanceOf[Option[Rep[HashMultiMap[K, V]] forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[HashMultiMap[K, V]] forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object HashMultiMapCompanionMethods {
    object defaultOf {
      def unapply(d: Def[_]): Option[Unit forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, _) if receiver.elem.isInstanceOf[HashMultiMapCompanionElem] && method.getName == "defaultOf" =>
          Some(()).asInstanceOf[Option[Unit forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Unit forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object empty {
      def unapply(d: Def[_]): Option[Unit forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, _) if receiver.elem.isInstanceOf[HashMultiMapCompanionElem] && method.getName == "empty" =>
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
        case MethodCall(receiver, method, Seq(name, _*)) if receiver.elem.isInstanceOf[HashMultiMapCompanionElem] && method.getName == "make" =>
          Some(name).asInstanceOf[Option[Rep[String] forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[String] forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromArray {
      def unapply(d: Def[_]): Option[Arr[(K,V)] forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(arr, _*)) if receiver.elem.isInstanceOf[HashMultiMapCompanionElem] && method.getName == "fromArray" =>
          Some(arr).asInstanceOf[Option[Arr[(K,V)] forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Arr[(K,V)] forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkHashMultiMap[K, V]
    (map: Rep[PMap[K,ArrayBuffer[V]]])(implicit elemKey: Elem[K], elemValue: Elem[V]) =
    new ExpHashMultiMap[K, V](map)
  def unmkHashMultiMap[K:Elem, V:Elem](p: Rep[HashMultiMap[K, V]]) =
    Some((p.map))

  object MultiMapMethods {
    object union {
      def unapply(d: Def[_]): Option[(Rep[MultiMap[K, V]], Rep[MultiMap[K,V]]) forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(that, _*)) if receiver.elem.isInstanceOf[MultiMapElem[_, _, _, _]] && method.getName == "union" =>
          Some((receiver, that)).asInstanceOf[Option[(Rep[MultiMap[K, V]], Rep[MultiMap[K,V]]) forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[MultiMap[K, V]], Rep[MultiMap[K,V]]) forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object isEmpty {
      def unapply(d: Def[_]): Option[Rep[MultiMap[K, V]] forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, _) if receiver.elem.isInstanceOf[MultiMapElem[_, _, _, _]] && method.getName == "isEmpty" =>
          Some(receiver).asInstanceOf[Option[Rep[MultiMap[K, V]] forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[MultiMap[K, V]] forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object contains {
      def unapply(d: Def[_]): Option[(Rep[MultiMap[K, V]], Rep[K]) forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(key, _*)) if receiver.elem.isInstanceOf[MultiMapElem[_, _, _, _]] && method.getName == "contains" =>
          Some((receiver, key)).asInstanceOf[Option[(Rep[MultiMap[K, V]], Rep[K]) forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[MultiMap[K, V]], Rep[K]) forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[MultiMap[K, V]], Rep[K]) forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(key, _*)) if receiver.elem.isInstanceOf[MultiMapElem[_, _, _, _]] && method.getName == "apply" =>
          Some((receiver, key)).asInstanceOf[Option[(Rep[MultiMap[K, V]], Rep[K]) forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[MultiMap[K, V]], Rep[K]) forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object applyIfBy {
      def unapply(d: Def[_]): Option[(Rep[MultiMap[K, V]], Rep[K], Rep[ArrayBuffer[V] => T], Rep[Unit => T]) forSome {type K; type V; type T}] = d match {
        case MethodCall(receiver, method, Seq(key, exists, otherwise, _*)) if receiver.elem.isInstanceOf[MultiMapElem[_, _, _, _]] && method.getName == "applyIfBy" =>
          Some((receiver, key, exists, otherwise)).asInstanceOf[Option[(Rep[MultiMap[K, V]], Rep[K], Rep[ArrayBuffer[V] => T], Rep[Unit => T]) forSome {type K; type V; type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[MultiMap[K, V]], Rep[K], Rep[ArrayBuffer[V] => T], Rep[Unit => T]) forSome {type K; type V; type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object add {
      def unapply(d: Def[_]): Option[(Rep[MultiMap[K, V]], Rep[K], Rep[V]) forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(key, value, _*)) if receiver.elem.isInstanceOf[MultiMapElem[_, _, _, _]] && method.getName == "add" =>
          Some((receiver, key, value)).asInstanceOf[Option[(Rep[MultiMap[K, V]], Rep[K], Rep[V]) forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[MultiMap[K, V]], Rep[K], Rep[V]) forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object addAll {
      def unapply(d: Def[_]): Option[(Rep[MultiMap[K, V]], Rep[K], Rep[ArrayBuffer[V]]) forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(key, value, _*)) if receiver.elem.isInstanceOf[MultiMapElem[_, _, _, _]] && method.getName == "addAll" =>
          Some((receiver, key, value)).asInstanceOf[Option[(Rep[MultiMap[K, V]], Rep[K], Rep[ArrayBuffer[V]]) forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[MultiMap[K, V]], Rep[K], Rep[ArrayBuffer[V]]) forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `reduceBy`: Method's return type PM[K,T] is not a Rep

    // WARNING: Cannot generate matcher for method `keys`: Method's return type Arr[K] is not a Rep

    // WARNING: Cannot generate matcher for method `values`: Method's return type Arr[ArrayBuffer[V]] is not a Rep

    // WARNING: Cannot generate matcher for method `toArray`: Method's return type Arr[(K,ArrayBuffer[V])] is not a Rep

    object size {
      def unapply(d: Def[_]): Option[Rep[MultiMap[K, V]] forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, _) if receiver.elem.isInstanceOf[MultiMapElem[_, _, _, _]] && method.getName == "size" =>
          Some(receiver).asInstanceOf[Option[Rep[MultiMap[K, V]] forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[MultiMap[K, V]] forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object toMap {
      def unapply(d: Def[_]): Option[Rep[MultiMap[K, V]] forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, _) if receiver.elem.isInstanceOf[MultiMapElem[_, _, _, _]] && method.getName == "toMap" =>
          Some(receiver).asInstanceOf[Option[Rep[MultiMap[K, V]] forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[MultiMap[K, V]] forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object MultiMapCompanionMethods {
    // WARNING: Cannot generate matcher for method `defaultOf`: Method's return type Default[Rep[MultiMap[K,V]]] is not a Rep

    object empty {
      def unapply(d: Def[_]): Option[Unit forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, _) if receiver.elem.isInstanceOf[MultiMapCompanionElem] && method.getName == "empty" =>
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
        case MethodCall(receiver, method, Seq(name, _*)) if receiver.elem.isInstanceOf[MultiMapCompanionElem] && method.getName == "make" =>
          Some(name).asInstanceOf[Option[Rep[String] forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[String] forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromArray {
      def unapply(d: Def[_]): Option[Arr[(K,V)] forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(a, _*)) if receiver.elem.isInstanceOf[MultiMapCompanionElem] && method.getName == "fromArray" =>
          Some(a).asInstanceOf[Option[Arr[(K,V)] forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Arr[(K,V)] forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromMap {
      def unapply(d: Def[_]): Option[Rep[PMap[K,ArrayBuffer[V]]] forSome {type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(map, _*)) if receiver.elem.isInstanceOf[MultiMapCompanionElem] && method.getName == "fromMap" =>
          Some(map).asInstanceOf[Option[Rep[PMap[K,ArrayBuffer[V]]] forSome {type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[PMap[K,ArrayBuffer[V]]] forSome {type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}
