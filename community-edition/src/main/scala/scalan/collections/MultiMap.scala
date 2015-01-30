package scalan.collections

import scalan._
import scalan.common.Default

trait MultiMaps extends Base { self: MultiMapsDsl =>

  trait MultiMap[K, V] extends Reifiable[MultiMap[K, V]] {
    implicit def elemKey: Elem[K]
    implicit def elemValue: Elem[V]

    def union(that: Rep[MultiMap[K, V]]): Rep[MultiMap[K, V]]
    def isEmpty: Rep[Boolean] = (size === 0)
    def contains(key: Rep[K]): Rep[Boolean]
    def apply(key: Rep[K]): Rep[ArrayBuffer[V]]
    def applyIf[T:Elem](key: Rep[K], exists:Rep[ArrayBuffer[V]] => Rep[T], otherwise: UnitRep=>Rep[T]): Rep[T]
    def add(key: Rep[K], value: Rep[V]): Rep[MultiMap[K, V]]
    def addAll(key: Rep[K], value: Rep[ArrayBuffer[V]]): Rep[MultiMap[K, V]]
    def reduceBy[T:Elem](f: Rep[Array[V] => T]): PM[K, T]
    def keys: Arr[K]
    def values: Arr[ArrayBuffer[V]]
    def toArray: Arr[(K,ArrayBuffer[V])]
    def size: Rep[Int]
    def toMap: Rep[PMap[K, ArrayBuffer[V]]]
  }

  def appendMultiMap[K:Elem,V:Elem](map: Rep[PMap[K,ArrayBuffer[V]]], key: Rep[K], value: Rep[V]): Rep[PMap[K,ArrayBuffer[V]]];

  implicit def defaultMultiMapElement[K:Elem,V:Elem]: Elem[MultiMap[K,V]] = element[HashMultiMap[K,V]].asElem[MultiMap[K,V]]

  trait MultiMapCompanion extends TypeFamily2[MultiMap] {
    def defaultOf[K:Elem,V:Elem]: Default[Rep[MultiMap[K, V]]] = HashMultiMap.defaultOf[K,V]
    def empty[K:Elem,V:Elem]: Rep[MultiMap[K, V]] = HashMultiMap.empty[K,V]
    def make[K:Elem,V:Elem](name: Rep[String]): Rep[MultiMap[K, V]] = HashMultiMap.make[K,V](name)
    def fromArray[K:Elem,V:Elem](a: Arr[(K,V)]): Rep[MultiMap[K, V]] = HashMultiMap.fromArray[K,V](a)
    def fromMap[K:Elem,V:Elem](map: Rep[PMap[K, ArrayBuffer[V]]]): Rep[MultiMap[K, V]] = HashMultiMap(map)
  }

  abstract class HashMultiMap[K,V](val map: Rep[PMap[K, ArrayBuffer[V]]])(implicit val elemKey: Elem[K], val elemValue: Elem[V]) extends MultiMap[K,V] { 
    def union(that: Rep[MultiMap[K, V]]): Rep[MultiMap[K, V]] = {      
      HashMultiMap(map.reduce(that.toMap, (b1:Rep[ArrayBuffer[V]], b2:Rep[ArrayBuffer[V]]) => b1 ++= b2.toArray))
    }

    def toMap = map

    def contains(key: Rep[K]): Rep[Boolean] = map.contains(key)

    def apply(key: Rep[K]): Rep[ArrayBuffer[V]] = IF (map.contains(key)) THEN map(key) ELSE ArrayBuffer.empty[V]

    def applyIf[T:Elem](key: Rep[K], exists:Rep[ArrayBuffer[V]] => Rep[T], otherwise: UnitRep=>Rep[T]): Rep[T] =
      map.applyIf[T](key, exists, otherwise)

    def add(key: Rep[K], value: Rep[V]): Rep[MultiMap[K, V]] = {
      (appendMultiMap(map, key, value) | self)
//      IF (map.contains(key)) THEN ((map(key) += value) | self) ELSE (map.update(key, ArrayBuffer(value)) | self)
    }
      
    def addAll(key: Rep[K], value: Rep[ArrayBuffer[V]]): Rep[MultiMap[K, V]] = {
      IF (map.contains(key)) THEN ((map(key) ++= value.toArray) | self) ELSE (map.update(key, value) | self)
    }
      
    def reduceBy[T:Elem](f: Rep[Array[V] => T]): PM[K, T] = {
      map.mapValues(v => f(v))
    }

    def keys: Arr[K] = map.keys
    def values: Arr[ArrayBuffer[V]] = map.values
    def toArray: Arr[(K,ArrayBuffer[V])] = map.toArray
    def size: Rep[Int] = map.size
  }    

  trait HashMultiMapCompanion extends ConcreteClass2[HashMultiMap] with MultiMapCompanion {
    override def defaultOf[K:Elem,V:Elem] = Default.defaultVal(HashMultiMap(element[PMap[K, ArrayBuffer[V]]].defaultRepValue))
    override def empty[K:Elem,V:Elem]: Rep[MultiMap[K, V]] = HashMultiMap(PMap.empty[K, ArrayBuffer[V]])
//    override def empty[K:Elem,V:Elem]: Rep[MultiMap[K, V]] = HashMultiMap(PMap.create[K, ArrayBuffer[V]](0, fun { i => (element[K].defaultRepValue, ArrayBuffer.empty[V])}))
   override def make[K:Elem,V:Elem](name: Rep[String]): Rep[MultiMap[K, V]] = HashMultiMap[K,V](PMap.make[K, ArrayBuffer[V]](name))
   override def fromArray[K:Elem,V:Elem](arr: Arr[(K,V)]): Rep[MultiMap[K, V]] = {
      // Why this doesn't work?
      //arr.fold(MultiMap.empty[K,V], (map:Rep[MultiMap[K,V]], pair:Rep[(K,V)]) => map.add(pair._1, pair._2))
      HashMultiMap(arr.mapReduce(p => (p._1, ArrayBuffer(p._2)), (a, b) => a ++= b.toArray))
    }
  }
}

trait MultiMapsDsl extends ScalanDsl with impl.MultiMapsAbs with MultiMaps {
  implicit class MultiMapExt[K:Elem,V:Elem](map: Rep[MultiMap[K,V]]) {
    def reduce[T:Elem](f: Arr[V] => Rep[T]): PM[K, T] = map.reduceBy[T](f)
  }
}

trait MultiMapsDslSeq extends MultiMapsDsl with impl.MultiMapsSeq with ScalanSeq {
  def appendMultiMap[K: Elem, V: Elem](map: Rep[PMap[K, ArrayBuffer[V]]], key: Rep[K], value: Rep[V]): Rep[PMap[K, ArrayBuffer[V]]] = {
    if (map.contains(key)) {
      map(key) += value
      map
    } else {
      map.update(key, ArrayBuffer(value))
      map
    }
  }
}

trait MultiMapsDslExp extends MultiMapsDsl with impl.MultiMapsExp with ScalanExp {
  def appendMultiMap[K: Elem, V: Elem](map: Rep[PMap[K, ArrayBuffer[V]]], key: Rep[K], value: Rep[V]): Rep[PMap[K, ArrayBuffer[V]]] =
    AppendMultiMap(map, key, value)

}
