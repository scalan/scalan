package scalan.collections

import scalan._

trait MultiMaps { self: MultiMapsDsl =>

  trait MMultiMap[K, V] extends Def[MMultiMap[K, V]] {
    implicit def elemKey: Elem[K]
    implicit def elemValue: Elem[V]
    def map: Rep[MMap[K, ArrayBuffer[V]]]
    def union(that: Rep[MMultiMap[K, V]]): Rep[MMultiMap[K, V]]
    def isEmpty: Rep[Boolean] = (size === 0)
    def contains(key: Rep[K]): Rep[Boolean]
    def apply(key: Rep[K]): Rep[ArrayBuffer[V]]
    def applyIfBy[T](key: Rep[K], exists: Rep[ArrayBuffer[V] => T], otherwise: Rep[Unit => T]): Rep[T]
    def add(key: Rep[K], value: Rep[V]): Rep[MMultiMap[K, V]]
    def addAll(key: Rep[K], value: Rep[ArrayBuffer[V]]): Rep[MMultiMap[K, V]]
    def reduceBy[T:Elem](f: Rep[Array[V] => T]): MM[K, T]
    def keys: Arr[K]
    def values: Arr[ArrayBuffer[V]]
    def toArray: Arr[(K,ArrayBuffer[V])]
    def size: Rep[Int]
    def toMap: Rep[MMap[K, ArrayBuffer[V]]]
  }

  def appendMultiMap[K:Elem,V:Elem](map: Rep[MMap[K,ArrayBuffer[V]]], key: Rep[K], value: Rep[V]): Rep[MMap[K,ArrayBuffer[V]]];

  trait MMultiMapCompanion extends TypeFamily2[MMultiMap] {
    def empty[K:Elem,V:Elem]: Rep[MMultiMap[K, V]] = HashMMultiMap.empty[K,V]
    def make[K:Elem,V:Elem](name: Rep[String]): Rep[MMultiMap[K, V]] = HashMMultiMap.make[K,V](name)
    def fromArray[K:Elem,V:Elem](a: Arr[(K,V)]): Rep[MMultiMap[K, V]] = HashMMultiMap.fromArray[K,V](a)
    def fromMap[K:Elem,V:Elem](map: Rep[MMap[K, ArrayBuffer[V]]]): Rep[MMultiMap[K, V]] = HashMMultiMap(map)
  }

  abstract class HashMMultiMap[K,V](val map: Rep[MMap[K, ArrayBuffer[V]]])(implicit val elemKey: Elem[K], val elemValue: Elem[V]) extends MMultiMap[K,V] {
    def union(that: Rep[MMultiMap[K, V]]): Rep[MMultiMap[K, V]] = {      
      HashMMultiMap(map.reduce(that.toMap, (b1:Rep[ArrayBuffer[V]], b2:Rep[ArrayBuffer[V]]) => b1 ++= b2.toArray))
    }

    def toMap = map

    def contains(key: Rep[K]): Rep[Boolean] = map.contains(key)

    def apply(key: Rep[K]): Rep[ArrayBuffer[V]] =
      map.applyIfBy[ArrayBuffer[V]](key, fun { x => x }, fun { _: Rep[Unit] => ArrayBuffer.empty[V] })
//      IF (map.contains(key)) THEN map(key) ELSE ArrayBuffer.empty[V]

    def applyIfBy[T](key: Rep[K], exists: Rep[ArrayBuffer[V] => T], otherwise: Rep[Unit => T]): Rep[T] =
      map.applyIfBy[T](key, exists, otherwise)

    def add(key: Rep[K], value: Rep[V]): Rep[MMultiMap[K, V]] = {
      (appendMultiMap(map, key, value) | self)
//      IF (map.contains(key)) THEN ((map(key) += value) | self) ELSE (map.update(key, ArrayBuffer(value)) | self)
    }
      
    def addAll(key: Rep[K], value: Rep[ArrayBuffer[V]]): Rep[MMultiMap[K, V]] = {
      IF (map.contains(key)) THEN ((map(key) ++= value.toArray) | self) ELSE (map.update(key, value) | self)
    }
      
    def reduceBy[T:Elem](f: Rep[Array[V] => T]): MM[K, T] = {
      map.mapValues(v => f(v.toArray))
    }

    def keys: Arr[K] = map.keys
    def values: Arr[ArrayBuffer[V]] = map.values
    def toArray: Arr[(K,ArrayBuffer[V])] = map.toArray
    def size: Rep[Int] = map.size
  }    

  trait HashMMultiMapCompanion extends ConcreteClass2[HashMMultiMap] with MMultiMapCompanion {
    override def empty[K:Elem,V:Elem]: Rep[MMultiMap[K, V]] = HashMMultiMap(MMap.empty[K, ArrayBuffer[V]])
//    override def empty[K:Elem,V:Elem]: Rep[MultiMap[K, V]] = HashMMultiMap(MMap.create[K, ArrayBuffer[V]](0, fun { i => (element[K].defaultRepValue, ArrayBuffer.empty[V])}))
   override def make[K:Elem,V:Elem](name: Rep[String]): Rep[MMultiMap[K, V]] = HashMMultiMap[K,V](MMap.make[K, ArrayBuffer[V]](name))
   override def fromArray[K:Elem,V:Elem](arr: Arr[(K,V)]): Rep[MMultiMap[K, V]] = {
      // Why this doesn't work?
      //arr.fold(MultiMap.empty[K,V], (map:Rep[MultiMap[K,V]], pair:Rep[(K,V)]) => map.add(pair._1, pair._2))
      HashMMultiMap(arr.mapReduce(p => (p._1, ArrayBuffer(p._2)), (a, b) => a ++= b.toArray))
    }
  }
}

trait MultiMapsDsl extends ScalanDsl with impl.MultiMapsAbs {
  implicit class MultiMapExt[K:Elem,V:Elem](map: Rep[MMultiMap[K,V]]) {
    def reduce[T: Elem](f: Arr[V] => Rep[T]): MM[K, T] = map.reduceBy[T](f)
    def applyIf[T: Elem](key: Rep[K], exists: Rep[ArrayBuffer[V]] => Rep[T], otherwise: () => Rep[T]): Rep[T] =
      map.applyIfBy(key, fun(exists), fun { _: Rep[Unit] => otherwise() })
  }
}

trait MultiMapsDslStd extends ScalanDslStd with impl.MultiMapsStd {
  def appendMultiMap[K: Elem, V: Elem](map: Rep[MMap[K, ArrayBuffer[V]]], key: Rep[K], value: Rep[V]): Rep[MMap[K, ArrayBuffer[V]]] = {
    if (map.contains(key)) {
      map(key) += value
      map
    } else {
      map.update(key, ArrayBuffer(value))
      map
    }
  }
}

trait MultiMapsDslExp extends ScalanDslExp with impl.MultiMapsExp {
  def appendMultiMap[K: Elem, V: Elem](map: Rep[MMap[K, ArrayBuffer[V]]], key: Rep[K], value: Rep[V]): Rep[MMap[K, ArrayBuffer[V]]] =
    AppendMultiMap(map, key, value)

}
