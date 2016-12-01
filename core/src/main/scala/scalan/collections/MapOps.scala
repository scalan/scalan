package scalan.collections

import scalan._
import scala.reflect.runtime.universe._
import scala.collection.mutable.Map
import scalan.staged.BaseExp
import scalan.util.{Covariant, Invariant};

trait MapOps extends  Base  { self: Scalan =>
  type MM[K, V] = Rep[MMap[K, V]]

  trait MMap[K, V] extends Def[MMap[K,V]] {
    implicit def elemKey: Elem[K]
    implicit def elemValue: Elem[V]
    val selfType = mMapElement(elemKey, elemValue)

    def union(that: MM[K, V]): MM[K, V]
    def difference(that: MM[K, V]): MM[K, V]
    def join[V2:Elem](that: MM[K, V2]): MM[K, (V, V2)]
    def reduce(that: MM[K, V], f:Rep[((V, V))=>V]): MM[K, V]
    def isEmpty: Rep[Boolean] = (size === 0)
    def contains(k: Rep[K]): Rep[Boolean]
    def apply(key: Rep[K]): Rep[V]
    def getOrElse(key: Rep[K], otherwise: => Rep[V]): Rep[V] = getOrElseBy(key, fun { _: Rep[Unit] => otherwise })
    def getOrElseBy(key: Rep[K], otherwise: Rep[Unit => V]): Rep[V]
    def mapValueIfExists[T: Elem](key: Rep[K], exists: Rep[V] => Rep[T], otherwise: () => Rep[T]): Rep[T] =
      mapValueIfExistsBy(key, fun(exists), fun { _: Rep[Unit] => otherwise() })
    def mapValueIfExistsBy[T](key: Rep[K], exists: Rep[V => T], otherwise: Rep[Unit => T]): Rep[T]
    def update(key: Rep[K], value: Rep[V]): Rep[Unit]
    def mapValues[T:Elem](f: Rep[V] => Rep[T]): MM[K, T] = mapValuesBy(fun(f))
    def mapValuesBy[T:Elem](f: Rep[V => T]): MM[K, T]
    def keys: Arr[K]
    def values: Arr[V]
    def toArray: Arr[(K,V)]
    def size: Rep[Int]
  }

  object MMap {
    def empty[K: Elem, V: Elem] = emptyMap[K, V]
    def create[K: Elem, V: Elem](count:Rep[Int], f:Rep[Int=>(K,V)]) = createMap[K, V](count,f)
    def make[K: Elem, V: Elem](name:Rep[String]) = makeMap[K, V](name)
    def fromArray[K: Elem, V: Elem](arr: Arr[(K, V)]) = mapFromArray(arr)
  }

  case class MMapElem[K, V](eKey: Elem[K], eValue: Elem[V]) extends Elem[MMap[K, V]] {
    override def isEntityType = eKey.isEntityType || eValue.isEntityType

    lazy val tag = {
      implicit val kt = eKey.tag
      implicit val vt = eValue.tag
      weakTypeTag[MMap[K, V]]
    }

    protected def getDefaultRep = emptyMap[K, V](eKey, eValue)

    lazy val typeArgs = TypeArgs("K" -> (eKey -> Invariant), "V" -> (eValue -> Covariant))
  }

  implicit def mMapElement[K, V](implicit eKey: Elem[K], eValue: Elem[V]): MMapElem[K, V] = new MMapElem(eKey, eValue)
  def extendMMapElement[K, V](implicit elem: Elem[MMap[K, V]]) = elem.asInstanceOf[MMapElem[K, V]]

  implicit def resolveMMap[K: Elem, V: Elem](map: MM[K, V]): MMap[K, V]

  def emptyMap[K: Elem, V: Elem]: MM[K, V]
  def mapFromArray[K: Elem, V: Elem](arr: Arr[(K, V)]): MM[K, V]
  def createMap[K: Elem, V: Elem](count: Rep[Int], f: Rep[Int=>(K,V)]): MM[K, V]
  def makeMap[K: Elem, V: Elem](name: Rep[String]): MM[K, V]
}

trait MapOpsStd extends MapOps { self: ScalanStd =>
  case class SeqMMap[K, V](val impl: Map[K, V])(implicit val elemKey: Elem[K], val elemValue: Elem[V]) extends MMap[K, V] {
    private def implOf[A,B](that: MMap[A, B]) = that match {
      case m: SeqMMap[A, B] => m.impl
      case _ => !!!(s"$that implements MMap in sequential context but is not SeqMap")
    }

    def union(that: MM[K, V]): MM[K, V]  = impl ++ implOf(that)
    def difference(that: MM[K, V]): MM[K, V]  = impl -- implOf(that).keys
    def join[V2:Elem](that: MM[K, V2]): MM[K, (V, V2)] = {
      val res = Map.empty[K, (V, V2)]
      val left = impl
      val right = implOf(that)
      for ((k,v) <- left) {
        if (right.contains(k)) res.update(k, (v, right(k)))
      }
      res
    }
    def reduce(that: MM[K, V], f:Rep[((V,V))=>V]): MM[K, V] = {
      val res = Map.empty[K, V]
      val left = impl
      val right = implOf(that)
      for ((k,v) <- left) {
        res.update(k, if (right.contains(k)) f((v, right(k))) else v)
      }
      for ((k,v) <- right) {
        if (!left.contains(k)) res.update(k, v)
      }
      res
    }
    def contains(key: Rep[K]): Rep[Boolean] = impl.contains(key)
    def apply(key: Rep[K]): Rep[V] = impl(key)
    def getOrElseBy(key: Rep[K], otherwise: Rep[Unit => V]): Rep[V] = {
      impl.getOrElse(key, otherwise(()))
    }
    def mapValueIfExistsBy[T](key: Rep[K], exists: Rep[V => T], otherwise: Rep[Unit => T]): Rep[T] = {
      if (impl.contains(key)) exists(impl(key)) else otherwise(())
    }
    def update(key: Rep[K], value: Rep[V]): Rep[Unit] = { impl.update(key, value) ; () }
    def keys: Arr[K] = impl.keys.toArray(elemKey.classTag)
    def values: Arr[V] = impl.values.toArray(elemValue.classTag)
    def toArray: Arr[(K, V)] = impl.toArray
    def size: Rep[Int] = impl.size
    def mapValuesBy[T:Elem](f: Rep[V => T]): MM[K, T] = {
      val res = Map.empty[K, T]
      for ((k,v) <- impl) {
         res.update(k, f(v))
      }
      res
    }
  }

  implicit def extendMap[K:Elem,V:Elem](m: Map[K,V]): MMap[K,V] = new SeqMMap(m)

  implicit def resolveMMap[K: Elem, V: Elem](map: MM[K, V]): MMap[K, V] = map

  def emptyMap[K: Elem, V: Elem]: MM[K, V] = Map.empty[K, V]
  def mapFromArray[K: Elem, V: Elem](arr: Arr[(K, V)]): MM[K, V] = Map(arr: _*)
  def createMap[K: Elem, V: Elem](count:Rep[Int], f:Rep[Int=>(K,V)]): MM[K, V] = {
    val map = Map.empty[K, V]
    for (i <- 0 until count) {
      val p = f(i)
      map.update(p._1, p._2)
    }
    map
  }
  def makeMap[K: Elem, V: Elem](name: Rep[String]): MM[K, V] = {
    Map.empty[K, V]
  }
}


trait MapOpsExp extends MapOps with BaseExp { self: ScalanExp =>
  abstract class MMapDef[K, V](implicit val elemKey: Elem[K], val elemValue: Elem[V]) extends MMap[K, V]  {

    def union(that: MM[K, V]): MM[K, V] = MapUnion(this, that)
    def difference(that: MM[K, V]): MM[K, V] = MapDifference(this, that)
    def join[V2:Elem](that: MM[K, V2]): MM[K, (V, V2)] = MapJoin(this, that)
    def reduce(that: MM[K, V], f:Rep[((V,V))=>V]): MM[K, V] = MapReduce(this, that, f)
    def contains(key: Rep[K]): Rep[Boolean] = MapContains(this, key)
    def apply(key: Rep[K]): Rep[V] = MapApply(this, key)
    def getOrElseBy(key: Rep[K], otherwise: Rep[Unit => V]): Rep[V] = {
      MapGetOrElse(self, key, otherwise)
    }
    def mapValueIfExistsBy[T](key: Rep[K], exists:Rep[V => T], otherwise: Rep[Unit => T]): Rep[T] = {
      implicit val eT: Elem[T] = otherwise.elem.eRange
      MapMapValueIfExists(this, key, exists, otherwise)
    }
    def update(key: Rep[K], value: Rep[V]): Rep[Unit] = MapUpdate(this, key, value)
    def size: Rep[Int] = MapSize(this)
    def keys: Arr[K] = MapKeys(this)
    def values: Arr[V] = MapValues(this)
    def toArray: Arr[(K, V)] = MapToArray(this)
    def mapValuesBy[T:Elem](f: Rep[V => T]): MM[K, T] = MapTransformValues[K,V,T](this, f)
  }

//  def emptyMap[K: Elem, V: Elem]: PM[K, V] = EmptyMap[K, V]()
  def emptyMap[K: Elem, V: Elem]: MM[K, V] = MapUsingFunc(0, fun { i => (element[K].defaultRepValue, element[V].defaultRepValue) })
  def mapFromArray[K: Elem, V: Elem](arr: Arr[(K, V)]) = MapFromArray(arr)
  def createMap[K: Elem, V: Elem](count:Rep[Int], f:Rep[Int=>(K,V)]) = MapUsingFunc(count, f)
  def makeMap[K: Elem, V: Elem](name: Rep[String]): MM[K, V] = MakeMap[K,V](name)


  case class AppendMultiMap[K, V](map: Rep[MMap[K, ArrayBuffer[V]]], key: Rep[K], value: Rep[V])(implicit elemKey: Elem[K], val eV: Elem[V])
    extends MMapDef[K,ArrayBuffer[V]]

  case class EmptyMap[K, V]()(implicit eK: Elem[K], eV: Elem[V]) extends MMapDef[K, V] {
    override def equals(other:Any) = {
      other match {
        case that:EmptyMap[_,_] => (this.selfType equals that.selfType)
        case _ => false
      }
    }
  }

  case class MapFromArray[K, V](arr: Arr[(K, V)])(implicit elemKey: Elem[K], elemValue: Elem[V]) extends MMapDef[K, V]

  case class MapUsingFunc[K, V](count: Rep[Int], f:Rep[Int => (K,V)])(implicit elemKey: Elem[K], elemValue: Elem[V]) extends MMapDef[K, V]

  case class MakeMap[K, V](ctx: Rep[String])(implicit elemKey: Elem[K], elemValue: Elem[V]) extends MMapDef[K, V]

  case class MapUnion[K, V](left: MM[K, V], right: MM[K, V])(implicit elemKey: Elem[K], elemValue: Elem[V]) extends MMapDef[K, V]

  case class MapDifference[K, V](left: MM[K, V], right: MM[K, V])(implicit elemKey: Elem[K], elemValue: Elem[V]) extends MMapDef[K, V]

  case class MapJoin[K, V1, V2](left: MM[K, V1], right: MM[K, V2])(implicit elemKey: Elem[K], val elemV1: Elem[V1], val elemV2: Elem[V2]) extends MMapDef[K, (V1, V2)]

  case class MapReduce[K, V](left: MM[K, V], right: MM[K, V], f:Rep[((V, V)) => V])(implicit elemKey: Elem[K], elemValue: Elem[V]) extends MMapDef[K, V]

  case class MapContains[K, V](map: MM[K, V], key: Rep[K])(implicit val eK: Elem[K], val eV: Elem[V]) extends BaseDef[Boolean]

  case class MapApply[K, V](map: MM[K, V], key: Rep[K])(implicit val eK: Elem[K], val eV: Elem[V]) extends BaseDef[V]

  case class MapGetOrElse[K, V]
    (map: MM[K, V], key: Rep[K], otherwise: Rep[Unit=>V])
    (implicit val eK: Elem[K] = extendMMapElement(map.elem).eKey,
              override val selfType: Elem[V] = extendMMapElement(map.elem).eValue) extends BaseDef[V]

  case class MapMapValueIfExists[K, V, T](map: MM[K, V], key: Rep[K], ifExists: Rep[V => T], otherwise: Rep[Unit=>T])(implicit val eK: Elem[K], val eV: Elem[V], selfType: Elem[T]) extends BaseDef[T]

  case class MapUpdate[K, V](map: MM[K, V], key: Rep[K], value: Rep[V])(implicit val eK: Elem[K], val eV: Elem[V]) extends BaseDef[Unit]

  case class MapSize[K, V](map: MM[K, V])(implicit val eK: Elem[K], val eV: Elem[V]) extends BaseDef[Int]

  case class MapToArray[K, V](map: MM[K, V])(implicit val eK: Elem[K], val eV: Elem[V]) extends ArrayDef[(K, V)]

  case class MapKeys[K, V](map: MM[K, V])(implicit val eK: Elem[K], val eV: Elem[V]) extends ArrayDef[K]

  case class MapValues[K, V](map: MM[K, V])(implicit val eK: Elem[K], val eV: Elem[V]) extends ArrayDef[V]

  case class MapTransformValues[K, V, T](map: MM[K, V], f: Rep[V => T])(implicit elemKey: Elem[K], val eV: Elem[V], val eT: Elem[T]) extends MMapDef[K, T]

  case class VarMM[K, V](map: MM[K, V])(implicit elemKey: Elem[K], elemValue: Elem[V]) extends MMapDef[K, V]

  implicit def resolveMMap[K: Elem, V: Elem](sym: MM[K, V]): MMap[K, V] = sym match  {
    case Def(d: MMapDef[_, _]) => d.asInstanceOf[MMap[K, V]]
    case s: Exp[_] => {
      val pmElem = s.elem.asInstanceOf[MMapElem[K, V]]
      VarMM(sym)(pmElem.eKey, pmElem.eValue)
    }
    case _ => ???("cannot resolve MMap", sym)
  }

  override def rewriteDef[T](d: Def[T]) = d match {
    case MapFromArray(arr @ Def(ArrayZip(Def(MapKeys(m1)), Def(MapValues(m2))))) if (m1 == m2) => m1

    // This rule is only valid if the array has distinct values for the keys
//    case MapKeys(Def(d@MapFromArray(arr: Rep[Array[(k,v)]]))) =>  {
//      implicit val eK = d.elemKey.asElem[k]
//      implicit val eV = d.elemValue.asElem[v]
//      implicit val eKV = PairElem(eK,eV)
//      array_map(arr, fun({a: Rep[(k,v)] => a._1})(toLazyElem(eKV), eK))(eK)
//    }
    case MapValues(Def(d@MapFromArray(arr: Rep[Array[(k,v)]]))) => {
      implicit val eK = d.elemKey.asElem[k]
      implicit val eV = d.elemValue.asElem[v]
      implicit val eKV = PairElem(eK,eV)
      array_map(arr, fun({a: Rep[(k,v)] => a._2})(toLazyElem(eKV), eV))(eV)
    }

    /* TODO: uncomment when flatten will be supported
    case MapUnion(Def(m1Def: MapFromArray[k,v]), Def(m2Def: MapFromArray[_,_])) => {
      implicit val eK = m2Def.elemKey.asElem[k]
      implicit val eV = m2Def.elemValue.asElem[v]
      implicit val eKV = PairElem(eK,eV)
      MMap.fromArray(array_concat(m1Def.arr,m2Def.arr.asRep[Array[(k,v)]])(eKV))(eK,eV)
    }*/
    case MapUnion(Def(MapUsingFunc(count, func)), m2@Def(m2Def: MapFromArray[k,v])) => {
      (count == toRep(0)) match {
        case true => m2
        case _ =>
          implicit val eK = m2Def.elemKey.asElem[k]
          implicit val eV = m2Def.elemValue.asElem[v]
          implicit val eKV = PairElem(eK,eV)
          val keys1 = array_map(m2Def.arr, fun({a: Rep[(k,v)]=>a._1})(toLazyElem(eKV), eK))(eK)
          val vals1 = array_map(m2Def.arr, fun({a: Rep[(k,v)]=>a._2})(toLazyElem(eKV), eV))(eV)
          val keys2 = SArray.rangeFrom0(count).map {i: Rep[Int] => func(i)._1}(eK)
          val vals2 = SArray.rangeFrom0(count).map {i: Rep[Int] => func(i)._2}(eV)
          val keys = array_concat(keys1, keys2)(eK)
          val vals = array_concat(vals1, vals2)(eV)
          MMap.fromArray(array_zip(keys,vals))(eK,eV)
      }
    }

    case _ =>
      super.rewriteDef(d)
  }
}

