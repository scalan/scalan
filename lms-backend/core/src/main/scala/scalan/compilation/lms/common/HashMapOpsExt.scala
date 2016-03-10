package scalan.compilation.lms.common

import java.util.HashMap

import scala.lms.common._
import scala.reflect.SourceContext
import scalan.compilation.lms.cxx.sharedptr.CxxShptrGenHashMapOps

trait HashMapOpsExt extends HashMapOps with IterableOps {
  def hashmap_keys_array[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]])(implicit pos: SourceContext): Rep[Array[K]] = {
    iterable_toarray( hashmap_keys(m) )
  }
}

trait HashMapOpsExpExt extends HashMapOpsExp {
  case class HashMapKeysArray[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]]) extends HashMapDef[K,V,Array[K]]

  def hashmap_keys_array[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]])(implicit pos: SourceContext): Rep[Array[K]] = {
    HashMapKeysArray(m)
  }
}

trait CxxShptrGenHashMapOpsExt extends CxxShptrGenHashMapOps {
  val IR: HashMapOpsExpExt

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case HashMapKeysArray(m) => emitValDef(sym, "scala.collection.JavaConverters.asScalaSetConverter("+quote(m)+".keySet).asScala.toIterable")
    case _ => super.emitNode(sym, rhs)
  }
}