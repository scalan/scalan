package scalan.compilation.lms.common

import java.util.HashMap

import scala.lms.common._
import scala.reflect.SourceContext

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

  override def mirrorDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = {
    (e match {
      case HashMapKeysArray(m) =>
        HashMapKeysArray(f(m))
      case _ =>
        super.mirrorDef(e,f)
    }).asInstanceOf[Def[A]]
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] =
    (e match {
      case Reflect(HashMapKeysArray(m), u, es) => reflectMirrored(Reflect(HashMapKeysArray(f(m)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case _ =>
        super.mirror(e,f)
    }).asInstanceOf[Exp[A]]

}

