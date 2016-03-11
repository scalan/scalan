package scalan.compilation.lms.cxx.sharedptr

import scala.lms.common.{CLikeGenEffect, HashMapOpsExp, BaseGenHashMapOps}

trait CxxShptrGenHashMapOps extends CLikeGenEffect with CxxShptrCodegen {
  val IR: HashMapOpsExp

  import IR._

  headerFiles ++= Seq( "unordered_map", "boost/functional/hash.hpp" )

  override def remap[A](m: Manifest[A]) : String = {
    m match {
      case _ if m.runtimeClass == classOf[java.util.HashMap[_,_]] =>
        src"std::unordered_map<${m.typeArguments(0)},${m.typeArguments(1)},boost::hash<${m.typeArguments(0)}>>"
      case _ =>
        super.remap(m)
    }
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case m@HashMapNew() =>
      emitConstruct(sym)
    case HashMapApply(m, k) =>
      emitValDef(sym, src"(*$m)[$k]")
    case HashMapUpdate(m, k, v) =>
      stream.println(src"(*$m)[$k] = $v;")
      emitValDef(sym, src"scalan::unit_value")
    case HashMapContains(m, i) =>
      emitValDef(sym, src"($m->end() != $m->find($i))")
    case HashMapSize(m) =>
      emitValDef(sym, src"$m->size()")
    case HashMapClear(m) =>
      emitValDef(sym, src"$m->clear()")
//    case HashMapValues(m) => emitValDef(sym, "scala.collection.JavaConverters.collectionAsScalaIterableConverter("+quote(m)+".values).asScala")
//    case HashMapKeySet(m) => emitValDef(sym, "scala.collection.JavaConverters.asScalaSetConverter("+quote(m)+".keySet).asScala")
//    case HashMapKeys(m) => emitValDef(sym, "scala.collection.JavaConverters.asScalaSetConverter("+quote(m)+".keySet).asScala.toIterable")
    case _ => super.emitNode(sym, rhs)
  }
}
