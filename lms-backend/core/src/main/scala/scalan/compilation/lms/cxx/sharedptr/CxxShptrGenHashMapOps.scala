package scalan.compilation.lms.cxx.sharedptr

import scala.lms.common.{CLikeGenEffect, HashMapOpsExp, BaseGenHashMapOps}

trait CxxShptrGenHashMapOps extends BaseGenHashMapOps with CLikeGenEffect {
  val IR: HashMapOpsExp

  import IR._

  override def remap[A](m: Manifest[A]) : String = {
    m match {
      case _ if m.runtimeClass == classOf[java.util.HashMap[_,_]] =>
        src"std::map<${m.typeArguments(0)}>"
      case _ =>
        super.remap(m)
    }
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case m@HashMapNew() => emitValDef(sym, "new java.util.HashMap[" + remap(m.mK) + "," + remap(m.mV) + "]()")
    case HashMapApply(m, k) => emitValDef(sym, quote(m) + ".get(" + quote(k) + ")")
    case HashMapUpdate(m, k, v) => emitValDef(sym, quote(m) + ".put(" + quote(k) + ", " + quote(v) + ")")
    case HashMapContains(m, i) => emitValDef(sym, quote(m) + ".containsKey(" + quote(i) + ")")
    case HashMapSize(m) => emitValDef(sym, quote(m) + ".size")
    //    case HashMapValues(m) => emitValDef(sym, "scala.collection.JavaConverters.collectionAsScalaIterableConverter("+quote(m)+".values).asScala")
    case HashMapClear(m) => emitValDef(sym, quote(m) + ".clear()")
    //    case HashMapKeySet(m) => emitValDef(sym, "scala.collection.JavaConverters.asScalaSetConverter("+quote(m)+".keySet).asScala")
//    case HashMapKeys(m) => emitValDef(sym, "scala.collection.JavaConverters.asScalaSetConverter("+quote(m)+".keySet).asScala.toIterable")
    case _ => super.emitNode(sym, rhs)
  }
}
