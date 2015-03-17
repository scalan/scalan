package scalan
package compilation
package lms
package cxx

import java.util

import scala.virtualization.lms.common.{HashMapOpsExp, CLikeGenEffect, BaseGenHashMapOps}

trait CXXGenHashMapOps extends CXXCodegen with BaseGenHashMapOps with CLikeGenEffect {
  val IR: HashMapOpsExp
  import IR._

  override def remap[A](m: Manifest[A]): String = {
    m.runtimeClass match {
      case c if c == classOf[util.HashMap[_,_]] =>
        val mK = m.typeArguments(0)
        val mV = m.typeArguments(1)
        if( mV != Manifest.Unit )
          s"std::unordered_map<${remap(mK)},${remap(mV)}>"
        else
          s"std::unordered_map<${remap(mK)},char>"
      case _ =>
        super.remap(m)
    }
  }


  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case m@HashMapNew() =>
      emitVarDecl(sym)
    case HashMapApply(m,k) =>
      emitValDef(sym, src"$m[$k] /*${sym.toString} = ${rhs.toString}*/")
    case HashMapUpdate(m,k,v) =>
      if( v.tp != Manifest.Unit)
        emitUnitStatement(src"$m[$k] = $v /*${sym.toString} = ${rhs.toString}*/")
      else
        emitUnitStatement(src"$m[$k] = char(-1) /*${sym.toString} = ${rhs.toString}*/")
    case HashMapContains(m,k) =>
      emitValDef(sym, src"($m.find($k) != $m.end())")
    case HashMapSize(m) =>
      emitValDef(sym, quote(m) + ".size()")
//    case HashMapValues(m) =>
//      emitValDef(sym, "scala.collection.JavaConverters.collectionAsScalaIterableConverter("+quote(m)+".values).asScala")
    case HashMapClear(m) =>
      emitUnitStatement(src"$m.clear()")
//    case HashMapKeySet(m) => emitValDef(sym, "scala.collection.JavaConverters.asScalaSetConverter("+quote(m)+".keySet).asScala")
//    case HashMapKeys(m) => emitValDef(sym, "scala.collection.JavaConverters.asScalaSetConverter("+quote(m)+".keySet).asScala.toIterable")
    case _ =>
      super.emitNode(sym, rhs)
  }
}