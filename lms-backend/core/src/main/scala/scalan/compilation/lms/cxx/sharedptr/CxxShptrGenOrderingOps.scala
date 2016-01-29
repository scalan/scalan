package scalan.compilation.lms.cxx.sharedptr

import scala.lms.common.CLikeGenOrderingOps

trait CxxShptrGenOrderingOps extends CxxShptrCodegen with CLikeGenOrderingOps {
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case OrderingCompare(a, b) =>
      emitValDef(sym, src"($a < $b) ? -1 : ($a == $b) ? 0 : 1")
    case _ => super.emitNode(sym, rhs)
  }
}