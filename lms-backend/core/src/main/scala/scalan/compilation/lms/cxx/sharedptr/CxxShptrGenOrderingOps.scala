package scalan.compilation.lms.cxx.sharedptr

import scala.lms.common.{CLikeGenOrderingOps}

trait CxxShptrGenOrderingOps extends CxxShptrCodegen with CLikeGenOrderingOps {
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case OrderingMax(a,b) =>
      emitValDef(sym, src"std::max($a, $b)")
    case OrderingMin(a,b) =>
      emitValDef(sym, src"std::min($a, $b)")
    case OrderingCompare(a, b) =>
      // since a and b are Syms or Consts, there is no possibility of double evaluation
      emitValDef(sym, src"($a < $b) ? -1 : ($a == $b) ? 0 : 1")
    case _ => super.emitNode(sym, rhs)
  }
}
