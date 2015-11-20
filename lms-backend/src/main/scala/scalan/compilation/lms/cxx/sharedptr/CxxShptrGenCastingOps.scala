package scalan.compilation.lms.cxx.sharedptr

import scala.lms.common.CastingOpsExp

trait CxxShptrGenCastingOps extends CxxShptrCodegen {
  val IR: CastingOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = {
    rhs match {
      case RepAsInstanceOf(sy, t1, t2) if t1 != t2 =>
        emitValDef(sym, src"static_cast<$t2>($sy)")
      case RepAsInstanceOf(sy, _, _) =>
        emitValDef(sym, quote(sy))
      case _ => super.emitNode(sym, rhs)
    }
  }
}
