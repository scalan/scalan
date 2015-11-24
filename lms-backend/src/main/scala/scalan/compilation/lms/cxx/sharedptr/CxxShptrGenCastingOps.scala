package scalan.compilation.lms.cxx.sharedptr

import scala.lms.common.CastingOpsExp

trait CxxShptrGenCastingOps extends CxxShptrCodegen {
  val IR: CastingOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = {
    rhs match {
      case RepAsInstanceOf(sy, t1, t2) =>
        val rhs1 = if (t1 != t2) src"static_cast<$t2>($sy)" else quote(sy)
        emitValDef(sym, rhs1)
      case _ => super.emitNode(sym, rhs)
    }
  }
}
