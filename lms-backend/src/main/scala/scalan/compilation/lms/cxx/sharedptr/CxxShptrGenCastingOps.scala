package scalan.compilation.lms.cxx.sharedptr

import scala.virtualization.lms.common.CastingOpsExp

trait CxxShptrGenCastingOps extends CxxShptrCodegen {
  val IR: CastingOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = {
    rhs match {
      case RepAsInstanceOf(sy, t1, t2) if t1 != t2 =>
        emitValDef(sym, s"static_cast<${remap(t2)}>(${quote(sy)})")
      case RepAsInstanceOf(sy, _, _) =>
        emitValDef(sym, quote(sy))
      case _ => super.emitNode(sym, rhs)
    }
  }
}
