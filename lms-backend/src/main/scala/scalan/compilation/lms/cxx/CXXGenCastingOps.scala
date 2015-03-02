package scalan.compilation.lms.cxx

import scala.virtualization.lms.common.CastingOpsExp

/**
 * Created by zotov on 1/27/15.
 */
trait CXXGenCastingOps extends CXXCodegen {
  val IR: CastingOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = {
    rhs match {
      case RepAsInstanceOf(sy, t1, t2) if t1 != t2 =>
        emitValDef(sym, s"static_cast<${remap(t2)}>(${quote(sy)}) /*${rhs.toString}*/")
      case RepAsInstanceOf(sy, _, _) =>
        emitValDef(sym, quote(sy) + s" /*${rhs.toString}*/")
      case _ => super.emitNode(sym, rhs)
    }
  }
}
