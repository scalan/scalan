package scalan.compilation.lms.cxx.sharedptr

import scala.lms.common.{BaseGenIterableOps, IterableOpsExp}

trait CxxShptrGenIterableOps extends BaseGenIterableOps with CxxShptrCodegen {
  val IR: IterableOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case IterableToArray(it) =>
        emitValDef(sym, src"scalan::toArray<${remap(it.tp)}>(*$it)")
      case _ => super.emitNode(sym, rhs)
    }
  }
}
