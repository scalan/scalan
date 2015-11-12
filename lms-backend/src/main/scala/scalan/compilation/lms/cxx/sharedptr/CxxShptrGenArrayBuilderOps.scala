package scalan.compilation.lms.cxx.sharedptr

import scala.collection.mutable
import scala.lms.common.{ArrayBuilderOpsExp, BaseGenArrayBuilderOps, CLikeGenEffect}

trait CxxShptrGenArrayBuilderOps  extends CxxShptrCodegen with BaseGenArrayBuilderOps with CLikeGenEffect {
  val IR: ArrayBuilderOpsExp
  import IR._

  override def remap[A](m: Manifest[A]): String = {
    m.runtimeClass match {
      case c if c == classOf[mutable.ArrayBuilder[_]] =>
        val mA = m.typeArguments(0)
        remap(Manifest.arrayType(mA))
      case _ =>
        super.remap(m)
    }
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case a@ArrayBuilderMake() =>
      emitVarDecl(sym)
    case ArrayBuilderAppend(l, e) =>
      stream.println(s"${quote(l)}->push_back(${quote(e)});")
      emitValDef(sym, "scalan::unit_value")
    case ArrayBuilderClear(l) =>
      stream.println(s"${quote(l)}->clear();")
      emitValDef(sym, "scalan::unit_value")
    case ArrayBuilderResult(x) =>
      emitValDef(sym, s"${quote(x)}")
    case _ =>
      super.emitNode(sym, rhs)
  }
}
