package scalan.compilation.lms.common

import scala.reflect.SourceContext
import scala.virtualization.lms.common.{ScalaGenBase, BaseExp, Base}

trait LstOps extends Base {

  def list_replicate[A: Manifest](len: Rep[Int], x: Rep[A])(implicit pos: SourceContext): Rep[List[A]]
}

trait LstOpsExp extends LstOps with BaseExp {

  case class ListReplicate[A: Manifest](len: Rep[Int], x: Rep[A]) extends Def[List[A]]

  def list_replicate[A: Manifest](len: Rep[Int], x: Rep[A])(implicit pos: SourceContext): Rep[List[A]] =
    ListReplicate(len, x)
}

trait ScalaGenLstOps extends ScalaGenBase {
  val IR: LstOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ListReplicate(len, x) => emitValDef(sym, src"List.fill($len)($x)")
    case _ => super.emitNode(sym, rhs)
  }
}
