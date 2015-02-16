package scalan.compilation.lms.common

import scala.reflect.SourceContext
import scala.virtualization.lms.common.{ScalaGenBase, BaseExp, Base}

trait LstOps extends Base {

  def list_replicate[A: Manifest](len: Rep[Int], x: Rep[A])(implicit pos: SourceContext): Rep[List[A]]
  def listCreateAndFill[A: Manifest](length: Rep[Int], elem: Rep[A]): Rep[A]
  def listRangeFrom0[A: Manifest](length: Rep[Int]): Rep[Int]

}

trait LstOpsExp extends LstOps with BaseExp {

  case class ListCreateAndFill[A: Manifest](length: Rep[Int], elem: Rep[A]) extends Def[A] {
    val m = manifest[A]
  }

  case class ListRangeFrom0Lms[A: Manifest](length: Rep[Int]) extends Def[Int]


  def listCreateAndFill[A: Manifest](length: Rep[Int], elem: Rep[A]): Exp[A] = {
    ListCreateAndFill(length, elem)
  }

  def listRangeFrom0[A: Manifest](length: Rep[Int]): Exp[Int] = {
    ListRangeFrom0Lms(length)
  }

  case class ListReplicate[A: Manifest](len: Rep[Int], x: Rep[A]) extends Def[List[A]]

  def list_replicate[A: Manifest](len: Rep[Int], x: Rep[A])(implicit pos: SourceContext): Rep[List[A]] =
    ListReplicate(len, x)

  override def mirror[A: Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case ListCreateAndFill(length, elem) => listCreateAndFill(f(length), f(elem))(mtype(manifest[A]))
    case _ => super.mirror(e, f)
  }

}

trait ScalaGenLstOps extends ScalaGenBase {
  val IR: LstOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ListReplicate(len, x) => emitValDef(sym, src"List.fill($len)($x)")
    case ListCreateAndFill(length, elem) => emitValDef(sym, src"List.fill(${quote(length)})(${quote(elem)})")
    case ListRangeFrom0Lms(length) => emitValDef(sym, src"List((for(i <- 0 to ${quote(length)}) yield i ) : _*)")
    case _ => super.emitNode(sym, rhs)
  }
}
