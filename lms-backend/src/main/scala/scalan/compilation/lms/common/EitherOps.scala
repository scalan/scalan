package scalan.compilation.lms.common

import scala.reflect.SourceContext
import scala.virtualization.lms.common._

trait EitherOps extends Base {

  def make_left[A: Manifest, B: Manifest](l: Rep[A])(implicit pos: SourceContext): Rep[Either[A, B]]
  def make_right[A: Manifest, B: Manifest](r: Rep[B])(implicit pos: SourceContext): Rep[Either[A, B]]
}

trait EitherOpsExp extends EitherOps with BaseExp {

  case class EitherLeft[A: Manifest, B: Manifest](a: Exp[A]) extends Def[Either[A, B]]
  case class EitherRight[A: Manifest, B: Manifest](b: Exp[B]) extends Def[Either[A, B]]

  def make_left[A: Manifest, B: Manifest](l: Rep[A])(implicit pos: SourceContext): Rep[Either[A, B]] =
    EitherLeft[A, B](l)

  def make_right[A: Manifest, B: Manifest](r: Rep[B])(implicit pos: SourceContext): Rep[Either[A, B]] =
    EitherRight[A, B](r)
}


trait ScalaGenEitherOps extends ScalaGenBase {
  val IR: EitherOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case EitherLeft(a) => emitValDef(sym, src"Left($a)")
    case EitherRight(b) => emitValDef(sym, src"Right($b)")
    case _ => super.emitNode(sym, rhs)
  }
}
