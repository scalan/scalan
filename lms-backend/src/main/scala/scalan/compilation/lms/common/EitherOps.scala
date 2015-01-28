package scalan.compilation.lms.common

import scala.reflect.SourceContext
import scala.virtualization.lms.common._

trait EitherOps extends Base {

  def make_left[A: Manifest, B: Manifest](l: Rep[A])(implicit pos: SourceContext): Rep[Either[A, B]]
  def make_right[A: Manifest, B: Manifest](r: Rep[B])(implicit pos: SourceContext): Rep[Either[A, B]]
  def make_isLeft[A: Manifest, B: Manifest](sum: Rep[Either[A, B]])(implicit pos: SourceContext): Rep[Boolean]
  def make_isRight[A: Manifest, B: Manifest](sum: Rep[Either[A, B]])(implicit pos: SourceContext): Rep[Boolean]
  def make_fold[A: Manifest, B: Manifest, R: Manifest](sum: Rep[Either[A, B]], left: Rep[A => R], right: Rep[B => R])(implicit pos: SourceContext): Rep[R]
}

trait EitherOpsExp extends EitherOps with BaseExp {

  case class EitherLeft[A: Manifest, B: Manifest](a: Exp[A]) extends Def[Either[A, B]]
  case class EitherRight[A: Manifest, B: Manifest](b: Exp[B]) extends Def[Either[A, B]]
  case class EitherIsLeft[A: Manifest, B: Manifest](sum: Rep[Either[A, B]]) extends Def[Boolean]
  case class EitherIsRight[A: Manifest, B: Manifest](sum: Rep[Either[A, B]]) extends Def[Boolean]
  case class EitherFold[A: Manifest, B: Manifest, R: Manifest](sum: Rep[Either[A, B]], left: Rep[A => R], right: Rep[B => R]) extends Def[R]

  def make_left[A: Manifest, B: Manifest](l: Rep[A])(implicit pos: SourceContext): Rep[Either[A, B]] =
    EitherLeft[A, B](l)

  def make_right[A: Manifest, B: Manifest](r: Rep[B])(implicit pos: SourceContext): Rep[Either[A, B]] =
    EitherRight[A, B](r)

  def make_isLeft[A: Manifest, B: Manifest](sum: Rep[Either[A, B]])(implicit pos: SourceContext): Rep[Boolean] =
    EitherIsLeft(sum)

  def make_isRight[A: Manifest, B: Manifest](sum: Rep[Either[A, B]])(implicit pos: SourceContext): Rep[Boolean] =
    EitherIsRight(sum)

  def make_fold[A: Manifest, B: Manifest, R: Manifest](sum: Rep[Either[A, B]], left: Rep[A => R], right: Rep[B => R])(implicit pos: SourceContext): Rep[R] =
    EitherFold(sum, left, right)
}


trait ScalaGenEitherOps extends ScalaGenBase {
  val IR: EitherOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case EitherLeft(a) => emitValDef(sym, src"Left($a)")
    case EitherRight(b) => emitValDef(sym, src"Right($b)")
    case EitherIsLeft(sum) => emitValDef(sym, src"$sum.isLeft")
    case EitherIsRight(sum) => emitValDef(sym, src"$sum.isRight")
    case EitherFold(sum, l, r) => emitValDef(sym, src"$sum.fold($l, $r)")
    case _ => super.emitNode(sym, rhs)
  }
}
