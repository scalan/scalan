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

  abstract class EitherDef[A: Manifest, B: Manifest, R: Manifest] extends Def[R] {
    val mA = manifest[A]
    val mB = manifest[B]
    val mR = manifest[R]
  }

  /**
   * Class should have Manifest in argument list because otherwise `EitherLeft[Unit, Int](()) == EitherLeft[Unit, Double](())`
   * which will cause caching of the first one in `globalDefsCache`.
   */
  case class EitherLeft[A: Manifest, B: Manifest](a: Exp[A], mA: Manifest[A], mB: Manifest[B]) extends Def[Either[A, B]]
  case class EitherRight[A: Manifest, B: Manifest](b: Exp[B], mA: Manifest[A], mB: Manifest[B]) extends Def[Either[A, B]]
  case class EitherIsLeft[A: Manifest, B: Manifest](sum: Rep[Either[A, B]]) extends EitherDef[A, B, Boolean]
  case class EitherIsRight[A: Manifest, B: Manifest](sum: Rep[Either[A, B]]) extends EitherDef[A, B, Boolean]
  case class EitherFold[A: Manifest, B: Manifest, R: Manifest](sum: Rep[Either[A, B]], left: Rep[A => R], right: Rep[B => R]) extends EitherDef[A, B, R]

  def make_left[A: Manifest, B: Manifest](l: Rep[A])(implicit pos: SourceContext): Rep[Either[A, B]] =
    EitherLeft[A, B](l, manifest[A], manifest[B])

  def make_right[A: Manifest, B: Manifest](r: Rep[B])(implicit pos: SourceContext): Rep[Either[A, B]] =
    EitherRight[A, B](r, manifest[A], manifest[B])

  def make_isLeft[A: Manifest, B: Manifest](sum: Rep[Either[A, B]])(implicit pos: SourceContext): Rep[Boolean] =
    EitherIsLeft(sum)

  def make_isRight[A: Manifest, B: Manifest](sum: Rep[Either[A, B]])(implicit pos: SourceContext): Rep[Boolean] =
    EitherIsRight(sum)

  def make_fold[A, B, R](sum: Rep[Either[A, B]], left: Rep[A => R], right: Rep[B => R])(implicit mA: Manifest[A], mB: Manifest[B], mR: Manifest[R], pos: SourceContext): Rep[R] =
    EitherFold(sum, left, right)

  override def mirror[A:Manifest](e: Def[A], t: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case EitherLeft(a, mA, mB) => make_left(t(a))(mA, mB, pos)
    case EitherRight(b, mA, mB) => make_right(t(b))(mA, mB, pos)
    case e @ EitherIsLeft(s) => make_isLeft(t(s))(e.mA, e.mB, pos)
    case e @ EitherIsRight(s) => make_isRight(t(s))(e.mA, e.mB, pos)
    case e @ EitherFold(s, l, r) => make_fold(t(s), t(l), t(r))(e.mA, e.mB, mtype(e.mR), pos)
    case _ => super.mirror(e, t)
  }).asInstanceOf[Exp[A]]
}

trait ScalaGenEitherOps extends ScalaGenBase {
  val IR: EitherOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case EitherLeft(a, _, _) => emitValDef(sym, src"Left($a)")
    case EitherRight(b, _, _) => emitValDef(sym, src"Right($b)")
    case EitherIsLeft(sum) => emitValDef(sym, src"$sum.isLeft")
    case EitherIsRight(sum) => emitValDef(sym, src"$sum.isRight")
    case EitherFold(sum, l, r) => emitValDef(sym, src"$sum.fold($l, $r)")
    case _ => super.emitNode(sym, rhs)
  }
}
