package scalan.compilation.lms.common

import scala.reflect.SourceContext
import scala.virtualization.lms.common._

trait EitherOps extends Base {

  def make_left[A: Manifest, B: Manifest](l: Rep[A])(implicit pos: SourceContext): Rep[Either[A, B]]
  def make_right[A: Manifest, B: Manifest](r: Rep[B])(implicit pos: SourceContext): Rep[Either[A, B]]
  def make_isLeft[A: Manifest, B: Manifest](sum: Rep[Either[A, B]])(implicit pos: SourceContext): Rep[Boolean]
  def make_isRight[A: Manifest, B: Manifest](sum: Rep[Either[A, B]])(implicit pos: SourceContext): Rep[Boolean]
  def make_fold[A: Manifest, B: Manifest, R: Manifest](sum: Rep[Either[A, B]], left: Rep[A => R], right: Rep[B => R])(implicit pos: SourceContext): Rep[R]
  def make_map[A: Manifest, B: Manifest, C: Manifest, D:Manifest](sum: Rep[Either[A, B]], left: Rep[A => C], right: Rep[B => D])(implicit pos: SourceContext): Rep[Either[C, D]]
}

trait EitherOpsExp extends EitherOps with BaseExp {

  abstract class EitherDef[A, B, R](implicit val mA: Manifest[A], val mB: Manifest[B], val mR: Manifest[R]) extends Def[R]

  /**
   * Class should have Manifest in argument list because otherwise `EitherLeft[Unit, Int](()) == EitherLeft[Unit, Double](())`
   * which will cause caching of the first one in `globalDefsCache`.
   */
  case class EitherLeft[A: Manifest, B: Manifest](a: Exp[A], override val mA: Manifest[A], override val mB: Manifest[B]) extends EitherDef[A, B, Either[A, B]]
  case class EitherRight[A: Manifest, B: Manifest](b: Exp[B], override val mA: Manifest[A], override val mB: Manifest[B]) extends EitherDef[A, B, Either[A, B]]
  case class EitherIsLeft[A: Manifest, B: Manifest](sum: Rep[Either[A, B]]) extends EitherDef[A, B, Boolean]
  case class EitherIsRight[A: Manifest, B: Manifest](sum: Rep[Either[A, B]]) extends EitherDef[A, B, Boolean]
  case class EitherFold[A: Manifest, B: Manifest, R: Manifest](sum: Rep[Either[A, B]], left: Rep[A => R], right: Rep[B => R]) extends EitherDef[A, B, R]
  case class EitherMap[A: Manifest, B: Manifest, C: Manifest, D:Manifest](
    sum: Rep[Either[A, B]], left: Rep[A => C], right: Rep[B => D])
    extends EitherDef[A, B, Either[C,D]] {
    def mC = manifest[C]
    def mD = manifest[D]
  }

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

  def make_map[A, B, C, D](sum: Rep[Either[A, B]], left: Rep[A => C], right: Rep[B => D])(implicit mA: Manifest[A], mB: Manifest[B], mC: Manifest[C], mD: Manifest[D], pos: SourceContext): Rep[Either[C, D]] =
    EitherMap(sum, left, right)

  override def mirror[A:Manifest](e: Def[A], t: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case EitherLeft(a, mA, mB) => make_left(t(a))(mA, mB, pos)
    case EitherRight(b, mA, mB) => make_right(t(b))(mA, mB, pos)
    case e @ EitherIsLeft(s) => make_isLeft(t(s))(e.mA, e.mB, pos)
    case e @ EitherIsRight(s) => make_isRight(t(s))(e.mA, e.mB, pos)
    case e @ EitherFold(s, l, r) => make_fold(t(s), t(l), t(r))(e.mA, e.mB, mtype(e.mR), pos)
    case e @ EitherMap(s, l, r) => make_map(t(s), t(l), t(r))(e.mA, e.mB, e.mC, e.mD, pos)
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
    case EitherMap(sum, l, r) => emitValDef(sym, src"$sum.fold(a => Left($l(a)), b => Right($r(b)))")
    case _ => super.emitNode(sym, rhs)
  }
}
