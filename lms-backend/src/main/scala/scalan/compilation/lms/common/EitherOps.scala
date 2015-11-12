package scalan.compilation.lms.common

import scala.reflect.SourceContext
import scala.lms.common._
import scalan.compilation.lms.cxx.sharedptr.CxxShptrCodegen

trait EitherOps extends Base {

  def either_left[A: Manifest, B: Manifest](l: Rep[A])(implicit pos: SourceContext): Rep[Either[A, B]]
  def either_right[A: Manifest, B: Manifest](r: Rep[B])(implicit pos: SourceContext): Rep[Either[A, B]]
  def either_isLeft[A: Manifest, B: Manifest](sum: Rep[Either[A, B]])(implicit pos: SourceContext): Rep[Boolean]
  def either_isRight[A: Manifest, B: Manifest](sum: Rep[Either[A, B]])(implicit pos: SourceContext): Rep[Boolean]
  def either_fold[A: Manifest, B: Manifest, R: Manifest](sum: Rep[Either[A, B]], left: Rep[A => R], right: Rep[B => R])(implicit pos: SourceContext): Rep[R]
  def either_map[A: Manifest, B: Manifest, C: Manifest, D:Manifest](sum: Rep[Either[A, B]], left: Rep[A => C], right: Rep[B => D])(implicit pos: SourceContext): Rep[Either[C, D]]
}

trait EitherOpsExp extends EitherOps with FunctionsExp with IfThenElseExp with BaseExp {

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
  case class EitherGetLeft[A: Manifest, B: Manifest](sum: Rep[Either[A,B]]) extends Def[A]
  case class EitherGetRight[A: Manifest, B: Manifest](sum: Rep[Either[A,B]]) extends Def[B]

  def either_left[A: Manifest, B: Manifest](l: Rep[A])(implicit pos: SourceContext): Rep[Either[A, B]] =
    EitherLeft[A, B](l, manifest[A], manifest[B])

  def either_right[A: Manifest, B: Manifest](r: Rep[B])(implicit pos: SourceContext): Rep[Either[A, B]] =
    EitherRight[A, B](r, manifest[A], manifest[B])

  def either_isLeft[A: Manifest, B: Manifest](sum: Rep[Either[A, B]])(implicit pos: SourceContext): Rep[Boolean] =
    EitherIsLeft(sum)

  def either_isRight[A: Manifest, B: Manifest](sum: Rep[Either[A, B]])(implicit pos: SourceContext): Rep[Boolean] =
    EitherIsRight(sum)

  def either_fold[A, B, R](sum: Rep[Either[A, B]], left: Rep[A => R], right: Rep[B => R])(implicit mA: Manifest[A], mB: Manifest[B], mR: Manifest[R], pos: SourceContext): Rep[R] = {
    //EitherFold(sum, left, right)
    val x = get_left(sum)(mA,mB)
    val l = reifyEffects(left(x))
    val x1 = get_right(sum)(mA,mB)
    val r = reifyEffects(right(x1))

    ifThenElse( either_isLeft(sum), l, r )
  }

  def either_map[A, B, C, D](sum: Rep[Either[A, B]], left: Rep[A => C], right: Rep[B => D])(implicit mA: Manifest[A], mB: Manifest[B], mC: Manifest[C], mD: Manifest[D], pos: SourceContext): Rep[Either[C, D]] =
    EitherMap(sum, left, right)

  def get_left[A: Manifest, B: Manifest](sum: Rep[Either[A,B]]): Rep[A] = EitherGetLeft(sum)
  def get_right[A: Manifest, B: Manifest](sum: Rep[Either[A,B]]): Rep[B] = EitherGetRight(sum)

  override def mirror[A:Manifest](e: Def[A], t: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case EitherGetLeft(Def(EitherLeft(a, _, _))) => t(a)
    case EitherGetLeft(sum) => get_left(t(sum))
    case EitherGetRight(Def(EitherRight(b, _, _))) => t(b)
    case EitherGetRight(sum) => get_right(t(sum))
    case EitherLeft(a, mA, mB) => either_left(t(a))(mA, mB, pos)
    case EitherRight(b, mA, mB) => either_right(t(b))(mA, mB, pos)
    case e @ EitherIsLeft(s) => either_isLeft(t(s))(e.mA, e.mB, pos)
    case e @ EitherIsRight(s) => either_isRight(t(s))(e.mA, e.mB, pos)
    case e @ EitherFold(s, l, r) => either_fold(t(s), t(l), t(r))(e.mA, e.mB, mtype(e.mR), pos)
    case e @ EitherMap(s, l, r) => either_map(t(s), t(l), t(r))(e.mA, e.mB, e.mC, e.mD, pos)
    case _ => super.mirror(e, t)
  }).asInstanceOf[Exp[A]]
}

trait ScalaGenEitherOps extends ScalaGenBase {
  val IR: EitherOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case EitherGetLeft(sum) => emitValDef(sym, src"$sum.left.get")
    case EitherGetRight(sum) => emitValDef(sym, src"$sum.right.get")
    case EitherLeft(a, _, _) => emitValDef(sym, src"Left($a)")
    case EitherRight(b, _, _) => emitValDef(sym, src"Right($b)")
    case EitherIsLeft(sum) => emitValDef(sym, src"$sum.isLeft")
    case EitherIsRight(sum) => emitValDef(sym, src"$sum.isRight")
    case EitherFold(sum, l, r) => emitValDef(sym, src"$sum.fold($l, $r)")
    case EitherMap(sum, l, r) => emitValDef(sym, src"$sum.fold(a => Left($l(a)), b => Right($r(b)))")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CxxShptrGenEitherOps extends CxxShptrCodegen {
  val IR: EitherOpsExp
  import IR._
  override def wrapSharedPtr:PartialFunction[Manifest[_],Manifest[_]] = {
    case m if m.runtimeClass == classOf[scala.util.Either[_,_]] => m
    case m =>
      super.wrapSharedPtr(m)
  }

  override def remap[A](m: Manifest[A]): String = {
    m.runtimeClass match {
      case c if c == classOf[scala.util.Either[_,_]] =>
        val mA = m.typeArguments(0)
        val mB = m.typeArguments(1)
        s"boost::variant<${remap(mA)},${remap(mB)}>"
      case _ =>
        super.remap(m)
    }
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case EitherGetLeft(sum) =>
      emitValDef(sym, src"boost::get<${remap(sum.tp.typeArguments(0))}>($sum)")
    case EitherGetRight(sum) =>
      emitValDef(sym, src"boost::get<${remap(sum.tp.typeArguments(1))}>($sum)")
    case EitherLeft(a, _, _) =>
      stream.println(s"${remap(sym.tp)} ${quote(sym)}(${quote(a)});")
    case EitherRight(b, _, _) =>
      stream.println(s"${remap(sym.tp)} ${quote(sym)}(${quote(b)});")
    case EitherIsLeft(sum) =>
      emitValDef(sym, s"${quote(sum)}.which() == 0")
    case EitherIsRight(sum) =>
      emitValDef(sym, s"${quote(sum)}.which() == 1")
    case EitherFold(sum, l, r) =>
      emitValDef(sym, s"${quote(sum)}.which() == 0 ? ${quote(l)}(boost::get<${remap(l.tp.typeArguments(0))}>(${quote(sum)})) : ${quote(r)}(boost::get<${remap(r.tp.typeArguments(0))}>(${quote(sum)}))")
    case EitherMap(sum, l, r) =>
      val tpl = remap(sum.tp.typeArguments(0))
      val tpr = remap(sum.tp.typeArguments(1))
      val rtp = remap(sym.tp)
      emitValDef(sym, src"${quote(sum)}.which() == 0 ? $rtp(${quote(l)}(boost::get<$tpl>(${quote(sum)}))) : $rtp(${quote(r)}(boost::get<$tpr>(${quote(sum)})))")
    case _ =>
      super.emitNode(sym, rhs)
  }
}
