package scalan.compilation.lms.common

import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scalan.compilation.lms.cxx.sharedptr.CxxShptrCodegen

trait EitherOps extends Base {

  def make_left[A: Manifest, B: Manifest](l: Rep[A])(implicit pos: SourceContext): Rep[Either[A, B]]
  def make_right[A: Manifest, B: Manifest](r: Rep[B])(implicit pos: SourceContext): Rep[Either[A, B]]
  def make_isLeft[A: Manifest, B: Manifest](sum: Rep[Either[A, B]])(implicit pos: SourceContext): Rep[Boolean]
  def make_isRight[A: Manifest, B: Manifest](sum: Rep[Either[A, B]])(implicit pos: SourceContext): Rep[Boolean]
  def make_fold[A: Manifest, B: Manifest, R: Manifest](sum: Rep[Either[A, B]], left: Rep[A => R], right: Rep[B => R])(implicit pos: SourceContext): Rep[R]
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
  case class EitherGetLeft[A: Manifest, B: Manifest](sum: Rep[Either[A,B]]) extends Def[A]
  case class EitherGetRight[A: Manifest, B: Manifest](sum: Rep[Either[A,B]]) extends Def[B]

  def make_left[A: Manifest, B: Manifest](l: Rep[A])(implicit pos: SourceContext): Rep[Either[A, B]] =
    EitherLeft[A, B](l, manifest[A], manifest[B])

  def make_right[A: Manifest, B: Manifest](r: Rep[B])(implicit pos: SourceContext): Rep[Either[A, B]] =
    EitherRight[A, B](r, manifest[A], manifest[B])

  def make_isLeft[A: Manifest, B: Manifest](sum: Rep[Either[A, B]])(implicit pos: SourceContext): Rep[Boolean] =
    EitherIsLeft(sum)

  def make_isRight[A: Manifest, B: Manifest](sum: Rep[Either[A, B]])(implicit pos: SourceContext): Rep[Boolean] =
    EitherIsRight(sum)

  def make_fold[A, B, R](sum: Rep[Either[A, B]], left: Rep[A => R], right: Rep[B => R])(implicit mA: Manifest[A], mB: Manifest[B], mR: Manifest[R], pos: SourceContext): Rep[R] = {
    //EitherFold(sum, left, right)
    val x = get_left(sum)(mA,mB)
    val l = reifyEffects(left(x))
    val x1 = get_right(sum)(mA,mB)
    val r = reifyEffects(right(x1))

    ifThenElse( make_isLeft(sum), l, r )
  }

  def get_left[A: Manifest, B: Manifest](sum: Rep[Either[A,B]]): Rep[A] = EitherGetLeft(sum)
  def get_right[A: Manifest, B: Manifest](sum: Rep[Either[A,B]]): Rep[B] = EitherGetRight(sum)

  override def mirror[A:Manifest](e: Def[A], t: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case EitherGetLeft(Def(EitherLeft(a, _, _))) => t(a)
    case EitherGetLeft(sum) => get_left(t(sum))
    case EitherGetRight(Def(EitherRight(b, _, _))) => t(b)
    case EitherGetRight(sum) => get_right(t(sum))
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
    case EitherGetLeft(sum) => emitValDef(sym, src"$sum.left")
    case EitherGetRight(sum) => emitValDef(sym, src"$sum.right")
    case EitherLeft(a, _, _) => emitValDef(sym, src"Left($a)")
    case EitherRight(b, _, _) => emitValDef(sym, src"Right($b)")
    case EitherIsLeft(sum) => emitValDef(sym, src"$sum.isLeft")
    case EitherIsRight(sum) => emitValDef(sym, src"$sum.isRight")
    case EitherFold(sum, l, r) => emitValDef(sym, src"$sum.fold($l, $r)")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CxxShptrGenEitherOps extends CxxShptrCodegen {
  val IR: EitherOpsExp
  import IR._

  private def remapUnit[A: Manifest](m: Manifest[A]): String = {
    if( m.runtimeClass == classOf[Unit] )
      "boost::blank"
    else
      remap(m)
  }

  override def remap[A](m: Manifest[A]): String = {
    m.runtimeClass match {
      case c if c == classOf[scala.util.Either[_,_]] =>
        val mA = m.typeArguments(0)
        val mB = m.typeArguments(1)
        s"boost::variant<${remapUnit(mA)},${remapUnit(mB)}>"
      case _ =>
        super.remap(m)
    }
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case EitherLeft(a, _, _) =>
      val construct = if(a.tp.runtimeClass == classOf[Unit]) "" else s"(${quote(a)})"
      stream.println(s"${remap(sym.tp)} ${quote(sym)}${construct};")
    case EitherRight(b, _, _) =>
      val construct = if(b.tp.runtimeClass == classOf[Unit]) "" else s"(${quote(b)})"
      stream.println(s"${remap(sym.tp)} ${quote(sym)}(${quote(b)});")
    case EitherIsLeft(sum) =>
      emitValDef(sym, s"${quote(sum)}.which() == 0")
    case EitherIsRight(sum) =>
      emitValDef(sym, s"${quote(sum)}.which() == 1")
    case EitherFold(sum, l, r) =>
      emitValDef(sym, s"${quote(sum)}.which() == 0 ? ${quote(l)}(boost::get<${remapUnit(l.tp.typeArguments(0))}>(${quote(sum)})) : ${quote(r)}(boost::get<${remapUnit(r.tp.typeArguments(0))}>(${quote(sum)}))")
    case _ =>
      super.emitNode(sym, rhs)
  }
}
