package scalan.compilation.lms.pointers

import scala.lms.common._
import scala.lms.internal.GenerationFailedException
import scalan.compilation.lms.cxx.sharedptr.CxxShptrCodegen

trait PointerLmsOps extends Base {
  trait Scalar[A]
  trait Pointer[A]
}

trait PointerLmsOpsExp
  extends PointerLmsOps
  with LoopsFatExp
  with IfThenElseExp
  with EqualExpBridge
  with FunctionsExp
  with BaseExp {

  // note: m: Manifest[A] need to distinct objects CreateScalar[Int](0) and CreateScalar[Double](0.0), when 0 == 0.0
  case class CreateScalar[A](source: Exp[A], m: Manifest[A]) extends Def[Scalar[A]]
  def create_scalar[A: Manifest](source: Exp[A]): Exp[Scalar[A]] = CreateScalar(source, manifest[A])

  // note: m: Manifest[A] need to distinct objects NullPtr[Int] and NullPtr[Double] to be correct type for result pointer
  case class NullPtr[A](m: Manifest[A]) extends Def[Pointer[A]]
  def null_ptr[A: Manifest]: Exp[Pointer[A]] = NullPtr(manifest[A])

  case class ScalarPtr[A: Manifest](xScalar: Exp[Scalar[A]]) extends Def[Pointer[A]]
  def scalar_ptr[A: Manifest](xScalar: Exp[Scalar[A]]): Exp[Pointer[A]] = ScalarPtr(xScalar)

  case class ArrayPtr[A: Manifest](xs: Rep[Array[A]]) extends Def[Pointer[A]]
  def array_ptr[A: Manifest](xs: Exp[Array[A]]): Exp[Pointer[A]] = ArrayPtr(xs)
}

trait CxxShptrGenPointer extends CxxShptrCodegen {
  val IR: PointerLmsOpsExp
  import IR._

  override def remap[A](m: Manifest[A]): String = {
    m.runtimeClass match {
      case c if c == classOf[Scalar[_]] => remap(m.typeArguments(0))
      case c if c == classOf[Pointer[_]] => s"${remap(m.typeArguments(0))}*"
      case _ =>
        super.remap(m)
    }
  }

  override protected def doNotWrap(m: Manifest[_]) = m.isOneOf(classOf[Scalar[_]], classOf[Pointer[_]]) ||
    super.doNotWrap(m)

  override protected def emitConstruct(sym: Sym[Any], shptrTp: Manifest[_], args: String*): Unit = {
    shptrTp.runtimeClass match {
      case c if c == classOf[PointerLmsOps#Pointer[_]] =>
        args.length match {
          case 0 => stream.println(src"$shptrTp $sym = nullptr;")
          case 1 => stream.println(src"$shptrTp $sym = ${args(0)};")
          case _ => throw new GenerationFailedException(s"CxxShptrCodegen.emitConstruct(): cannot initialize pointer from several arguments")
        }
      case _ =>
        super.emitConstruct(sym, shptrTp, args: _*)
    }
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case CreateScalar(x, _) =>
      emitValDef(sym, quote(x))

    case NullPtr(_) =>
      emitValDef(sym, "nullptr")

    case ScalarPtr(xScalar) =>
      emitValDef(sym, src"std::addressof($xScalar)")

    case ArrayPtr(xs) =>
      emitValDef(sym, src"std::addressof((*$xs)[0])")

    case _ =>
      super.emitNode(sym, rhs)
  }
}
