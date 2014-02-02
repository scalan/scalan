package scalan

import scala.language.{higherKinds, implicitConversions}
import scala.annotation.unchecked.uncheckedVariance


trait Base { self: Scalan =>
  type |[+A,+B] = Either[A,B]
  type L[A] = (A|Unit)
  type R[A] = (Unit|A)
  type Rep[+A]
  type Elem[A]
  type IntRep = Rep[Int]
  type BoolRep = Rep[Boolean]
  type UnitRep = Rep[Unit]
  type ByteRep = Rep[Byte]
  type ShortRep = Rep[Short]
  type CharRep = Rep[Char]
  type LongRep = Rep[Long]
  type FloatRep = Rep[Float]
  type DoubleRep = Rep[Double]
  type FoldStep[A,B,Acc] = ((A,Acc)) => (L[B], Acc)
  type FS[A,B,Acc] = Rep[FoldStep[A,B,Acc]]
  type :=>[-A,+B] = PartialFunction[A,B]

  class StagingException[A](message: String, val syms: List[Rep[_]]) extends RuntimeException(message)

  def ???(): Nothing = ???("not implemented")
  def ???(msg: String): Nothing = sys.error(msg)
  def ???(msg: String, syms: Rep[_]*): Nothing = throw new StagingException(msg + " " + syms.mkString, syms.toList)

  def !!! = sys.error("should not be called")
  def !!!(msg: String): Nothing = sys.error(msg)
  def !!!(msg: String, syms: Rep[_]*): Nothing = throw new StagingException(msg + " " + syms.mkString, syms.toList)

  val isDebug: Boolean = false

  implicit class RepForSomeExtension(x: Rep[_]) {
    def asRep[T]: Rep[T] = x.asInstanceOf[Rep[T]]
  }

  trait UnOpBase[TArg,R] {
    def arg: Rep[TArg]
    def copyWith(arg: Rep[TArg]): Rep[R]
    def opName: String
    def objType: Elem[R]
    override def toString = "%s(%s)".format(this.getClass.getSimpleName, arg)
  }

  trait BinOpBase[TArg,R] {
    def lhs: Rep[TArg]
    def rhs: Rep[TArg]
    def copyWith(l: Rep[TArg], r:Rep[TArg]): Rep[R]
    def opName: String
    def objType: Elem[R]
    override def toString = this.getClass.getSimpleName + "(" + lhs + ", " + rhs + ")"
  }

  trait ReifiableObject[+A] {     // implemented as Def[A] in staged context
    def objType: Elem[A @uncheckedVariance]
    def thisSymbol: Rep[A] = !!!("should not be called")
    def name = getClass.getSimpleName
    def mirror(f: Transformer): Rep[_] = !!!("don't know how to mirror " + this)
    def decompose: Option[Rep[_]] = None
    def isScalarOp: Boolean = true
  }

  trait ReifiableObject1 {     // implemented as Def[A] in staged context
    type ThisType
    def thisSymbol: Rep[ThisType] = !!!("should not be called")
    def name = getClass.getSimpleName
    def mirror(f: Transformer): Rep[_] = !!!("don't know how to mirror " + this)
    def decompose: Option[Rep[_]] = None
    def isScalarOp: Boolean = true
  }
  type ReifiableObjectAux[+T] = ReifiableObject1 { type ThisType <: T }

  abstract class Transformer {
    def apply[A](x: Rep[A]): Rep[A]
    def isDefinedAt(x: Rep[_]): Boolean
    def domain: Set[Rep[_]]
    def apply[A](xs: Seq[Rep[A]]): Seq[Rep[A]] = xs map (e => apply(e))
    def apply[X,A](f: X=>Rep[A]): X=>Rep[A] = (z:X) => apply(f(z))
    def apply[X,Y,A](f: (X,Y)=>Rep[A]): (X,Y)=>Rep[A] = (z1:X,z2:Y) => apply(f(z1,z2))
  }

  trait TransformerOps[Ctx <: Transformer] {
    def empty: Ctx
    def add(ctx: Ctx, kv: (Rep[_], Rep[_])): Ctx
    def merge(ctx1: Ctx, ctx2: Ctx): Ctx = ctx2.domain.foldLeft(ctx1)((t,s) => add(t, (s, ctx2(s))))
  }

  implicit class TransformerEx[Ctx <: Transformer](self: Ctx)(implicit ops: TransformerOps[Ctx]) {
    def +(kv: (Rep[_], Rep[_])) = ops.add(self, kv)
    def ++(kvs: Map[Rep[_], Rep[_]]) = kvs.foldLeft(self)((ctx, kv) => ops.add(ctx,kv))
    def merge(other: Ctx): Ctx = ops.merge(self, other)
  }

  trait CanBeReified[C[_]] {
    def resolve[A](sym: Rep[C[A]]): C[A]
  }

  implicit def reifyObject[A:LElem](obj: ReifiableObject[A]): Rep[A]
  //def reifyObject1[A:Elem](obj: ReifiableObjectAux[A]): Rep[obj.ThisType]
  def toRep[A](x: A)(implicit eA: Elem[A]): Rep[A] = !!!(s"Don't know how to create Rep for: $x") //= element[A].toRep(x)
  implicit def liftToRep[A:Elem](x: A) = toRep(x)
}
