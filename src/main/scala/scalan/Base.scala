package scalan

import scala.language.{higherKinds, implicitConversions}



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
  def ???[A](msg: String, sym: Rep[A]): Nothing = throw new StagingException(msg + " " + sym.toString, List(sym))
  def ???[A](msg: String, syms: List[Rep[_]]): Nothing = throw new StagingException(msg + " " + syms.toString, syms)

  def !!! = sys.error("should not be called")
  def !!!(msg: String): Nothing = sys.error(msg)
  def !!![A](msg: String, sym: Rep[A]): Nothing = throw new StagingException(msg + " " + sym.toString, List(sym))

  val isDebug: Boolean = false

  implicit class RepForSomeExtension(x: Rep[_]) {
    def asRep[T]: Rep[T] = x.asInstanceOf[Rep[T]]
  }

  trait UnOpBase[TArg,R] {
    def arg: Rep[TArg]
    def copyWith(arg: Rep[TArg]): Rep[R]
    def opName: String
    def elem: Elem[R]
    override def toString = "%s(%s)".format(this.getClass.getSimpleName, arg)
  }

  trait BinOpBase[TArg,R] {
    def lhs: Rep[TArg]
    def rhs: Rep[TArg]
    def copyWith(l: Rep[TArg], r:Rep[TArg]): Rep[R]
    def opName: String
    def elem: Elem[R]
    override def toString = this.getClass.getSimpleName + "(" + lhs + ", " + rhs + ")"
  }

  trait ReifiableObject[+A] {     // implemented as Def[A] in staged context
    def thisSymbol: Rep[A] = !!!("should not be called")
    def name = getClass.getSimpleName
    def mirror(f: Transformer): Rep[_] = !!!("don't know how to mirror " + this)
    def decompose: Option[Rep[_]] = None
    def isScalarOp: Boolean = true
  }

  abstract class Transformer { outer =>
    type Self = this.type
    def apply[A](x: Rep[A]): Rep[A]
    def isDefinedAt(x: Rep[_]): Boolean
    def domain: Set[Rep[_]]
    protected def add(self: Self, kv: (Rep[_], Rep[_])): Self
    def +(kv: (Rep[_], Rep[_])) = add(this, kv)
    def ++(kvs: Map[Rep[_], Rep[_]]) = kvs.foldLeft(this: Self)((ctx, kv) => add(ctx,kv))
    def merge(other: Self): Self = other.domain.foldLeft[Self](outer)((t,s) => t + (s, other(s)))
    def apply[A](xs: List[Rep[A]]): List[Rep[A]] = xs map (e => apply(e))
    def apply[A](xs: Seq[Rep[A]]): Seq[Rep[A]] = xs map (e => apply(e))
    def apply[X,A](f: X=>Rep[A]): X=>Rep[A] = (z:X) => apply(f(z))
    def apply[X,Y,A](f: (X,Y)=>Rep[A]): (X,Y)=>Rep[A] = (z1:X,z2:Y) => apply(f(z1,z2))
  }

  trait CanBeReified[C[_]] {
    def resolve[A](sym: Rep[C[A]]): C[A]
  }

  implicit def reifyObject[A:Elem](obj: ReifiableObject[A]): Rep[A]
  implicit def toRep[A:Elem](x: A): Rep[A] = element[A].toRep(x)
}
