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
    def selfType: Elem[R]
    override def toString = "%s(%s)".format(this.getClass.getSimpleName, arg)
  }

  trait BinOpBase[TArg,R] {
    def lhs: Rep[TArg]
    def rhs: Rep[TArg]
    def copyWith(l: Rep[TArg], r:Rep[TArg]): Rep[R]
    def opName: String
    def selfType: Elem[R]
    override def toString = this.getClass.getSimpleName + "(" + lhs + ", " + rhs + ")"
  }

  def toRep[A](x: A)(implicit eA: Elem[A]): Rep[A] = !!!(s"Don't know how to create Rep for: $x") //= element[A].toRep(x)
  implicit def liftToRep[A:Elem](x: A) = toRep(x)
}
