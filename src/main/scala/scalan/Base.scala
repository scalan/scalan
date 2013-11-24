package scalan

import scala.language.higherKinds

trait Base {
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
  //implicit def extendRepForSome(x: Rep[_]) = new RepForSomeExtension(x)

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

}
