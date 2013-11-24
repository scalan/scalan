package scalan.primitives

import scalan.staged.{BaseExp}
import scalan.{ScalanStaged, Scalan, ScalanSeq}
import scala.language.{implicitConversions}

trait TypeSum {  self: Scalan =>

  trait SumOps[A, B] {
    def eA: Elem[A]
    def eB: Elem[B]
    def isLeft: Rep[Boolean]
    def isRight: Rep[Boolean]
    def fold[R: Elem](l: Rep[A] => Rep[R], r: Rep[B] => Rep[R]): Rep[R]
  }

  implicit def pimpSum[A: Elem, B: Elem](s: Rep[(A | B)]): SumOps[A, B]

  def toLeft[A: Elem](a: Rep[A]): Rep[(A|Unit)]

  def toRight[A: Elem](a: Rep[A]): Rep[R[A]]

  def toLeftSum[A: Elem, B: Elem](a: Rep[A]): Rep[(A | B)]

  def toRightSum[A: Elem, B: Elem](a: Rep[B]): Rep[(A | B)]
}

trait TypeSumSeq extends TypeSum { self: ScalanSeq =>

  def toLeft[A: Elem](a: Rep[A]): Rep[(A|Unit)] = Left(a)

  def toRight[A: Elem](a: Rep[A]): Rep[R[A]] = Right(a)

  def toLeftSum[A: Elem, B: Elem](a: Rep[A]): Rep[(A | B)] = Left[A,B](a)

  def toRightSum[A: Elem, B: Elem](a: Rep[B]): Rep[(A | B)] = Right[A,B](a)

  class SeqSumOps[A: Elem, B: Elem](s: Rep[(A | B)]) extends SumOps[A,B] {
    def eA = element[A]; def eB = element[B]
    def fold[R: Elem](l: Rep[A] => Rep[R], r: Rep[B] => Rep[R]): Rep[R] = s.fold(l,r)
    def isLeft = s.isLeft
    def isRight = s.isRight
  }
  implicit def pimpSum[A: Elem, B: Elem](s: Rep[(A | B)]): SumOps[A, B] = new SeqSumOps[A,B](s)

}

trait TypeSumExp extends TypeSum with BaseExp { self: ScalanStaged =>

  case class Left[A:Elem, B:Elem](left: Exp[A]) extends Def[(A | B)] {
    override def mirror(t: Transformer) = Left[A,B](t(left))
  }

  case class Right[A:Elem, B:Elem](right: Exp[B]) extends Def[(A | B)] {
    override def mirror(t: Transformer) = Right[A,B](t(right))
  }

  def toLeft[A:Elem](a: Rep[A]): Rep[L[A]] = Left[A,Unit](a)
  def toRight[A:Elem](a: Rep[A]): Rep[R[A]] = Right[Unit,A](a)
  def toLeftSum[A:Elem,B:Elem](a: Rep[A]): Rep[(A|B)] = Left[A,B](a)
  def toRightSum[A:Elem,B:Elem](b: Rep[B]): Rep[(A|B)] = Right[A,B](b)

  case class IsLeft[A, B](sum: Exp[(A | B)]) extends Def[Boolean] {
    override def mirror(t: Transformer) = IsLeft(t(sum))
  }

  case class IsRight[A, B](sum: Exp[(A | B)]) extends Def[Boolean] {
    override def mirror(t: Transformer) = IsRight(t(sum))
  }

  case class SumFold[A, B, R](sum: Exp[(A | B)], left: Exp[A => R], right: Exp[B => R])
                             (implicit val eR: Elem[R]) extends Def[R] {
    override def mirror(t: Transformer) = SumFold(t(sum), t(left), t(right))
  }

  class StagedSumOps[A:Elem, B:Elem](s: Rep[(A | B)]) extends SumOps[A,B] {
    def eA = element[A]; def eB = element[B]
    def fold[R: Elem](l: Rep[A] => Rep[R], r: Rep[B] => Rep[R]): Rep[R] = SumFold(s, fun(l), fun(r))
    def isLeft = IsLeft(s)
    def isRight = IsRight(s)
  }
  implicit def pimpSum[A: Elem, B: Elem](s: Rep[(A | B)]): SumOps[A, B] = new StagedSumOps[A,B](s)

  override def rewrite[T](d: Def[T])(implicit eT: Elem[T]) = d match {
    case f@SumFold(Def(Left(left)), l, _) => {
      val eR: Elem[T] = f.eR.asInstanceOf[Elem[T]]
      mkApply(l, left)(left.Elem, eR)
    }
    case f@SumFold(Def(Right(right)), _, r) => {
      val eR: Elem[T] = f.eR.asInstanceOf[Elem[T]]
      mkApply(r, right)(right.Elem, eR)
    }
    //TODO case IsLeft, IsRight
    case _ => super.rewrite(d)
  }
}